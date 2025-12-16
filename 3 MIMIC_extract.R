library(data.table)
library(parallel)
library(RMySQL)
library(survival)
library(dplyr)
library(ggfortify)
library(latexpdf)
library(Matching)
library(MatchIt)
library(DataExplorer)
library(stringr)
library(magrittr)  
library(ggplot2)
library(readr)
library(RSQLite)

#db_folder_path <- "/media/felixmichelson/EC76CD8D76CD58CA/" #AG Schock
db_folder_path <- "/media/scherer/T7/MIMIC/" #AG Schock
#db_folder_path <- "F:/" #Zuhause
db_file_path <- file.path(db_folder_path, "MIMIC_III_database.sqlite")


con <- dbConnect(RSQLite::SQLite(), dbname = db_file_path)


result <- dbGetQuery(con, "

WITH union_vasopress AS (
  SELECT SUBJECT_ID FROM INPUTEVENTS_CV
  WHERE ITEMID LIKE '30306' OR ITEMID LIKE '30042' OR ITEMID LIKE '30051' OR ITEMID LIKE '42273' OR ITEMID LIKE '42802' OR ITEMID LIKE '30119' OR ITEMID LIKE '30309' OR ITEMID LIKE '30044'
  UNION
  SELECT SUBJECT_ID FROM INPUTEVENTS_MV
  WHERE ITEMID LIKE '221653' OR ITEMID LIKE '222315' OR ITEMID LIKE '221289' OR ITEMID LIKE '221906'
)



SELECT SUBJECT_ID
FROM DIAGNOSES_ICD
WHERE ICD9_CODE IN (
  SELECT ICD9_CODE
  FROM D_ICD_DIAGNOSES
  WHERE LONG_TITLE LIKE '%card%'
  OR LONG_TITLE LIKE '%heart%'
  OR LONG_TITLE LIKE '%coro%'
  OR LONG_TITLE LIKE '%Pectoris%'
  OR LONG_TITLE LIKE '%myocardial%'
  OR LONG_TITLE LIKE '%arrhythmia%'
  OR LONG_TITLE LIKE '%atrial%'
  OR LONG_TITLE LIKE '%ventricular%'
  OR LONG_TITLE LIKE '%cardiomyopathy%'
  OR LONG_TITLE LIKE '%ischemic%'
  OR LONG_TITLE LIKE '%stenosis%'
  OR LONG_TITLE LIKE '%valvular%'
  OR LONG_TITLE LIKE '%valve%'
)

INTERSECT

SELECT SUBJECT_ID
FROM LABEVENTS
WHERE ITEMID LIKE '50813' AND VALUE > 2

INTERSECT

SELECT SUBJECT_ID
FROM union_vasopress


                      ")


patient_list <- unique(result$SUBJECT_ID)

mimic_dataset <- dbGetQuery(con, "SELECT SUBJECT_ID, ICUSTAY_ID, INTIME, OUTTIME FROM ICUSTAYS")

mimic_dataset <- filter(mimic_dataset, mimic_dataset$SUBJECT_ID %in% patient_list)

death_values <- dbGetQuery(con, " SELECT SUBJECT_ID, DEATHTIME from ADMISSIONS")
death_values <- filter(death_values, SUBJECT_ID %in% patient_list)
death_values <- filter(death_values, !is.na(DEATHTIME))
death_values <- distinct(death_values, SUBJECT_ID, .keep_all = TRUE)
mimic_dataset <- merge(mimic_dataset, death_values[, c("DEATHTIME", "SUBJECT_ID")], by = "SUBJECT_ID", all.x = TRUE)
mimic_dataset$OUTTIME <- ifelse((mimic_dataset$DEATHTIME < mimic_dataset$OUTTIME & !is.na(mimic_dataset$DEATHTIME)), mimic_dataset$DEATHTIME, mimic_dataset$OUTTIME)

mimic_dataset$DurationICUStay <- difftime(mimic_dataset$OUTTIME,mimic_dataset$INTIME,unit="days")

rows_to_delete <- which(mimic_dataset$DurationICUStay <= 1)
mimic_dataset <- mimic_dataset[-rows_to_delete, ]

colnames(mimic_dataset)[colnames(mimic_dataset) == "INTIME"] <- "ICUStay_begin"
colnames(mimic_dataset)[colnames(mimic_dataset) == "OUTTIME"] <- "ICUStay_end"
colnames(mimic_dataset)[colnames(mimic_dataset) == "ICUSTAY_ID"] <- "PatientID"

mimic_dataset$ICUStay_begin <- as.POSIXlt(as.character(mimic_dataset$ICUStay_begin))
mimic_dataset$ICUStay_end <- as.POSIXlt(as.character(mimic_dataset$ICUStay_end))

new_dataset <- list()

for (patient_id in unique(mimic_dataset$PatientID)) {
  patient_data_schleife <- mimic_dataset[mimic_dataset$PatientID == patient_id, ]
  
  patient_data_schleife <- na.omit(patient_data_schleife)
  
  if (nrow(patient_data_schleife) > 0) {
    new_dates <- seq(from = min(patient_data_schleife$ICUStay_begin), 
                     to = max(patient_data_schleife$ICUStay_end), 
                     by = "24 hours")
    
    new_patient_data <- data.frame(ID = rep(patient_id, length(new_dates)),
                                   Datum = new_dates)
    
    new_dataset[[patient_id]] <- new_patient_data
  }
}

new_dataset <- do.call(rbind, new_dataset)

new_dataset <- new_dataset %>%
  group_by(ID) %>%
  filter(Datum < max(Datum))

new_dataset <- new_dataset %>%
  group_by(ID) %>%
  mutate(DayNumber = row_number()) %>%
  ungroup()

colnames(mimic_dataset)[colnames(mimic_dataset) == "PatientID"] <- "ID"
new_dataset <- merge(new_dataset, mimic_dataset[, c("ID", "SUBJECT_ID", "ICUStay_end")], by = "ID", all.x = TRUE)

new_dataset$ID <- NULL
colnames(new_dataset)[colnames(new_dataset) == "SUBJECT_ID"] <- "ID"

patient_list_dates <- data.frame(ID = new_dataset$ID)

########################################################################################

Age_values <- dbGetQuery(con, " SELECT SUBJECT_ID, DOB from PATIENTS")
Age_values <- filter(Age_values, SUBJECT_ID %in% patient_list)
Age_values <- left_join(Age_values, mimic_dataset %>% distinct(SUBJECT_ID, .keep_all = TRUE) %>% dplyr::select(SUBJECT_ID, ICUStay_begin), by = "SUBJECT_ID")
Age_values$AgeOnInclusion <- difftime(Age_values$ICUStay_begin, Age_values$DOB, units = "days") / 365.25

Age_values$AgeOnInclusion <- ifelse(Age_values$AgeOnInclusion > 90, 90, Age_values$AgeOnInclusion)
Age_values$AgeOnInclusion <- ifelse(Age_values$AgeOnInclusion < 18, NA, Age_values$AgeOnInclusion)

Age_values$AgeOnInclusion <- str_replace_all(Age_values$AgeOnInclusion, "[^0-9.-]", "")
Age_values$AgeOnInclusion <- as.numeric(Age_values$AgeOnInclusion)

colnames(Age_values)[colnames(Age_values) == "SUBJECT_ID"] <- "ID"

Age_values <- filter(Age_values, !is.na(AgeOnInclusion))

new_dataset <- merge(new_dataset, Age_values[, c("ID", "AgeOnInclusion")], by = "ID", all.x = TRUE)

new_dataset <- filter(new_dataset, !is.na(AgeOnInclusion))
patient_list_dates <- data.frame(ID = new_dataset$ID)

########################################################################################

RRDia_values <- dbGetQuery(con, "SELECT SUBJECT_ID, CHARTTIME, VALUE	from CHARTEVENTS	WHERE ITEMID like '220051' OR ITEMID like '220180' OR ITEMID like '8368'")

RRSys_values <- dbGetQuery(con, "SELECT SUBJECT_ID, CHARTTIME, VALUE	from CHARTEVENTS	WHERE ITEMID like '220050' OR ITEMID like '220179' OR ITEMID like '51'")

so2_values <- dbGetQuery(con, "SELECT SUBJECT_ID, CHARTTIME, VALUE	from CHARTEVENTS	WHERE ITEMID like '220277'")

RRDia_values <- filter(RRDia_values, SUBJECT_ID %in% patient_list)
RRSys_values <- filter(RRSys_values, SUBJECT_ID %in% patient_list)
so2_values <- filter(so2_values, SUBJECT_ID %in% patient_list)

colnames(RRDia_values)[colnames(RRDia_values) == "VALUE"] <- "Value"
colnames(RRSys_values)[colnames(RRSys_values) == "VALUE"] <- "Value"
colnames(so2_values)[colnames(so2_values) == "VALUE"] <- "Value"

RRDia_values$Value <- str_replace_all(RRDia_values$Value, "[^0-9.-]", "")
RRDia_values$Value <- as.numeric(RRDia_values$Value)

RRSys_values$Value <- str_replace_all(RRSys_values$Value, "[^0-9.-]", "")
RRSys_values$Value <- as.numeric(RRSys_values$Value)

so2_values$Value <- str_replace_all(so2_values$Value, "[^0-9.-]", "")
so2_values$Value <- as.numeric(so2_values$Value)

colnames(RRDia_values)[colnames(RRDia_values) == "CHARTTIME"] <- "DateTime"
colnames(RRSys_values)[colnames(RRSys_values) == "CHARTTIME"] <- "DateTime"
colnames(so2_values)[colnames(so2_values) == "CHARTTIME"] <- "DateTime"

colnames(RRDia_values)[colnames(RRDia_values) == "SUBJECT_ID"] <- "PatientID"
colnames(RRSys_values)[colnames(RRSys_values) == "SUBJECT_ID"] <- "PatientID"
colnames(so2_values)[colnames(so2_values) == "SUBJECT_ID"] <- "PatientID"

RRDia_values <- filter(RRDia_values, !is.na(Value))
RRSys_values <- filter(RRSys_values, !is.na(Value))
so2_values <- filter(so2_values, !is.na(Value))

RRDia_values$DateTime <- as.POSIXlt(as.character(RRDia_values$DateTime))
RRSys_values$DateTime <- as.POSIXlt(as.character(RRSys_values$DateTime))
so2_values$DateTime <- as.POSIXlt(as.character(so2_values$DateTime))

RRDiaWeightedMeanValue <- get_weighted_mean_values_in_range_patient_list_parallel("RRDiaWeightedMeanValue", RRDia_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$RRDiaWeightedMeanValue <- RRDiaWeightedMeanValue$RRDiaWeightedMeanValue

RRSysWeightedMeanValue <- get_weighted_mean_values_in_range_patient_list_parallel("RRSysWeightedMeanValue", RRSys_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$RRSysWeightedMeanValue <- RRSysWeightedMeanValue$RRSysWeightedMeanValue

sO2WeightedMeanValue <- get_weighted_mean_values_in_range_patient_list_parallel("sO2WeightedMeanValue", so2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$sO2WeightedMeanValue <- sO2WeightedMeanValue$sO2WeightedMeanValue


new_dataset$RRDiaWeightedMeanValue <- ifelse(new_dataset$RRDiaWeightedMeanValue > 140, NA, new_dataset$RRDiaWeightedMeanValue)
new_dataset$RRDiaWeightedMeanValue <- ifelse(new_dataset$RRDiaWeightedMeanValue < 20, NA, new_dataset$RRDiaWeightedMeanValue)
new_dataset$RRSysWeightedMeanValue <- ifelse(new_dataset$RRSysWeightedMeanValue > 240, NA, new_dataset$RRSysWeightedMeanValue)
new_dataset$RRSysWeightedMeanValue <- ifelse(new_dataset$RRSysWeightedMeanValue < 30, NA, new_dataset$RRSysWeightedMeanValue)
new_dataset$sO2WeightedMeanValue <- ifelse(new_dataset$sO2WeightedMeanValue > 100, NA, new_dataset$sO2WeightedMeanValue)

######################################################################################

death_values <- dbGetQuery(con, " SELECT SUBJECT_ID, DEATHTIME, HOSPITAL_EXPIRE_FLAG from ADMISSIONS")
death_values <- filter(death_values, SUBJECT_ID %in% patient_list)

colnames(death_values)[colnames(death_values) == "DEATHTIME"] <- "DateTime"
colnames(death_values)[colnames(death_values) == "SUBJECT_ID"] <- "PatientID"
colnames(death_values)[colnames(death_values) == "HOSPITAL_EXPIRE_FLAG"] <- "Value"

death_values <- filter(death_values, !is.na(DateTime))

death_values$DateTime <- as.POSIXlt(death_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
death_values$DateTimeEnd <- death_values$DateTime
death_values$DateTimeEnd <- as.POSIXlt(death_values$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")

Death_in24h <- check_in_range_3("Death_in24h", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*48)
Death_in3d <- check_in_range_3("Death_in3d", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*4)
Death_inICUStay <- check_in_range_3("Death_inICUStay", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$ICUStay_end))
Death_in1w <- check_in_range_3("Death_in1w", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*8)
Death_in2w <- check_in_range_3("Death_in2w", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*15)
Death_in3w <- check_in_range_3("Death_in3w", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*22)

Death_in24h$Death_in24h <- grepl("1",tolower(Death_in24h$Death_in24h))
Death_in3d$Death_in3d <- grepl("1",tolower(Death_in3d$Death_in3d))
Death_inICUStay$Death_inICUStay <- grepl("1",tolower(Death_inICUStay$Death_inICUStay))
Death_in1w$Death_in1w <- grepl("1",tolower(Death_in1w$Death_in1w))
Death_in2w$Death_in2w <- grepl("1",tolower(Death_in2w$Death_in2w))
Death_in3w$Death_in3w <- grepl("1",tolower(Death_in3w$Death_in3w))

new_dataset$Death_in24h <- Death_in24h$Death_in24h
new_dataset$Death_in3d <- Death_in3d$Death_in3d
new_dataset$Death_inICUStay <- Death_inICUStay$Death_inICUStay
new_dataset$Death_in1w <- Death_in1w$Death_in1w
new_dataset$Death_in2w <- Death_in2w$Death_in2w
new_dataset$Death_in3w <- Death_in3w$Death_in3w

new_dataset <- replace(new_dataset, new_dataset == TRUE, 1)
new_dataset <- replace(new_dataset, new_dataset == FALSE, 0)


colnames(death_values)[colnames(death_values) == "PatientID"] <- "ID"
death_values <- filter(death_values, death_values$Value == 1)
death_values <- death_values %>% distinct(ID, .keep_all = TRUE)
new_dataset <- merge(new_dataset, death_values[, c("ID", "DateTime")], by = "ID", all.x = TRUE)
new_dataset$DaystillDeath <- difftime(new_dataset$DateTime,new_dataset$Datum,unit="days")
new_dataset$DateTime <- NULL


####################################################################################


bili_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '50885'")

bili_values <- filter(bili_values, SUBJECT_ID %in% patient_list)

colnames(bili_values)[colnames(bili_values) == "VALUE"] <- "Value"
colnames(bili_values)[colnames(bili_values) == "SUBJECT_ID"] <- "PatientID"
colnames(bili_values)[colnames(bili_values) == "CHARTTIME"] <- "DateTime"

bili_values$DateTime <- as.POSIXlt(bili_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

bili_values$Value <- str_replace_all(bili_values$Value, "[^0-9.-]", "")
bili_values$Value <- as.numeric(bili_values$Value)

BilirubinWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("BilirubinWeightedMean", bili_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$BilirubinWeightedMean <- BilirubinWeightedMean$BilirubinWeightedMean


####################################################################################

ph_values <- dbGetQuery(con, "  SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '50820'  ")

ph_values <- filter(ph_values, SUBJECT_ID %in% patient_list)

colnames(ph_values)[colnames(ph_values) == "VALUE"] <- "Value"
colnames(ph_values)[colnames(ph_values) == "SUBJECT_ID"] <- "PatientID"
colnames(ph_values)[colnames(ph_values) == "CHARTTIME"] <- "DateTime"

ph_values$DateTime <- as.POSIXlt(ph_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

ph_values$Value <- str_replace_all(ph_values$Value, "[^0-9.-]", "")
ph_values <- filter(ph_values, !is.na(Value))
ph_values$Value <- as.numeric(ph_values$Value)

PHWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("PHWeightedMean", ph_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$PHWeightedMean <- PHWeightedMean$PHWeightedMean

LowestpH <- get_min_values_in_range_patient_list_parallel("LowestpH", ph_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$LowestpH <- LowestpH$LowestpH


####################################################################################

lac_values <- dbGetQuery(con, "  SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '50813'  ")

lac_values <- filter(lac_values, SUBJECT_ID %in% patient_list)

colnames(lac_values)[colnames(lac_values) == "VALUE"] <- "Value"
colnames(lac_values)[colnames(lac_values) == "SUBJECT_ID"] <- "PatientID"
colnames(lac_values)[colnames(lac_values) == "CHARTTIME"] <- "DateTime"

lac_values$DateTime <- as.POSIXlt(lac_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
lac_values <- filter(lac_values, !is.na(Value))

lac_values$Value <- str_replace_all(lac_values$Value, "[^0-9.-]", "")
lac_values$Value <- as.numeric(lac_values$Value)

LactateWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("LactateWeightedMean", lac_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$LactateWeightedMean <- LactateWeightedMean$LactateWeightedMean

HighestLactate <- get_max_values_in_range_patient_list_parallel("HighestLactate", lac_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$HighestLactate <- HighestLactate$HighestLactate

####################################################################################

crea_values <- dbGetQuery(con, "  SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '50912'  ")

crea_values <- filter(crea_values, SUBJECT_ID %in% patient_list)

colnames(crea_values)[colnames(crea_values) == "VALUE"] <- "Value"
colnames(crea_values)[colnames(crea_values) == "SUBJECT_ID"] <- "PatientID"
colnames(crea_values)[colnames(crea_values) == "CHARTTIME"] <- "DateTime"

crea_values$DateTime <- as.POSIXlt(crea_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
crea_values <- filter(crea_values, !is.na(Value))

crea_values$Value <- str_replace_all(crea_values$Value, "[^0-9.-]", "")
crea_values$Value <- as.numeric(crea_values$Value)

CreatinineWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("CreatinineWeightedMean", crea_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$CreatinineWeightedMean <- CreatinineWeightedMean$CreatinineWeightedMean

####################################################################################

glu_values <- dbGetQuery(con, "  SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '50931'  ")

glu_values <- filter(glu_values, SUBJECT_ID %in% patient_list)

colnames(glu_values)[colnames(glu_values) == "VALUE"] <- "Value"
colnames(glu_values)[colnames(glu_values) == "SUBJECT_ID"] <- "PatientID"
colnames(glu_values)[colnames(glu_values) == "CHARTTIME"] <- "DateTime"

glu_values$DateTime <- as.POSIXlt(glu_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
glu_values <- filter(glu_values, !is.na(Value))

glu_values$Value <- str_replace_all(glu_values$Value, "[^0-9.-]", "")
glu_values$Value <- as.numeric(glu_values$Value)

gluWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("gluWeightedMean", glu_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$gluWeightedMean <- gluWeightedMean$gluWeightedMean

####################################################################################

be_values <- dbGetQuery(con, "  SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '50802'  ")

be_values <- filter(be_values, SUBJECT_ID %in% patient_list)

colnames(be_values)[colnames(be_values) == "VALUE"] <- "Value"
colnames(be_values)[colnames(be_values) == "SUBJECT_ID"] <- "PatientID"
colnames(be_values)[colnames(be_values) == "CHARTTIME"] <- "DateTime"

be_values$DateTime <- as.POSIXlt(be_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

be_values$Value <- str_replace_all(be_values$Value, "[^0-9.-]", "")
be_values <- filter(be_values, !is.na(Value))
be_values$Value <- as.numeric(be_values$Value)

BEWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("BEWeightedMean", be_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$BEWeightedMean <- BEWeightedMean$BEWeightedMean

####################################################################################


hoc3_values <- dbGetQuery(con, "  SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '50882'  ")

hoc3_values <- filter(hoc3_values, SUBJECT_ID %in% patient_list)

colnames(hoc3_values)[colnames(hoc3_values) == "VALUE"] <- "Value"
colnames(hoc3_values)[colnames(hoc3_values) == "SUBJECT_ID"] <- "PatientID"
colnames(hoc3_values)[colnames(hoc3_values) == "CHARTTIME"] <- "DateTime"

hoc3_values$DateTime <- as.POSIXlt(hoc3_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

hoc3_values$Value <- str_replace_all(hoc3_values$Value, "[^0-9.-]", "")
hoc3_values <- filter(hoc3_values, !is.na(Value))
hoc3_values$Value <- as.numeric(hoc3_values$Value)

HCO3WeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("HCO3WeightedMean", hoc3_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$HCO3WeightedMean <- HCO3WeightedMean$HCO3WeightedMean


####################################################################################



PO2_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '50821'")

PO2_values <- filter(PO2_values, SUBJECT_ID %in% patient_list)

colnames(PO2_values)[colnames(PO2_values) == "VALUE"] <- "Value"
colnames(PO2_values)[colnames(PO2_values) == "SUBJECT_ID"] <- "PatientID"
colnames(PO2_values)[colnames(PO2_values) == "CHARTTIME"] <- "DateTime"

PO2_values$DateTime <- as.POSIXlt(PO2_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

PO2_values$Value <- str_replace_all(PO2_values$Value, "[^0-9.-]", "")
PO2_values$Value <- as.numeric(PO2_values$Value)

PO2WeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("PO2WeightedMean", PO2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$PO2WeightedMean <- PO2WeightedMean$PO2WeightedMean

####################################################################################


PCO2_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '50818'")

PCO2_values <- filter(PCO2_values, SUBJECT_ID %in% patient_list)

colnames(PCO2_values)[colnames(PCO2_values) == "VALUE"] <- "Value"
colnames(PCO2_values)[colnames(PCO2_values) == "SUBJECT_ID"] <- "PatientID"
colnames(PCO2_values)[colnames(PCO2_values) == "CHARTTIME"] <- "DateTime"

PCO2_values$DateTime <- as.POSIXlt(PCO2_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

PCO2_values$Value <- str_replace_all(PCO2_values$Value, "[^0-9.-]", "")
PCO2_values$Value <- as.numeric(PCO2_values$Value)

PCO2WeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("PCO2WeightedMean", PCO2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$PCO2WeightedMean <- PCO2WeightedMean$PCO2WeightedMean

####################################################################################


HB_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '51222'")

HB_values <- filter(HB_values, SUBJECT_ID %in% patient_list)

colnames(HB_values)[colnames(HB_values) == "VALUE"] <- "Value"
colnames(HB_values)[colnames(HB_values) == "SUBJECT_ID"] <- "PatientID"
colnames(HB_values)[colnames(HB_values) == "CHARTTIME"] <- "DateTime"

HB_values$DateTime <- as.POSIXlt(HB_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

HB_values$Value <- str_replace_all(HB_values$Value, "[^0-9.-]", "")
HB_values$Value <- as.numeric(HB_values$Value)

HBWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("HBWeightedMean", HB_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$HBWeightedMean <- HBWeightedMean$HBWeightedMean

####################################################################################


leuco_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '51301'")

leuco_values <- filter(leuco_values, SUBJECT_ID %in% patient_list)

colnames(leuco_values)[colnames(leuco_values) == "VALUE"] <- "Value"
colnames(leuco_values)[colnames(leuco_values) == "SUBJECT_ID"] <- "PatientID"
colnames(leuco_values)[colnames(leuco_values) == "CHARTTIME"] <- "DateTime"

leuco_values$DateTime <- as.POSIXlt(leuco_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

leuco_values$Value <- str_replace_all(leuco_values$Value, "[^0-9.-]", "")
leuco_values$Value <- as.numeric(leuco_values$Value)

leucoWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("leucoWeightedMean", leuco_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$leucoWeightedMean <- leucoWeightedMean$leucoWeightedMean

####################################################################################


urea_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '51006'")

urea_values <- filter(urea_values, SUBJECT_ID %in% patient_list)

colnames(urea_values)[colnames(urea_values) == "VALUE"] <- "Value"
colnames(urea_values)[colnames(urea_values) == "SUBJECT_ID"] <- "PatientID"
colnames(urea_values)[colnames(urea_values) == "CHARTTIME"] <- "DateTime"

urea_values$DateTime <- as.POSIXlt(urea_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

urea_values$Value <- str_replace_all(urea_values$Value, "[^0-9.-]", "")
urea_values$Value <- as.numeric(urea_values$Value)

ureaWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("ureaWeightedMean", urea_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$ureaWeightedMean <- ureaWeightedMean$ureaWeightedMean

####################################################################################


platelets_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '51265'")

platelets_values <- filter(platelets_values, SUBJECT_ID %in% patient_list)

colnames(platelets_values)[colnames(platelets_values) == "VALUE"] <- "Value"
colnames(platelets_values)[colnames(platelets_values) == "SUBJECT_ID"] <- "PatientID"
colnames(platelets_values)[colnames(platelets_values) == "CHARTTIME"] <- "DateTime"

platelets_values$DateTime <- as.POSIXlt(platelets_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

platelets_values$Value <- str_replace_all(platelets_values$Value, "[^0-9.-]", "")
platelets_values$Value <- as.numeric(platelets_values$Value)

plateletsWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("plateletsWeightedMean", platelets_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$plateletsWeightedMean <- plateletsWeightedMean$plateletsWeightedMean

####################################################################################

GPT_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '50861'")

GPT_values <- filter(GPT_values, SUBJECT_ID %in% patient_list)

colnames(GPT_values)[colnames(GPT_values) == "VALUE"] <- "Value"
colnames(GPT_values)[colnames(GPT_values) == "SUBJECT_ID"] <- "PatientID"
colnames(GPT_values)[colnames(GPT_values) == "CHARTTIME"] <- "DateTime"

GPT_values$DateTime <- as.POSIXlt(GPT_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

GPT_values$Value <- str_replace_all(GPT_values$Value, "[^0-9.-]", "")
GPT_values$Value <- as.numeric(GPT_values$Value)

GPTWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("GPTWeightedMean", GPT_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$GPTWeightedMean <- GPTWeightedMean$GPTWeightedMean

####################################################################################


GammaGT_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '50927'")

GammaGT_values <- filter(GammaGT_values, SUBJECT_ID %in% patient_list)

colnames(GammaGT_values)[colnames(GammaGT_values) == "VALUE"] <- "Value"
colnames(GammaGT_values)[colnames(GammaGT_values) == "SUBJECT_ID"] <- "PatientID"
colnames(GammaGT_values)[colnames(GammaGT_values) == "CHARTTIME"] <- "DateTime"

GammaGT_values$DateTime <- as.POSIXlt(GammaGT_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

GammaGT_values$Value <- str_replace_all(GammaGT_values$Value, "[^0-9.-]", "")
GammaGT_values$Value <- as.numeric(GammaGT_values$Value)

GammaGTWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("GammaGTWeightedMean", GammaGT_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$GammaGTWeightedMean <- GammaGTWeightedMean$GammaGTWeightedMean

####################################################################################


CK_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '50910'")

CK_values <- filter(CK_values, SUBJECT_ID %in% patient_list)

colnames(CK_values)[colnames(CK_values) == "VALUE"] <- "Value"
colnames(CK_values)[colnames(CK_values) == "SUBJECT_ID"] <- "PatientID"
colnames(CK_values)[colnames(CK_values) == "CHARTTIME"] <- "DateTime"

CK_values$DateTime <- as.POSIXlt(CK_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

CK_values$Value <- str_replace_all(CK_values$Value, "[^0-9.-]", "")
CK_values$Value <- as.numeric(CK_values$Value)

CKWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("CKWeightedMean", CK_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$CKWeightedMean <- CKWeightedMean$CKWeightedMean


####################################################################################


K_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '50971'")

K_values <- filter(K_values, SUBJECT_ID %in% patient_list)

colnames(K_values)[colnames(K_values) == "VALUE"] <- "Value"
colnames(K_values)[colnames(K_values) == "SUBJECT_ID"] <- "PatientID"
colnames(K_values)[colnames(K_values) == "CHARTTIME"] <- "DateTime"

K_values$DateTime <- as.POSIXlt(K_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

K_values$Value <- str_replace_all(K_values$Value, "[^0-9.-]", "")
K_values$Value <- as.numeric(K_values$Value)

KWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("KWeightedMean", K_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$KWeightedMean <- KWeightedMean$KWeightedMean

####################################################################################


Na_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '50983'")

Na_values <- filter(Na_values, SUBJECT_ID %in% patient_list)

colnames(Na_values)[colnames(Na_values) == "VALUE"] <- "Value"
colnames(Na_values)[colnames(Na_values) == "SUBJECT_ID"] <- "PatientID"
colnames(Na_values)[colnames(Na_values) == "CHARTTIME"] <- "DateTime"

Na_values$DateTime <- as.POSIXlt(Na_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

Na_values$Value <- str_replace_all(Na_values$Value, "[^0-9.-]", "")
Na_values$Value <- as.numeric(Na_values$Value)

NaWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("NaWeightedMean", Na_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$NaWeightedMean <- NaWeightedMean$NaWeightedMean

####################################################################################


Cl_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM LABEVENTS WHERE ITEMID LIKE '50902'")

Cl_values <- filter(Cl_values, SUBJECT_ID %in% patient_list)

colnames(Cl_values)[colnames(Cl_values) == "VALUE"] <- "Value"
colnames(Cl_values)[colnames(Cl_values) == "SUBJECT_ID"] <- "PatientID"
colnames(Cl_values)[colnames(Cl_values) == "CHARTTIME"] <- "DateTime"

Cl_values$DateTime <- as.POSIXlt(Cl_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

Cl_values$Value <- str_replace_all(Cl_values$Value, "[^0-9.-]", "")
Cl_values$Value <- as.numeric(Cl_values$Value)

ClWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("ClWeightedMean", Cl_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$ClWeightedMean <- ClWeightedMean$ClWeightedMean

####################################################################################

HR_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM CHARTEVENTS WHERE ITEMID LIKE '211' OR ITEMID LIKE '220045' ")

HR_values <- filter(HR_values, SUBJECT_ID %in% patient_list)

colnames(HR_values)[colnames(HR_values) == "VALUE"] <- "Value"
colnames(HR_values)[colnames(HR_values) == "SUBJECT_ID"] <- "PatientID"
colnames(HR_values)[colnames(HR_values) == "CHARTTIME"] <- "DateTime"

HR_values$DateTime <- as.POSIXlt(HR_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

HR_values$Value <- str_replace_all(HR_values$Value, "[^0-9.-]", "")
HR_values$Value <- as.numeric(HR_values$Value)
HR_values <- filter(HR_values, !is.na(Value))

HRWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("HRWeightedMean", HR_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$HRWeightedMean <- HRWeightedMean$HRWeightedMean

####################################################################################


Temp_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM CHARTEVENTS WHERE ITEMID LIKE '223761' OR  ITEMID LIKE '678' ")

Temp_values <- filter(Temp_values, SUBJECT_ID %in% patient_list)

colnames(Temp_values)[colnames(Temp_values) == "VALUE"] <- "fahrenheit"
Temp_values$VALUE <- (Temp_values$fahrenheit - 32) * 5/9
Temp_values$fahrenheit <- NULL

Temp_values_cel <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM CHARTEVENTS WHERE ITEMID LIKE '223762' OR  ITEMID LIKE '676' ")

Temp_values <- rbind(Temp_values, Temp_values_cel)

colnames(Temp_values)[colnames(Temp_values) == "VALUE"] <- "Value"
colnames(Temp_values)[colnames(Temp_values) == "SUBJECT_ID"] <- "PatientID"
colnames(Temp_values)[colnames(Temp_values) == "CHARTTIME"] <- "DateTime"

Temp_values$DateTime <- as.POSIXlt(Temp_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

Temp_values$Value <- str_replace_all(Temp_values$Value, "[^0-9.-]", "")
Temp_values$Value <- as.numeric(Temp_values$Value)
Temp_values <- filter(Temp_values, !is.na(Value))

TempWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("TempWeightedMean", Temp_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$TempWeightedMean <- TempWeightedMean$TempWeightedMean

####################################################################################


height_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM CHARTEVENTS WHERE ITEMID LIKE '226730'")
height_values <- filter(height_values, SUBJECT_ID %in% patient_list)


colnames(height_values)[colnames(height_values) == "VALUE"] <- "Height"
colnames(height_values)[colnames(height_values) == "SUBJECT_ID"] <- "ID"
colnames(height_values)[colnames(height_values) == "CHARTTIME"] <- "DateTime"

height_values$DateTime <- as.POSIXlt(height_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

height_values <- height_values[!duplicated(height_values$ID), ]

new_dataset <- merge(new_dataset, height_values[, c("ID", "Height")], by = "ID", all.x = TRUE)

########################################################################################


weight_values <- dbGetQuery(con, " SELECT SUBJECT_ID, CHARTTIME,VALUE FROM CHARTEVENTS WHERE ITEMID LIKE '3693' OR ITEMID LIKE '226512'")
weight_values <- filter(weight_values, SUBJECT_ID %in% patient_list)


colnames(weight_values)[colnames(weight_values) == "VALUE"] <- "Weight"
colnames(weight_values)[colnames(weight_values) == "SUBJECT_ID"] <- "ID"
colnames(weight_values)[colnames(weight_values) == "CHARTTIME"] <- "DateTime"

weight_values$DateTime <- as.POSIXlt(weight_values$DateTime, format = "%Y-%m-%d %H:%M:%S")

weight_values <- weight_values[!duplicated(weight_values$ID), ]

new_dataset <- merge(new_dataset, weight_values[, c("ID", "Weight")], by = "ID", all.x = TRUE)

########################################################################################

path <- "/home/scherer"  
filename <- paste0("MIMIC_last.csv")
filepath <- file.path(path, filename)


write.csv(new_dataset, file = filepath, fileEncoding = "UTF-8")

