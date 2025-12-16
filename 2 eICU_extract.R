library(data.table)
library(parallel)
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
db_folder_path <- "/media/scherer/T7/eICU/" #AG Schock
db_folder_path <- "F:/eICU/" #Zuhause
db_file_path <- file.path(db_folder_path, "eICU_database.sqlite")

con <- dbConnect(RSQLite::SQLite(), dbname = db_file_path)




result <- dbGetQuery(con, "
 

-- Schritt 1: UNION der ersten beiden Abfragen (OR-Verknüpfung)
WITH union_diag_admit AS (
    SELECT patientunitstayid FROM diagnosis
    WHERE diagnosisstring LIKE '%card%' OR diagnosisstring LIKE '%heart%'
    UNION
    SELECT patientunitstayid FROM admissionDx
    WHERE admitdxpath LIKE '%card%' OR admitdxpath LIKE '%heart%'
),

-- Schritt 2: UNION der nächsten beiden Abfragen (OR-Verknüpfung)
union_lab_med AS (
    SELECT patientunitstayid FROM infusionDrug
    WHERE drugName LIKE '%epinephrine%' OR drugName LIKE '%dobutamine%' OR drugName LIKE '%vasopressin%'
    UNION
    SELECT patientunitstayid FROM medication
    WHERE drugName LIKE '%epinephrine%' OR drugName LIKE '%dobutamine%' OR drugName LIKE '%vasopressin%'
)

-- Schritt 3: INTERSECT der UNION-Ergebnisse mit den anderen Abfragen (NOT-Verknüpfung)

SELECT patientunitstayid
FROM union_diag_admit
INTERSECT
SELECT patientunitstayid
FROM (
    SELECT patientunitstayid FROM lab
    WHERE labname LIKE '%lac%' AND labresult > 2
) AS lab
INTERSECT
SELECT patientunitstayid
FROM union_lab_med
INTERSECT
SELECT patientunitstayid
FROM (
    SELECT patientunitstayid FROM patient
    WHERE hospitalDischargeStatus IN ('Alive', 'Expired')
    AND hospitalDischargeOffset IS NOT NULL
) AS pat;


                      ")
 
 patient_list <- result$patientunitstayid
 eicu_dataset <- dbGetQuery(con, "SELECT patientunitstayid, unitDischargeOffset FROM patient")
 eicu_dataset <- filter(eicu_dataset, patientunitstayid %in% patient_list)
 
 eicu_dataset <- eicu_dataset %>% mutate(unitdischargeoffset = unitdischargeoffset / 1440)
 
 rows_to_delete <- which(eicu_dataset$unitdischargeoffset <= 1)
 eicu_dataset <- eicu_dataset[-rows_to_delete, ]
 
 eicu_dataset$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 
 eicu_dataset$ICUStay_end <- eicu_dataset$ICUStay_begin + as.numeric(eicu_dataset$unitdischargeoffset) * 86400 
 
 
  patient_list <- unique(eicu_dataset$patientunitstayid)

 
 
 
 new_dataset <- list()
 
 for (patient_id in unique(eicu_dataset$patientunitstayid)) {
   patient_data_schleife <- eicu_dataset[eicu_dataset$patientunitstayid == patient_id, ]
   
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
 
 patient_list_dates <- data.frame(ID = new_dataset$ID)
 

 ###########################################################################
 
 RRDia_values <- dbGetQuery(con, "SELECT patientunitstayid, nursingchartoffset, nursingchartvalue, nursingchartcelltypevalname FROM nurseCharting WHERE nursingchartcelltypevalname like '%dia%'")
 
 RRSys_values <- dbGetQuery(con, "SELECT patientunitstayid, nursingchartoffset, nursingchartvalue, nursingchartcelltypevalname FROM nurseCharting WHERE nursingchartcelltypevalname like '%sys%'")
 
 so2_values <- dbGetQuery(con, "SELECT patientunitstayid, nursingchartoffset, nursingchartvalue, nursingchartcelltypevalname FROM nurseCharting  WHERE nursingchartcelltypevallabel like '%O2 Saturation%'")
 
 RRDia_values <- filter(RRDia_values, patientunitstayid %in% patient_list)
 RRSys_values <- filter(RRSys_values, patientunitstayid %in% patient_list)
 so2_values <- filter(so2_values, patientunitstayid %in% patient_list)
 
 colnames(RRDia_values)[colnames(RRDia_values) == "nursingchartvalue"] <- "Value"
 colnames(RRSys_values)[colnames(RRSys_values) == "nursingchartvalue"] <- "Value"
 colnames(so2_values)[colnames(so2_values) == "nursingchartvalue"] <- "Value"
 
 RRDia_values$Value <- str_replace_all(RRDia_values$Value, "[^0-9.-]", "")
 RRDia_values$Value <- as.numeric(RRDia_values$Value)
 
 RRSys_values$Value <- str_replace_all(RRSys_values$Value, "[^0-9.-]", "")
 RRSys_values$Value <- as.numeric(RRSys_values$Value)
 
 so2_values$Value <- str_replace_all(so2_values$Value, "[^0-9.-]", "")
 so2_values$Value <- as.numeric(so2_values$Value)
 
 RRDia_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 RRDia_values$DateTime <- RRDia_values$ICUStay_begin + as.numeric(RRDia_values$nursingchartoffset) * 60 
 RRDia_values$ICUStay_begin <- NULL
 RRDia_values$nursingchartoffset <- NULL
 RRDia_values$nursingchartcelltypevalname <- NULL
 
 
 RRSys_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 RRSys_values$DateTime <- RRSys_values$ICUStay_begin + as.numeric(RRSys_values$nursingchartoffset) * 60 
 RRSys_values$ICUStay_begin <- NULL
 RRSys_values$nursingchartoffset <- NULL
 RRSys_values$nursingchartcelltypevalname <- NULL
 
 so2_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 so2_values$DateTime <- so2_values$ICUStay_begin + as.numeric(so2_values$nursingchartoffset) * 60 
 so2_values$ICUStay_begin <- NULL
 so2_values$nursingchartoffset <- NULL
 so2_values$nursingchartcelltypevalname <- NULL
 
 colnames(RRDia_values)[colnames(RRDia_values) == "patientunitstayid"] <- "PatientID"
 colnames(RRSys_values)[colnames(RRSys_values) == "patientunitstayid"] <- "PatientID"
 colnames(so2_values)[colnames(so2_values) == "patientunitstayid"] <- "PatientID"
 
 RRDia_values <- filter(RRDia_values, !is.na(Value))
 RRSys_values <- filter(RRSys_values, !is.na(Value))
 so2_values <- filter(so2_values, !is.na(Value))
 
 RRDiaWeightedMeanValue <- get_weighted_mean_values_in_range_patient_list_parallel("RRDiaWeightedMeanValue", RRDia_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$RRDiaWeightedMeanValue <- RRDiaWeightedMeanValue$RRDiaWeightedMeanValue
 
 RRSysWeightedMeanValue <- get_weighted_mean_values_in_range_patient_list_parallel("RRSysWeightedMeanValue", RRSys_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$RRSysWeightedMeanValue <- RRSysWeightedMeanValue$RRSysWeightedMeanValue
 
 sO2WeightedMeanValue <- get_weighted_mean_values_in_range_patient_list_parallel("sO2WeightedMeanValue", so2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$sO2WeightedMeanValue <- sO2WeightedMeanValue$sO2WeightedMeanValue
 
 print('hi')
 #####################################################################################
 
 Age_values <- dbGetQuery(con, " SELECT patientunitstayid, age FROM patient ")
 
 Age_values <- filter(Age_values, patientunitstayid %in% patient_list)
 
 colnames(Age_values)[colnames(Age_values) == "age"] <- "AgeOnInclusion"
 
 Age_values$AgeOnInclusion <- ifelse(Age_values$AgeOnInclusion == "> 89", 95, Age_values$AgeOnInclusion)
 
 Age_values$AgeOnInclusion <- str_replace_all(Age_values$AgeOnInclusion, "[^0-9.-]", "")
 Age_values$AgeOnInclusion <- as.numeric(Age_values$AgeOnInclusion)
 
 colnames(Age_values)[colnames(Age_values) == "patientunitstayid"] <- "ID"
 
 Age_values <- filter(Age_values, !is.na(AgeOnInclusion))
 
 new_dataset <- merge(new_dataset, Age_values[, c("ID", "AgeOnInclusion")], by = "ID", all.x = TRUE)
 
 #####################################################################################
 
 ethnicity_values <- dbGetQuery(con, " SELECT patientunitstayid, ethnicity FROM patient ")
 
 ethnicity_values <- filter(ethnicity_values, patientunitstayid %in% patient_list)
 
 colnames(ethnicity_values)[colnames(ethnicity_values) == "patientunitstayid"] <- "ID"
 
 ethnicity_values <- filter(ethnicity_values, !is.na(ethnicity))
 
 new_dataset <- merge(new_dataset, ethnicity_values[, c("ID", "ethnicity")], by = "ID", all.x = TRUE)
 
 #####################################################################################
 
 gender_values <- dbGetQuery(con, " SELECT patientunitstayid, gender FROM patient ")
 
 gender_values <- filter(gender_values, patientunitstayid %in% patient_list)
 
 colnames(gender_values)[colnames(gender_values) == "patientunitstayid"] <- "ID"
 
 gender_values <- filter(gender_values, !is.na(gender))
 
 new_dataset <- merge(new_dataset, gender_values[, c("ID", "gender")], by = "ID", all.x = TRUE)
 
########################################################################################
 
 death_values <- dbGetQuery(con, " SELECT patientunitstayid, unitDischargeStatus, unitDischargeOffset  FROM patient ")

 death_values <- filter(death_values, patientunitstayid %in% patient_list)
 
 colnames(death_values)[colnames(death_values) == "unitdischargestatus"] <- "Value"
 colnames(death_values)[colnames(death_values) == "patientunitstayid"] <- "PatientID"
 
 death_values$Value <- ifelse(death_values$Value == "Alive", FALSE, death_values$Value)
 death_values$Value <- ifelse(death_values$Value == "Expired", TRUE, death_values$Value)
 death_values$Value <- ifelse(death_values$Value == "NULL", NA, death_values$Value)
 
 death_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 death_values$DateTime <- death_values$ICUStay_begin + as.numeric(death_values$unitdischargeoffset) * 60 
 death_values$ICUStay_begin <- NULL
 death_values$unitdischargeoffset <- NULL

 death_values$DateTime <- as.POSIXlt(death_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 death_values$DateTimeEnd <- death_values$DateTime
 death_values$DateTimeEnd <- as.POSIXlt(death_values$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")
 
 Death_in24h <- check_in_range_3_parallel("Death_in24h", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*48)
 Death_in3d <- check_in_range_3("Death_in3d", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*4)
 Death_inICUStay <- check_in_range_3("Death_inICUStay", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*250000000000000000000)
 Death_in1w <- check_in_range_3("Death_in1w", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*8)
 Death_in2w <- check_in_range_3("Death_in2w", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*15)
 Death_in3w <- check_in_range_3("Death_in3w", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*22)
 
 Death_in24h <- Death_in24h %>%
   mutate(Death_in24h = ifelse((Death_in24h == FALSE | Death_in24h == 'NA'), FALSE, Death_in24h))
 
 Death_in3d <- Death_in3d %>%
   mutate(Death_in3d = ifelse((Death_in3d == FALSE | Death_in3d == 'NA'), FALSE, Death_in3d))
 
 Death_inICUStay <- Death_inICUStay %>%
   mutate(Death_inICUStay = ifelse((Death_inICUStay == FALSE | Death_inICUStay == 'NA'), FALSE, Death_inICUStay))
 
 Death_in1w <- Death_in1w %>%
   mutate(Death_in1w = ifelse((Death_in1w == FALSE | Death_in1w == 'NA'), FALSE, Death_in1w))
 
 Death_in2w <- Death_in2w %>%
   mutate(Death_in2w = ifelse((Death_in2w == FALSE | Death_in2w == 'NA'), FALSE, Death_in2w))
 
 Death_in3w <- Death_in3w %>%
   mutate(Death_in3w = ifelse((Death_in3w == FALSE | Death_in3w == 'NA'), FALSE, Death_in3w))
 
 
 new_dataset$Death_in24h <- Death_in24h$Death_in24h
 new_dataset$Death_in3d <- Death_in3d$Death_in3d
 new_dataset$Death_inICUStay <- Death_inICUStay$Death_inICUStay
 new_dataset$Death_in1w <- Death_in1w$Death_in1w
 new_dataset$Death_in2w <- Death_in2w$Death_in2w
 new_dataset$Death_in3w <- Death_in3w$Death_in3w

 colnames(death_values)[colnames(death_values) == "PatientID"] <- "ID"
 death_values <- filter(death_values, death_values$Value == TRUE)
 new_dataset <- merge(new_dataset, death_values[, c("ID", "DateTime")], by = "ID", all.x = TRUE)
 new_dataset$DaystillDeath <- difftime(new_dataset$DateTime,new_dataset$Datum,unit="days")
 new_dataset$DateTime <- NULL

 ####################################################################################
 
 
 bili_values <- dbGetQuery(con, "  SELECT patientunitstayid, labresultoffset, labresulttext FROM lab WHERE labname like '%total bilirubin%' ")
 
 bili_values <- filter(bili_values, patientunitstayid %in% patient_list)
 
 colnames(bili_values)[colnames(bili_values) == "labresulttext"] <- "Value"
 colnames(bili_values)[colnames(bili_values) == "patientunitstayid"] <- "PatientID"
 
 bili_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 bili_values$DateTime <- bili_values$ICUStay_begin + as.numeric(bili_values$labresultoffset) * 60 
 bili_values$ICUStay_begin <- NULL
 bili_values$labresultoffset <- NULL
 
 bili_values$DateTime <- as.POSIXlt(bili_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 bili_values$Value <- str_replace_all(bili_values$Value, "[^0-9.-]", "")
 bili_values$Value <- as.numeric(bili_values$Value)
 
 BilirubineWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("BilirubineWeightedMean", bili_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$BilirubineWeightedMean <- BilirubineWeightedMean$BilirubineWeightedMean
 


 ####################################################################################
 
 ph_values <- dbGetQuery(con, "  SELECT patientunitstayid, labresultoffset, labresulttext FROM lab WHERE labname like 'ph' ")
 
 ph_values <- filter(ph_values, patientunitstayid %in% patient_list)
 
 colnames(ph_values)[colnames(ph_values) == "labresulttext"] <- "Value"
 colnames(ph_values)[colnames(ph_values) == "patientunitstayid"] <- "PatientID"
 
 ph_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 ph_values$DateTime <- ph_values$ICUStay_begin + as.numeric(ph_values$labresultoffset) * 60 
 ph_values$ICUStay_begin <- NULL
 ph_values$labresultoffset <- NULL
 
 ph_values$DateTime <- as.POSIXlt(ph_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 ph_values$Value <- str_replace_all(ph_values$Value, "[^0-9.-]", "")
 ph_values$Value <- as.numeric(ph_values$Value)
 
 PHWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("PHWeightedMean", ph_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$PHWeightedMean <- PHWeightedMean$PHWeightedMean
 
 LowestpH <- get_min_values_in_range_patient_list_parallel("LowestpH", ph_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$LowestpH <- LowestpH$LowestpH
 
 ####################################################################################
 
 lac_values <- dbGetQuery(con, "  SELECT patientunitstayid, labresultoffset, labresulttext FROM lab WHERE labname like '%lac%' ")
 
 lac_values <- filter(lac_values, patientunitstayid %in% patient_list)
 
 colnames(lac_values)[colnames(lac_values) == "labresulttext"] <- "Value"
 colnames(lac_values)[colnames(lac_values) == "patientunitstayid"] <- "PatientID"
 
 lac_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 lac_values$DateTime <- lac_values$ICUStay_begin + as.numeric(lac_values$labresultoffset) * 60 
 lac_values$ICUStay_begin <- NULL
 lac_values$labresultoffset <- NULL
 
 lac_values$DateTime <- as.POSIXlt(lac_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 lac_values$Value <- str_replace_all(lac_values$Value, "[^0-9.-]", "")
 lac_values$Value <- as.numeric(lac_values$Value)
 
 LactateWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("LactateWeightedMean", lac_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$LactateWeightedMean <- LactateWeightedMean$LactateWeightedMean
 
 HighestLactate <- get_max_values_in_range_patient_list_parallel("HighestLactate", lac_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HighestLactate <- HighestLactate$HighestLactate
 
 ####################################################################################
 
 glu_values <- dbGetQuery(con, "  SELECT patientunitstayid, labresultoffset, labresulttext FROM lab WHERE labname like '%glu%' ")
 
 glu_values <- filter(glu_values, patientunitstayid %in% patient_list)
 
 colnames(glu_values)[colnames(glu_values) == "labresulttext"] <- "Value"
 colnames(glu_values)[colnames(glu_values) == "patientunitstayid"] <- "PatientID"
 
 glu_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 glu_values$DateTime <- glu_values$ICUStay_begin + as.numeric(glu_values$labresultoffset) * 60 
 glu_values$ICUStay_begin <- NULL
 glu_values$labresultoffset <- NULL
 
 glu_values$DateTime <- as.POSIXlt(glu_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 glu_values$Value <- str_replace_all(glu_values$Value, "[^0-9.-]", "")
 glu_values$Value <- as.numeric(glu_values$Value)
 
 gluWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("gluWeightedMean", glu_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$gluWeightedMean <- gluWeightedMean$gluWeightedMean
 


 ####################################################################################
 
 
 hoc3_values <- dbGetQuery(con, "  SELECT patientunitstayid, labresultoffset, labresulttext FROM lab WHERE labname like '%bicarbonate%' ")
 
 hoc3_values <- filter(hoc3_values, patientunitstayid %in% patient_list)
 
 colnames(hoc3_values)[colnames(hoc3_values) == "labresulttext"] <- "Value"
 colnames(hoc3_values)[colnames(hoc3_values) == "patientunitstayid"] <- "PatientID"
 
 hoc3_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 hoc3_values$DateTime <- hoc3_values$ICUStay_begin + as.numeric(hoc3_values$labresultoffset) * 60 
 hoc3_values$ICUStay_begin <- NULL
 hoc3_values$labresultoffset <- NULL
 
 hoc3_values$DateTime <- as.POSIXlt(hoc3_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 hoc3_values$Value <- str_replace_all(hoc3_values$Value, "[^0-9.-]", "")
 hoc3_values$Value <- as.numeric(hoc3_values$Value)
 
 HCO3WeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("HCO3WeightedMean", hoc3_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HCO3WeightedMean <- HCO3WeightedMean$HCO3WeightedMean
 
 
 ####################################################################################

 
 PO2_values <- dbGetQuery(con, "  SELECT patientunitstayid, labresultoffset, labresulttext FROM lab 
                   WHERE labname like 'paO2' ")
 
 PO2_values <- filter(PO2_values, patientunitstayid %in% patient_list)
 
 colnames(PO2_values)[colnames(PO2_values) == "labresulttext"] <- "Value"
 colnames(PO2_values)[colnames(PO2_values) == "patientunitstayid"] <- "PatientID"
 
 PO2_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 PO2_values$DateTime <- PO2_values$ICUStay_begin + as.numeric(PO2_values$labresultoffset) * 60 
 PO2_values$ICUStay_begin <- NULL
 PO2_values$labresultoffset <- NULL
 
 PO2_values$DateTime <- as.POSIXlt(PO2_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 PO2_values$Value <- str_replace_all(PO2_values$Value, "[^0-9.-]", "")
 PO2_values$Value <- as.numeric(PO2_values$Value)
 
 PO2WeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("PO2WeightedMean", PO2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$PO2WeightedMean <- PO2WeightedMean$PO2WeightedMean
 
 
 ####################################################################################
 
 
 PCO2_values <- dbGetQuery(con, "  SELECT patientunitstayid, labresultoffset, labresulttext FROM lab 
                   WHERE labname like 'paCO2' ")
 
 PCO2_values <- filter(PCO2_values, patientunitstayid %in% patient_list)
 
 colnames(PCO2_values)[colnames(PCO2_values) == "labresulttext"] <- "Value"
 colnames(PCO2_values)[colnames(PCO2_values) == "patientunitstayid"] <- "PatientID"
 
 PCO2_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 PCO2_values$DateTime <- PCO2_values$ICUStay_begin + as.numeric(PCO2_values$labresultoffset) * 60 
 PCO2_values$ICUStay_begin <- NULL
 PCO2_values$labresultoffset <- NULL
 
 PCO2_values$DateTime <- as.POSIXlt(PCO2_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 PCO2_values$Value <- str_replace_all(PCO2_values$Value, "[^0-9.-]", "")
 PCO2_values$Value <- as.numeric(PCO2_values$Value)
 
 PCO2WeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("PCO2WeightedMean", PCO2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$PCO2WeightedMean <- PCO2WeightedMean$PCO2WeightedMean
 
 
 ####################################################################################
 
 

 HB_values <- dbGetQuery(con, "  SELECT patientunitstayid, labresultoffset, labresulttext FROM lab 
                   WHERE labname like 'Hgb' ")
 
 HB_values <- filter(HB_values, patientunitstayid %in% patient_list)
 
 colnames(HB_values)[colnames(HB_values) == "labresulttext"] <- "Value"
 colnames(HB_values)[colnames(HB_values) == "patientunitstayid"] <- "PatientID"
 
 HB_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 HB_values$DateTime <- HB_values$ICUStay_begin + as.numeric(HB_values$labresultoffset) * 60 
 HB_values$ICUStay_begin <- NULL
 HB_values$labresultoffset <- NULL
 
 HB_values$DateTime <- as.POSIXlt(HB_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 HB_values$Value <- str_replace_all(HB_values$Value, "[^0-9.-]", "")
 HB_values$Value <- as.numeric(HB_values$Value)
 
 HBWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("HBWeightedMean", HB_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HBWeightedMean <- HBWeightedMean$HBWeightedMean
 
 
 ####################################################################################
 
 
 leuco_values <- dbGetQuery(con, "  SELECT patientunitstayid, labresultoffset, labresulttext FROM lab 
                   WHERE labname like 'WBC x 1000' ")
 
 leuco_values <- filter(leuco_values, patientunitstayid %in% patient_list)
 
 colnames(leuco_values)[colnames(leuco_values) == "labresulttext"] <- "Value"
 colnames(leuco_values)[colnames(leuco_values) == "patientunitstayid"] <- "PatientID"
 
 leuco_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 leuco_values$DateTime <- leuco_values$ICUStay_begin + as.numeric(leuco_values$labresultoffset) * 60 
 leuco_values$ICUStay_begin <- NULL
 leuco_values$labresultoffset <- NULL
 
 leuco_values$DateTime <- as.POSIXlt(leuco_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 leuco_values$Value <- str_replace_all(leuco_values$Value, "[^0-9.-]", "")
 leuco_values$Value <- as.numeric(leuco_values$Value)
 
 leucoWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("leucoWeightedMean", leuco_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$leucoWeightedMean <- leucoWeightedMean$leucoWeightedMean
 
 
 ####################################################################################
 
 
 urea_values <- dbGetQuery(con, "  SELECT patientunitstayid, labresultoffset, labresulttext FROM lab 
                   WHERE labname like 'BUN' ")
 
 urea_values <- filter(urea_values, patientunitstayid %in% patient_list)
 
 colnames(urea_values)[colnames(urea_values) == "labresulttext"] <- "Value"
 colnames(urea_values)[colnames(urea_values) == "patientunitstayid"] <- "PatientID"
 
 urea_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 urea_values$DateTime <- urea_values$ICUStay_begin + as.numeric(urea_values$labresultoffset) * 60 
 urea_values$ICUStay_begin <- NULL
 urea_values$labresultoffset <- NULL
 
 urea_values$DateTime <- as.POSIXlt(urea_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 urea_values$Value <- str_replace_all(urea_values$Value, "[^0-9.-]", "")
 urea_values$Value <- as.numeric(urea_values$Value)
 
 ureaWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("ureaWeightedMean", urea_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$ureaWeightedMean <- ureaWeightedMean$ureaWeightedMean
 
 

 
 
 ####################################################################################
 
 
 Na_values <- dbGetQuery(con, "  SELECT patientunitstayid, labresultoffset, labresulttext FROM lab 
                   WHERE labname like 'sodium' ")
 
 Na_values <- filter(Na_values, patientunitstayid %in% patient_list)
 
 colnames(Na_values)[colnames(Na_values) == "labresulttext"] <- "Value"
 colnames(Na_values)[colnames(Na_values) == "patientunitstayid"] <- "PatientID"
 
 Na_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 Na_values$DateTime <- Na_values$ICUStay_begin + as.numeric(Na_values$labresultoffset) * 60 
 Na_values$ICUStay_begin <- NULL
 Na_values$labresultoffset <- NULL
 
 Na_values$DateTime <- as.POSIXlt(Na_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 Na_values$Value <- str_replace_all(Na_values$Value, "[^0-9.-]", "")
 Na_values$Value <- as.numeric(Na_values$Value)
 
 NaWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("NaWeightedMean", Na_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$NaWeightedMean <- NaWeightedMean$NaWeightedMean
 
 
 ####################################################################################
 
 
 K_values <- dbGetQuery(con, "  SELECT patientunitstayid, labresultoffset, labresulttext FROM lab 
                   WHERE labname like 'potassium' ")
 
 K_values <- filter(K_values, patientunitstayid %in% patient_list)
 
 colnames(K_values)[colnames(K_values) == "labresulttext"] <- "Value"
 colnames(K_values)[colnames(K_values) == "patientunitstayid"] <- "PatientID"
 
 K_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 K_values$DateTime <- K_values$ICUStay_begin + as.numeric(K_values$labresultoffset) * 60 
 K_values$ICUStay_begin <- NULL
 K_values$labresultoffset <- NULL
 
 K_values$DateTime <- as.POSIXlt(K_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 K_values$Value <- str_replace_all(K_values$Value, "[^0-9.-]", "")
 K_values$Value <- as.numeric(K_values$Value)
 
 KWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("KWeightedMean", K_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$KWeightedMean <- KWeightedMean$KWeightedMean
 
 
 ####################################################################################
 
 
 Cl_values <- dbGetQuery(con, "  SELECT patientunitstayid, labresultoffset, labresulttext FROM lab 
                   WHERE labname like 'chloride' ")
 
 Cl_values <- filter(Cl_values, patientunitstayid %in% patient_list)
 
 colnames(Cl_values)[colnames(Cl_values) == "labresulttext"] <- "Value"
 colnames(Cl_values)[colnames(Cl_values) == "patientunitstayid"] <- "PatientID"
 
 Cl_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 Cl_values$DateTime <- Cl_values$ICUStay_begin + as.numeric(Cl_values$labresultoffset) * 60 
 Cl_values$ICUStay_begin <- NULL
 Cl_values$labresultoffset <- NULL
 
 Cl_values$DateTime <- as.POSIXlt(Cl_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 Cl_values$Value <- str_replace_all(Cl_values$Value, "[^0-9.-]", "")
 Cl_values$Value <- as.numeric(Cl_values$Value)
 
 ClWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("ClWeightedMean", Cl_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$ClWeightedMean <- ClWeightedMean$ClWeightedMean
 
 
 ####################################################################################
 
 
 platelets_values <- dbGetQuery(con, "  SELECT patientunitstayid, labresultoffset, labresulttext FROM lab 
                   WHERE labname like 'platelets x 1000' ")
 
 platelets_values <- filter(platelets_values, patientunitstayid %in% patient_list)
 
 colnames(platelets_values)[colnames(platelets_values) == "labresulttext"] <- "Value"
 colnames(platelets_values)[colnames(platelets_values) == "patientunitstayid"] <- "PatientID"
 
 platelets_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 platelets_values$DateTime <- platelets_values$ICUStay_begin + as.numeric(platelets_values$labresultoffset) * 60 
 platelets_values$ICUStay_begin <- NULL
 platelets_values$labresultoffset <- NULL
 
 platelets_values$DateTime <- as.POSIXlt(platelets_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 platelets_values$Value <- str_replace_all(platelets_values$Value, "[^0-9.-]", "")
 platelets_values$Value <- as.numeric(platelets_values$Value)
 
 plateletsWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("plateletsWeightedMean", platelets_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$plateletsWeightedMean <- plateletsWeightedMean$plateletsWeightedMean
 
 ####################################################################################
 
 
 crea_values <- dbGetQuery(con, "  SELECT patientunitstayid, labresultoffset, labresulttext FROM lab 
                   WHERE labname like 'creatinine' ")
 
 crea_values <- filter(crea_values, patientunitstayid %in% patient_list)
 
 colnames(crea_values)[colnames(crea_values) == "labresulttext"] <- "Value"
 colnames(crea_values)[colnames(crea_values) == "patientunitstayid"] <- "PatientID"
 
 crea_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 crea_values$DateTime <- crea_values$ICUStay_begin + as.numeric(crea_values$labresultoffset) * 60 
 crea_values$ICUStay_begin <- NULL
 crea_values$labresultoffset <- NULL
 
 crea_values$DateTime <- as.POSIXlt(crea_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 crea_values$Value <- str_replace_all(crea_values$Value, "[^0-9.-]", "")
 crea_values$Value <- as.numeric(crea_values$Value)
 
 CreaWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("CreaWeightedMean", crea_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$CreaWeightedMean <- CreaWeightedMean$CreaWeightedMean
 

 ####################################################################################
 
 
 HR_values <- dbGetQuery(con, "  SELECT patientunitstayid, nursingchartoffset, nursingchartvalue FROM nurseCharting
                         WHERE nursingchartcelltypevalname like 'Heart Rate'")
 
 HR_values <- filter(HR_values, patientunitstayid %in% patient_list)
 
 colnames(HR_values)[colnames(HR_values) == "nursingchartvalue"] <- "Value"
 colnames(HR_values)[colnames(HR_values) == "patientunitstayid"] <- "PatientID"
 
 HR_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 HR_values$DateTime <- HR_values$ICUStay_begin + as.numeric(HR_values$nursingchartoffset) * 60 
 HR_values$ICUStay_begin <- NULL
 HR_values$nursingchartoffset <- NULL
 
 HR_values$DateTime <- as.POSIXlt(HR_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 HR_values$Value <- str_replace_all(HR_values$Value, "[^0-9.-]", "")
 HR_values$Value <- as.numeric(HR_values$Value)
 
 HRWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("HRWeightedMean", HR_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HRWeightedMean <- HRWeightedMean$HRWeightedMean
 
 
 ####################################################################################
 
 Temp_values <- dbGetQuery(con, "  SELECT patientunitstayid, nursingchartoffset, nursingchartvalue FROM nurseCharting
                         WHERE nursingchartcelltypevalname like 'Temperature (C)'")
 
 Temp_values <- filter(Temp_values, patientunitstayid %in% patient_list)
 
 colnames(Temp_values)[colnames(Temp_values) == "nursingchartvalue"] <- "Value"
 colnames(Temp_values)[colnames(Temp_values) == "patientunitstayid"] <- "PatientID"
 
 Temp_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 Temp_values$DateTime <- Temp_values$ICUStay_begin + as.numeric(Temp_values$nursingchartoffset) * 60 
 Temp_values$ICUStay_begin <- NULL
 Temp_values$nursingchartoffset <- NULL
 
 Temp_values$DateTime <- as.POSIXlt(Temp_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 
 Temp_values$Value <- str_replace_all(Temp_values$Value, "[^0-9.-]", "")
 Temp_values$Value <- as.numeric(Temp_values$Value)
 
 TempWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("TempWeightedMean", Temp_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$TempWeightedMean <- TempWeightedMean$TempWeightedMean
 
 
 ####################################################################################
 
 
 height_values <- dbGetQuery(con, " SELECT patientunitstayid, admissionheight FROM patient ")
 
 height_values <- filter(height_values, patientunitstayid %in% patient_list)
 
 colnames(height_values)[colnames(height_values) == "admissionheight"] <- "Height"
 
 height_values$Height <- str_replace_all(height_values$Height, "[^0-9.-]", "")
 height_values$Height <- as.numeric(height_values$Height)
 
 colnames(height_values)[colnames(height_values) == "patientunitstayid"] <- "ID"
 
 height_values <- filter(height_values, !is.na(Height))
 
 new_dataset <- merge(new_dataset, height_values[, c("ID", "Height")], by = "ID", all.x = TRUE)
 
 ######################################################################################
 
 weight_values <- dbGetQuery(con, " SELECT patientunitstayid, admissionweight FROM patient ")
 
 weight_values <- filter(weight_values, patientunitstayid %in% patient_list)
 
 colnames(weight_values)[colnames(weight_values) == "admissionweight"] <- "Weight"
 
 weight_values$Weight <- str_replace_all(weight_values$Weight, "[^0-9.-]", "")
 weight_values$Weight <- as.numeric(weight_values$Weight)
 
 colnames(weight_values)[colnames(weight_values) == "patientunitstayid"] <- "ID"
 
 weight_values <- filter(weight_values, !is.na(Weight))
 
 new_dataset <- merge(new_dataset, weight_values[, c("ID", "Weight")], by = "ID", all.x = TRUE)
 
 ######################################################################################
 
 path <- "/home/scherer"  
 path <- "D:/"  
 filename <- paste0("eICU_last.csv")
 filepath <- file.path(path, filename)
 
 
 write.csv(new_dataset, file = filepath, fileEncoding = "UTF-8")
 
 

 

 
