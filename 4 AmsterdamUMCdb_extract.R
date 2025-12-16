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


#db_folder_path <- "/media/felixmichelson/T7/" #AG Schock
db_folder_path <- "/media/scherer/T7/1_AmsterdamUMCdb/" #AG Schock
db_folder_path <- "F:/1_AmsterdamUMCdb/" #Zuhause
db_file_path <- file.path(db_folder_path, "AmsterdamUMCdb_database_1.sqlite")

con <- dbConnect(RSQLite::SQLite(), dbname = db_file_path)

result <- dbGetQuery(con, "


select field2
from admissions
where field19 like '%cardio%' OR field19 like '%hart%'

INTERSECT

SELECT field1
from numericitems
where field3 like 'Lactaat (bloed)' AND field5 > 2

INTERSECT

SELECT  field1
FROM drugitems
where field6 like '%press%' OR field6 like '%dobutamine%' OR field6 like '%adrenaline%' 

")


 patient_list <- unique(result$field2)
 
 UMCdb_dataset <- dbGetQuery(con, "SELECT field2, field10, field9 FROM admissions")
 
 UMCdb_dataset <- UMCdb_dataset %>% slice(-1)
 UMCdb_dataset$field2 <- as.numeric(UMCdb_dataset$field2)
 UMCdb_dataset$field9 <- as.numeric(UMCdb_dataset$field9)
 UMCdb_dataset$field10 <- as.numeric(UMCdb_dataset$field10)
 
 UMCdb_dataset <- filter(UMCdb_dataset, field2 %in% patient_list)
 
 UMCdb_dataset <- UMCdb_dataset %>% mutate(field9 = field9 / 3600000)
 UMCdb_dataset$zeit_diff <- (UMCdb_dataset$field9 - UMCdb_dataset$field10)
 UMCdb_dataset$zeit_diff <- ifelse((UMCdb_dataset$field9 - UMCdb_dataset$field10) < 24, 0, UMCdb_dataset$zeit_diff)
 UMCdb_dataset <- UMCdb_dataset %>% mutate(zeit_diff = zeit_diff * 3600000)

 UMCdb_dataset <- UMCdb_dataset %>% mutate(field10 = field10 / 24)
 
 rows_to_delete <- which(UMCdb_dataset$field10 <= 1)
 UMCdb_dataset <- UMCdb_dataset[-rows_to_delete, ]

 UMCdb_dataset$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 
 UMCdb_dataset$ICUStay_end <- UMCdb_dataset$ICUStay_begin + as.numeric(UMCdb_dataset$field10) * 86400 
 
 patient_list <- unique(UMCdb_dataset$field2)
 
 new_dataset <- list()
 
 for (patient_id in unique(UMCdb_dataset$field2)) {
   patient_data_schleife <- UMCdb_dataset[UMCdb_dataset$field2 == patient_id, ]
   
   patient_data_schleife <- na.omit(patient_data_schleife)
   
   if (nrow(patient_data_schleife) > 0) {
     new_dates <- seq(from = min(patient_data_schleife$ICUStay_begin), 
                      to = max(patient_data_schleife$ICUStay_end), 
                      by = "24 hours")
     
     new_patient_data <- data.frame(ID = rep(patient_id, length(new_dates)),
                                    Datum = new_dates)
     
     new_dataset[[as.character(patient_id)]] <- new_patient_data
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
 
 colnames(UMCdb_dataset)[colnames(UMCdb_dataset) == "field2"] <- "field1"
 
 ########################################################################################

 RRDia_values <- dbGetQuery(con, "SELECT field1, field5, field9 from numericitems where field3 like 'ABP diastolisch'")
 
 RRSys_values <- dbGetQuery(con, "SELECT field1, field5, field9 from numericitems where field3 like 'ABP systolisch'")
 
 so2_values <- dbGetQuery(con, "SELECT field1, field5, field9 from numericitems where field3 like 'Saturatie (Monitor)'")
 
 RRDia_values <- filter(RRDia_values, field1 %in% patient_list)
 RRSys_values <- filter(RRSys_values, field1 %in% patient_list)
 so2_values <- filter(so2_values, field1 %in% patient_list)
 
 RRDia_values <- merge(RRDia_values, UMCdb_dataset[, c("field1", "zeit_diff")], by = "field1", all.x = TRUE)
 RRSys_values <- merge(RRSys_values, UMCdb_dataset[, c("field1", "zeit_diff")], by = "field1", all.x = TRUE)
 so2_values <- merge(so2_values, UMCdb_dataset[, c("field1", "zeit_diff")], by = "field1", all.x = TRUE)
 
 RRDia_values$field9 <- as.numeric(RRDia_values$field9)
 RRSys_values$field9 <- as.numeric(RRSys_values$field9)
 so2_values$field9 <- as.numeric(so2_values$field9)
 
 RRDia_values <- RRDia_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 RRSys_values <- RRSys_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 so2_values <- so2_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 RRDia_values <- RRDia_values[-which(RRDia_values$field9 < 0), ]
 RRSys_values <- RRSys_values[-which(RRSys_values$field9 < 0), ]
 so2_values <- so2_values[-which(so2_values$field9 < 0), ]
 
 colnames(RRDia_values)[colnames(RRDia_values) == "field5"] <- "Value"
 colnames(RRSys_values)[colnames(RRSys_values) == "field5"] <- "Value"
 colnames(so2_values)[colnames(so2_values) == "field5"] <- "Value"
 
 RRDia_values$Value <- str_replace_all(RRDia_values$Value, "[^0-9.-]", "")
 RRDia_values$Value <- as.numeric(RRDia_values$Value)
 
 RRSys_values$Value <- str_replace_all(RRSys_values$Value, "[^0-9.-]", "")
 RRSys_values$Value <- as.numeric(RRSys_values$Value)
 
 so2_values$Value <- str_replace_all(so2_values$Value, "[^0-9.-]", "")
 so2_values$Value <- as.numeric(so2_values$Value)
 
 RRDia_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 RRDia_values$DateTime <- RRDia_values$ICUStay_begin + as.numeric(RRDia_values$field9) * 3600 
 RRDia_values$ICUStay_begin <- NULL
 RRDia_values$field9 <- NULL

 
 RRSys_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 RRSys_values$DateTime <- RRSys_values$ICUStay_begin + as.numeric(RRSys_values$field9) * 3600 
 RRSys_values$ICUStay_begin <- NULL
 RRSys_values$field9 <- NULL

 so2_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 so2_values$DateTime <- so2_values$ICUStay_begin + as.numeric(so2_values$field9) * 3600 
 so2_values$ICUStay_begin <- NULL
 so2_values$field9 <- NULL

 colnames(RRDia_values)[colnames(RRDia_values) == "field1"] <- "PatientID"
 colnames(RRSys_values)[colnames(RRSys_values) == "field1"] <- "PatientID"
 colnames(so2_values)[colnames(so2_values) == "field1"] <- "PatientID"
 
 RRDia_values <- filter(RRDia_values, !is.na(Value))
 RRSys_values <- filter(RRSys_values, !is.na(Value))
 so2_values <- filter(so2_values, !is.na(Value))
 
 RRDiaWeightedMeanValue <- get_weighted_mean_values_in_range_patient_list_parallel("RRDiaWeightedMeanValue", RRDia_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$RRDiaWeightedMeanValue <- RRDiaWeightedMeanValue$RRDiaWeightedMeanValue
 
 RRSysWeightedMeanValue <- get_weighted_mean_values_in_range_patient_list_parallel("RRSysWeightedMeanValue", RRSys_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$RRSysWeightedMeanValue <- RRSysWeightedMeanValue$RRSysWeightedMeanValue
 
 sO2WeightedMeanValue <- get_weighted_mean_values_in_range_patient_list_parallel("sO2WeightedMeanValue", so2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$sO2WeightedMeanValue <- sO2WeightedMeanValue$sO2WeightedMeanValue
 
 ######################################################################################
 Age_values <- dbGetQuery(con, " SELECT field2, field13 FROM admissions ")
 
 Age_values <- filter(Age_values, field2 %in% patient_list)
 
 colnames(Age_values)[colnames(Age_values) == "field13"] <- "AgeOnInclusion"
 
 Age_values$AgeOnInclusion <- ifelse(Age_values$AgeOnInclusion == "60-69", 65, Age_values$AgeOnInclusion)
 Age_values$AgeOnInclusion <- ifelse(Age_values$AgeOnInclusion == "50-59", 55, Age_values$AgeOnInclusion)
 Age_values$AgeOnInclusion <- ifelse(Age_values$AgeOnInclusion == "70-79", 75, Age_values$AgeOnInclusion)
 Age_values$AgeOnInclusion <- ifelse(Age_values$AgeOnInclusion == "40-49", 45, Age_values$AgeOnInclusion)
 Age_values$AgeOnInclusion <- ifelse(Age_values$AgeOnInclusion == "18-39", 30, Age_values$AgeOnInclusion)
 Age_values$AgeOnInclusion <- ifelse(Age_values$AgeOnInclusion == "80+", 85, Age_values$AgeOnInclusion)
 
 Age_values$AgeOnInclusion <- as.numeric(Age_values$AgeOnInclusion)
 
 colnames(Age_values)[colnames(Age_values) == "field2"] <- "ID"
 
 Age_values <- filter(Age_values, !is.na(AgeOnInclusion))
 
 new_dataset <- merge(new_dataset, Age_values[, c("ID", "AgeOnInclusion")], by = "ID", all.x = TRUE)
 
 ######################################################################################
 gender_values <- dbGetQuery(con, " SELECT field2, field12 FROM admissions ")
 
 gender_values <- filter(gender_values, field2 %in% patient_list)
 
 colnames(gender_values)[colnames(gender_values) == "field12"] <- "gender"
 
 colnames(gender_values)[colnames(gender_values) == "field2"] <- "ID"
 
 gender_values <- filter(gender_values, !is.na(gender))
 
 new_dataset <- merge(new_dataset, gender_values[, c("ID", "gender")], by = "ID", all.x = TRUE)
 
########################################################################################
 
 death_values <- dbGetQuery(con, " SELECT field2, field14 FROM admissions")

 death_values <- filter(death_values, field2 %in% patient_list)
 
 colnames(death_values)[colnames(death_values) == "field14"] <- "DeathTime"
 colnames(death_values)[colnames(death_values) == "field2"] <- "PatientID"
 
 death_values$Value <- ifelse(is.na(death_values$DeathTime), 0, 1)
 
 colnames(UMCdb_dataset)[colnames(UMCdb_dataset) == "field1"] <- "PatientID"
 death_values <- merge(death_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 death_values$DeathTime <- as.numeric(death_values$DeathTime)
 death_values <- death_values %>% mutate(DeathTime = (DeathTime-zeit_diff) / 3600000)
 
 death_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 death_values$DateTime <- death_values$ICUStay_begin + as.numeric(death_values$DeathTime) * 3600 
 death_values$ICUStay_begin <- NULL
 
 death_values$DateTime <- as.POSIXlt(death_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 death_values$DateTimeEnd <- death_values$DateTime
 death_values$DateTimeEnd <- as.POSIXlt(death_values$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")
 
 Death_in24h <- check_in_range_3("Death_in24h", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*48)
 Death_in3d <- check_in_range_3("Death_in3d", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*4)
 Death_inICUStay <- check_in_range_3("Death_inICUStay", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*250000000000000000000)
 Death_in1w <- check_in_range_3("Death_in1w", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*8)
 Death_in2w <- check_in_range_3("Death_in2w", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*15)
 Death_in3w <- check_in_range_3("Death_in3w", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*22)
 
 Death_in24h <- Death_in24h %>%
   mutate(Death_in24h = ifelse((Death_in24h == 0 | Death_in24h == 'NA'), 0, Death_in24h))
 
 Death_in3d <- Death_in3d %>%
   mutate(Death_in3d = ifelse((Death_in3d == 0 | Death_in3d == 'NA'), 0, Death_in3d))
 
 Death_inICUStay <- Death_inICUStay %>%
   mutate(Death_inICUStay = ifelse((Death_inICUStay == 0 | Death_inICUStay == 'NA'), 0, Death_inICUStay))

 Death_in1w <- Death_in1w %>%
   mutate(Death_in1w = ifelse((Death_in1w == 0 | Death_in1w == 'NA'), 0, Death_in1w))
 
 Death_in2w <- Death_in2w %>%
   mutate(Death_in2w = ifelse((Death_in2w == 0 | Death_in2w == 'NA'), 0, Death_in2w))
 
 Death_in3w <- Death_in3w %>%
   mutate(Death_in3w = ifelse((Death_in3w == 0 | Death_in3w == 'NA'), 0, Death_in3w))
 
 new_dataset$Death_in24h <- Death_in24h$Death_in24h
 new_dataset$Death_in3d <- Death_in3d$Death_in3d
 new_dataset$Death_inICUStay <- Death_inICUStay$Death_inICUStay
 new_dataset$Death_in1w <- Death_in1w$Death_in1w
 new_dataset$Death_in2w <- Death_in2w$Death_in2w
 new_dataset$Death_in3w <- Death_in3w$Death_in3w

 colnames(death_values)[colnames(death_values) == "PatientID"] <- "ID"
 death_values <- filter(death_values, death_values$Value == 1)
 death_values <- death_values %>% distinct(ID, .keep_all = TRUE)
 new_dataset <- merge(new_dataset, death_values[, c("ID", "DateTime")], by = "ID", all.x = TRUE)
 new_dataset$DaystillDeath <- difftime(new_dataset$DateTime,new_dataset$Datum,unit="days")
 new_dataset$DateTime <- NULL

 ####################################################################################
 
 bili_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%Bilirubine (bloed)%'")
 
 bili_values <- filter(bili_values, field1 %in% patient_list)
 
 colnames(bili_values)[colnames(bili_values) == "field1"] <- "PatientID"
 
 bili_values <- merge(bili_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 bili_values$field9 <- as.numeric(bili_values$field9)
 
 bili_values <- bili_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 bili_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 bili_values$DateTime <- bili_values$ICUStay_begin + as.numeric(bili_values$field9) * 3600 
 bili_values$ICUStay_begin <- NULL
 bili_values$field9 <- NULL
 
 colnames(bili_values)[colnames(bili_values) == "field5"] <- "Value"
 
 bili_values$Value <- str_replace_all(bili_values$Value, "[^0-9.-]", "")
 bili_values$Value <- as.numeric(bili_values$Value)
 
 bili_values <- filter(bili_values, !is.na(Value))
 
 BilirubineWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("BilirubineWeightedMean", bili_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$BilirubineWeightedMean <- BilirubineWeightedMean$BilirubineWeightedMean
 
 ####################################################################################
 
 
 ph_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%pH (bloed)%'")
 
 ph_values <- filter(ph_values, field1 %in% patient_list)
 
 colnames(ph_values)[colnames(ph_values) == "field1"] <- "PatientID"
 
 ph_values <- merge(ph_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 ph_values$field9 <- as.numeric(ph_values$field9)
 
 ph_values <- ph_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 ph_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 ph_values$DateTime <- ph_values$ICUStay_begin + as.numeric(ph_values$field9) * 3600 
 ph_values$ICUStay_begin <- NULL
 ph_values$field9 <- NULL
 
 colnames(ph_values)[colnames(ph_values) == "field5"] <- "Value"
 
 ph_values$Value <- str_replace_all(ph_values$Value, "[^0-9.-]", "")
 ph_values$Value <- as.numeric(ph_values$Value)
 ph_values <- filter(ph_values, !is.na(Value))
 
 PHWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("PHWeightedMean", ph_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$PHWeightedMean <- PHWeightedMean$PHWeightedMean
 
 LowestpH <- get_min_values_in_range_patient_list_parallel("LowestpH", ph_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$LowestpH <- LowestpH$LowestpH
 
 ####################################################################################
 
 lac_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%Lactaat (bloed)%'")
 
 lac_values <- filter(lac_values, field1 %in% patient_list)
 
 colnames(lac_values)[colnames(lac_values) == "field1"] <- "PatientID"
 
 lac_values <- merge(lac_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 lac_values$field9 <- as.numeric(lac_values$field9)
 
 lac_values <- lac_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 lac_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 lac_values$DateTime <- lac_values$ICUStay_begin + as.numeric(lac_values$field9) * 3600 
 lac_values$ICUStay_begin <- NULL
 lac_values$field9 <- NULL
 
 colnames(lac_values)[colnames(lac_values) == "field5"] <- "Value"
 
 lac_values$Value <- str_replace_all(lac_values$Value, "[^0-9.-]", "")
 lac_values$Value <- as.numeric(lac_values$Value)
 lac_values <- filter(lac_values, !is.na(Value))
 
 LactateWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("LactateWeightedMean", lac_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$LactateWeightedMean <- LactateWeightedMean$LactateWeightedMean
 
 HighestLactate <- get_max_values_in_range_patient_list_parallel("HighestLactate", lac_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HighestLactate <- HighestLactate$HighestLactate
 
 
 ####################################################################################
 
 
 crea_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like 'Kreatinine (bloed)'")
 
 crea_values <- filter(crea_values, field1 %in% patient_list)
 
 colnames(crea_values)[colnames(crea_values) == "field1"] <- "PatientID"
 
 crea_values <- merge(crea_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 crea_values$field9 <- as.numeric(crea_values$field9)
 
 crea_values <- crea_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 crea_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 crea_values$DateTime <- crea_values$ICUStay_begin + as.numeric(crea_values$field9) * 3600 
 crea_values$ICUStay_begin <- NULL
 crea_values$field9 <- NULL
 
 colnames(crea_values)[colnames(crea_values) == "field5"] <- "Value"
 
 crea_values$Value <- str_replace_all(crea_values$Value, "[^0-9.-]", "")
 crea_values$Value <- as.numeric(crea_values$Value)
 crea_values <- filter(crea_values, !is.na(Value))
 
 CreatinineWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("CreatinineWeightedMean", crea_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$CreatinineWeightedMean <- CreatinineWeightedMean$CreatinineWeightedMean
 
 
 ####################################################################################
 
 
 glu_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%Glucose (bloed)%'")
 
 glu_values <- filter(glu_values, field1 %in% patient_list)
 
 colnames(glu_values)[colnames(glu_values) == "field1"] <- "PatientID"
 
 glu_values <- merge(glu_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 glu_values$field9 <- as.numeric(glu_values$field9)
 
 glu_values <- glu_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 glu_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 glu_values$DateTime <- glu_values$ICUStay_begin + as.numeric(glu_values$field9) * 3600 
 glu_values$ICUStay_begin <- NULL
 glu_values$field9 <- NULL
 
 colnames(glu_values)[colnames(glu_values) == "field5"] <- "Value"
 
 glu_values$Value <- str_replace_all(glu_values$Value, "[^0-9.-]", "")
 glu_values$Value <- as.numeric(glu_values$Value)
 glu_values <- filter(glu_values, !is.na(Value))
 
 gluWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("gluWeightedMean", glu_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$gluWeightedMean <- gluWeightedMean$gluWeightedMean
 

 ####################################################################################
 
 be_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%B.E. (bloed)%'")
 
 be_values <- filter(be_values, field1 %in% patient_list)
 
 colnames(be_values)[colnames(be_values) == "field1"] <- "PatientID"
 
 be_values <- merge(be_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 be_values$field9 <- as.numeric(be_values$field9)
 
 be_values <- be_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 be_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 be_values$DateTime <- be_values$ICUStay_begin + as.numeric(be_values$field9) * 3600 
 be_values$ICUStay_begin <- NULL
 be_values$field9 <- NULL
 
 colnames(be_values)[colnames(be_values) == "field5"] <- "Value"
 
 be_values$Value <- str_replace_all(be_values$Value, "[^0-9.-]", "")
 be_values$Value <- as.numeric(be_values$Value)
 be_values <- filter(be_values, !is.na(Value))
 
 BEWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("BEWeightedMean", be_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$BEWeightedMean <- BEWeightedMean$BEWeightedMean
 
 
 ####################################################################################
 
 hoc3_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%Act.HCO3 (bloed)%'")
 
 hoc3_values <- filter(hoc3_values, field1 %in% patient_list)
 
 colnames(hoc3_values)[colnames(hoc3_values) == "field1"] <- "PatientID"
 
 hoc3_values <- merge(hoc3_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 hoc3_values$field9 <- as.numeric(hoc3_values$field9)
 
 hoc3_values <- hoc3_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 hoc3_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 hoc3_values$DateTime <- hoc3_values$ICUStay_begin + as.numeric(hoc3_values$field9) * 3600 
 hoc3_values$ICUStay_begin <- NULL
 hoc3_values$field9 <- NULL
 
 colnames(hoc3_values)[colnames(hoc3_values) == "field5"] <- "Value"
 
 hoc3_values$Value <- str_replace_all(hoc3_values$Value, "[^0-9.-]", "")
 hoc3_values$Value <- as.numeric(hoc3_values$Value)
 hoc3_values <- filter(hoc3_values, !is.na(Value))
 
 HCO3WeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("HCO3WeightedMean", hoc3_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HCO3WeightedMean <- HCO3WeightedMean$HCO3WeightedMean
 
 
 ####################################################################################
 
 
 
 
 PO2_value <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%PO2 (bloed)%'")
 
 PO2_value <- filter(PO2_value, field1 %in% patient_list)
 
 colnames(PO2_value)[colnames(PO2_value) == "field1"] <- "PatientID"
 
 PO2_value <- merge(PO2_value, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 PO2_value$field9 <- as.numeric(PO2_value$field9)
 
 PO2_value <- PO2_value %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 PO2_value$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 PO2_value$DateTime <- PO2_value$ICUStay_begin + as.numeric(PO2_value$field9) * 3600 
 PO2_value$ICUStay_begin <- NULL
 PO2_value$field9 <- NULL
 
 colnames(PO2_value)[colnames(PO2_value) == "field5"] <- "Value"
 
 PO2_value$Value <- str_replace_all(PO2_value$Value, "[^0-9.-]", "")
 PO2_value$Value <- as.numeric(PO2_value$Value)
 PO2_value <- filter(PO2_value, !is.na(Value))
 
 PO2WeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("PO2WeightedMean", PO2_value, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$PO2WeightedMean <- PO2WeightedMean$PO2WeightedMean
 
 
 ####################################################################################
 
 
 
 PCO2_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%pCO2 (bloed)%'")
 
 PCO2_values <- filter(PCO2_values, field1 %in% patient_list)
 
 colnames(PCO2_values)[colnames(PCO2_values) == "field1"] <- "PatientID"
 
 PCO2_values <- merge(PCO2_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 PCO2_values$field9 <- as.numeric(PCO2_values$field9)
 
 PCO2_values <- PCO2_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 PCO2_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 PCO2_values$DateTime <- PCO2_values$ICUStay_begin + as.numeric(PCO2_values$field9) * 3600 
 PCO2_values$ICUStay_begin <- NULL
 PCO2_values$field9 <- NULL
 
 colnames(PCO2_values)[colnames(PCO2_values) == "field5"] <- "Value"
 
 PCO2_values$Value <- str_replace_all(PCO2_values$Value, "[^0-9.-]", "")
 PCO2_values$Value <- as.numeric(PCO2_values$Value)
 PCO2_values <- filter(PCO2_values, !is.na(Value))
 
 PCO2WeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("PCO2WeightedMean", PCO2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$PCO2WeightedMean <- PCO2WeightedMean$PCO2WeightedMean
 
 
 ####################################################################################
 
 
 
 HB_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%Hb (bloed)%'")
 
 HB_values <- filter(HB_values, field1 %in% patient_list)
 
 colnames(HB_values)[colnames(HB_values) == "field1"] <- "PatientID"
 
 HB_values <- merge(HB_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 HB_values$field9 <- as.numeric(HB_values$field9)
 
 HB_values <- HB_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 HB_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 HB_values$DateTime <- HB_values$ICUStay_begin + as.numeric(HB_values$field9) * 3600 
 HB_values$ICUStay_begin <- NULL
 HB_values$field9 <- NULL
 
 colnames(HB_values)[colnames(HB_values) == "field5"] <- "Value"
 
 HB_values$Value <- str_replace_all(HB_values$Value, "[^0-9.-]", "")
 HB_values$Value <- as.numeric(HB_values$Value)
 HB_values <- filter(HB_values, !is.na(Value))
 
 HBWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("HBWeightedMean", HB_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HBWeightedMean <- HBWeightedMean$HBWeightedMean
 
 
 ####################################################################################
 
 
 
 leuco_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%Leuco''s (bloed)%';")
 
 leuco_values <- filter(leuco_values, field1 %in% patient_list)
 
 colnames(leuco_values)[colnames(leuco_values) == "field1"] <- "PatientID"
 
 leuco_values <- merge(leuco_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 leuco_values$field9 <- as.numeric(leuco_values$field9)
 
 leuco_values <- leuco_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 leuco_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 leuco_values$DateTime <- leuco_values$ICUStay_begin + as.numeric(leuco_values$field9) * 3600 
 leuco_values$ICUStay_begin <- NULL
 leuco_values$field9 <- NULL
 
 colnames(leuco_values)[colnames(leuco_values) == "field5"] <- "Value"
 
 leuco_values$Value <- str_replace_all(leuco_values$Value, "[^0-9.-]", "")
 leuco_values$Value <- as.numeric(leuco_values$Value)
 leuco_values <- filter(leuco_values, !is.na(Value))
 
 leucoWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("leucoWeightedMean", leuco_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$leucoWeightedMean <- leucoWeightedMean$leucoWeightedMean
 
 
 ####################################################################################
 
 
 
 urea_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%Ureum (bloed)%'")
 
 urea_values <- filter(urea_values, field1 %in% patient_list)
 
 colnames(urea_values)[colnames(urea_values) == "field1"] <- "PatientID"
 
 urea_values <- merge(urea_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 urea_values$field9 <- as.numeric(urea_values$field9)
 
 urea_values <- urea_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 urea_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 urea_values$DateTime <- urea_values$ICUStay_begin + as.numeric(urea_values$field9) * 3600 
 urea_values$ICUStay_begin <- NULL
 urea_values$field9 <- NULL
 
 colnames(urea_values)[colnames(urea_values) == "field5"] <- "Value"
 
 urea_values$Value <- str_replace_all(urea_values$Value, "[^0-9.-]", "")
 urea_values$Value <- as.numeric(urea_values$Value)
 urea_values <- filter(urea_values, !is.na(Value))
 
 ureaWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("ureaWeightedMean", urea_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$ureaWeightedMean <- ureaWeightedMean$ureaWeightedMean
 
 
 ####################################################################################
 
 
 
  platelets_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%Thrombo''s (bloed)%'")
 
  platelets_values <- filter( platelets_values, field1 %in% patient_list)
 
 colnames( platelets_values)[colnames( platelets_values) == "field1"] <- "PatientID"
 
  platelets_values <- merge( platelets_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
  platelets_values$field9 <- as.numeric( platelets_values$field9)
 
  platelets_values <-  platelets_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
  platelets_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
  platelets_values$DateTime <-  platelets_values$ICUStay_begin + as.numeric( platelets_values$field9) * 3600 
  platelets_values$ICUStay_begin <- NULL
  platelets_values$field9 <- NULL
 
 colnames( platelets_values)[colnames( platelets_values) == "field5"] <- "Value"
 
  platelets_values$Value <- str_replace_all( platelets_values$Value, "[^0-9.-]", "")
  platelets_values$Value <- as.numeric( platelets_values$Value)
  platelets_values <- filter( platelets_values, !is.na(Value))
 
  plateletsWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("plateletsWeightedMean",  platelets_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$plateletsWeightedMean <-  plateletsWeightedMean$plateletsWeightedMean
 

 ####################################################################################
 
 
 
 HR_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%Hartfrequentie%'")
 
 HR_values <- filter(HR_values, field1 %in% patient_list)
 
 colnames(HR_values)[colnames(HR_values) == "field1"] <- "PatientID"
 
 HR_values <- merge(HR_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 HR_values$field9 <- as.numeric(HR_values$field9)
 
 HR_values <- HR_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 HR_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 HR_values$DateTime <- HR_values$ICUStay_begin + as.numeric(HR_values$field9) * 3600 
 HR_values$ICUStay_begin <- NULL
 HR_values$field9 <- NULL
 
 colnames(HR_values)[colnames(HR_values) == "field5"] <- "Value"
 
 HR_values$Value <- str_replace_all(HR_values$Value, "[^0-9.-]", "")
 HR_values$Value <- as.numeric(HR_values$Value)
 HR_values <- filter(HR_values, !is.na(Value))
 
 HRWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("HRWeightedMean", HR_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HRWeightedMean <- HRWeightedMean$HRWeightedMean
 
 
 ####################################################################################
 
 
 
 Temp_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%Temperatuur Perifeer%'")
 
 Temp_values <- filter(Temp_values, field1 %in% patient_list)
 
 colnames(Temp_values)[colnames(Temp_values) == "field1"] <- "PatientID"
 
 Temp_values <- merge(Temp_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 Temp_values$field9 <- as.numeric(Temp_values$field9)
 
 Temp_values <- Temp_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 Temp_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 Temp_values$DateTime <- Temp_values$ICUStay_begin + as.numeric(Temp_values$field9) * 3600 
 Temp_values$ICUStay_begin <- NULL
 Temp_values$field9 <- NULL
 
 colnames(Temp_values)[colnames(Temp_values) == "field5"] <- "Value"
 
 Temp_values$Value <- str_replace_all(Temp_values$Value, "[^0-9.-]", "")
 Temp_values$Value <- as.numeric(Temp_values$Value)
 Temp_values <- filter(Temp_values, !is.na(Value))
 
 TempWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("TempWeightedMean", Temp_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$TempWeightedMean <- TempWeightedMean$TempWeightedMean
 
 
 ####################################################################################
 
 
 
 K_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%Kalium (bloed)%'")
 
 K_values <- filter(K_values, field1 %in% patient_list)
 
 colnames(K_values)[colnames(K_values) == "field1"] <- "PatientID"
 
 K_values <- merge(K_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 K_values$field9 <- as.numeric(K_values$field9)
 
 K_values <- K_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 K_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 K_values$DateTime <- K_values$ICUStay_begin + as.numeric(K_values$field9) * 3600 
 K_values$ICUStay_begin <- NULL
 K_values$field9 <- NULL
 
 colnames(K_values)[colnames(K_values) == "field5"] <- "Value"
 
 K_values$Value <- str_replace_all(K_values$Value, "[^0-9.-]", "")
 K_values$Value <- as.numeric(K_values$Value)
 K_values <- filter(K_values, !is.na(Value))
 
 KWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("KWeightedMean", K_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$KWeightedMean <- KWeightedMean$KWeightedMean
 
 
 ####################################################################################
 
 
 
 Na_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%Natrium (bloed)%'")
 
 Na_values <- filter(Na_values, field1 %in% patient_list)
 
 colnames(Na_values)[colnames(Na_values) == "field1"] <- "PatientID"
 
 Na_values <- merge(Na_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 Na_values$field9 <- as.numeric(Na_values$field9)
 
 Na_values <- Na_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 Na_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 Na_values$DateTime <- Na_values$ICUStay_begin + as.numeric(Na_values$field9) * 3600 
 Na_values$ICUStay_begin <- NULL
 Na_values$field9 <- NULL
 
 colnames(Na_values)[colnames(Na_values) == "field5"] <- "Value"
 
 Na_values$Value <- str_replace_all(Na_values$Value, "[^0-9.-]", "")
 Na_values$Value <- as.numeric(Na_values$Value)
 Na_values <- filter(Na_values, !is.na(Value))
 
 NaWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("NaWeightedMean", Na_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$NaWeightedMean <- NaWeightedMean$NaWeightedMean
 
 
 ####################################################################################
 
 
 
 Cl_values <- dbGetQuery(con, "  SELECT field1, field5, field9 from numericitems where field3 like '%Chloor (bloed)%'")
 
 Cl_values <- filter(Cl_values, field1 %in% patient_list)
 
 colnames(Cl_values)[colnames(Cl_values) == "field1"] <- "PatientID"
 
 Cl_values <- merge(Cl_values, UMCdb_dataset[, c("PatientID", "zeit_diff")], by = "PatientID", all.x = TRUE)
 
 Cl_values$field9 <- as.numeric(Cl_values$field9)
 
 Cl_values <- Cl_values %>% mutate(field9 = (field9-zeit_diff) / 3600000)
 
 Cl_values$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 Cl_values$DateTime <- Cl_values$ICUStay_begin + as.numeric(Cl_values$field9) * 3600 
 Cl_values$ICUStay_begin <- NULL
 Cl_values$field9 <- NULL
 
 colnames(Cl_values)[colnames(Cl_values) == "field5"] <- "Value"
 
 Cl_values$Value <- str_replace_all(Cl_values$Value, "[^0-9.-]", "")
 Cl_values$Value <- as.numeric(Cl_values$Value)
 Cl_values <- filter(Cl_values, !is.na(Value))
 
 ClWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("ClWeightedMean", Cl_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$ClWeightedMean <- ClWeightedMean$ClWeightedMean
 
 
 ####################################################################################
 Height_values <- dbGetQuery(con, " SELECT field2, field17 FROM admissions ")

 Height_values <- filter(Height_values, field2 %in% patient_list)
 
 colnames(Height_values)[colnames(Height_values) == "field17"] <- "Height"
 
 Height_values$Height <- ifelse(Height_values$Height == "159-", 155, Height_values$Height)
 Height_values$Height <- ifelse(Height_values$Height == "160-169", 165, Height_values$Height)
 Height_values$Height <- ifelse(Height_values$Height == "170-179", 175, Height_values$Height)
 Height_values$Height <- ifelse(Height_values$Height == "180-189", 185, Height_values$Height)
 Height_values$Height <- ifelse(Height_values$Height == "190+", 195, Height_values$Height)
 
 Height_values$Height <- as.numeric(Height_values$Height)
 
 colnames(Height_values)[colnames(Height_values) == "field2"] <- "ID"
 
 Height_values <- filter(Height_values, !is.na(Height))
 
 new_dataset <- merge(new_dataset, Height_values[, c("ID", "Height")], by = "ID", all.x = TRUE)
 
 ########################################################################################
 Weight_values <- dbGetQuery(con, " SELECT field2, field15 FROM admissions ")

 Weight_values <- filter(Weight_values, field2 %in% patient_list)
 
 colnames(Weight_values)[colnames(Weight_values) == "field15"] <- "Weight"
 
 Weight_values$Weight <- ifelse(Weight_values$Weight == "110+", 115, Weight_values$Weight)
 Weight_values$Weight <- ifelse(Weight_values$Weight == "59-", 55, Weight_values$Weight)
 Weight_values$Weight <- ifelse(Weight_values$Weight == "60-69", 65, Weight_values$Weight)
 Weight_values$Weight <- ifelse(Weight_values$Weight == "70-79", 75, Weight_values$Weight)
 Weight_values$Weight <- ifelse(Weight_values$Weight == "80-89", 85, Weight_values$Weight)
 Weight_values$Weight <- ifelse(Weight_values$Weight == "90-99", 95, Weight_values$Weight)
 Weight_values$Weight <- ifelse(Weight_values$Weight == "100-109", 105, Weight_values$Weight)
 
 Weight_values$Weight <- as.numeric(Weight_values$Weight)
 
 colnames(Weight_values)[colnames(Weight_values) == "field2"] <- "ID"
 
 Weight_values <- filter(Weight_values, !is.na(Weight))
 
 new_dataset <- merge(new_dataset, Weight_values[, c("ID", "Weight")], by = "ID", all.x = TRUE)
 
 ########################################################################################

 path <- "/home/scherer"  
 path <- "D:"  
 filename <- paste0("AmsterdamUMCDB_last.csv")
 filepath <- file.path(path, filename)
 
 
 write.csv(new_dataset, file = filepath, fileEncoding = "UTF-8")

