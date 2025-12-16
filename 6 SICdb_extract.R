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
db_folder_path <- "/media/scherer/T7/3_Salzburg/" #AG Schock
db_folder_path <- "F:/3_Salzburg/" #Zuhause
db_file_path <- file.path(db_folder_path, "SICDB.sqlite")

con <- dbConnect(RSQLite::SQLite(), dbname = db_file_path)

result <- dbGetQuery(con, "
select field1
from cases
where field19 like '%herz%' OR field19 like '%kard%' OR field20 like '%herz%' OR field20 like '%kard%' OR
      field19 like '%klapp%' OR field20 like '%klapp%'OR field19 like '%vorhof%' OR field20 like '%vorhof%'
      OR field19 like '%arrhythmi%' OR field20 like '%arrhythmi%'

INTERSECT

SELECT field2
FROM medication
WHERE field4 IN ('733', '734', '772', '773', '1502', 
                 '1562', '1559', '2306', '2307', '1550', '1496', 
                 '1483')

INTERSECT

SELECT field2
FROM laboratory
WHERE field3 LIKE '657'
      AND field5 >= 2



")


 patient_list <- unique(result$field1)
 
 SICdb_dataset <- dbGetQuery(con, "SELECT field1, field4, field5 FROM cases")
 
 SICdb_dataset <- SICdb_dataset %>% slice(-1)
 SICdb_dataset$field1 <- as.numeric(SICdb_dataset$field1)
 SICdb_dataset$field4 <- as.numeric(SICdb_dataset$field4)
 SICdb_dataset$field5 <- as.numeric(SICdb_dataset$field5)
 
 SICdb_dataset <- filter(SICdb_dataset, field1 %in% patient_list)
 
 SICdb_dataset <- SICdb_dataset %>% mutate(field4 = field4 / 86400)
 SICdb_dataset <- SICdb_dataset %>% mutate(field5 = field5 / 86400)
 
 SICdb_dataset$ICUStay_begin <- as.POSIXlt("2000-01-01 01:01:01", format = "%Y-%m-%d %H:%M:%S")
 
 SICdb_dataset$ICUStay_begin <- SICdb_dataset$ICUStay_begin + as.numeric(SICdb_dataset$field5) * 86400 
 SICdb_dataset$ICUStay_end <- SICdb_dataset$ICUStay_begin + as.numeric(SICdb_dataset$field4) * 86400 
 
 
 SICdb_dataset$DurationICUStay <- difftime(SICdb_dataset$ICUStay_end,SICdb_dataset$ICUStay_begin,unit="days")
 SICdb_dataset$DurationICUStay <- gsub("days", "", SICdb_dataset$DurationICUStay)
 SICdb_dataset$DurationICUStay <- as.numeric(as.character(SICdb_dataset$DurationICUStay))
 rows_to_delete <- which(SICdb_dataset$DurationICUStay <= 1)
 SICdb_dataset <- SICdb_dataset[-rows_to_delete, ]
 
 patient_list <- unique(SICdb_dataset$field1)
 
 
 new_dataset <- list()
 
 for (patient_id in unique(SICdb_dataset$field1)) {
   patient_data_schleife <- SICdb_dataset[SICdb_dataset$field1 == patient_id, ]
   
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
 

 ########################################################################################
 
 RRDia_values <- dbGetQuery(con, "select field2, field4, field5
from data_float_h
where field3 like '702'")
 
 RRSys_values <- dbGetQuery(con, "select field2, field4, field5
from data_float_h
where field3 like '701'")

 so2_values <- dbGetQuery(con, "select field2, field4, field5
from data_float_h
where field3 like '710'")
 
 RRSys_values <- filter(RRSys_values, field2 %in% patient_list)
 RRDia_values <- filter(RRDia_values, field2 %in% patient_list)
 so2_values <- filter(so2_values, field2 %in% patient_list)

 
 colnames(RRDia_values)[colnames(RRDia_values) == "field5"] <- "Value"
 colnames(RRSys_values)[colnames(RRSys_values) == "field5"] <- "Value"
 colnames(so2_values)[colnames(so2_values) == "field5"] <- "Value"
 
 RRDia_values$Value <- str_replace_all(RRDia_values$Value, "[^0-9.-]", "")
 RRDia_values$Value <- as.numeric(RRDia_values$Value)
 
 RRSys_values$Value <- str_replace_all(RRSys_values$Value, "[^0-9.-]", "")
 RRSys_values$Value <- as.numeric(RRSys_values$Value)
 
 so2_values$Value <- str_replace_all(so2_values$Value, "[^0-9.-]", "")
 so2_values$Value <- as.numeric(so2_values$Value)
 
 colnames(RRDia_values)[colnames(RRDia_values) == "field2"] <- "PatientID"
 colnames(RRSys_values)[colnames(RRSys_values) == "field2"] <- "PatientID"
 colnames(so2_values)[colnames(so2_values) == "field2"] <- "PatientID"
 
 colnames(SICdb_dataset)[colnames(SICdb_dataset) == "field1"] <- "PatientID"
 
 
 RRDia_values <- merge(RRDia_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 RRDia_values$DateTime <- RRDia_values$ICUStay_begin + as.numeric(RRDia_values$field4)
 RRDia_values$ICUStay_begin <- NULL
 RRDia_values$field4 <- NULL

 RRSys_values <- merge(RRSys_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 RRSys_values$DateTime <- RRSys_values$ICUStay_begin + as.numeric(RRSys_values$field4) 
 RRSys_values$ICUStay_begin <- NULL
 RRSys_values$field4 <- NULL

 so2_values <- merge(so2_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 so2_values$DateTime <- so2_values$ICUStay_begin + as.numeric(so2_values$field4) 
 so2_values$ICUStay_begin <- NULL
 so2_values$field4 <- NULL
 
 RRDia_values <- filter(RRDia_values, !is.na(Value))
 RRSys_values <- filter(RRSys_values, !is.na(Value))
 so2_values <- filter(so2_values, !is.na(Value))
 
 RRDia_values$PatientID <- as.numeric(RRDia_values$PatientID)
 RRSys_values$PatientID <- as.numeric(RRSys_values$PatientID)
 so2_values$PatientID <- as.numeric(so2_values$PatientID)
 
 
 RRDiaWeightedMeanValue <- get_weighted_mean_values_in_range_patient_list_parallel("RRDiaWeightedMeanValue", RRDia_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$RRDiaWeightedMeanValue <- RRDiaWeightedMeanValue$RRDiaWeightedMeanValue
 
 RRSysWeightedMeanValue <- get_weighted_mean_values_in_range_patient_list_parallel("RRSysWeightedMeanValue", RRSys_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$RRSysWeightedMeanValue <- RRSysWeightedMeanValue$RRSysWeightedMeanValue
 
 sO2WeightedMeanValue <- get_weighted_mean_values_in_range_patient_list_parallel("sO2WeightedMeanValue", so2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$sO2WeightedMeanValue <- sO2WeightedMeanValue$sO2WeightedMeanValue

 ######################################################################################
 Age_values <- dbGetQuery(con, " SELECT field1, field15 from cases")
 
 Age_values <- filter(Age_values, field1 %in% patient_list)
 
 colnames(Age_values)[colnames(Age_values) == "field15"] <- "AgeOnInclusion"
 
 Age_values$AgeOnInclusion <- str_replace_all(Age_values$AgeOnInclusion, "[^0-9.-]", "")
 Age_values$AgeOnInclusion <- as.numeric(Age_values$AgeOnInclusion)
 
 colnames(Age_values)[colnames(Age_values) == "field1"] <- "ID"
 
 Age_values <- filter(Age_values, !is.na(AgeOnInclusion))
 
 new_dataset <- merge(new_dataset, Age_values[, c("ID", "AgeOnInclusion")], by = "ID", all.x = TRUE)
 
 ######################################################################################
 gender_values <- dbGetQuery(con, " SELECT field1, field12 from cases")
 
 gender_values <- filter(gender_values, field1 %in% patient_list)
 
 colnames(gender_values)[colnames(gender_values) == "field12"] <- "gender"
 
 colnames(gender_values)[colnames(gender_values) == "field1"] <- "ID"
 
 gender_values <- filter(gender_values, !is.na(gender))
 
 new_dataset <- merge(new_dataset, gender_values[, c("ID", "gender")], by = "ID", all.x = TRUE)
 
########################################################################################
 
 death_values <- dbGetQuery(con, " SELECT field1, field10 from cases")
 death_values <- death_values %>% slice(-1)
 
 death_values <- filter(death_values, field1 %in% patient_list)
 
 colnames(death_values)[colnames(death_values) == "field1"] <- "PatientID"
 death_values <- filter(death_values, !is.na(field10))
 
 death_values$Value <- 1
 
 death_values <- merge(death_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 death_values$DateTime <- death_values$ICUStay_begin + as.numeric(death_values$field10)
 death_values$ICUStay_begin <- NULL
 death_values$field10 <- NULL
 
 death_values$DateTime <- as.POSIXlt(death_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
 death_values$DateTimeEnd <- death_values$DateTime
 death_values$DateTimeEnd <- as.POSIXlt(death_values$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")
 
 
 Death_in24h <- check_in_range_3("Death_in24h", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*48)

 Death_in24h <- Death_in24h %>%
   mutate(Death_in24h = ifelse((is.na(Death_in24h) | Death_in24h == 'NA'), 0, Death_in24h))
 
 new_dataset$Death_in24h <- Death_in24h$Death_in24h
 
 
 Death_in3d <- check_in_range_3("Death_in3d", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*4)
 
 Death_in3d <- Death_in3d %>%
   mutate(Death_in3d = ifelse((is.na(Death_in3d) | Death_in3d == 'NA'), 0, Death_in3d))
 
 new_dataset$Death_in3d <- Death_in3d$Death_in3d
 
 
 
 Death_in1w <- check_in_range_3("Death_in1w", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*8)
 
 Death_in1w <- Death_in1w %>%
   mutate(Death_in1w = ifelse((is.na(Death_in1w) | Death_in1w == 'NA'), 0, Death_in1w))
 

 
 
  
 Death_in2w <- check_in_range_3("Death_in2w", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*15)
 
 Death_in2w <- Death_in2w %>%
   mutate(Death_in2w = ifelse((is.na(Death_in2w) | Death_in2w == 'NA'), 0, Death_in2w))
 

 
 
  
 Death_in3w <- check_in_range_3("Death_in3w", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*22)
 
 Death_in3w <- Death_in3w %>%
   mutate(Death_in3w = ifelse((is.na(Death_in3w) | Death_in3w == 'NA'), 0, Death_in3w))
 

 colnames(death_values)[colnames(death_values) == "PatientID"] <- "ID"
 #death_values <- filter(death_values, death_values$Value == 1)
 death_values <- death_values %>% distinct(ID, .keep_all = TRUE)
 new_dataset <- merge(new_dataset, death_values[, c("ID", "DateTime")], by = "ID", all.x = TRUE)
 new_dataset$DaystillDeath <- difftime(new_dataset$DateTime,new_dataset$Datum,unit="days")
 new_dataset$DateTime <- NULL

  ####################################################################################
 

 bili_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '333'")
 
 bili_values <- filter(bili_values, field2 %in% patient_list)
 
 colnames(bili_values)[colnames(bili_values) == "field2"] <- "PatientID"
 colnames(bili_values)[colnames(bili_values) == "field5"] <- "Value"
 
 bili_values <- merge(bili_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 bili_values$DateTime <- bili_values$ICUStay_begin + as.numeric(bili_values$field4)
 bili_values$ICUStay_begin <- NULL
 bili_values$field4 <- NULL
 
 bili_values$Value <- str_replace_all(bili_values$Value, "[^0-9.-]", "")
 bili_values$Value <- as.numeric(bili_values$Value)
 bili_values$PatientID <- as.numeric(bili_values$PatientID)
 bili_values <- filter(bili_values, !is.na(Value))
 
 BilirubineWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("BilirubineWeightedMean", bili_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$BilirubineWeightedMean <- BilirubineWeightedMean$BilirubineWeightedMean


 ####################################################################################
 
 ph_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '688'")
 
 ph_values <- filter(ph_values, field2 %in% patient_list)
 
 colnames(ph_values)[colnames(ph_values) == "field2"] <- "PatientID"
 colnames(ph_values)[colnames(ph_values) == "field5"] <- "Value"
 
 ph_values <- merge(ph_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 ph_values$DateTime <- ph_values$ICUStay_begin + as.numeric(ph_values$field4)
 ph_values$ICUStay_begin <- NULL
 ph_values$field4 <- NULL
 
 ph_values$Value <- str_replace_all(ph_values$Value, "[^0-9.-]", "")
 ph_values$Value <- as.numeric(ph_values$Value)
 ph_values$PatientID <- as.numeric(ph_values$PatientID)
 ph_values <- filter(ph_values, !is.na(Value))
 
 PHWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("PHWeightedMean", ph_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$PHWeightedMean <- PHWeightedMean$PHWeightedMean
 
 LowestpH <- get_min_values_in_range_patient_list_parallel("LowestpH", ph_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$LowestpH <- LowestpH$LowestpH
 
 
 ####################################################################################

 
 lac_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '657'")
 
 lac_values <- filter(lac_values, field2 %in% patient_list)
 
 colnames(lac_values)[colnames(lac_values) == "field2"] <- "PatientID"
 colnames(lac_values)[colnames(lac_values) == "field5"] <- "Value"
 
 lac_values <- merge(lac_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 lac_values$DateTime <- lac_values$ICUStay_begin + as.numeric(lac_values$field4)
 lac_values$ICUStay_begin <- NULL
 lac_values$field4 <- NULL
 
 lac_values$Value <- str_replace_all(lac_values$Value, "[^0-9.-]", "")
 lac_values$Value <- as.numeric(lac_values$Value)
 lac_values$PatientID <- as.numeric(lac_values$PatientID)
 lac_values <- filter(lac_values, !is.na(Value))
 
 LactateWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("LactateWeightedMean", lac_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$LactateWeightedMean <- LactateWeightedMean$LactateWeightedMean
 
 HighestLactate <- get_max_values_in_range_patient_list_parallel("HighestLactate", lac_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HighestLactate <- HighestLactate$HighestLactate
 
 
 ####################################################################################
 
 
 
 crea_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '367'")
 
 crea_values <- filter(crea_values, field2 %in% patient_list)
 
 colnames(crea_values)[colnames(crea_values) == "field2"] <- "PatientID"
 colnames(crea_values)[colnames(crea_values) == "field5"] <- "Value"
 
 crea_values <- merge(crea_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 crea_values$DateTime <- crea_values$ICUStay_begin + as.numeric(crea_values$field4)
 crea_values$ICUStay_begin <- NULL
 crea_values$field4 <- NULL
 
 crea_values$Value <- str_replace_all(crea_values$Value, "[^0-9.-]", "")
 crea_values$Value <- as.numeric(crea_values$Value)
 crea_values$PatientID <- as.numeric(crea_values$PatientID)
 crea_values <- filter(crea_values, !is.na(Value))
 
 CreatinineWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("CreatinineWeightedMean", crea_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$CreatinineWeightedMean <- CreatinineWeightedMean$CreatinineWeightedMean
 
 
 ####################################################################################
 

 
 glu_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '656'")
 
 glu_values <- filter(glu_values, field2 %in% patient_list)
 
 colnames(glu_values)[colnames(glu_values) == "field2"] <- "PatientID"
 colnames(glu_values)[colnames(glu_values) == "field5"] <- "Value"
 
 glu_values <- merge(glu_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 glu_values$DateTime <- glu_values$ICUStay_begin + as.numeric(glu_values$field4)
 glu_values$ICUStay_begin <- NULL
 glu_values$field4 <- NULL
 
 glu_values$Value <- str_replace_all(glu_values$Value, "[^0-9.-]", "")
 glu_values$Value <- as.numeric(glu_values$Value)
 glu_values$PatientID <- as.numeric(glu_values$PatientID)
 glu_values <- filter(glu_values, !is.na(Value))
 
 gluWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("gluWeightedMean", glu_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$gluWeightedMean <- gluWeightedMean$gluWeightedMean
 

 ####################################################################################
 
 be_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '668'")
 
 be_values <- filter(be_values, field2 %in% patient_list)
 
 colnames(be_values)[colnames(be_values) == "field2"] <- "PatientID"
 colnames(be_values)[colnames(be_values) == "field5"] <- "Value"
 
 be_values <- merge(be_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 be_values$DateTime <- be_values$ICUStay_begin + as.numeric(be_values$field4)
 be_values$ICUStay_begin <- NULL
 be_values$field4 <- NULL
 
 be_values$Value <- str_replace_all(be_values$Value, "[^0-9.-]", "")
 be_values$Value <- as.numeric(be_values$Value)
 be_values$PatientID <- as.numeric(be_values$PatientID)
 be_values <- filter(be_values, !is.na(Value))
 
 BEWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("BEWeightedMean", be_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$BEWeightedMean <- BEWeightedMean$BEWeightedMean
 
 ###################################################################################

 
 hoc3_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '666'")
 
 hoc3_values <- filter(hoc3_values, field2 %in% patient_list)
 
 colnames(hoc3_values)[colnames(hoc3_values) == "field2"] <- "PatientID"
 colnames(hoc3_values)[colnames(hoc3_values) == "field5"] <- "Value"
 
 hoc3_values <- merge(hoc3_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 hoc3_values$DateTime <- hoc3_values$ICUStay_begin + as.numeric(hoc3_values$field4)
 hoc3_values$ICUStay_begin <- NULL
 hoc3_values$field4 <- NULL
 
 hoc3_values$Value <- str_replace_all(hoc3_values$Value, "[^0-9.-]", "")
 hoc3_values$Value <- as.numeric(hoc3_values$Value)
 hoc3_values$PatientID <- as.numeric(hoc3_values$PatientID)
 hoc3_values <- filter(hoc3_values, !is.na(Value))
 
 HCO3WeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("HCO3WeightedMean", hoc3_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HCO3WeightedMean <- HCO3WeightedMean$HCO3WeightedMean
 
 
 ####################################################################################
 
 
 
 PO2_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '689'")
 
 PO2_values <- filter(PO2_values, field2 %in% patient_list)
 
 colnames(PO2_values)[colnames(PO2_values) == "field2"] <- "PatientID"
 colnames(PO2_values)[colnames(PO2_values) == "field5"] <- "Value"
 
 PO2_values <- merge(PO2_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 PO2_values$DateTime <- PO2_values$ICUStay_begin + as.numeric(PO2_values$field4)
 PO2_values$ICUStay_begin <- NULL
 PO2_values$field4 <- NULL
 
 PO2_values$Value <- str_replace_all(PO2_values$Value, "[^0-9.-]", "")
 PO2_values$Value <- as.numeric(PO2_values$Value)
 PO2_values$PatientID <- as.numeric(PO2_values$PatientID)
 PO2_values <- filter(PO2_values, !is.na(Value))
 
 PO2WeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("PO2WeightedMean", PO2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$PO2WeightedMean <- PO2WeightedMean$PO2WeightedMean
 
 
 ####################################################################################
 
 
 
 PCO2_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '687'")
 
 PCO2_values <- filter(PCO2_values, field2 %in% patient_list)
 
 colnames(PCO2_values)[colnames(PCO2_values) == "field2"] <- "PatientID"
 colnames(PCO2_values)[colnames(PCO2_values) == "field5"] <- "Value"
 
 PCO2_values <- merge(PCO2_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 PCO2_values$DateTime <- PCO2_values$ICUStay_begin + as.numeric(PCO2_values$field4)
 PCO2_values$ICUStay_begin <- NULL
 PCO2_values$field4 <- NULL
 
 PCO2_values$Value <- str_replace_all(PCO2_values$Value, "[^0-9.-]", "")
 PCO2_values$Value <- as.numeric(PCO2_values$Value)
 PCO2_values$PatientID <- as.numeric(PCO2_values$PatientID)
 PCO2_values <- filter(PCO2_values, !is.na(Value))
 
 PCO2WeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("PCO2WeightedMean", PCO2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$PCO2WeightedMean <- PCO2WeightedMean$PCO2WeightedMean
 
 
 ####################################################################################
 
 
 HB_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '658'")
 
 HB_values <- filter(HB_values, field2 %in% patient_list)
 
 colnames(HB_values)[colnames(HB_values) == "field2"] <- "PatientID"
 colnames(HB_values)[colnames(HB_values) == "field5"] <- "Value"
 
 HB_values <- merge(HB_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 HB_values$DateTime <- HB_values$ICUStay_begin + as.numeric(HB_values$field4)
 HB_values$ICUStay_begin <- NULL
 HB_values$field4 <- NULL
 
 HB_values$Value <- str_replace_all(HB_values$Value, "[^0-9.-]", "")
 HB_values$Value <- as.numeric(HB_values$Value)
 HB_values$PatientID <- as.numeric(HB_values$PatientID)
 HB_values <- filter(HB_values, !is.na(Value))
 
 HBWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("HBWeightedMean", HB_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HBWeightedMean <- HBWeightedMean$HBWeightedMean
 
 
 ####################################################################################
 
 
 
 leuco_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '301'")
 
 leuco_values <- filter(leuco_values, field2 %in% patient_list)
 
 colnames(leuco_values)[colnames(leuco_values) == "field2"] <- "PatientID"
 colnames(leuco_values)[colnames(leuco_values) == "field5"] <- "Value"
 
 leuco_values <- merge(leuco_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 leuco_values$DateTime <- leuco_values$ICUStay_begin + as.numeric(leuco_values$field4)
 leuco_values$ICUStay_begin <- NULL
 leuco_values$field4 <- NULL
 
 leuco_values$Value <- str_replace_all(leuco_values$Value, "[^0-9.-]", "")
 leuco_values$Value <- as.numeric(leuco_values$Value)
 leuco_values$PatientID <- as.numeric(leuco_values$PatientID)
 leuco_values <- filter(leuco_values, !is.na(Value))
 
 leucoWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("leucoWeightedMean", leuco_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$leucoWeightedMean <- leucoWeightedMean$leucoWeightedMean
 
 
 ####################################################################################
 
 
 urea_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '355'")
 
 urea_values <- filter(urea_values, field2 %in% patient_list)
 
 colnames(urea_values)[colnames(urea_values) == "field2"] <- "PatientID"
 colnames(urea_values)[colnames(urea_values) == "field5"] <- "Value"
 
 urea_values <- merge(urea_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 urea_values$DateTime <- urea_values$ICUStay_begin + as.numeric(urea_values$field4)
 urea_values$ICUStay_begin <- NULL
 urea_values$field4 <- NULL
 
 urea_values$Value <- str_replace_all(urea_values$Value, "[^0-9.-]", "")
 urea_values$Value <- as.numeric(urea_values$Value)
 urea_values$PatientID <- as.numeric(urea_values$PatientID)
 urea_values <- filter(urea_values, !is.na(Value))
 
 ureaWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("ureaWeightedMean", urea_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$ureaWeightedMean <- ureaWeightedMean$ureaWeightedMean
 
 
 ####################################################################################

 
 
 platelets_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '314'")
 
 platelets_values <- filter(platelets_values, field2 %in% patient_list)
 
 colnames(platelets_values)[colnames(platelets_values) == "field2"] <- "PatientID"
 colnames(platelets_values)[colnames(platelets_values) == "field5"] <- "Value"
 
 platelets_values <- merge(platelets_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 platelets_values$DateTime <- platelets_values$ICUStay_begin + as.numeric(platelets_values$field4)
 platelets_values$ICUStay_begin <- NULL
 platelets_values$field4 <- NULL
 
 platelets_values$Value <- str_replace_all(platelets_values$Value, "[^0-9.-]", "")
 platelets_values$Value <- as.numeric(platelets_values$Value)
 platelets_values$PatientID <- as.numeric(platelets_values$PatientID)
 platelets_values <- filter(platelets_values, !is.na(Value))
 
 plateletsWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("plateletsWeightedMean", platelets_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$plateletsWeightedMean <- plateletsWeightedMean$plateletsWeightedMean
 
 
 ####################################################################################


 HR_values <- dbGetQuery(con, "  select field2, field4, field5
from data_float_h
where field3 like '708'")
 
 HR_values <- filter(HR_values, field2 %in% patient_list)
 
 colnames(HR_values)[colnames(HR_values) == "field2"] <- "PatientID"
 colnames(HR_values)[colnames(HR_values) == "field5"] <- "Value"
 
 HR_values <- merge(HR_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 HR_values$DateTime <- HR_values$ICUStay_begin + as.numeric(HR_values$field4)
 HR_values$ICUStay_begin <- NULL
 HR_values$field4 <- NULL
 
 HR_values$Value <- str_replace_all(HR_values$Value, "[^0-9.-]", "")
 HR_values$Value <- as.numeric(HR_values$Value)
 HR_values$PatientID <- as.numeric(HR_values$PatientID)
 HR_values <- filter(HR_values, !is.na(Value))
 
 HRWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("HRWeightedMean", HR_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HRWeightedMean <- HRWeightedMean$HRWeightedMean
 
 
 ####################################################################################
 
 
 
 Temp_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM data_float_h
WHERE field3 LIKE '709'")
 
 Temp_values <- filter(Temp_values, field2 %in% patient_list)
 
 colnames(Temp_values)[colnames(Temp_values) == "field2"] <- "PatientID"
 colnames(Temp_values)[colnames(Temp_values) == "field5"] <- "Value"
 
 Temp_values <- merge(Temp_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 Temp_values$DateTime <- Temp_values$ICUStay_begin + as.numeric(Temp_values$field4)
 Temp_values$ICUStay_begin <- NULL
 Temp_values$field4 <- NULL
 
 Temp_values$Value <- str_replace_all(Temp_values$Value, "[^0-9.-]", "")
 Temp_values$Value <- as.numeric(Temp_values$Value)
 Temp_values$PatientID <- as.numeric(Temp_values$PatientID)
 Temp_values <- filter(Temp_values, !is.na(Value))
 
 TempWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("TempWeightedMean", Temp_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$TempWeightedMean <- TempWeightedMean$TempWeightedMean
 
 
 ####################################################################################
 
 
 
 K_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '685'")
 
 K_values <- filter(K_values, field2 %in% patient_list)
 
 colnames(K_values)[colnames(K_values) == "field2"] <- "PatientID"
 colnames(K_values)[colnames(K_values) == "field5"] <- "Value"
 
 K_values <- merge(K_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 K_values$DateTime <- K_values$ICUStay_begin + as.numeric(K_values$field4)
 K_values$ICUStay_begin <- NULL
 K_values$field4 <- NULL
 
 K_values$Value <- str_replace_all(K_values$Value, "[^0-9.-]", "")
 K_values$Value <- as.numeric(K_values$Value)
 K_values$PatientID <- as.numeric(K_values$PatientID)
 K_values <- filter(K_values, !is.na(Value))
 
 KWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("KWeightedMean", K_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$KWeightedMean <- KWeightedMean$KWeightedMean
 
 
 ####################################################################################
 
 
 
 Na_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '686'")
 
 Na_values <- filter(Na_values, field2 %in% patient_list)
 
 colnames(Na_values)[colnames(Na_values) == "field2"] <- "PatientID"
 colnames(Na_values)[colnames(Na_values) == "field5"] <- "Value"
 
 Na_values <- merge(Na_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 Na_values$DateTime <- Na_values$ICUStay_begin + as.numeric(Na_values$field4)
 Na_values$ICUStay_begin <- NULL
 Na_values$field4 <- NULL
 
 Na_values$Value <- str_replace_all(Na_values$Value, "[^0-9.-]", "")
 Na_values$Value <- as.numeric(Na_values$Value)
 Na_values$PatientID <- as.numeric(Na_values$PatientID)
 Na_values <- filter(Na_values, !is.na(Value))
 
 NaWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("NaWeightedMean", Na_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$NaWeightedMean <- NaWeightedMean$NaWeightedMean
 
 
 ####################################################################################
 
 
 
 Cl_values <- dbGetQuery(con, "  SELECT field2, field4, field5
FROM laboratory
WHERE field3 LIKE '683'")
 
 Cl_values <- filter(Cl_values, field2 %in% patient_list)
 
 colnames(Cl_values)[colnames(Cl_values) == "field2"] <- "PatientID"
 colnames(Cl_values)[colnames(Cl_values) == "field5"] <- "Value"
 
 Cl_values <- merge(Cl_values, SICdb_dataset[, c("PatientID", "ICUStay_begin")], by = "PatientID", all.x = TRUE)
 Cl_values$DateTime <- Cl_values$ICUStay_begin + as.numeric(Cl_values$field4)
 Cl_values$ICUStay_begin <- NULL
 Cl_values$field4 <- NULL
 
 Cl_values$Value <- str_replace_all(Cl_values$Value, "[^0-9.-]", "")
 Cl_values$Value <- as.numeric(Cl_values$Value)
 Cl_values$PatientID <- as.numeric(Cl_values$PatientID)
 Cl_values <- filter(Cl_values, !is.na(Value))
 
 ClWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("ClWeightedMean", Cl_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$ClWeightedMean <- ClWeightedMean$ClWeightedMean
 
 
 ####################################################################################
 
 Height_values <- dbGetQuery(con, " SELECT field1, field14 from cases")

 Height_values <- filter(Height_values, field1 %in% patient_list)
 
 colnames(Height_values)[colnames(Height_values) == "field14"] <- "Height"
 
 Height_values$Height <- str_replace_all(Height_values$Height, "[^0-9.-]", "")
 Height_values$Height <- as.numeric(Height_values$Height)
 
 colnames(Height_values)[colnames(Height_values) == "field1"] <- "ID"
 
 Height_values <- filter(Height_values, !is.na(Height))
 
 new_dataset <- merge(new_dataset, Height_values[, c("ID", "Height")], by = "ID", all.x = TRUE)
 
 ########################################################################################
 Weight_values <- dbGetQuery(con, " SELECT field1, field13 from cases")

 Weight_values <- filter(Weight_values, field1 %in% patient_list)
 
 colnames(Weight_values)[colnames(Weight_values) == "field13"] <- "Weight"
 
 Weight_values$Weight <- str_replace_all(Weight_values$Weight, "[^0-9.-]", "")
 Weight_values$Weight <- as.numeric(Weight_values$Weight)
 
 colnames(Weight_values)[colnames(Weight_values) == "field1"] <- "ID"
 
 Weight_values <- filter(Weight_values, !is.na(Weight))
 
 new_dataset <- merge(new_dataset, Weight_values[, c("ID", "Weight")], by = "ID", all.x = TRUE)
 
 
 new_dataset$Weight <-  new_dataset$Weight/1000

 ####################################################################################

 path <- "/home/scherer"  
 path <- "D:"  
 filename <- paste0("SICdb_last.csv")
 filepath <- file.path(path, filename)
 write.csv(new_dataset, file = filepath, fileEncoding = "UTF-8")
 

 