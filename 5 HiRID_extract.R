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
#library(RPostgres)



#db_folder_path <- "/media/felixmichelson/T7/" #AG Schock
db_folder_path <- "/media/scherer/T7/2_Hirid/" #AG Schock
#db_folder_path <- "D:/DailyDeathProject_Github" #Zuhause
db_file_path <- file.path(db_folder_path, "hirid.sqlite")

con <- dbConnect(RSQLite::SQLite(), dbname = db_file_path)



result <- dbGetQuery(con, "

SELECT field3
FROM observation_table AS obs
WHERE obs.field8 IN ('24000732', '24000485', '24000524')
      AND obs.field7 > 2


INTERSECT

SELECT pharma.field1
FROM pharma_records AS pharma
WHERE pharma.field2 IN ('1000462', '1000656', '1000657', '1000658', '71', '1000750', 
                        '1000649', '1000650', '1000655', '112', '113', '1000793', 
                        '1001061', '426')
")


#result$field3 <- as.numeric(result$field3)

 patient_list <- unique(result$field3)
 
 HIRID_dataset <- dbGetQuery(con, "SELECT field1, field2 FROM general_table")
 
 HIRID_dataset <- HIRID_dataset %>% slice(-1)
 HIRID_dataset$field1 <- as.numeric(HIRID_dataset$field1)
 HIRID_dataset$field2 <- as.POSIXlt(HIRID_dataset$field2, format = "%Y-%m-%d %H:%M:%S")

 HIRID_dataset <- filter(HIRID_dataset, field1 %in% patient_list)

 RRSys_values <- dbGetQuery(con, "select field3, field1, field7
from observation_table
where field8 like '600' OR field8 like '100'")
 RRSys_values <- filter(RRSys_values, field3 %in% patient_list)
 RRSys_values$field1 <- as.POSIXlt(RRSys_values$field1, format = "%Y-%m-%d %H:%M:%S")
 
 
 lastdates_values <- RRSys_values
 colnames(lastdates_values)[colnames(lastdates_values) == "field3"] <- "PatientID"
 colnames(lastdates_values)[colnames(lastdates_values) == "field1"] <- "Date"
 lastdates_values$Date <- as.POSIXlt(lastdates_values$Date, format = "%Y-%m-%d %H:%M:%S")
 lastdates_values$PatientID <- as.numeric(lastdates_values$PatientID)

 patient_list_df <- data.table(ID = patient_list)
 patient_list_df$ID <- as.numeric(patient_list_df$ID)
 last_dates <- get_last_event_patient_list_hirid('last_dates', lastdates_values, patient_list_df)
 first_dates <- get_first_event_patient_list_hirid('first_dates', lastdates_values, patient_list_df)
 colnames(last_dates)[colnames(last_dates) == "ID"] <- "field1"
 colnames(first_dates)[colnames(first_dates) == "ID"] <- "field1"
 HIRID_dataset <- merge(HIRID_dataset, last_dates[, c("field1", "last_dates_last_date")], by = "field1", all.x = TRUE)
 HIRID_dataset <- merge(HIRID_dataset, first_dates[, c("field1", "first_dates_first_date")], by = "field1", all.x = TRUE)
 colnames(HIRID_dataset)[colnames(HIRID_dataset) == "last_dates_last_date"] <- "ICUStay_end"
 colnames(HIRID_dataset)[colnames(HIRID_dataset) == "first_dates_first_date"] <- "ICUStay_begin"
 
 
 HIRID_dataset$DurationICUStay <- difftime(HIRID_dataset$ICUStay_end,HIRID_dataset$ICUStay_begin,unit="days")
 HIRID_dataset$DurationICUStay <- gsub("days", "", HIRID_dataset$DurationICUStay)
 HIRID_dataset$DurationICUStay <- as.numeric(as.character(HIRID_dataset$DurationICUStay))
 rows_to_delete <- which(HIRID_dataset$DurationICUStay <= 1)
 HIRID_dataset <- HIRID_dataset[-rows_to_delete, ]
 #HIRID_dataset <- filter(HIRID_dataset, !is.na(ICUStay_end))
 
 patient_list <- unique(HIRID_dataset$field1)
 
 
 new_dataset <- list()
 
 for (patient_id in unique(HIRID_dataset$field1)) {
   patient_data_schleife <- HIRID_dataset[HIRID_dataset$field1 == patient_id, ]
   
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
 
 RRDia_values <- dbGetQuery(con, "select field3, field1, field7
from observation_table
where field8 like '620' OR field8 like '120'")
 
 so2_values <- dbGetQuery(con, "SELECT field3, field1, field7
from observation_table
where field8 like '4000' OR field8 like '8280' OR field8 like '4200'  ")
 

 RRSys_values <- filter(RRSys_values, field3 %in% patient_list)
 RRSys_values$field1 <- as.POSIXlt(RRSys_values$field1, format = "%Y-%m-%d %H:%M:%S")
 
 RRDia_values <- filter(RRDia_values, field3 %in% patient_list)
 RRDia_values$field1 <- as.POSIXlt(RRDia_values$field1, format = "%Y-%m-%d %H:%M:%S")
 
 so2_values <- filter(so2_values, field3 %in% patient_list)
 so2_values$field1 <- as.POSIXlt(so2_values$field1, format = "%Y-%m-%d %H:%M:%S")

 RRDia_values <- RRDia_values[-which(RRDia_values$field7 < 0), ]
 RRSys_values <- RRSys_values[-which(RRSys_values$field7 < 0), ]

 colnames(RRDia_values)[colnames(RRDia_values) == "field7"] <- "Value"
 colnames(RRSys_values)[colnames(RRSys_values) == "field7"] <- "Value"
 colnames(so2_values)[colnames(so2_values) == "field7"] <- "Value"
 
 RRDia_values$Value <- str_replace_all(RRDia_values$Value, "[^0-9.-]", "")
 RRDia_values$Value <- as.numeric(RRDia_values$Value)
 
 RRSys_values$Value <- str_replace_all(RRSys_values$Value, "[^0-9.-]", "")
 RRSys_values$Value <- as.numeric(RRSys_values$Value)
 
 so2_values$Value <- str_replace_all(so2_values$Value, "[^0-9.-]", "")
 so2_values$Value <- as.numeric(so2_values$Value)
 
 colnames(RRDia_values)[colnames(RRDia_values) == "field1"] <- "DateTime"
 colnames(RRSys_values)[colnames(RRSys_values) == "field1"] <- "DateTime"
 colnames(so2_values)[colnames(so2_values) == "field1"] <- "DateTime"
 
 colnames(RRDia_values)[colnames(RRDia_values) == "field3"] <- "PatientID"
 colnames(RRSys_values)[colnames(RRSys_values) == "field3"] <- "PatientID"
 colnames(so2_values)[colnames(so2_values) == "field3"] <- "PatientID"
 
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
 Age_values <- dbGetQuery(con, " SELECT field1, field4 from general_table")

 Age_values <- filter(Age_values, field1 %in% patient_list)

 colnames(Age_values)[colnames(Age_values) == "field4"] <- "AgeOnInclusion"

 Age_values$AgeOnInclusion <- str_replace_all(Age_values$AgeOnInclusion, "[^0-9.-]", "")
 Age_values$AgeOnInclusion <- as.numeric(Age_values$AgeOnInclusion)
 
 colnames(Age_values)[colnames(Age_values) == "field1"] <- "ID"

 Age_values <- filter(Age_values, !is.na(AgeOnInclusion))

 new_dataset <- merge(new_dataset, Age_values[, c("ID", "AgeOnInclusion")], by = "ID", all.x = TRUE)
 
########################################################################################
 
 death_values <- dbGetQuery(con, " SELECT field1, field5 from general_table")
 death_values <- death_values %>% slice(-1)
 
 death_values <- filter(death_values, field1 %in% patient_list)
 
 colnames(death_values)[colnames(death_values) == "field5"] <- "Value"
 colnames(death_values)[colnames(death_values) == "field1"] <- "PatientID"
 
 death_values$Value <- ifelse(death_values$Value == 'alive', 0, 1)
 death_values <- filter(death_values, !is.na(Value))
 
 colnames(HIRID_dataset)[colnames(HIRID_dataset) == "field1"] <- "PatientID"
 HIRID_dataset$PatientID <- as.numeric(HIRID_dataset$PatientID)
 death_values$PatientID <- as.numeric(death_values$PatientID)
 death_values <- merge(death_values, HIRID_dataset[, c("PatientID", "ICUStay_end")], by = "PatientID", all.x = TRUE)
 
 death_values$DateTime <- as.POSIXlt(death_values$ICUStay_end, format = "%Y-%m-%d %H:%M:%S")
 death_values$DateTimeEnd <- death_values$DateTime
 death_values$DateTimeEnd <- as.POSIXlt(death_values$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")
 
 Death_in24h <- check_in_range_3("Death_in24h", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*48)

 Death_in24h <- Death_in24h %>%
   mutate(Death_in24h = ifelse((Death_in24h == 0 | Death_in24h == 'NA'), 0, Death_in24h))
 
 new_dataset$Death_in24h <- Death_in24h$Death_in24h
 
 new_dataset <- filter(new_dataset, !is.na(Death_in24h))
 
 patient_list_dates <- data.frame(ID = new_dataset$ID)
 
 
 
 Death_in3d <- check_in_range_3("Death_in3d", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*4)
 
 Death_in3d <- Death_in3d %>%
   mutate(Death_in3d = ifelse((Death_in3d == 0 | Death_in3d == 'NA'), 0, Death_in3d))
 
 new_dataset$Death_in3d <- Death_in3d$Death_in3d

 
 
 Death_in1w <- check_in_range_3("Death_in1w", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*8)
 
 Death_in1w <- Death_in1w %>%
   mutate(Death_in1w = ifelse((Death_in1w == 0 | Death_in1w == 'NA'), 0, Death_in1w))
 
 new_dataset$Death_in1w <- Death_in1w$Death_in1w
 
 
 Death_in2w <- check_in_range_3("Death_in2w", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*15)
 
 Death_in2w <- Death_in2w %>%
   mutate(Death_in2w = ifelse((Death_in2w == 0 | Death_in2w == 'NA'), 0, Death_in2w))
 
 new_dataset$Death_in2w <- Death_in2w$Death_in2w
 
 
 Death_in3w <- check_in_range_3("Death_in3w", death_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*22)
 
 Death_in3w <- Death_in3w %>%
   mutate(Death_in3w = ifelse((Death_in3w == 0 | Death_in3w == 'NA'), 0, Death_in3w))
 
 new_dataset$Death_in3w <- Death_in3w$Death_in3w
 
 
 colnames(death_values)[colnames(death_values) == "PatientID"] <- "ID"
 death_values <- filter(death_values, death_values$Value == 1)
 death_values <- death_values %>% distinct(ID, .keep_all = TRUE)
 new_dataset <- merge(new_dataset, death_values[, c("ID", "DateTime")], by = "ID", all.x = TRUE)
 new_dataset$DaystillDeath <- difftime(new_dataset$DateTime,new_dataset$Datum,unit="days")
 new_dataset$DateTime <- NULL
 
 ####################################################################################
 
 bili_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '20004300'")
 
 bili_values <- filter(bili_values, field3 %in% patient_list)
 
 colnames(bili_values)[colnames(bili_values) == "field3"] <- "PatientID"
 colnames(bili_values)[colnames(bili_values) == "field1"] <- "DateTime"
 colnames(bili_values)[colnames(bili_values) == "field7"] <- "Value"
 
 bili_values$Value <- str_replace_all(bili_values$Value, "[^0-9.-]", "")
 bili_values$Value <- as.numeric(bili_values$Value)
 bili_values <- filter(bili_values, !is.na(Value))
 
 BilirubineWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("BilirubineWeightedMean", bili_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$BilirubineWeightedMean <- BilirubineWeightedMean$BilirubineWeightedMean
 
 ####################################################################################


 ph_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '20000300'")
 
 ph_values <- filter(ph_values, field3 %in% patient_list)
 
 colnames(ph_values)[colnames(ph_values) == "field3"] <- "PatientID"
 colnames(ph_values)[colnames(ph_values) == "field1"] <- "DateTime"
 colnames(ph_values)[colnames(ph_values) == "field7"] <- "Value"
 
 ph_values$Value <- str_replace_all(ph_values$Value, "[^0-9.-]", "")
 ph_values$Value <- as.numeric(ph_values$Value)
 ph_values <- filter(ph_values, !is.na(Value))
 
 PHWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("PHWeightedMean", ph_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$PHWeightedMean <- PHWeightedMean$PHWeightedMean
 
 LowestpH <- get_min_values_in_range_patient_list_parallel("LowestpH", ph_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$LowestpH <- LowestpH$LowestpH
 
 ####################################################################################

 lac_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '24000732' OR field8 like '24000485' OR field8 like '24000524' ")
 
 lac_values <- filter(lac_values, field3 %in% patient_list)
 
 colnames(lac_values)[colnames(lac_values) == "field3"] <- "PatientID"
 colnames(lac_values)[colnames(lac_values) == "field1"] <- "DateTime"
 colnames(lac_values)[colnames(lac_values) == "field7"] <- "Value"
 
 lac_values$Value <- str_replace_all(lac_values$Value, "[^0-9.-]", "")
 lac_values$Value <- as.numeric(lac_values$Value)
 lac_values <- filter(lac_values, !is.na(Value))
 
 LactateWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("LactateWeightedMean", lac_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$LactateWeightedMean <- LactateWeightedMean$LactateWeightedMean
 
 HighestLactate <- get_max_values_in_range_patient_list_parallel("HighestLactate", lac_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HighestLactate <- HighestLactate$HighestLactate
 
 
 ####################################################################################
 
 
 
 crea_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '20000600'  ")
 
 crea_values <- filter(crea_values, field3 %in% patient_list)
 
 colnames(crea_values)[colnames(crea_values) == "field3"] <- "PatientID"
 colnames(crea_values)[colnames(crea_values) == "field1"] <- "DateTime"
 colnames(crea_values)[colnames(crea_values) == "field7"] <- "Value"
 
 crea_values$Value <- str_replace_all(crea_values$Value, "[^0-9.-]", "")
 crea_values$Value <- as.numeric(crea_values$Value)
 crea_values <- filter(crea_values, !is.na(Value))
 
 CreatinineWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("CreatinineWeightedMean", crea_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$CreatinineWeightedMean <- CreatinineWeightedMean$CreatinineWeightedMean
 
 ####################################################################################
 

 
 glu_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '20005110' OR field8 like '24000523' OR field8 like '24000585' ")
 
 glu_values <- filter(glu_values, field3 %in% patient_list)
 
 colnames(glu_values)[colnames(glu_values) == "field3"] <- "PatientID"
 colnames(glu_values)[colnames(glu_values) == "field1"] <- "DateTime"
 colnames(glu_values)[colnames(glu_values) == "field7"] <- "Value"
 
 glu_values$Value <- str_replace_all(glu_values$Value, "[^0-9.-]", "")
 glu_values$Value <- as.numeric(glu_values$Value)
 glu_values <- filter(glu_values, !is.na(Value))
 
 gluWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("gluWeightedMean", glu_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$gluWeightedMean <- gluWeightedMean$gluWeightedMean
 
 
 ####################################################################################
 
 hoc3_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '20004200'")
 
 hoc3_values <- filter(hoc3_values, field3 %in% patient_list)
 
 colnames(hoc3_values)[colnames(hoc3_values) == "field3"] <- "PatientID"
 colnames(hoc3_values)[colnames(hoc3_values) == "field1"] <- "DateTime"
 colnames(hoc3_values)[colnames(hoc3_values) == "field7"] <- "Value"
 
 hoc3_values$Value <- str_replace_all(hoc3_values$Value, "[^0-9.-]", "")
 hoc3_values$Value <- as.numeric(hoc3_values$Value)
 hoc3_values <- filter(hoc3_values, !is.na(Value))
 
 HCO3WeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("HCO3WeightedMean", hoc3_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HCO3WeightedMean <- HCO3WeightedMean$HCO3WeightedMean
 
 
 #####################################################################################
 
 PO2_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '20000200'")
 
 PO2_values <- filter(PO2_values, field3 %in% patient_list)
 
 colnames(PO2_values)[colnames(PO2_values) == "field3"] <- "PatientID"
 colnames(PO2_values)[colnames(PO2_values) == "field1"] <- "DateTime"
 colnames(PO2_values)[colnames(PO2_values) == "field7"] <- "Value"
 
 PO2_values$Value <- str_replace_all(PO2_values$Value, "[^0-9.-]", "")
 PO2_values$Value <- as.numeric(PO2_values$Value)
 PO2_values$PatientID <- as.numeric(PO2_values$PatientID)
 PO2_values <- filter(PO2_values, !is.na(Value))
 
 PO2WeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("PO2WeightedMean", PO2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$PO2WeightedMean <- PO2WeightedMean$PO2WeightedMean
 
 ####################################################################################
 
 
 PCO2_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '20001200'")
 
 PCO2_values <- filter(PCO2_values, field3 %in% patient_list)
 
 colnames(PCO2_values)[colnames(PCO2_values) == "field3"] <- "PatientID"
 colnames(PCO2_values)[colnames(PCO2_values) == "field1"] <- "DateTime"
 colnames(PCO2_values)[colnames(PCO2_values) == "field7"] <- "Value"
 
 PCO2_values$Value <- str_replace_all(PCO2_values$Value, "[^0-9.-]", "")
 PCO2_values$Value <- as.numeric(PCO2_values$Value)
 PCO2_values$PatientID <- as.numeric(PCO2_values$PatientID)
 PCO2_values <- filter(PCO2_values, !is.na(Value))
 
 PCO2WeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("PCO2WeightedMean", PCO2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$PCO2WeightedMean <- PCO2WeightedMean$PCO2WeightedMean
 
 ####################################################################################
 
 
 
 HB_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '20000900' OR field8 like '24000836' ")
 
 HB_values <- filter(HB_values, field3 %in% patient_list)
 
 colnames(HB_values)[colnames(HB_values) == "field3"] <- "PatientID"
 colnames(HB_values)[colnames(HB_values) == "field1"] <- "DateTime"
 colnames(HB_values)[colnames(HB_values) == "field7"] <- "Value"
 
 HB_values$Value <- str_replace_all(HB_values$Value, "[^0-9.-]", "")
 HB_values$Value <- as.numeric(HB_values$Value)
 HB_values$PatientID <- as.numeric(HB_values$PatientID)
 HB_values <- filter(HB_values, !is.na(Value))
 
 HBWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("HBWeightedMean", HB_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HBWeightedMean <- HBWeightedMean$HBWeightedMean
 
 ####################################################################################
 
 
 
 leuco_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '20000700'")
 
 leuco_values <- filter(leuco_values, field3 %in% patient_list)
 
 colnames(leuco_values)[colnames(leuco_values) == "field3"] <- "PatientID"
 colnames(leuco_values)[colnames(leuco_values) == "field1"] <- "DateTime"
 colnames(leuco_values)[colnames(leuco_values) == "field7"] <- "Value"
 
 leuco_values$Value <- str_replace_all(leuco_values$Value, "[^0-9.-]", "")
 leuco_values$Value <- as.numeric(leuco_values$Value)
 leuco_values$PatientID <- as.numeric(leuco_values$PatientID)
 leuco_values <- filter(leuco_values, !is.na(Value))
 
 leucoWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("leucoWeightedMean", leuco_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$leucoWeightedMean <- leucoWeightedMean$leucoWeightedMean
 
 ####################################################################################
 
 
 
 
 urea_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '20004100'")
 
 urea_values <- filter(urea_values, field3 %in% patient_list)
 
 colnames(urea_values)[colnames(urea_values) == "field3"] <- "PatientID"
 colnames(urea_values)[colnames(urea_values) == "field1"] <- "DateTime"
 colnames(urea_values)[colnames(urea_values) == "field7"] <- "Value"
 
 urea_values$Value <- str_replace_all(urea_values$Value, "[^0-9.-]", "")
 urea_values$Value <- as.numeric(urea_values$Value)
 urea_values$PatientID <- as.numeric(urea_values$PatientID)
 urea_values <- filter(urea_values, !is.na(Value))
 
 ureaWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("ureaWeightedMean", urea_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$ureaWeightedMean <- ureaWeightedMean$ureaWeightedMean
 


 ####################################################################################
 
 Na_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '20000400' OR field8 like '24000519' OR field8 like '24000658' OR field8 like '24000835' OR field8 like '24000866'")
 
 Na_values <- filter(Na_values, field3 %in% patient_list)
 
 colnames(Na_values)[colnames(Na_values) == "field3"] <- "PatientID"
 colnames(Na_values)[colnames(Na_values) == "field1"] <- "DateTime"
 colnames(Na_values)[colnames(Na_values) == "field7"] <- "Value"
 
 Na_values$Value <- str_replace_all(Na_values$Value, "[^0-9.-]", "")
 Na_values$Value <- as.numeric(Na_values$Value)
 Na_values$PatientID <- as.numeric(Na_values$PatientID)
 Na_values <- filter(Na_values, !is.na(Value))
 
 NaWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("NaWeightedMean", Na_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$NaWeightedMean <- NaWeightedMean$NaWeightedMean
 
 ####################################################################################
 
 
 
 K_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '20000500' OR field8 like '24000520' OR field8 like '24000833' OR field8 like '24000867'")
 
 K_values <- filter(K_values, field3 %in% patient_list)
 
 colnames(K_values)[colnames(K_values) == "field3"] <- "PatientID"
 colnames(K_values)[colnames(K_values) == "field1"] <- "DateTime"
 colnames(K_values)[colnames(K_values) == "field7"] <- "Value"
 
 K_values$Value <- str_replace_all(K_values$Value, "[^0-9.-]", "")
 K_values$Value <- as.numeric(K_values$Value)
 K_values$PatientID <- as.numeric(K_values$PatientID)
 K_values <- filter(K_values, !is.na(Value))
 
 KWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("KWeightedMean", K_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$KWeightedMean <- KWeightedMean$KWeightedMean
 
 ####################################################################################
 
 
 
 
 Cl_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '24000439' OR field8 like '24000521' ")
 
 Cl_values <- filter(Cl_values, field3 %in% patient_list)
 
 colnames(Cl_values)[colnames(Cl_values) == "field3"] <- "PatientID"
 colnames(Cl_values)[colnames(Cl_values) == "field1"] <- "DateTime"
 colnames(Cl_values)[colnames(Cl_values) == "field7"] <- "Value"
 
 Cl_values$Value <- str_replace_all(Cl_values$Value, "[^0-9.-]", "")
 Cl_values$Value <- as.numeric(Cl_values$Value)
 Cl_values$PatientID <- as.numeric(Cl_values$PatientID)
 Cl_values <- filter(Cl_values, !is.na(Value))
 
 ClWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("ClWeightedMean", Cl_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$ClWeightedMean <- ClWeightedMean$ClWeightedMean
 
 ####################################################################################
 
 
 
 platelets_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '20000110'")
 
 platelets_values <- filter(platelets_values, field3 %in% patient_list)
 
 colnames(platelets_values)[colnames(platelets_values) == "field3"] <- "PatientID"
 colnames(platelets_values)[colnames(platelets_values) == "field1"] <- "DateTime"
 colnames(platelets_values)[colnames(platelets_values) == "field7"] <- "Value"
 
 platelets_values$Value <- str_replace_all(platelets_values$Value, "[^0-9.-]", "")
 platelets_values$Value <- as.numeric(platelets_values$Value)
 platelets_values$PatientID <- as.numeric(platelets_values$PatientID)
 platelets_values <- filter(platelets_values, !is.na(Value))
 
 plateletsWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("plateletsWeightedMean", platelets_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$plateletsWeightedMean <- plateletsWeightedMean$plateletsWeightedMean
 
 ####################################################################################
 
 
 
 HR_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '200'")
 
 HR_values <- filter(HR_values, field3 %in% patient_list)
 
 colnames(HR_values)[colnames(HR_values) == "field3"] <- "PatientID"
 colnames(HR_values)[colnames(HR_values) == "field1"] <- "DateTime"
 colnames(HR_values)[colnames(HR_values) == "field7"] <- "Value"
 
 HR_values$Value <- str_replace_all(HR_values$Value, "[^0-9.-]", "")
 HR_values$Value <- as.numeric(HR_values$Value)
 HR_values$PatientID <- as.numeric(HR_values$PatientID)
 HR_values <- filter(HR_values, !is.na(Value))
 
 HRWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("HRWeightedMean", HR_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$HRWeightedMean <- HRWeightedMean$HRWeightedMean
 
 ####################################################################################
 
 
 
 Temp_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '410' OR field8 like '7100' OR field8 like '400'")
 
 Temp_values <- filter(Temp_values, field3 %in% patient_list)
 
 colnames(Temp_values)[colnames(Temp_values) == "field3"] <- "PatientID"
 colnames(Temp_values)[colnames(Temp_values) == "field1"] <- "DateTime"
 colnames(Temp_values)[colnames(Temp_values) == "field7"] <- "Value"
 
 Temp_values$Value <- str_replace_all(Temp_values$Value, "[^0-9.-]", "")
 Temp_values$Value <- as.numeric(Temp_values$Value)
 Temp_values$PatientID <- as.numeric(Temp_values$PatientID)
 Temp_values <- filter(Temp_values, !is.na(Value))
 
 TempWeightedMean <- get_weighted_mean_values_in_range_patient_list_parallel("TempWeightedMean", Temp_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
 new_dataset$TempWeightedMean <- TempWeightedMean$TempWeightedMean
 
 ####################################################################################
 
 
 
 height_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '10000450'")
 
 height_values <- filter(height_values, field3 %in% patient_list)
 
 colnames(height_values)[colnames(height_values) == "field3"] <- "ID"
 colnames(height_values)[colnames(height_values) == "field1"] <- "DateTime"
 colnames(height_values)[colnames(height_values) == "field7"] <- "Height"
 
 height_values$Height <- str_replace_all(height_values$Height, "[^0-9.-]", "")
 height_values$Height <- as.numeric(height_values$Height)
 height_values$ID <- as.numeric(height_values$ID)
 height_values <- filter(height_values, !is.na(Height))
 
 height_values <- height_values[!duplicated(height_values$ID), ]
 
 new_dataset <- merge(new_dataset, height_values[, c("ID", "Height")], by = "ID", all.x = TRUE)
 

 
 ####################################################################################
 
 
 
 
 weight_values <- dbGetQuery(con, "  select field3, field1, field7
from observation_table
where field8 like '10000400'")
 
weight_values <- filter(weight_values, field3 %in% patient_list)
 
 colnames(weight_values)[colnames(weight_values) == "field3"] <- "ID"
 colnames(weight_values)[colnames(weight_values) == "field1"] <- "DateTime"
 colnames(weight_values)[colnames(weight_values) == "field7"] <- "Weight"
 
 weight_values$Weight <- str_replace_all(weight_values$Weight, "[^0-9.-]", "")
 weight_values$Weight <- as.numeric(weight_values$Weight)
 weight_values$ID <- as.numeric(weight_values$ID)
 weight_values <- filter(weight_values, !is.na(Weight))
 
 weight_values <- weight_values[!duplicated(weight_values$ID), ]
 
 new_dataset <- merge(new_dataset, weight_values[, c("ID", "Weight")], by = "ID", all.x = TRUE)

 ####################################################################################


 path <- "/home/scherer"  
 filename <- paste0("HiRID_last.csv")
 filepath <- file.path(path, filename)
 write.csv(new_dataset, file = filepath, fileEncoding = "UTF-8")
 
 
