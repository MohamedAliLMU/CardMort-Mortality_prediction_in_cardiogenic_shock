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

mydb <- dbConnect()##########################################

rs <- dbSendQuery(mydb, "select ID,Trial,RandomisationID,InclusionDate,AgeOnInclusion,GenderAtBirth,RandomisationTime from patients
WHERE ID != 1 AND Trial = 1") 
patient_data <- fetch(rs, n=-1) 

patient_list <- patient_data[1]
global_patient_data <- data.frame(ID = patient_list)

global_patient_data <- custom_merge(global_patient_data,data.frame(ID = patient_data[1], InclusionDate = patient_data[4]))
global_patient_data <- custom_merge(global_patient_data,data.frame(ID = patient_data[1], RandomisationID = patient_data[3]))

global_patient_data <- custom_merge(global_patient_data,get_first_event_patient_list("ICUStay",import_event("cs_admission","admission_hospital"),patient_list))

global_patient_data <- na.omit(global_patient_data)

global_patient_data <- global_patient_data %>%
  filter(ICUStay_end != "0000-00-00 00:00:00")

global_patient_data$ICUStay_begin <- as.POSIXlt(as.character(global_patient_data$ICUStay_begin))
global_patient_data$ICUStay_end <- as.POSIXlt(as.character(global_patient_data$ICUStay_end))

global_patient_data$DurationICUStay <- difftime(global_patient_data$ICUStay_end,global_patient_data$ICUStay_begin,unit="days")

global_patient_data$DurationICUStay <- gsub("days", "", global_patient_data$DurationICUStay)

global_patient_data$DurationICUStay <- as.numeric(as.character(global_patient_data$DurationICUStay))

rows_to_delete <- which(global_patient_data$DurationICUStay <= 1)

global_patient_data <- global_patient_data[-rows_to_delete, ]

gestern <- as.POSIXlt(Sys.time()) - 86400  

global_patient_data <- global_patient_data[global_patient_data$ICUStay_end <= gestern, ]

global_patient_data$ICUStay_end <- as.POSIXlt(as.character(global_patient_data$ICUStay_end))

global_patient_data <- subset(global_patient_data, !(RandomisationID %in% c(869429277, 165228446, 254777872, 546215009)))

patient_list <- data.frame(ID = global_patient_data$ID)

#-------------------------------------------------------------------------

new_dataset <- list()


for (patient_id in unique(global_patient_data$ID)) {
  patient_data_schleife <- global_patient_data[global_patient_data$ID == patient_id, ]
  
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

#----------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------

new_dataset <- merge(new_dataset, patient_data[, c("ID", "RandomisationID")], by = "ID", all.x = TRUE)
new_dataset <- merge(new_dataset, patient_data[, c("ID", "AgeOnInclusion")], by = "ID", all.x = TRUE)
new_dataset <- merge(new_dataset, patient_data[, c("ID", "GenderAtBirth")], by = "ID", all.x = TRUE)

new_dataset$GenderAtBirth <- as.factor(new_dataset$GenderAtBirth)
new_dataset$AgeOnInclusion <- as.integer(new_dataset$AgeOnInclusion)

height_values <- import_value("admission","admission_hospital_general_height")
weight_values <- import_value("vitalsigns",7)

Height <- get_first_value_patient_list("Height",height_values,patient_list,"numeric")
Weight <- get_first_value_patient_list("Weight",weight_values,patient_list,"numeric")

new_dataset <- merge(new_dataset, Height[, c("ID", "Height")], by = "ID", all.x = TRUE)
new_dataset <- merge(new_dataset, Weight[, c("ID", "Weight")], by = "ID", all.x = TRUE)

new_dataset$BMI <- new_dataset$Weight / ( as.integer(as.character(new_dataset$Height))/100 *  as.integer(as.character(new_dataset$Height))/100)

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

TypeOfCHF <- get_first_value_patient_list("TypeOfCHF",import_value("admission","admission_hospital_medical_history_chf"),patient_list,"factor") #faktor != numeric
new_dataset <- merge(new_dataset, TypeOfCHF[, c("ID", "TypeOfCHF")], by = "ID", all.x = TRUE)

new_dataset$TypeOfCHF_hfref <- grepl("hfref", tolower(new_dataset$TypeOfCHF))
new_dataset$TypeOfCHF_yes <- grepl("yes|unknown|hfpef|hfmref|hfref", tolower(new_dataset$TypeOfCHF))
new_dataset$TypeOfCHF <- NULL

CauseOfCHF <- get_first_value_patient_list("CauseOfCHF",import_value("admission","admission_hospital_medical_history_struct_hd"),patient_list,"factor")
new_dataset <- merge(new_dataset, CauseOfCHF[, c("ID", "CauseOfCHF")], by = "ID", all.x = TRUE)

new_dataset$CauseOfCHF_icm <- grepl("icm", tolower(new_dataset$CauseOfCHF))
new_dataset$CauseOfCHF_dcm <- grepl("dcm", tolower(new_dataset$CauseOfCHF))
new_dataset$CauseOfCHF_other <- grepl("unknown|other|rcm|hcm|arvc|tako|lvnc", tolower(new_dataset$CauseOfCHF))
new_dataset$CauseOfCHF <- NULL

#-----------------------------------------------------------------------------------------------------------
CardiacDevices <- get_all_values_patient_list("CardiacDevices",import_value("admission","admission_hospital_medical_history_c_devices"),patient_list)
new_dataset <- merge(new_dataset, CardiacDevices[, c("ID", "CardiacDevices")], by = "ID", all.x = TRUE)

new_dataset$CardiacDevices_binaer <- grepl("pacemaker|icd|crt",tolower(new_dataset$CardiacDevices))
new_dataset$CardiacDevices <- NULL

#---------------------------------------------------------------------------------------------------------
Arrythmia <- get_all_values_patient_list("Arrythmia",import_value("admission","admission_hospital_medical_history_arrhythmia"),patient_list)
new_dataset <- merge(new_dataset, Arrythmia[, c("ID", "Arrythmia")], by = "ID", all.x = TRUE)


new_dataset$Arrythmia_0 <- grepl("none",tolower(new_dataset$Arrythmia))
new_dataset$Arrythmia_1 <- grepl("afib|aflut",tolower(new_dataset$Arrythmia))
new_dataset$Arrythmia_2 <- grepl("vfib|mvt|pvt|vt_unknown|tdp",tolower(new_dataset$Arrythmia))
new_dataset$Arrythmia_3 <- grepl("other|unknown|avnrt|avrt|fat|mfat",tolower(new_dataset$Arrythmia))
new_dataset$Arrythmia <- NULL

#---------------------------------------------------------------------------------------------------------

Hypertension <- get_first_value_patient_list("Hypertension",import_value("admission","admission_hospital_cv_factors_hypertension"),patient_list,"factor")
new_dataset <- merge(new_dataset, Hypertension[, c("ID", "Hypertension")], by = "ID", all.x = TRUE)

new_dataset$Hypertension[new_dataset$Hypertension == "%%%UNKNOWN%%%"] <- NA
new_dataset <- new_dataset %>%
  mutate(Hypertension = ifelse(is.na(Hypertension), NA, ifelse(Hypertension == "yes", 1, 0)))


Diabetes <- get_first_value_patient_list("Diabetes",import_value("admission","admission_hospital_cv_factors_diabetes"),patient_list,"factor")
new_dataset <- merge(new_dataset, Diabetes[, c("ID", "Diabetes")], by = "ID", all.x = TRUE)

new_dataset$Diabetes[new_dataset$Diabetes == "%%%UNKNOWN%%%"] <- NA

new_dataset$Diabetes_binaer <- grepl("diet|insulin|medication", tolower(new_dataset$Diabetes))
new_dataset <- new_dataset %>%
  mutate(Diabetes_binaer = ifelse(is.na(Diabetes), NA, Diabetes_binaer))
new_dataset$Diabetes <- NULL


Hyperlipidemia <- get_first_value_patient_list("Hyperlipidemia",import_value("admission","admission_hospital_cv_factors_high_cholesterol"),patient_list,"factor")
new_dataset <- merge(new_dataset, Hyperlipidemia[, c("ID", "Hyperlipidemia")], by = "ID", all.x = TRUE)

new_dataset$Hyperlipidemia[new_dataset$Hyperlipidemia == "%%%UNKNOWN%%%"] <- NA
new_dataset <- new_dataset %>%
  mutate(Hyperlipidemia = ifelse(is.na(Hyperlipidemia), NA, ifelse(Hyperlipidemia == "yes", 1, 0)))


Smoking <- get_first_value_patient_list("Smoking",import_value("admission","admission_hospital_cv_factors_smoker"),patient_list,"factor")
new_dataset <- merge(new_dataset, Smoking[, c("ID", "Smoking")], by = "ID", all.x = TRUE)

new_dataset$Smoking <- replace(new_dataset$Smoking, new_dataset$Smoking == "", NA)
new_dataset$Smoking[new_dataset$Smoking == "%%%UNKNOWN%%%"] <- NA
new_dataset$Smoking_ex_smoker <- grepl("ex",tolower(new_dataset$Smoking))
new_dataset$Smoking_active_smoker <- grepl("active",tolower(new_dataset$Smoking))

new_dataset <- new_dataset %>%
  mutate(Smoking_ex_smoker = ifelse(is.na(Smoking), NA, Smoking_ex_smoker))
new_dataset <- new_dataset %>%
  mutate(Smoking_active_smoker = ifelse(is.na(Smoking), NA, Smoking_active_smoker))
new_dataset$Smoking <- NULL

#------------------------------------------------------------------------------------------------------------
COPD <- get_first_value_patient_list("COPD",import_value("admission","admission_hospital_medical_history_copd"),patient_list,"factor")
new_dataset <- merge(new_dataset, COPD[, c("ID", "COPD")], by = "ID", all.x = TRUE)
new_dataset <- new_dataset %>%
  mutate(COPD = ifelse(is.na(COPD), NA, ifelse(COPD == "yes", 1, 0)))


Asthma <- get_first_value_patient_list("Asthma",import_value("admission","admission_hospital_medical_history_asthma"),patient_list,"factor")
new_dataset <- merge(new_dataset, Asthma[, c("ID", "Asthma")], by = "ID", all.x = TRUE)
new_dataset <- new_dataset %>%
  mutate(Asthma = ifelse(is.na(Asthma), NA, ifelse(Asthma == "yes", 1, 0)))


CKD <- get_first_value_patient_list("CKD",import_value("admission","admission_hospital_medical_history_ckd"),patient_list,"factor")
new_dataset <- merge(new_dataset, CKD[, c("ID", "CKD")], by = "ID", all.x = TRUE)

new_dataset$CKD_binaer <- grepl("yes|g1|g2|g3a|g3b|g4|g5", tolower(new_dataset$CKD))
new_dataset$CKD <- NULL

CAD <- get_first_value_patient_list("CAD",import_value("admission","admission_hospital_medical_history_cad"),patient_list,"factor")
new_dataset <- merge(new_dataset, CAD[, c("ID", "CAD")], by = "ID", all.x = TRUE)
new_dataset <- new_dataset %>%
  mutate(CAD = ifelse(is.na(CAD), NA, ifelse(CAD == "yes", 1, 0)))


AfibByICD <- get_first_value_patient_list("AfibByICD",import_value("diagnoses","I48"),patient_list,"factor")
new_dataset <- merge(new_dataset, AfibByICD[, c("ID", "AfibByICD")], by = "ID", all.x = TRUE)
new_dataset <- new_dataset %>%
  mutate(AfibByICD = ifelse(!is.na(AfibByICD), TRUE, FALSE))


Cancer <- get_first_value_patient_list("Cancer",import_value("admission","admission_hospital_medical_history_cancer"),patient_list,"factor")
new_dataset <- merge(new_dataset, Cancer[, c("ID", "Cancer")], by = "ID", all.x = TRUE)
new_dataset <- new_dataset %>%
  mutate(Cancer = ifelse(is.na(Cancer), NA, ifelse(Cancer == "yes", 1, 0)))


Metastatic_Carcinoma <- get_first_value_patient_list("Metastatic Carcinoma",import_value("admission","admission_hospital_medical_history_metastatic"),patient_list,"factor")
new_dataset <- merge(new_dataset, Metastatic_Carcinoma[, c("ID", "Metastatic Carcinoma")], by = "ID", all.x = TRUE)
colnames(new_dataset)[colnames(new_dataset) == "Metastatic Carcinoma"] <- "Metastatic_Carcinoma"
new_dataset <- new_dataset %>%
  mutate(Metastatic_Carcinoma = ifelse(is.na(Metastatic_Carcinoma), NA, ifelse(Metastatic_Carcinoma == "yes", 1, 0)))


Immunosuppression <- get_first_value_patient_list("Immunosuppression",import_value("admission","admission_hospital_medical_history_imsup"),patient_list,"factor")
new_dataset <- merge(new_dataset, Immunosuppression[, c("ID", "Immunosuppression")], by = "ID", all.x = TRUE)
new_dataset <- new_dataset %>%
  mutate(Immunosuppression = ifelse(is.na(Immunosuppression), NA, ifelse(Immunosuppression == "yes", 1, 0)))


ChronicDialysis <- get_first_value_patient_list("ChronicDialysis",import_value("admission","admission_hospital_medical_history_dialysis"),patient_list)
new_dataset <- merge(new_dataset, ChronicDialysis[, c("ID", "ChronicDialysis")], by = "ID", all.x = TRUE)
new_dataset <- new_dataset %>%
  mutate(ChronicDialysis = ifelse(is.na(ChronicDialysis), NA, ifelse(ChronicDialysis == "yes", 1, 0)))


#----------------------------------------------------------------------------------------------

PADInterventions <- import_value("admission","admission_hospital_medical_history_pad_int")

PADInterventionsAdmission <- get_first_value_patient_list("PADInterventionsAdmission",PADInterventions,patient_list)
PADInterventionsAdmission$PADInterventionsAdmission[which(PADInterventionsAdmission$PADInterventionsAdmission == "no")] <- FALSE

PADInterventionsAdmission$PADInterventionsAdmission[PADInterventionsAdmission$PADInterventionsAdmission == ""] <- NA

PADInterventionsAdmission$PADInterventionsAdmission_binaer <- !grepl("false", tolower(PADInterventionsAdmission$PADInterventionsAdmission))

PADInterventionsAdmission <- PADInterventionsAdmission %>%
  mutate(PADInterventionsAdmission_binaer = ifelse(PADInterventionsAdmission == "%%%UNKNOWN%%%", TRUE, PADInterventionsAdmission_binaer))

new_dataset <- merge(new_dataset, PADInterventionsAdmission[, c("ID", "PADInterventionsAdmission_binaer")], by = "ID", all.x = TRUE)

#---------------------------------------------------------------------------------------------
PreviousMyocardialInfarction_values <- import_value("admission","admission_hospital_medical_history_mi")
PreviousPAD_values <- import_value("admission","admission_hospital_medical_history_pad")

PreviousMyocardialInfarction <- get_first_value_patient_list("PreviousMyocardialInfarction",PreviousMyocardialInfarction_values,patient_list)
PreviousPAD <- get_first_value_patient_list("PreviousPAD",PreviousPAD_values,patient_list)

PreviousMyocardialInfarction$PreviousMyocardialInfarction[which(PreviousMyocardialInfarction$PreviousMyocardialInfarction == "yes")] <- TRUE
PreviousPAD$PreviousPAD[which(PreviousPAD$PreviousPAD == "yes")] <- TRUE
PreviousMyocardialInfarction$PreviousMyocardialInfarction[which(PreviousMyocardialInfarction$PreviousMyocardialInfarction == "no")] <- FALSE
PreviousPAD$PreviousPAD[which(PreviousPAD$PreviousPAD == "no")] <- FALSE

new_dataset <- merge(new_dataset, PreviousMyocardialInfarction[, c("ID", "PreviousMyocardialInfarction")], by = "ID", all.x = TRUE)
new_dataset <- merge(new_dataset, PreviousPAD[, c("ID", "PreviousPAD")], by = "ID", all.x = TRUE)
new_dataset$PreviousMyocardialInfarction[new_dataset$PreviousMyocardialInfarction == "%%%UNKNOWN%%%"] <- NA
new_dataset$PreviousPAD[new_dataset$PreviousPAD == "%%%UNKNOWN%%%"] <- NA

#--------------------------------------------------------------------------------------------
PreviousCABG_values <- import_value("admission","admission_hospital_medical_history_cabg")
PreviousCABG <- get_first_value_patient_list("PreviousCABG",PreviousCABG_values,patient_list)

PreviousCABG$PreviousCABG[PreviousCABG$PreviousCABG == ""] <- NA

PreviousCABG$PreviousCABG_binaer <- !grepl("no", tolower(PreviousCABG$PreviousCABG))

PreviousCABG <- PreviousCABG %>%
  mutate(PreviousCABG_binaer = ifelse(PreviousCABG == "%%%UNKNOWN%%%", TRUE, PreviousCABG_binaer))

new_dataset <- merge(new_dataset, PreviousCABG[, c("ID", "PreviousCABG_binaer")], by = "ID", all.x = TRUE)

#----------------------------------------------------------------------------------------------
PreviousPCI_values <- import_value("admission","admission_hospital_medical_history_pci")
PreviousPCI <- get_first_value_patient_list("PreviousPCI",PreviousPCI_values,patient_list)

PreviousPCI$PreviousPCI[PreviousPCI$PreviousPCI == ""] <- NA

PreviousPCI$PreviousPCI_binaer <- !grepl("no", tolower(PreviousPCI$PreviousPCI))

PreviousPCI <- PreviousPCI %>%
  mutate(PreviousPCI_binaer = ifelse(PreviousPCI == "%%%UNKNOWN%%%", TRUE, PreviousPCI_binaer))

new_dataset <- merge(new_dataset, PreviousPCI[, c("ID", "PreviousPCI_binaer")], by = "ID", all.x = TRUE)

#----------------------------------------------------------------------------------------------
PreviousStroke_values <- import_value("admission","admission_hospital_medical_history_stroke")
PreviousStroke <- get_first_value_patient_list("PreviousStroke",PreviousStroke_values,patient_list)

PreviousStroke$PreviousStroke[PreviousStroke$PreviousStroke == ""] <- NA

PreviousStroke$PreviousStroke_binaer <- !grepl("no", tolower(PreviousStroke$PreviousStroke))

PreviousStroke <- PreviousStroke %>%
  mutate(PreviousStroke_binaer = ifelse(PreviousStroke == "%%%UNKNOWN%%%", TRUE, PreviousStroke_binaer))

new_dataset <- merge(new_dataset, PreviousStroke[, c("ID", "PreviousStroke_binaer")], by = "ID", all.x = TRUE)

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

EtiologyCardiogenicShock_values  <- import_value("inclusion","inclusion_etiology_cardiogenic_shock")
ElectiveProcedureCauseofCS_values  <- import_value("inclusion","inclusion_etiology_elective_proc")

EtiologyCardiogenicShock <- get_first_value_patient_list("EtiologyCardiogenicShock",EtiologyCardiogenicShock_values,patient_list)
ElectiveProcedureCauseofCS <- get_first_value_patient_list("ElectiveProcedureCauseofCS",ElectiveProcedureCauseofCS_values,patient_list)

EtiologyCardiogenicShock$EtiologyCardiogenicShock[EtiologyCardiogenicShock$EtiologyCardiogenicShock == "%%%UNKNOWN%%%"] <- NA
ElectiveProcedureCauseofCS$ElectiveProcedureCauseofCS[ElectiveProcedureCauseofCS$ElectiveProcedureCauseofCS == "%%%UNKNOWN%%%"] <- NA

new_dataset <- merge(new_dataset, EtiologyCardiogenicShock[, c("ID", "EtiologyCardiogenicShock")], by = "ID", all.x = TRUE)
new_dataset <- merge(new_dataset, ElectiveProcedureCauseofCS[, c("ID", "ElectiveProcedureCauseofCS")], by = "ID", all.x = TRUE)

EtiologyCardiogenicShock$EtiologyCardiogenicShock_stemi <- ifelse(grepl("stemi", tolower(EtiologyCardiogenicShock$EtiologyCardiogenicShock)) & !grepl("nstemi", tolower(EtiologyCardiogenicShock$EtiologyCardiogenicShock)), TRUE, FALSE)
EtiologyCardiogenicShock$EtiologyCardiogenicShock_nstemi <- grepl("nstemi", tolower(EtiologyCardiogenicShock$EtiologyCardiogenicShock))
EtiologyCardiogenicShock$EtiologyCardiogenicShock_cardiomyopathy <- grepl("cardiomyopathy", tolower(EtiologyCardiogenicShock$EtiologyCardiogenicShock))
EtiologyCardiogenicShock$EtiologyCardiogenicShock_arrhythmia <- grepl("arrhythmia", tolower(EtiologyCardiogenicShock$EtiologyCardiogenicShock))
EtiologyCardiogenicShock$EtiologyCardiogenicShock_valvular <- grepl("valvular", tolower(EtiologyCardiogenicShock$EtiologyCardiogenicShock))
EtiologyCardiogenicShock$EtiologyCardiogenicShock_other <- !grepl("valvular|stemi|nstemi|cardiomyopathy|arrhythmia", tolower(EtiologyCardiogenicShock$EtiologyCardiogenicShock))

new_dataset <- merge(new_dataset, EtiologyCardiogenicShock[, c("ID", "EtiologyCardiogenicShock_stemi")], by = "ID", all.x = TRUE)
new_dataset <- merge(new_dataset, EtiologyCardiogenicShock[, c("ID", "EtiologyCardiogenicShock_nstemi")], by = "ID", all.x = TRUE)
new_dataset <- merge(new_dataset, EtiologyCardiogenicShock[, c("ID", "EtiologyCardiogenicShock_cardiomyopathy")], by = "ID", all.x = TRUE)
new_dataset <- merge(new_dataset, EtiologyCardiogenicShock[, c("ID", "EtiologyCardiogenicShock_arrhythmia")], by = "ID", all.x = TRUE)
new_dataset <- merge(new_dataset, EtiologyCardiogenicShock[, c("ID", "EtiologyCardiogenicShock_valvular")], by = "ID", all.x = TRUE)
new_dataset <- merge(new_dataset, EtiologyCardiogenicShock[, c("ID", "EtiologyCardiogenicShock_other")], by = "ID", all.x = TRUE)
new_dataset$EtiologyCardiogenicShock <- NULL


new_dataset <- new_dataset %>%
  mutate(ElectiveProcedureCauseofCS = ifelse(is.na(ElectiveProcedureCauseofCS), NA, ifelse(ElectiveProcedureCauseofCS == "yes", 1, 0)))

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
##Reanimation##

OHCA_values <- import_value("admission","admission_initial_event_cardiac_arrest")

OHCA <- get_first_value_patient_list("OHCA",OHCA_values,patient_list)
OHCA$OHCA[is.na(OHCA$OHCA)] <- "no"

OHCA$OHCA[which(OHCA$OHCA == "yes")] <- TRUE
OHCA$OHCA[which(OHCA$OHCA == "no")] <- FALSE

new_dataset <- merge(new_dataset, OHCA[, c("ID", "OHCA")], by = "ID", all.x = TRUE)
#--------------------------------------------------------------------------------------
OH_InitialECG_values  <- import_value("admission","admission_initial_event_rhythm")

OH_InitialECG <- get_first_value_patient_list("OH_InitialECG",OH_InitialECG_values,patient_list)

new_dataset <- merge(new_dataset, OH_InitialECG[, c("ID", "OH_InitialECG")], by = "ID", all.x = TRUE)

new_dataset$OH_InitialECG <- ifelse(new_dataset$OHCA == FALSE, "non-applicable", new_dataset$OH_InitialECG)
new_dataset$OH_InitialECG[new_dataset$OH_InitialECG == "%%%UNKNOWN%%%"] <- NA

new_dataset$OH_InitialECG_1 <- grepl("asystole|pea", tolower(new_dataset$OH_InitialECG))
new_dataset$OH_InitialECG_2 <- grepl("vfib|pulseless_vt", tolower(new_dataset$OH_InitialECG))
new_dataset$OH_InitialECG_not_applicable <- ifelse(new_dataset$OHCA == FALSE, TRUE, FALSE)
new_dataset <- new_dataset %>%
  mutate(OH_InitialECG_1 = ifelse(is.na(OH_InitialECG), NA, OH_InitialECG_1))
new_dataset <- new_dataset %>%
  mutate(OH_InitialECG_2 = ifelse(is.na(OH_InitialECG), NA, OH_InitialECG_2))
new_dataset <- new_dataset %>%
  mutate(OH_InitialECG_not_applicable = ifelse(is.na(OH_InitialECG), NA, OH_InitialECG_not_applicable))

new_dataset$OH_InitialECG <- NULL
#-------------------------------------------------------------------------------------------------

OH_BystanderCPRTime_Values <- import_value("admission","admission_initial_event_duration_bystander_cpr")
OH_ProfessionalCPRTime_values <- import_value("admission","admission_initial_event_duration_professional_cpr")                            

OH_BystanderCPRTime <- get_first_value_patient_list("OH_BystanderCPRTime",OH_BystanderCPRTime_Values,patient_list)
OH_ProfessionalCPRTime <- get_first_value_patient_list("OH_ProfessionalCPRTime",OH_ProfessionalCPRTime_values,patient_list)

new_dataset <- merge(new_dataset, OH_BystanderCPRTime[, c("ID", "OH_BystanderCPRTime")], by = "ID", all.x = TRUE)
new_dataset <- merge(new_dataset, OH_ProfessionalCPRTime[, c("ID", "OH_ProfessionalCPRTime")], by = "ID", all.x = TRUE)

new_dataset$OH_BystanderCPRTime[new_dataset$OH_BystanderCPRTime == "%%%UNKNOWN%%%"] <- NA
new_dataset$OH_ProfessionalCPRTime[new_dataset$OH_ProfessionalCPRTime == "%%%UNKNOWN%%%"] <- NA

new_dataset$OH_BystanderCPRTime <- as.numeric(new_dataset$OH_BystanderCPRTime)
new_dataset$OH_ProfessionalCPRTime <- as.numeric(new_dataset$OH_ProfessionalCPRTime)

new_dataset$OH_BystanderCPRTime <- ifelse(new_dataset$OHCA == FALSE, NA, new_dataset$OH_BystanderCPRTime)
new_dataset$OH_ProfessionalCPRTime <- ifelse(new_dataset$OHCA == FALSE, NA, new_dataset$OH_ProfessionalCPRTime)

new_dataset$OH_TotalCPRTime <- rowSums(data.frame(new_dataset$OH_BystanderCPRTime,new_dataset$OH_ProfessionalCPRTime),na.rm=TRUE)
new_dataset <- new_dataset %>%
  mutate(OH_TotalCPRTime = ifelse(is.na(OH_BystanderCPRTime) & is.na(OH_ProfessionalCPRTime) , NA, OH_TotalCPRTime))

new_dataset$OH_BystanderCPRTime <- NULL

new_dataset <- new_dataset %>%
  mutate(OH_ProfessionalCPRTime_factors = ifelse(OH_ProfessionalCPRTime == 0, "0", NA))
new_dataset <- new_dataset %>%
  mutate(OH_ProfessionalCPRTime_factors = ifelse(OH_ProfessionalCPRTime > 0 & OH_ProfessionalCPRTime < 25, "1_24", OH_ProfessionalCPRTime_factors))
new_dataset <- new_dataset %>%
  mutate(OH_ProfessionalCPRTime_factors = ifelse(OH_ProfessionalCPRTime >= 25 & OH_ProfessionalCPRTime < 50, "25_49", OH_ProfessionalCPRTime_factors))
new_dataset <- new_dataset %>%
  mutate(OH_ProfessionalCPRTime_factors = ifelse(OH_ProfessionalCPRTime >= 50, "50_150", OH_ProfessionalCPRTime_factors))
new_dataset <- new_dataset %>%
  mutate(OH_ProfessionalCPRTime_factors = ifelse(OHCA == FALSE & is.na(OH_ProfessionalCPRTime), "not_applicable", OH_ProfessionalCPRTime_factors))
new_dataset$OH_ProfessionalCPRTime <- NULL

new_dataset <- new_dataset %>%
  mutate(OH_TotalCPRTime_factors = ifelse(OH_TotalCPRTime == 0, "0", NA))
new_dataset <- new_dataset %>%
  mutate(OH_TotalCPRTime_factors = ifelse(OH_TotalCPRTime > 0 & OH_TotalCPRTime < 25, "1_24", OH_TotalCPRTime_factors))
new_dataset <- new_dataset %>%
  mutate(OH_TotalCPRTime_factors = ifelse(OH_TotalCPRTime >= 25 & OH_TotalCPRTime < 50, "25_49", OH_TotalCPRTime_factors))
new_dataset <- new_dataset %>%
  mutate(OH_TotalCPRTime_factors = ifelse(OH_TotalCPRTime >= 50, "50_150", OH_TotalCPRTime_factors))
new_dataset <- new_dataset %>%
  mutate(OH_TotalCPRTime_factors = ifelse(OH_ProfessionalCPRTime_factors == "not_applicable", "not_applicable", OH_TotalCPRTime_factors))
new_dataset$OH_TotalCPRTime <- NULL

#--------------------------------------------------------------------------------------------------------

OH_TotalEpinephrine_values <-import_value("admission","admission_initial_event_cumulative_dose_cats")

OH_TotalEpinephrine <- get_first_value_patient_list("OH_TotalEpinephrine",OH_TotalEpinephrine_values,patient_list)

new_dataset <- merge(new_dataset, OH_TotalEpinephrine[, c("ID", "OH_TotalEpinephrine")], by = "ID", all.x = TRUE)

new_dataset$OH_TotalEpinephrine <- replace(new_dataset$OH_TotalEpinephrine, new_dataset$OH_TotalEpinephrine == "", NA)

new_dataset$OH_TotalEpinephrine <- replace(new_dataset$OH_TotalEpinephrine, new_dataset$OH_TotalEpinephrine == "%%%UNKNOWN%%%", NA)
new_dataset$OH_TotalEpinephrine <- as.numeric(new_dataset$OH_TotalEpinephrine)

new_dataset <- new_dataset %>%
  mutate(OH_TotalEpinephrine_factors = ifelse(OH_TotalEpinephrine == 0, "0", NA))
new_dataset <- new_dataset %>%
  mutate(OH_TotalEpinephrine_factors = ifelse(OH_TotalEpinephrine > 0 & OH_TotalEpinephrine < 5, "1_4.9", OH_TotalEpinephrine_factors))
new_dataset <- new_dataset %>%
  mutate(OH_TotalEpinephrine_factors = ifelse(OH_TotalEpinephrine >= 5 & OH_TotalEpinephrine < 10, "5_10", OH_TotalEpinephrine_factors))
new_dataset <- new_dataset %>%
  mutate(OH_TotalEpinephrine_factors = ifelse(OH_TotalEpinephrine >= 10, ">=10", OH_TotalEpinephrine_factors))
new_dataset <- new_dataset %>%
  mutate(OH_TotalEpinephrine_factors = ifelse(OHCA == FALSE & is.na(OH_TotalEpinephrine), "not_applicable", OH_TotalEpinephrine_factors))
new_dataset$OH_TotalEpinephrine <- NULL

#---------------------------------------------------------------------------------------------------------------

OH_EmergencyFibrinolysis_values <- import_value("admission","admission_initial_event_fibrinolysis")

OH_EmergencyFibrinolysis <- get_first_value_patient_list("OH_EmergencyFibrinolysis",OH_EmergencyFibrinolysis_values,patient_list)

new_dataset <- merge(new_dataset, OH_EmergencyFibrinolysis[, c("ID", "OH_EmergencyFibrinolysis")], by = "ID", all.x = TRUE)
new_dataset$OH_EmergencyFibrinolysis <- replace(new_dataset$OH_EmergencyFibrinolysis, new_dataset$OH_EmergencyFibrinolysis == "%%%UNKNOWN%%%", NA)
new_dataset$OH_EmergencyFibrinolysis <- ifelse(new_dataset$OHCA == FALSE & is.na(new_dataset$OH_EmergencyFibrinolysis), "not_applicable", new_dataset$OH_EmergencyFibrinolysis)

new_dataset$OH_EmergencyFibrinolysis_yes <- grepl("yes", tolower(new_dataset$OH_EmergencyFibrinolysis))
new_dataset$OH_EmergencyFibrinolysis_not_applicable <- grepl("not", tolower(new_dataset$OH_EmergencyFibrinolysis))

new_dataset <- new_dataset %>%
  mutate(OH_EmergencyFibrinolysis_yes = ifelse(is.na(OH_EmergencyFibrinolysis), NA, OH_EmergencyFibrinolysis_yes))
new_dataset <- new_dataset %>%
  mutate(OH_EmergencyFibrinolysis_yes = ifelse(OH_EmergencyFibrinolysis_not_applicable == TRUE, "not-applicable", OH_EmergencyFibrinolysis_yes))

new_dataset$OH_EmergencyFibrinolysis_not_applicable <- NULL
new_dataset$OH_EmergencyFibrinolysis <- NULL

#----------------------------------------------------------------------------------------------------------

OH_ROSCAchieved_values <- import_value("admission","admission_initial_event_rosc")
OH_ROSCAchieved <- get_first_value_patient_list("OH_ROSCAchieved",OH_ROSCAchieved_values,patient_list)
new_dataset <- merge(new_dataset, OH_ROSCAchieved[, c("ID", "OH_ROSCAchieved")], by = "ID", all.x = TRUE)

new_dataset$OH_ROSCAchieved <- ifelse(new_dataset$OHCA == FALSE & is.na(new_dataset$OH_ROSCAchieved), "not_applicable", new_dataset$OH_ROSCAchieved)

new_dataset <- new_dataset %>%
  mutate(OH_ROSCAchieved = ifelse(OH_ROSCAchieved == "%%%UNKNOWN%%%", NA, OH_ROSCAchieved))
#--------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
IHCA_event_values <- import_event("cs_admission","admission_scd")
IHCA_event_values$Value  <- TRUE

IHCA_event_values$Date1 <- as.POSIXlt(IHCA_event_values$Date1, format = "%Y-%m-%d %H:%M:%S")
IHCA_event_values$Date2 <- as.POSIXlt(IHCA_event_values$Date2, format = "%Y-%m-%d %H:%M:%S")

IHCA_event <- check_in_range("IHCA_event", IHCA_event_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
Had_IHCA_event <- check_in_range("Had_IHCA_event", IHCA_event_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*250000000000,as.POSIXct(new_dataset$Datum)+3600*24)

IHCA_event$IHCA_event[is.na(IHCA_event$IHCA_event)] <- FALSE
Had_IHCA_event$Had_IHCA_event[is.na(Had_IHCA_event$Had_IHCA_event)] <- FALSE

new_dataset$IHCA_event <- IHCA_event$IHCA_event
new_dataset$Had_IHCA_event <- Had_IHCA_event$Had_IHCA_event

new_dataset$IHCA_event <- grepl("true", tolower(new_dataset$IHCA_event))
new_dataset$Had_IHCA_event <- grepl("true", tolower(new_dataset$Had_IHCA_event))

#----------------------------------------------------------------------------

IH_InitialECG_values  <- import_value("admission","admission_scd_rhythm")

colnames(IH_InitialECG_values)[colnames(IH_InitialECG_values) == "DateTime"] <- "Date1"
colnames(IH_InitialECG_values)[colnames(IH_InitialECG_values) == "DateTimeEnd"] <- "Date2"
IH_InitialECG_values$Date1 <- as.POSIXlt(IH_InitialECG_values$Date1, format = "%Y-%m-%d %H:%M:%S")
IH_InitialECG_values$Date2 <- as.POSIXlt(IH_InitialECG_values$Date2, format = "%Y-%m-%d %H:%M:%S")

IH_InitialECG_FromStayBegin <- check_in_range("IH_InitialECG_FromStayBegin", IH_InitialECG_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*2500000000000000,as.POSIXct(new_dataset$Datum)+3600*24)

new_dataset$IH_InitialECG_FromStayBegin <- IH_InitialECG_FromStayBegin$IH_InitialECG_FromStayBegin

new_dataset$IH_InitialECG_FromStayBegin <- ifelse(new_dataset$Had_IHCA_event == FALSE, "non-applicable", new_dataset$IH_InitialECG_FromStayBegin)
new_dataset$IH_InitialECG_FromStayBegin[new_dataset$IH_InitialECG_FromStayBegin == "%%%UNKNOWN%%%"] <- NA
new_dataset$IH_InitialECG_FromStayBegin[new_dataset$IH_InitialECG_FromStayBegin == "%%%UNKNOWN%%%, %%%UNKNOWN%%%"] <- NA
new_dataset$IH_InitialECG_FromStayBegin[new_dataset$IH_InitialECG_FromStayBegin == "%%%UNKNOWN%%%, %%%UNKNOWN%%%, NA"] <- NA
new_dataset$IH_InitialECG_FromStayBegin[new_dataset$IH_InitialECG_FromStayBegin == "%%%UNKNOWN%%%, NA"] <- NA
new_dataset$IH_InitialECG_FromStayBegin[new_dataset$IH_InitialECG_FromStayBegin == "%%%UNKNOWN%%%, NA, NA, NA, NA"] <- NA
new_dataset$IH_InitialECG_FromStayBegin[new_dataset$IH_InitialECG_FromStayBegin == "NA, %%%UNKNOWN%%%"] <- NA

new_dataset$IH_InitialECG_FromStayBegin_1 <- grepl("asystole|pea", tolower(new_dataset$IH_InitialECG_FromStayBegin))
new_dataset$IH_InitialECG_FromStayBegin_2 <- grepl("vfib|pulseless_vt", tolower(new_dataset$IH_InitialECG_FromStayBegin))
new_dataset$IH_InitialECG_FromStayBegin_other <- grepl("sinus|av_block|other|arrhythmia|pulse_vt", tolower(new_dataset$IH_InitialECG_FromStayBegin))
new_dataset$IH_InitialECG_FromStayBegin_not_applicable <- ifelse(new_dataset$Had_IHCA_event == FALSE, TRUE, FALSE)
new_dataset <- new_dataset %>%
  mutate(IH_InitialECG_FromStayBegin_1 = ifelse(is.na(IH_InitialECG_FromStayBegin), NA, IH_InitialECG_FromStayBegin_1))
new_dataset <- new_dataset %>%
  mutate(IH_InitialECG_FromStayBegin_2 = ifelse(is.na(IH_InitialECG_FromStayBegin), NA, IH_InitialECG_FromStayBegin_2))
new_dataset <- new_dataset %>%
  mutate(IH_InitialECG_FromStayBegin_other = ifelse(is.na(IH_InitialECG_FromStayBegin), NA, IH_InitialECG_FromStayBegin_other))
new_dataset <- new_dataset %>%
  mutate(IH_InitialECG_FromStayBegin_not_applicable = ifelse(is.na(IH_InitialECG_FromStayBegin), NA, IH_InitialECG_FromStayBegin_not_applicable))
new_dataset$IH_InitialECG_FromStayBegin <- NULL

#-------------------------------------------------------------------------------------------

IH_ProfessionalCPRTime_Value <- import_value("admission","admission_scd_duration_professional_cpr")                            

colnames(IH_ProfessionalCPRTime_Value)[colnames(IH_ProfessionalCPRTime_Value) == "DateTime"] <- "Date1"
colnames(IH_ProfessionalCPRTime_Value)[colnames(IH_ProfessionalCPRTime_Value) == "DateTimeEnd"] <- "Date2"

IH_ProfessionalCPRTime_Value$Date1 <- as.POSIXlt(IH_ProfessionalCPRTime_Value$Date1, format = "%Y-%m-%d %H:%M:%S")
IH_ProfessionalCPRTime_Value$Date2 <- as.POSIXlt(IH_ProfessionalCPRTime_Value$Date2, format = "%Y-%m-%d %H:%M:%S")

IH_ProfessionalCPRTimeFromStayBegin <- check_in_range("IH_ProfessionalCPRTimeFromStayBegin", IH_ProfessionalCPRTime_Value, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*25000000000000,as.POSIXct(new_dataset$Datum)+3600*24)

IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin <- gsub("[,%%%UNKNOWN%%%]", "", IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin)
IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin <- gsub("[A]", "", IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin)

IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin <- ifelse(IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin == "", NA, IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin)
IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin <- ifelse(IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin == " ", NA, IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin)
IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin <- ifelse(IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin == "  ", NA, IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin)
IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin <- ifelse(IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin == "   ", NA, IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin)
IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin <- ifelse(IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin == "    ", NA, IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin)
IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin <- ifelse(IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin == "     ", NA, IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin)

IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin_sum <- sapply(strsplit(IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin, " "), function(x) sum(as.numeric(x), na.rm = TRUE))

IH_ProfessionalCPRTimeFromStayBegin <- IH_ProfessionalCPRTimeFromStayBegin %>%
  mutate(IH_ProfessionalCPRTimeFromStayBegin_sum = ifelse(is.na(IH_ProfessionalCPRTimeFromStayBegin), NA, IH_ProfessionalCPRTimeFromStayBegin_sum))

new_dataset$IH_ProfessionalCPRTimeFromStayBegin <- IH_ProfessionalCPRTimeFromStayBegin$IH_ProfessionalCPRTimeFromStayBegin_sum

new_dataset$IH_ProfessionalCPRTimeFromStayBegin <- as.numeric(new_dataset$IH_ProfessionalCPRTimeFromStayBegin)

new_dataset <- new_dataset %>%
  mutate(IH_ProfessionalCPRTimeFromStayBegin_factors = ifelse(IH_ProfessionalCPRTimeFromStayBegin == 0, "0", NA))
new_dataset <- new_dataset %>%
  mutate(IH_ProfessionalCPRTimeFromStayBegin_factors = ifelse(IH_ProfessionalCPRTimeFromStayBegin > 0 & IH_ProfessionalCPRTimeFromStayBegin <= 10, "1_10", IH_ProfessionalCPRTimeFromStayBegin_factors))
new_dataset <- new_dataset %>%
  mutate(IH_ProfessionalCPRTimeFromStayBegin_factors = ifelse(IH_ProfessionalCPRTimeFromStayBegin > 10 & IH_ProfessionalCPRTimeFromStayBegin < 50, "11_49", IH_ProfessionalCPRTimeFromStayBegin_factors))
new_dataset <- new_dataset %>%
  mutate(IH_ProfessionalCPRTimeFromStayBegin_factors = ifelse(IH_ProfessionalCPRTimeFromStayBegin >= 50 , ">=50", IH_ProfessionalCPRTimeFromStayBegin_factors))
new_dataset <- new_dataset %>%
  mutate(IH_ProfessionalCPRTimeFromStayBegin_factors = ifelse(Had_IHCA_event == FALSE, "not_applicable", IH_ProfessionalCPRTimeFromStayBegin_factors))
new_dataset$IH_ProfessionalCPRTimeFromStayBegin <- NULL

#----------------------------------------------------------------------------------------------------

IH_TotalEpinephrine_values <-import_value("admission","admission_scd_cumulative_dose_cats")

colnames(IH_TotalEpinephrine_values)[colnames(IH_TotalEpinephrine_values) == "DateTime"] <- "Date1"
colnames(IH_TotalEpinephrine_values)[colnames(IH_TotalEpinephrine_values) == "DateTimeEnd"] <- "Date2"
IH_TotalEpinephrine_values$Date1 <- as.POSIXlt(IH_TotalEpinephrine_values$Date1, format = "%Y-%m-%d %H:%M:%S")
IH_TotalEpinephrine_values$Date2 <- as.POSIXlt(IH_TotalEpinephrine_values$Date2, format = "%Y-%m-%d %H:%M:%S")

IH_TotalEpinephrineFromStayBegin <- check_in_range("IH_TotalEpinephrineFromStayBegin", IH_TotalEpinephrine_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*2500000000000000,as.POSIXct(new_dataset$Datum)+3600*24)

IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin <- gsub("[,%%%UNKNOWN%%%]", "", IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin)
IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin <- gsub("[A]", "", IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin)

IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin <- ifelse(IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin == "", NA, IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin)
IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin <- ifelse(IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin == " ", NA, IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin)
IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin <- ifelse(IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin == "  ", NA, IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin)
IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin <- ifelse(IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin == "   ", NA, IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin)
IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin <- ifelse(IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin == "    ", NA, IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin)
IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin <- ifelse(IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin == "     ", NA, IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin)

IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin_sum <- sapply(strsplit(IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin, " "), function(x) sum(as.numeric(x), na.rm = TRUE))

IH_TotalEpinephrineFromStayBegin <- IH_TotalEpinephrineFromStayBegin %>%
  mutate(IH_TotalEpinephrineFromStayBegin_sum = ifelse(is.na(IH_TotalEpinephrineFromStayBegin), NA, IH_TotalEpinephrineFromStayBegin_sum))

new_dataset$IH_TotalEpinephrineFromStayBegin <- IH_TotalEpinephrineFromStayBegin$IH_TotalEpinephrineFromStayBegin_sum

new_dataset$IH_TotalEpinephrineFromStayBegin <- as.numeric(new_dataset$IH_TotalEpinephrineFromStayBegin)

new_dataset <- new_dataset %>%
  mutate(IH_TotalEpinephrineFromStayBegin_factors = ifelse(IH_TotalEpinephrineFromStayBegin == 0, "0", NA))
new_dataset <- new_dataset %>%
  mutate(IH_TotalEpinephrineFromStayBegin_factors = ifelse(IH_TotalEpinephrineFromStayBegin > 0 & IH_TotalEpinephrineFromStayBegin < 7, "1_6.9", IH_TotalEpinephrineFromStayBegin_factors))
new_dataset <- new_dataset %>%
  mutate(IH_TotalEpinephrineFromStayBegin_factors = ifelse(IH_TotalEpinephrineFromStayBegin >= 7, ">=7", IH_TotalEpinephrineFromStayBegin_factors))
new_dataset <- new_dataset %>%
  mutate(IH_TotalEpinephrineFromStayBegin_factors = ifelse(Had_IHCA_event == FALSE, "not_applicable", IH_TotalEpinephrineFromStayBegin_factors))
new_dataset$IH_TotalEpinephrineFromStayBegin <- NULL

#-----------------------------------------------------------------------------------------------

IH_EmergencyFibrinolysis_values <- import_value("admission","admission_scd_fibrinolysis")

IH_EmergencyFibrinolysis_values <- subset(IH_EmergencyFibrinolysis_values, Value != "%%%UNKNOWN%%%")
IH_EmergencyFibrinolysis_values <- subset(IH_EmergencyFibrinolysis_values, Value != "")

colnames(IH_EmergencyFibrinolysis_values)[colnames(IH_EmergencyFibrinolysis_values) == "DateTime"] <- "Date1"
colnames(IH_EmergencyFibrinolysis_values)[colnames(IH_EmergencyFibrinolysis_values) == "DateTimeEnd"] <- "Date2"
IH_EmergencyFibrinolysis_values$Date1 <- as.POSIXlt(IH_EmergencyFibrinolysis_values$Date1, format = "%Y-%m-%d %H:%M:%S")
IH_EmergencyFibrinolysis_values$Date2 <- as.POSIXlt(IH_EmergencyFibrinolysis_values$Date2, format = "%Y-%m-%d %H:%M:%S")

IH_EmergencyFibrinolysis <- check_in_range("IH_EmergencyFibrinolysis", IH_EmergencyFibrinolysis_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*2500000000000000,as.POSIXct(new_dataset$Datum)+3600*24)

IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis <- ifelse(IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis == "NA", NA, IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis)
IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis <- ifelse(IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis == "NA, NA", NA, IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis)
IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis <- ifelse(IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis == "NA, NA, NA", NA, IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis)

IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis_no <- grepl("no",tolower(IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis))
IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis_yes <- grepl("yes",tolower(IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis))

IH_EmergencyFibrinolysis <- IH_EmergencyFibrinolysis %>%
  mutate(IH_EmergencyFibrinolysis_neu = ifelse(IH_EmergencyFibrinolysis_no == TRUE, "no", NA))

IH_EmergencyFibrinolysis <- IH_EmergencyFibrinolysis %>%
  mutate(IH_EmergencyFibrinolysis_neu = ifelse(IH_EmergencyFibrinolysis_yes == TRUE, "yes", IH_EmergencyFibrinolysis_neu))

IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis_neu <- ifelse(new_dataset$Had_IHCA_event == FALSE, "not_applicable", IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis_neu)

new_dataset$IH_EmergencyFibrinolysis <- IH_EmergencyFibrinolysis$IH_EmergencyFibrinolysis_neu

#-----------------------------------------------------------------------------------------------------

TherapeuticHypothermia_values <- import_event("cs_procedures","procedures_therapeutic_hypothermia")

TherapeuticHypothermia_values$Date1 <- as.POSIXlt(TherapeuticHypothermia_values$Date1, format = "%Y-%m-%d %H:%M:%S")
TherapeuticHypothermia_values$Date2 <- TherapeuticHypothermia_values$Date1
TherapeuticHypothermia_values$Value <- TRUE
TherapeuticHypothermia_values$Date2 <- as.POSIXlt(TherapeuticHypothermia_values$Date2, format = "%Y-%m-%d %H:%M:%S")

HadTherapeuticHypothermia <- check_in_range("HadTherapeuticHypothermia", TherapeuticHypothermia_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*2500000000,as.POSIXct(new_dataset$Datum)+3600*24)

HadTherapeuticHypothermia$HadTherapeuticHypothermia <- grepl("true",tolower(HadTherapeuticHypothermia$HadTherapeuticHypothermia))
new_dataset$HadTherapeuticHypothermia <- HadTherapeuticHypothermia$HadTherapeuticHypothermia

#---------------------------------------------------------------------------------------------------------

EndpointBleeding_values <- import_event("cs_endpoint","endpoint_bleeding")

EndpointBleeding_values$Date1 <- as.POSIXlt(EndpointBleeding_values$Date1, format = "%Y-%m-%d %H:%M:%S")
EndpointBleeding_values$Date2 <- EndpointBleeding_values$Date1
EndpointBleeding_values$Value <- TRUE
EndpointBleeding_values$Date2 <- as.POSIXlt(EndpointBleeding_values$Date2, format = "%Y-%m-%d %H:%M:%S")

HadBleeding <- check_in_range("HadBleeding", EndpointBleeding_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*2500000000,as.POSIXct(new_dataset$Datum)+3600*24)
HadBleeding$HadBleeding <- grepl("true",tolower(HadBleeding$HadBleeding))

new_dataset$HadBleeding <- HadBleeding$HadBleeding

#---------------------------------------------------------------------------------------------------------

EndpointStroke_values <- import_event("cs_endpoint","endpoint_stroke")

EndpointStroke_values$Date1 <- as.POSIXlt(EndpointStroke_values$Date1, format = "%Y-%m-%d %H:%M:%S")
EndpointStroke_values$Date2 <- EndpointStroke_values$Date1
EndpointStroke_values$Value <- TRUE
EndpointStroke_values$Date2 <- as.POSIXlt(EndpointStroke_values$Date2, format = "%Y-%m-%d %H:%M:%S")

HadStroke <- check_in_range("HadStroke", EndpointStroke_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*2500000000,as.POSIXct(new_dataset$Datum)+3600*24)
HadStroke$HadStroke <- grepl("true",tolower(HadStroke$HadStroke))

new_dataset$HadStroke <- HadStroke$HadStroke

#---------------------------------------------------------------------------------------------------------

EndpointMyocardialInfarctionType_values <- import_value("endpoint","endpoint_mi_type")

colnames(EndpointMyocardialInfarctionType_values)[colnames(EndpointMyocardialInfarctionType_values) == "DateTime"] <- "Date1"
colnames(EndpointMyocardialInfarctionType_values)[colnames(EndpointMyocardialInfarctionType_values) == "DateTimeEnd"] <- "Date2"

EndpointMyocardialInfarctionType_values$Date1 <- as.POSIXlt(EndpointMyocardialInfarctionType_values$Date1, format = "%Y-%m-%d %H:%M:%S")
EndpointMyocardialInfarctionType_values$Date2 <- EndpointMyocardialInfarctionType_values$Date1
EndpointMyocardialInfarctionType_values$Date2 <- as.POSIXlt(EndpointMyocardialInfarctionType_values$Date2, format = "%Y-%m-%d %H:%M:%S")

HadMyocardialInfarction_Type <- check_in_range("HadMyocardialInfarction_Type", EndpointMyocardialInfarctionType_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*2500000000,as.POSIXct(new_dataset$Datum)+3600*24)

new_dataset$HadMyocardialInfarction_Type <- HadMyocardialInfarction_Type$HadMyocardialInfarction_Type

new_dataset$HadMyocardialInfarction_Type1 <- grepl("type1",tolower(new_dataset$HadMyocardialInfarction_Type))

new_dataset$HadMyocardialInfarction_binaer <- grepl("type1",tolower(new_dataset$HadMyocardialInfarction_Type))

new_dataset <- new_dataset %>%
  mutate(HadMyocardialInfarction_Type = ifelse(HadMyocardialInfarction_Type == "NA", NA, HadMyocardialInfarction_Type))

new_dataset <- new_dataset %>%
  mutate(HadMyocardialInfarction_binaer = ifelse(is.na(new_dataset$HadMyocardialInfarction_Type), FALSE, TRUE))

new_dataset$HadMyocardialInfarction_Type <- NULL

#-------------------------------------------------------------------------------------------------------

new_dataset <- replace(new_dataset, new_dataset == TRUE, 1)
new_dataset <- replace(new_dataset, new_dataset == FALSE, 0)
new_dataset$OHCA <- as.numeric(new_dataset$OHCA)


#-----------------------------------------------------------------------------------------------------


epinephrine_perf <- import_value("medication",8)
epinephrine_bolus <- import_value("medication",86)

norepinephrine_perf <- import_value("medication",9)
norepinephrine_bolus <- import_value("medication",49)

dobutamine_perf <- import_value("medication",37)
dobutamine_bolus <- import_value("medication",379)

vasopressin_perf <- import_value("medication",10)
vasopressin_bolus <- import_value("medication",160)

Epinephrine <- get_medication_info_patient_list("Epinephrine",epinephrine_perf, epinephrine_bolus, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24, count_zero = TRUE)


new_dataset$Epinephrine_duration <- Epinephrine$Epinephrine_duration
new_dataset$Epinephrine_weighted_mean <- Epinephrine$Epinephrine_weighted_mean
new_dataset$Epinephrine_full_dose <- Epinephrine$Epinephrine_full_dose


Norepinephrine <- get_medication_info_patient_list("Norepinephrine", norepinephrine_perf, norepinephrine_bolus, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24, count_zero = TRUE)

new_dataset$Norepinephrine_duration <- Norepinephrine$Norepinephrine_duration
new_dataset$Norepinephrine_weighted_mean <- Norepinephrine$Norepinephrine_weighted_mean
new_dataset$Norepinephrine_full_dose <- Norepinephrine$Norepinephrine_full_dose


Dobutamine <- get_medication_info_patient_list("Dobutamine",dobutamine_perf, dobutamine_bolus, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24, count_zero = TRUE)

new_dataset$Dobutamine_duration <- Dobutamine$Dobutamine_duration
new_dataset$Dobutamine_weighted_mean <- Dobutamine$Dobutamine_weighted_mean
new_dataset$Dobutamine_full_dose <- Dobutamine$Dobutamine_full_dose


Vasopressin <- get_medication_info_patient_list("Vasopressin",vasopressin_perf, vasopressin_bolus, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)

new_dataset$Vasopressin_duration <- Vasopressin$Vasopressin_duration
new_dataset$Vasopressin_weighted_mean <- Vasopressin$Vasopressin_weighted_mean
new_dataset$Vasopressin_full_dose <- Vasopressin$Vasopressin_full_dose


new_dataset$Catecholamines <- (new_dataset$Epinephrine_weighted_mean + new_dataset$Norepinephrine_weighted_mean)*100 + new_dataset$Dobutamine_weighted_mean

#######################################################################

#--------------------------------------------------------------------------------------------------------------------------------

propofol_sedation <- import_value("medication",13)
midazolam_sedation <- import_value("medication",16)
ketamin_sedation <- import_value("medication",15)
sufentanyl_sedation <- import_value("medication",14)

propofol_bolus <- import_value("medication",84)
midazolam_bolus <- import_value("medication",85)
ketamin_bolus <- import_value("medication",107)
sufentanyl_bolus <- import_value("medication",54)

Propofol <- get_medication_info_patient_list("Propofol",propofol_sedation, propofol_bolus, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24, count_zero = TRUE)

new_dataset$Propofol_duration <- Propofol$Propofol_duration
new_dataset$Propofol_weighted_mean <- Propofol$Propofol_weighted_mean
new_dataset$Propofol_full_dose <- Propofol$Propofol_full_dose


Midazolam <- get_medication_info_patient_list("Midazolam",midazolam_sedation, midazolam_bolus, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24, count_zero = TRUE)

new_dataset$Midazolam_duration <- Midazolam$Midazolam_duration
new_dataset$Midazolam_weighted_mean <- Midazolam$Midazolam_weighted_mean
new_dataset$Midazolam_full_dose <- Midazolam$Midazolam_full_dose


Ketamin <- get_medication_info_patient_list("Ketamin",ketamin_sedation, ketamin_bolus, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24, count_zero = TRUE)

new_dataset$Ketamin_duration <- Ketamin$Ketamin_duration
new_dataset$Ketamin_weighted_mean <- Ketamin$Ketamin_weighted_mean
new_dataset$Ketamin_full_dose <- Ketamin$Ketamin_full_dose


Sufentanyl <- get_medication_info_patient_list("Sufentanyl",sufentanyl_sedation, sufentanyl_bolus, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24, count_zero = TRUE)

new_dataset$Sufentanyl_duration <- Sufentanyl$Sufentanyl_duration
new_dataset$Sufentanyl_weighted_mean <- Sufentanyl$Sufentanyl_weighted_mean
new_dataset$Sufentanyl_full_dose <- Sufentanyl$Sufentanyl_full_dose

#----------------------------------------------------------------------------------------------
Meropenem_po <- import_value("medication",1)
Meropenem_iv <- import_value("medication",40)
piptaz_perfusor <- import_value("medication",370)
piptaz_bolus <- import_value("medication",27)

PipTaz <- get_medication_info_patient_list("PipTaz",piptaz_perfusor, piptaz_bolus, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24, count_zero = TRUE)

new_dataset$PipTaz_duration <- PipTaz$PipTaz_duration
new_dataset$PipTaz_weighted_mean <- PipTaz$PipTaz_weighted_mean
new_dataset$PipTaz_full_dose <- PipTaz$PipTaz_full_dose

Meropenem_po$DateTimeEnd <- Meropenem_po$DateTime
Meropenem_po$DateTime <- as.POSIXlt(Meropenem_po$DateTime, format = "%Y-%m-%d %H:%M:%S")
Meropenem_po$DateTimeEnd <- as.POSIXlt(Meropenem_po$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")

Meropenem_po_check <- check_in_range_medi("Meropenem_po_check", Meropenem_po, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$Meropenem_po_check <- Meropenem_po_check$Meropenem_po_check
new_dataset$Meropenem_po_check[is.na(new_dataset$Meropenem_po_check)] <- 0

Meropenem_iv$DateTimeEnd <- Meropenem_iv$DateTime
Meropenem_iv$DateTime <- as.POSIXlt(Meropenem_iv$DateTime, format = "%Y-%m-%d %H:%M:%S")
Meropenem_iv$DateTimeEnd <- as.POSIXlt(Meropenem_iv$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")

Meropenem_iv_check <- check_in_range_medi("Meropenem_iv_check", Meropenem_iv, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$Meropenem_iv_check <- Meropenem_iv_check$Meropenem_iv_check
new_dataset$Meropenem_iv_check[is.na(new_dataset$Meropenem_iv_check)] <- 0

#---------------------------------------------------------------------------------------------------
#Antiarrythmika
Amiodaron_perfusor <- import_value("medication",31) 
Amiodaron_po <- import_value("medication",30) 
Amiodaron_iv <- import_value("medication",88) 

Digitoxin_po_1 <- import_value("medication",332)
Digitoxin_po_2 <- import_value("medication",290) 
Digitoxin_po <- rbind(Digitoxin_po_1, Digitoxin_po_2)
Digitoxin_iv <- import_value("medication",254) 
Digoxin_iv <- import_value("medication",132) 
Digoxin_po_1 <- import_value("medication",418) 
Digoxin_po_2 <- import_value("medication",504) 
Digoxin_po <- rbind(Digoxin_po_1, Digoxin_po_2) 


Amiodaron <- get_medication_info_patient_list("Amiodaron",Amiodaron_perfusor, Amiodaron_iv, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24, count_zero = TRUE)

new_dataset$Amiodaron_duration <- Amiodaron$Amiodaron_duration
new_dataset$Amiodaron_weighted_mean <- Amiodaron$Amiodaron_weighted_mean
new_dataset$Amiodaron_full_dose <- Amiodaron$Amiodaron_full_dose

Amiodaron_po$DateTimeEnd <- Amiodaron_po$DateTime
Amiodaron_po$DateTime <- as.POSIXlt(Amiodaron_po$DateTime, format = "%Y-%m-%d %H:%M:%S")
Amiodaron_po$DateTimeEnd <- as.POSIXlt(Amiodaron_po$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")

Amiodaron_po_check <- check_in_range_medi("Amiodaron_po_check", Amiodaron_po, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$Amiodaron_po_check <- Amiodaron_po_check$Amiodaron_po_check
new_dataset$Amiodaron_po_check[is.na(new_dataset$Amiodaron_po_check)] <- 0

Digitoxin_po$DateTimeEnd <- Digitoxin_po$DateTime
Digitoxin_po$DateTime <- as.POSIXlt(Digitoxin_po$DateTime, format = "%Y-%m-%d %H:%M:%S")
Digitoxin_po$DateTimeEnd <- as.POSIXlt(Digitoxin_po$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")

Digitoxin_po_check <- check_in_range_medi("Digitoxin_po_check", Digitoxin_po, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$Digitoxin_po_check <- Digitoxin_po_check$Digitoxin_po_check
new_dataset$Digitoxin_po_check[is.na(new_dataset$Digitoxin_po_check)] <- 0

Digitoxin_iv$DateTimeEnd <- Digitoxin_iv$DateTime
Digitoxin_iv$DateTime <- as.POSIXlt(Digitoxin_iv$DateTime, format = "%Y-%m-%d %H:%M:%S")
Digitoxin_iv$DateTimeEnd <- as.POSIXlt(Digitoxin_iv$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")

Digitoxin_iv_check <- check_in_range_medi("Digitoxin_iv_check", Digitoxin_iv, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$Digitoxin_iv_check <- Digitoxin_iv_check$Digitoxin_iv_check
new_dataset$Digitoxin_iv_check[is.na(new_dataset$Digitoxin_iv_check)] <- 0

Digoxin_iv$DateTimeEnd <- Digoxin_iv$DateTime
Digoxin_iv$DateTime <- as.POSIXlt(Digoxin_iv$DateTime, format = "%Y-%m-%d %H:%M:%S")
Digoxin_iv$DateTimeEnd <- as.POSIXlt(Digoxin_iv$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")

Digoxin_iv_check <- check_in_range_medi("Digoxin_iv_check", Digoxin_iv, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$Digoxin_iv_check <- Digoxin_iv_check$Digoxin_iv_check
new_dataset$Digoxin_iv_check[is.na(new_dataset$Digoxin_iv_check)] <- 0

Digoxin_po$DateTimeEnd <- Digoxin_po$DateTime
Digoxin_po$DateTime <- as.POSIXlt(Digoxin_po$DateTime, format = "%Y-%m-%d %H:%M:%S")
Digoxin_po$DateTimeEnd <- as.POSIXlt(Digoxin_po$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")

Digoxin_po_check <- check_in_range_medi("Digoxin_po_check", Digoxin_po, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$Digoxin_po_check <- Digoxin_po_check$Digoxin_po_check
new_dataset$Digoxin_po_check[is.na(new_dataset$Digoxin_po_check)] <- 0

new_dataset <- new_dataset %>%
  mutate(Had_Digoxin_Digitoxin = ifelse(Digitoxin_po_check != 0 |Digitoxin_iv_check != 0|Digoxin_iv_check != 0|Digoxin_po_check != 0 , 1, 0))

new_dataset$Digitoxin_iv_check <- NULL
new_dataset$Digitoxin_po_check <- NULL
new_dataset$Digoxin_po_check <- NULL

#-------------------------------------------------------------------------------------------------

Heparin_perfusor <- import_value("medication",17) 
Heparin_iv <- import_value("medication",52)

Prednisolon_iv <- import_value("medication",65) 
MethylPrednisolon_iv <- import_value("medication",180) 
Prednisolon_po <- import_value("medication",204) 
MethylPrednisolon_po <- import_value("medication",564) 

MethylPrednisolon_iv$Value <- MethylPrednisolon_iv$Value * 1.25
MethylPrednisolon_po$Value <- MethylPrednisolon_po$Value * 1.25
prednisolon_combi_iv <- rbind(Prednisolon_iv, MethylPrednisolon_iv ) 
prednisolon_combi_po <- rbind(Prednisolon_po, MethylPrednisolon_po ) 

Heparin <- get_medication_info_patient_list("Heparin",Heparin_perfusor, Heparin_iv, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24, count_zero = TRUE)

new_dataset$Heparin_duration <- Heparin$Heparin_duration
new_dataset$Heparin_weighted_mean <- Heparin$Heparin_weighted_mean
new_dataset$Heparin_full_dose <- Heparin$Heparin_full_dose

prednisolon_combi_po$DateTimeEnd <- prednisolon_combi_po$DateTime
prednisolon_combi_po$DateTime <- as.POSIXlt(prednisolon_combi_po$DateTime, format = "%Y-%m-%d %H:%M:%S")
prednisolon_combi_po$DateTimeEnd <- as.POSIXlt(prednisolon_combi_po$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")

prednisolon_combi_po_check <- check_in_range_medi("prednisolon_combi_po_check", prednisolon_combi_po, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$prednisolon_combi_po_check <- prednisolon_combi_po_check$prednisolon_combi_po_check
new_dataset$prednisolon_combi_po_check[is.na(new_dataset$prednisolon_combi_po_check)] <- 0

prednisolon_combi_iv$DateTimeEnd <- prednisolon_combi_iv$DateTime
prednisolon_combi_iv$DateTime <- as.POSIXlt(prednisolon_combi_iv$DateTime, format = "%Y-%m-%d %H:%M:%S")
prednisolon_combi_iv$DateTimeEnd <- as.POSIXlt(prednisolon_combi_iv$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")

prednisolon_combi_iv_check <- check_in_range_medi("prednisolon_combi_iv_check", prednisolon_combi_iv, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$prednisolon_combi_iv_check <- prednisolon_combi_iv_check$prednisolon_combi_iv_check
new_dataset$prednisolon_combi_iv_check[is.na(new_dataset$prednisolon_combi_iv_check)] <- 0


###############################################################################

No_Dokumentation_Values <- c(253, 485, 1704, 1705, 1706, 1707, 2136, 2137, 2138, 3573, 3574, 3575, 3576, 3697, 3698, 3699, 3700, 3701, 3702, 3703, 3704, 3705, 3706, 3707, 3708, 3709, 3710, 3711, 3712, 3713, 3714, 3715, 3716, 3717, 3718, 3719, 3720, 3926, 3927, 3965, 3966, 3967, 3968, 3969, 4396, 4397, 4599, 6846, 6847, 6848, 6849, 6850, 6851, 6852, 6853, 6854, 6855, 6856, 6857, 6858, 6859, 6860, 6861, 6862, 6863, 6864, 6865, 6866, 7072, 7073, 7074, 7075, 7076, 7077, 7078, 7079, 7080, 7081, 7082, 7083, 7084, 7085, 7086, 7087, 7088, 7089, 7308, 7309, 7310, 7311, 7312, 7313, 7314, 7315, 7316, 7317, 7318, 7319, 7320, 7321, 7322, 7323, 7324, 7325, 7326, 7327, 7328, 7329, 8858, 8859, 8860, 8861, 8862, 8863, 8864, 8865, 8866, 9222, 9223, 9224, 9225, 9226, 9227, 9228, 9229, 9230, 9231, 9232, 9233, 9234, 9235, 9236, 9237, 9238, 9637, 9638, 9639, 9640, 9641, 9642, 9643, 9644, 9645, 9646, 9647, 9648, 9649, 9650, 9651, 9652, 9653, 9654, 9655, 9656, 9657, 9658, 9659, 9660, 9661, 9662, 9663, 9664, 9665, 9695, 910017, 10018, 10019, 10020, 10021, 10022, 10023, 10024, 10025, 10026, 11185, 11186, 11187)

new_dataset$X <- 1:nrow(new_dataset)

matching_indices <- which(new_dataset$X %in% No_Dokumentation_Values)

new_dataset$Epinephrine_duration[matching_indices] <- NA
new_dataset$Epinephrine_weighted_mean[matching_indices] <- NA
new_dataset$Epinephrine_full_dose[matching_indices] <- NA
new_dataset$Norepinephrine_duration[matching_indices] <- NA
new_dataset$Norepinephrine_weighted_mean[matching_indices] <- NA
new_dataset$Norepinephrine_full_dose[matching_indices] <- NA
new_dataset$Dobutamine_duration[matching_indices] <- NA
new_dataset$Dobutamine_weighted_mean[matching_indices] <- NA
new_dataset$Dobutamine_full_dose[matching_indices] <- NA
new_dataset$Vasopressin_duration[matching_indices] <- NA
new_dataset$Vasopressin_weighted_mean[matching_indices] <- NA
new_dataset$Vasopressin_full_dose[matching_indices] <- NA
new_dataset$Catecholamines[matching_indices] <- NA

new_dataset$Propofol_duration[matching_indices] <- NA
new_dataset$Propofol_weighted_mean[matching_indices] <- NA
new_dataset$Propofol_full_dose[matching_indices] <- NA
new_dataset$Midazolam_duration[matching_indices] <- NA
new_dataset$Midazolam_weighted_mean[matching_indices] <- NA
new_dataset$Midazolam_full_dose[matching_indices] <- NA
new_dataset$Ketamin_duration[matching_indices] <- NA
new_dataset$Ketamin_weighted_mean[matching_indices] <- NA
new_dataset$Ketamin_full_dose[matching_indices] <- NA
new_dataset$Sufentanyl_duration[matching_indices] <- NA
new_dataset$Sufentanyl_weighted_mean[matching_indices] <- NA
new_dataset$Sufentanyl_full_dose[matching_indices] <- NA

new_dataset$PipTaz_weighted_mean[matching_indices] <- NA
new_dataset$PipTaz_full_dose[matching_indices] <- NA
new_dataset$PipTaz_duration[matching_indices] <- NA
new_dataset$Meropenem_po_check[matching_indices] <- NA
new_dataset$Meropenem_iv_check[matching_indices] <- NA

new_dataset$Amiodaron_weighted_mean[matching_indices] <- NA
new_dataset$Amiodaron_full_dose[matching_indices] <- NA
new_dataset$Amiodaron_duration[matching_indices] <- NA
new_dataset$Amiodaron_po_check[matching_indices] <- NA
new_dataset$Digoxin_iv_check[matching_indices] <- NA
new_dataset$Had_Digoxin_Digitoxin[matching_indices] <- NA

new_dataset$Heparin_weighted_mean[matching_indices] <- NA
new_dataset$Heparin_full_dose[matching_indices] <- NA
new_dataset$Heparin_duration[matching_indices] <- NA
new_dataset$prednisolon_combi_iv_check[matching_indices] <- NA
new_dataset$prednisolon_combi_po_check[matching_indices] <- NA

#----------------------------------------------------------------------------------------

lactate_values <- import_value("lab",249)#
lactate_arterial_values <- import_value("lab",c(249),"arterial")
lactate_venous_values <- import_value("lab",c(249),"venous")


HighestLactate_0_12 <- get_max_values_in_range_patient_list("HighestLactate_0_12", lactate_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*12)
new_dataset$HighestLactate_0_12 <- HighestLactate_0_12$HighestLactate_0_12

HighestLactate_12_24 <- get_max_values_in_range_patient_list("HighestLactate_12_24", lactate_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*12,as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$HighestLactate_12_24 <- HighestLactate_12_24$HighestLactate_12_24

lactate_values$Value <- str_replace_all(lactate_values$Value, "[^0-9.-]", "")
lactate_values$Value <- as.numeric(lactate_values$Value)

LactateWeightedMean <- get_weighted_mean_values_in_range_patient_list("LactateWeightedMean", lactate_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)

new_dataset$LactateWeightedMean <- LactateWeightedMean$LactateWeightedMean


#################################################################################################

pH_values <- import_value("lab",253)#
pH_arterial_values <- import_value("lab",c(253),"arterial")
pH_venous_values <- import_value("lab",c(253),"venous")

LowestpH_0_12 <- get_min_values_in_range_patient_list("LowestpH_0_12", pH_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*12)
new_dataset$LowestpH_0_12 <- LowestpH_0_12$LowestpH_0_12

LowestpH_12_24 <- get_min_values_in_range_patient_list("LowestpH_12_24", pH_values, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*12,as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$LowestpH_12_24 <- LowestpH_12_24$LowestpH_12_24


pH_values$Value <- str_replace_all(pH_values$Value, "[^0-9.-]", "")
pH_values$Value <- as.numeric(pH_values$Value)

PHWeightedMean <- get_weighted_mean_values_in_range_patient_list("PHWeightedMean", pH_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)

new_dataset$PHWeightedMean <- PHWeightedMean$PHWeightedMean

#-----------------------------------------------------------------------------------------------------------

platelets_values <- import_value("lab",c(62,173,300,418,533,534))#

PlateletsWeightedMean <- get_weighted_mean_values_in_range_patient_list("PlateletsWeightedMean", platelets_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$PlateletsWeightedMean <- PlateletsWeightedMean$PlateletsWeightedMean

new_dataset$PlateletsWeightedMean <- str_replace_all(new_dataset$PlateletsWeightedMean, "[^0-9.-]", "")
new_dataset$PlateletsWeightedMean <- as.numeric(new_dataset$PlateletsWeightedMean)

######################################################################################
Creatinine_values <- import_value("lab",c(6))#

CreatinineWeightedMean <- get_weighted_mean_values_in_range_patient_list("CreatinineWeightedMean", Creatinine_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$CreatinineWeightedMean <- CreatinineWeightedMean$CreatinineWeightedMean

new_dataset$CreatinineWeightedMean <- str_replace_all(new_dataset$CreatinineWeightedMean, "[^0-9.-]", "")
new_dataset$CreatinineWeightedMean <- as.numeric(new_dataset$CreatinineWeightedMean)



###############################################################################################
Bilirubine_values <- import_value("lab",c(26))#

BilirubineWeightedMean <- get_weighted_mean_values_in_range_patient_list("BilirubineWeightedMean", Bilirubine_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$BilirubineWeightedMean <- BilirubineWeightedMean$BilirubineWeightedMean

new_dataset$BilirubineWeightedMean <- str_replace_all(new_dataset$BilirubineWeightedMean, "[^0-9.-]", "")
new_dataset$BilirubineWeightedMean <- as.numeric(new_dataset$BilirubineWeightedMean)


###############################################################################################
Chloride_values <- import_value("lab",c(245))#

Chloride_values <- filter(Chloride_values, !is.na(Chloride_values$Value))

Chloride_values$Value <- str_replace_all(Chloride_values$Value, "[^0-9.-]", "")
Chloride_values$Value <- as.numeric(Chloride_values$Value)

ClWeightedMean <- get_weighted_mean_values_in_range_patient_list("ClWeightedMean", Chloride_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$ClWeightedMean <- ClWeightedMean$ClWeightedMean


###############################################################################################
BaseExcess_values <- import_value("lab",c(230,231))#
BaseExcess_values <- filter(BaseExcess_values, !is.na(BaseExcess_values$Value))

BaseExcess_values$Value <- str_replace_all(BaseExcess_values$Value, "[^0-9.-]", "")
BaseExcess_values$Value <- as.numeric(BaseExcess_values$Value)

BEWeightedMean1 <- get_weighted_mean_values_in_range_patient_list("BEWeightedMean1", BaseExcess_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)

new_dataset$BEWeightedMean1 <- BEWeightedMean1$BEWeightedMean1


###############################################################################################
Bicarbonate_values <- import_value("lab",c(229))#

Bicarbonate_values$Value <- str_replace_all(Bicarbonate_values$Value, "[^0-9.-]", "")
Bicarbonate_values$Value <- as.numeric(Bicarbonate_values$Value)

HCO3WeightedMean1 <- get_weighted_mean_values_in_range_patient_list("HCO3WeightedMean1", Bicarbonate_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)

new_dataset$HCO3WeightedMean1 <- HCO3WeightedMean1$HCO3WeightedMean1

###############################################################################################
sodium_values <- import_value("lab",c(243))#


sodium_values <- filter(sodium_values, !is.na(sodium_values$Value))

sodium_values$Value <- str_replace_all(sodium_values$Value, "[^0-9.-]", "")
sodium_values$Value <- as.numeric(sodium_values$Value)

NaWeightedMean <- get_weighted_mean_values_in_range_patient_list("NaWeightedMean", sodium_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$NaWeightedMean <- NaWeightedMean$NaWeightedMean

###############################################################################################
Potassium_values <- import_value("lab",c(244))#

Potassium_values <- filter(Potassium_values, !is.na(Potassium_values$Value))

Potassium_values$Value <- str_replace_all(Potassium_values$Value, "[^0-9.-]", "")
Potassium_values$Value <- as.numeric(Potassium_values$Value)

KWeightedMean <- get_weighted_mean_values_in_range_patient_list("KWeightedMean", Potassium_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$KWeightedMean <- KWeightedMean$KWeightedMean


###############################################################################################
Urea_values <- import_value("lab",c(5))#
Urea_values$Value <- as.numeric( gsub("[^0-9.-]","",Urea_values$Value,perl=T))

UreaWeightedMean <- get_weighted_mean_values_in_range_patient_list("UreaWeightedMean", Urea_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$UreaWeightedMean <- UreaWeightedMean$UreaWeightedMean

new_dataset$UreaWeightedMean <- str_replace_all(new_dataset$UreaWeightedMean, "[^0-9.-]", "")
new_dataset$UreaWeightedMean <- as.numeric(new_dataset$UreaWeightedMean)

###############################################################################################
Glucose_values <- import_value("lab",c(248))#


Glucose_values$Value <- str_replace_all(Glucose_values$Value, "[^0-9.-]", "")
Glucose_values$Value <- as.numeric(Glucose_values$Value)

gluWeightedMean1 <- get_weighted_mean_values_in_range_patient_list("gluWeightedMean1", Glucose_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)

new_dataset$gluWeightedMean1 <- gluWeightedMean1$gluWeightedMean1

###############################################################################################
LDH_values <- import_value("lab",c(95,34))#


LDH_values <- filter(LDH_values, !is.na(LDH_values$Value))

LDH_values$Value <- str_replace_all(LDH_values$Value, "[^0-9.-]", "")
LDH_values$Value <- as.numeric(LDH_values$Value)

LDHWeightedMean <- get_weighted_mean_values_in_range_patient_list("LDHWeightedMean", LDH_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$LDHWeightedMean <- LDHWeightedMean$LDHWeightedMean


#------------------------------------------------------------------------------------------

new_dataset$LowestpH_0_12 <- as.numeric(new_dataset$LowestpH_0_12)
new_dataset$LowestpH_12_24 <- as.numeric(new_dataset$LowestpH_12_24)

#--------------------------------------------------------------------------------------------

###############################################################################################
HighestTemperatur_values <- import_value("vitalsigns",c(6))#


HighestTemperatur_values <- filter(HighestTemperatur_values, !is.na(HighestTemperatur_values$Value))

HighestTemperatur_values$Value <- str_replace_all(HighestTemperatur_values$Value, "[^0-9.-]", "")
HighestTemperatur_values$Value <- as.numeric(HighestTemperatur_values$Value)

TempWeightedMean <- get_weighted_mean_values_in_range_patient_list("TempWeightedMean", HighestTemperatur_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$TempWeightedMean <- TempWeightedMean$TempWeightedMean

###############################################################################################
RRSys_values <- import_value("vitalsigns",c(1))#

RRSys_values$Value <- str_replace_all(RRSys_values$Value, "[^0-9.-]", "")
RRSys_values$Value <- as.numeric(RRSys_values$Value)

#ich habe hier einfach das weighted mean genommen 
RRSysWeightedMeanValue <- get_weighted_mean_values_in_range_patient_list("RRSysWeightedMeanValue", RRSys_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$RRSysWeightedMeanValue <- RRSysWeightedMeanValue$RRSysWeightedMeanValue

###############################################################################################
RRDia_values <- import_value("vitalsigns",c(2))#

RRDia_values$Value <- str_replace_all(RRDia_values$Value, "[^0-9.-]", "")
RRDia_values$Value <- as.numeric(RRDia_values$Value)

RRDiaWeightedMeanValue <- get_weighted_mean_values_in_range_patient_list("RRDiaWeightedMeanValue", RRDia_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$RRDiaWeightedMeanValue <- RRDiaWeightedMeanValue$RRDiaWeightedMeanValue

###############################################################################################
HeartRate_values <- import_value("vitalsigns",c(3))#

HeartRate_values$Value <- str_replace_all(HeartRate_values$Value, "[^0-9.-]", "")
HeartRate_values$Value <- as.numeric(HeartRate_values$Value)

HRWeightedMean <- get_weighted_mean_values_in_range_patient_list("HRWeightedMean", HeartRate_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$HRWeightedMean <- HRWeightedMean$HRWeightedMean

###############################################################################################
sO2_values <- import_value("vitalsigns",c(4))#

sO2_values$Value <- str_replace_all(sO2_values$Value, "[^0-9.-]", "")
sO2_values$Value <- as.numeric(sO2_values$Value)

sO2WeightedMeanValue <- get_weighted_mean_values_in_range_patient_list("sO2WeightedMeanValue", sO2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$sO2WeightedMeanValue <- sO2WeightedMeanValue$sO2WeightedMeanValue

#######################################################################################################
Hemoglobin_values <- import_value("lab",c(57,235))#

Hemoglobin_values$Value <- str_replace_all(Hemoglobin_values$Value, "[^0-9.-]", "")
Hemoglobin_values$Value <- as.numeric(Hemoglobin_values$Value)

HBWeightedMean <- get_weighted_mean_values_in_range_patient_list("HBWeightedMean", Hemoglobin_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$HBWeightedMean <- HBWeightedMean$HBWeightedMean

###############################################################################################
Leukocytes_values <- import_value("lab",c(202))#

Leukocytes_values$Value <- str_replace_all(Leukocytes_values$Value, "[^0-9.-]", "")
Leukocytes_values$Value <- as.numeric(Leukocytes_values$Value)

LeukocytesWeightedMean <- get_weighted_mean_values_in_range_patient_list("LeukocytesWeightedMean", Leukocytes_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$LeukocytesWeightedMean <- LeukocytesWeightedMean$LeukocytesWeightedMean

new_dataset$LeukocytesWeightedMean <- str_replace_all(new_dataset$LeukocytesWeightedMean, "[^0-9.-]", "")
new_dataset$LeukocytesWeightedMean <- as.numeric(new_dataset$LeukocytesWeightedMean)

###############################################################################################


#-------------------------------------------------------------------------------
#Beatmung
#---------------------------------------------------------------------------------------
global_patient_data$VentilationAdmission <- NA

for (i in 1:nrow(global_patient_data)) {
  begin <- global_patient_data$ICUStay_begin[i]
  PatientID <- global_patient_data$ID[i]
  
  sql_invasive <- paste0("SELECT   
    IFNULL(TIME_TO_SEC(TIMEDIFF((SELECT MIN(DateTime) FROM cs_dv_data_ventilation WHERE DateTime > vent_table.DateTime AND Type = 9
    AND PatientID = vent_table.PatientID),vent_table.DateTime)),0) AS Length,PatientID
    FROM `cs_dv_data_ventilation` AS vent_table WHERE vent_table.Type=9 AND (vent_table.Value = 'BIPAP' OR vent_table.Value = 'CPAP' OR vent_table.Value='niv' OR vent_table.Value='SIMV'
    OR Value='MMV' Or Value='IPPV' OR Value='manually') AND vent_table.PatientID = ",PatientID," AND DateTime >= '",as.POSIXct(begin)-24*3600,"' AND DateTime <='",as.POSIXct(begin)+3600*12,"'")
  rs_invasive <- dbSendQuery(mydb, sql_invasive)
  data_invasive <- fetch(rs_invasive, n=-1)
  time_invasive <- colSums(data_invasive)[[1]]
  
  if (!is.na(time_invasive) && time_invasive > 0)
    global_patient_data$VentilationAdmission[i] <- TRUE
  
  sql_non_invasive <- paste0("SELECT   
    IFNULL(TIME_TO_SEC(TIMEDIFF((SELECT MIN(DateTime) FROM cs_dv_data_ventilation WHERE DateTime > vent_table.DateTime AND Type = 9
    AND PatientID = vent_table.PatientID),vent_table.DateTime)),0) AS Length,PatientID
    FROM `cs_dv_data_ventilation` AS vent_table WHERE vent_table.Type=9 AND (vent_table.Value = 'air' OR vent_table.Value = 'cannula' OR vent_table.Value='mask' OR vent_table.Value='highflow'
    OR Value='reservoir' Or Value='tracheal_cannula_oxygen') AND vent_table.PatientID = ",PatientID," AND DateTime >= '",as.POSIXct(begin)-24*3600,"' AND DateTime <='",as.POSIXct(begin)+3600*12,"'")
  rs_non_invasive <- dbSendQuery(mydb, sql_non_invasive)
  data_non_invasive <- fetch(rs_non_invasive, n=-1)
  time_non_invasive <- colSums(data_non_invasive)[[1]]
  
  if (!is.na(time_non_invasive) && time_non_invasive > 0)
    global_patient_data$VentilationAdmission[i] <- FALSE
}

new_dataset <- merge(new_dataset, global_patient_data[, c("ID", "VentilationAdmission")], by = "ID", all.x = TRUE)

new_dataset <- new_dataset %>%
  mutate(VentilationAdmission = ifelse(is.na(VentilationAdmission), FALSE, VentilationAdmission))
#---------------------------------------------------------------------------------
vent_mode <- import_value("ventilation",9,NULL,TRUE)

CPAP_fehlende_daten <- read.csv("PS_CPAP_fehlende_daten.csv")
Air_fehlende_daten <- read.csv("Air_fehlende_daten.csv")

CPAP_fehlende_daten$X <- NULL
Air_fehlende_daten$X <- NULL

vent_mode <- rbind(vent_mode, CPAP_fehlende_daten)
vent_mode <- rbind(vent_mode, Air_fehlende_daten)

VentMode <- merge_and_concat_text("VentMode", vent_mode, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)

new_dataset$VentMode <- VentMode$VentMode

new_dataset$HadVentilation <- grepl("bipap|cpap|niv|simv|mmv|ippv|cppv", tolower(new_dataset$VentMode))

keywords <- c("air" = 1, "mask" = 2, "reservoir" = 2,  "cannula" = 2, "highflow" = 2,"tracheal_cannula_oxygen" = 2, "manually" = 2,
              "CPAP" = 3, "niv" = 3, "BIPAP" = 4, "SIMV" = 4, "IPPV" = 4, "CPPV" = 4, "MMV" = 4)

VentMode$HighestLevel <- sapply(VentMode$VentMode, get_highest_level)

new_dataset$VentModeHighestLevel <- VentMode$HighestLevel

new_dataset <- new_dataset %>%
  mutate(VentModeHighestLevel = ifelse( VentModeHighestLevel == "-Inf", 1, VentModeHighestLevel))

#----------------------------------------------------------------------------------------------------------

vent_mode$DateTimeEnd <- vent_mode$DateTime
vent_mode$DateTime <- as.POSIXlt(vent_mode$DateTime, format = "%Y-%m-%d %H:%M:%S")
vent_mode$DateTimeEnd <- as.POSIXlt(vent_mode$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")

vent_mode_beatmung <- vent_mode[grepl("^(BIPAP|CPAP|niv|SIMV|MMV|IPPV|CPPV)$", vent_mode$Value), ]

vent_time <- ventilation_time_sum("vent_time", vent_mode_beatmung, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$vent_time <- vent_time$vent_time

#---------------------------------------------------------------------------------------------------


endotrachealtubeadmission <- import_value("accesssites","Endotracheal tube")

EndotrachealTubeAdmission <- get_all_values_in_range_patient_list ("EndotrachealTubeAdmission",endotrachealtubeadmission,patient_list,as.POSIXct(global_patient_data$ICUStay_begin)-3600*24,as.POSIXct(global_patient_data$ICUStay_begin)+3600*24)
new_dataset <- merge(new_dataset, EndotrachealTubeAdmission[, c("ID", "EndotrachealTubeAdmission")], by = "ID", all.x = TRUE)

new_dataset$EndotrachealTubeAdmission[!is.na(new_dataset$EndotrachealTubeAdmission)] <- TRUE
new_dataset[is.na(new_dataset$EndotrachealTubeAdmission),]$EndotrachealTubeAdmission <- FALSE

endotrachealtubeadmission_values2 <- import_value("accesssites","Endotracheal tube")

endotrachealtubeadmission_values2$DateTime <- as.POSIXlt(endotrachealtubeadmission_values2$DateTime, format = "%Y-%m-%d %H:%M:%S")
endotrachealtubeadmission_values2$DateTimeEnd <- as.POSIXlt(endotrachealtubeadmission_values2$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")

Endotrachealtube <- check_in_range_3("Endotrachealtube", endotrachealtubeadmission_values2, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)

new_dataset$Endotrachealtube <- Endotrachealtube$Endotrachealtube

new_dataset$Endotrachealtube <- grepl("endotracheal",tolower(new_dataset$Endotrachealtube))

#-------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

oxygen_flow <- import_value("ventilation",11)
fiO2 <- import_value("ventilation",10)
p_insp <- import_value("ventilation",12)
p_exp <- import_value("ventilation",13)
tidal_volume <- import_value("ventilation",16)
minute_volume <- import_value("ventilation",17)

paO2_values <- import_value("lab",226)  
paCO2_values <- import_value("lab",227)  
paO2_arterial_values <- import_value("lab",226,c("arterial"))
paCO2_arterial_values <- import_value("lab",227,c("arterial"))#

oxygen_flow$Value2 <- oxygen_flow$Value

oxygen_flow <- convert_oxygen_flow_to_fiO2(oxygen_flow, "Value2", "Value")

oxygen_flow$Value2 <- NULL

fiO2 <- rbind(fiO2 , oxygen_flow)



FIO2weightedmean <- get_weighted_mean_values_in_range_patient_list_OhneAusreisser_2("FIO2weightedmean", fiO2, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$FIO2weightedmean <- FIO2weightedmean$FIO2weightedmean

TidalVolweightedmean <- get_weighted_mean_values_in_range_patient_list_OhneAusreisser_2("TidalVolweightedmean", tidal_volume, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$TidalVolweightedmean <- TidalVolweightedmean$TidalVolweightedmean

Pao2weightedmean <- get_weighted_mean_values_in_range_patient_list("Pao2weightedmean", paO2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$Pao2weightedmean <- Pao2weightedmean$Pao2weightedmean

PaCO2weightedmean <- get_weighted_mean_values_in_range_patient_list("PaCO2weightedmean", paCO2_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$PaCO2weightedmean <- PaCO2weightedmean$PaCO2weightedmean

Pinspweightedmean <- get_weighted_mean_values_in_range_patient_list_OhneAusreisser_2("Pinspweightedmean", p_insp, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$Pinspweightedmean <- Pinspweightedmean$Pinspweightedmean

Pexpweightedmean <- get_weighted_mean_values_in_range_patient_list_OhneAusreisser_2("Pexpweightedmean", p_exp, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$Pexpweightedmean <- Pexpweightedmean$Pexpweightedmean

MinuteVolweightedmean <- get_weighted_mean_values_in_range_patient_list_OhneAusreisser_2("MinuteVolweightedmean", minute_volume, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$MinuteVolweightedmean <- MinuteVolweightedmean$MinuteVolweightedmean


#------------------------------------------------------------------------------------------------------

fiO2$Value <- as.numeric(fiO2$Value)
paO2_values$Value <- as.numeric(paO2_values$Value)

paO2_values <- paO2_values %>%
  filter(DateTime != "0000-00-00 00:00:00")

paO2_values <- paO2_values[complete.cases(paO2_values$Value), ]
paO2_values <- paO2_values[complete.cases(paO2_values$DateTime), ]

paO2_values$DateTime <- as.POSIXct(paO2_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
fiO2$DateTime <- as.POSIXct(fiO2$DateTime, format = "%Y-%m-%d %H:%M:%S")

horoviz_2 <- calculate_horovitz_index("Value", paO2_values, fiO2)
horoviz_2$DateTime <- as.POSIXct(horoviz_2$DateTime, origin = "1970-01-01", tz = "UTC-1")

horoviz_2 <- horoviz_2[complete.cases(horoviz_2$Value), ]
horoviz_2 <- horoviz_2 %>%
  filter(Value != "Inf")

horoviz_2$Value <- as.numeric(horoviz_2$Value)

horoviz_WeightedMeanValue <- get_weighted_mean_values_in_range_patient_list_OhneAusreisser_2("horoviz", horoviz_2, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)

horoviz_WeightedMeanValue <- horoviz_WeightedMeanValue %>%
  mutate(horoviz = ifelse(horoviz > 600, NA, horoviz))

new_dataset$horoviz_WeightedMeanValue <- horoviz_WeightedMeanValue$horoviz

#-------------------------------------------------------------------------------------------

keywords_not_applicable <- c("air" = 1, "mask" = 2, "reservoir" = 2,  "cannula" = 2, "highflow" = 2, "manually" = 2,"tracheal_cannula_oxygen" = 3,
                             "CPAP" = 5, "niv" = 4, "BIPAP" = 5, "SIMV" = 6, "IPPV" = 6, "CPPV" = 6, "MMV" = 6)

get_highest_level_not_applicable <- function(text) {
  levels <- unique(unlist(lapply(strsplit(text, ", "), function(x) keywords_not_applicable[x])))
  max(levels)
}


new_dataset$VentModeHighestLevel_not_applicable <- sapply(new_dataset$VentMode, get_highest_level_not_applicable)


#--------------------------------------------------------------------------------------------

new_dataset$FIO2weightedmean_factors <- NA
new_dataset <- new_dataset %>%
  mutate(FIO2weightedmean_factors = ifelse(FIO2weightedmean > 0 & FIO2weightedmean < 35, "0_34,9", FIO2weightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(FIO2weightedmean_factors = ifelse(FIO2weightedmean >= 35& FIO2weightedmean < 45, "35_44,9", FIO2weightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(FIO2weightedmean_factors = ifelse(FIO2weightedmean >= 45, ">=45", FIO2weightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(FIO2weightedmean_factors = ifelse( (VentModeHighestLevel_not_applicable == 1 | VentModeHighestLevel_not_applicable == "-Inf"), "not_applicable", FIO2weightedmean_factors))

new_dataset$FIO2weightedmean <- NULL

new_dataset$Pinspweightedmean_factors <- NA
new_dataset <- new_dataset %>%
  mutate(Pinspweightedmean_factors = ifelse(Pinspweightedmean > 0 & Pinspweightedmean < 15, "0_14,9", Pinspweightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(Pinspweightedmean_factors = ifelse(Pinspweightedmean >= 15 & Pinspweightedmean < 20, "15_19,9", Pinspweightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(Pinspweightedmean_factors = ifelse(Pinspweightedmean >= 20 & Pinspweightedmean < 25, "20_24,9", Pinspweightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(Pinspweightedmean_factors = ifelse(Pinspweightedmean >= 25, ">=25", Pinspweightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(Pinspweightedmean_factors = ifelse( is.na(new_dataset$Pinspweightedmean) & is.na(new_dataset$Pexpweightedmean)& (new_dataset$VentModeHighestLevel_not_applicable == 1 | new_dataset$VentModeHighestLevel_not_applicable == 2 | new_dataset$VentModeHighestLevel_not_applicable == "-Inf"), "not_applicable", Pinspweightedmean_factors))

new_dataset$Pinspweightedmean <- NULL

new_dataset$Pexpweightedmean_factors <- NA
new_dataset <- new_dataset %>%
  mutate(Pexpweightedmean_factors = ifelse(Pexpweightedmean >= 0 & Pexpweightedmean < 7.5, "0_7,4", Pexpweightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(Pexpweightedmean_factors = ifelse(Pexpweightedmean >= 7.5 & Pexpweightedmean < 10, "7,5_10", Pexpweightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(Pexpweightedmean_factors = ifelse(Pexpweightedmean >= 10, ">=10", Pexpweightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(Pexpweightedmean_factors = ifelse( Pinspweightedmean_factors == "not_applicable", "not_applicable", Pexpweightedmean_factors))

new_dataset$Pexpweightedmean <- NULL

new_dataset$TidalVolweightedmean_factors <- NA
new_dataset <- new_dataset %>%
  mutate(TidalVolweightedmean_factors = ifelse(TidalVolweightedmean > 0 & TidalVolweightedmean < 500, "0_499", TidalVolweightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(TidalVolweightedmean_factors = ifelse(TidalVolweightedmean >= 500 & TidalVolweightedmean < 650, "500_649", TidalVolweightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(TidalVolweightedmean_factors = ifelse(TidalVolweightedmean >= 650, ">=650", TidalVolweightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(TidalVolweightedmean_factors = ifelse(  new_dataset$VentModeHighestLevel_not_applicable == 1 | new_dataset$VentModeHighestLevel_not_applicable == 2 | new_dataset$VentModeHighestLevel_not_applicable == "-Inf" | new_dataset$VentModeHighestLevel_not_applicable == 4, "not_applicable", TidalVolweightedmean_factors))

new_dataset$TidalVolweightedmean <- NULL

new_dataset$MinuteVolweightedmean_factors <- NA
new_dataset <- new_dataset %>%
  mutate(MinuteVolweightedmean_factors = ifelse(MinuteVolweightedmean > 0 & MinuteVolweightedmean < 8, "0_7,9", MinuteVolweightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(MinuteVolweightedmean_factors = ifelse(MinuteVolweightedmean >= 8 & MinuteVolweightedmean < 12, "8_11,9", MinuteVolweightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(MinuteVolweightedmean_factors = ifelse(MinuteVolweightedmean >= 12, ">=12", MinuteVolweightedmean_factors))
new_dataset <- new_dataset %>%
  mutate(MinuteVolweightedmean_factors = ifelse( TidalVolweightedmean_factors == "not_applicable", "not_applicable", MinuteVolweightedmean_factors))

new_dataset$MinuteVolweightedmean <- NULL


new_dataset$VentMode <- NULL
new_dataset$VentModeHighestLevel_not_applicable <- NULL

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------


LVEF_echo_values <- import_value("echo","tte_lv_lvef")

LVEFAdmission <- get_first_value_patient_list("LVEFAdmission", LVEF_echo_values, patient_list_dates,"numeric")
new_dataset$LVEFAdmission <- LVEFAdmission$LVEFAdmission

#------------------------------------------------------------------------------------------------------

troponinI_values <- import_value("lab",c(40))
troponinT_values <- import_value("lab",c(41))

troponinI_values$Value <- str_replace_all(troponinI_values$Value, "[^0-9.-]", "")
troponinI_values$Value <- as.numeric(troponinI_values$Value)

troponinT_values$Value <- str_replace_all(troponinT_values$Value, "[^0-9.-]", "")
troponinT_values$Value <- as.numeric(troponinT_values$Value)

TroponinIMin <- get_min_values_in_range_patient_list("TroponinIMin", troponinI_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*250000000000000000,as.POSIXct(new_dataset$Datum)+3600*24)

TroponinIMax <- get_max_values_in_range_patient_list("TroponinIMax", troponinI_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*250000000000000000,as.POSIXct(new_dataset$Datum)+3600*24)


TroponinTMin <- get_min_values_in_range_patient_list("TroponinTMin", troponinT_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*250000000000000000,as.POSIXct(new_dataset$Datum)+3600*24)

TroponinTMax <- get_max_values_in_range_patient_list("TroponinTMax", troponinT_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*250000000000000000,as.POSIXct(new_dataset$Datum)+3600*24)

new_dataset$TroponinIMax_inICUStay <- TroponinIMax$TroponinIMax
new_dataset$TroponinTMax_inICUStay <- TroponinTMax$TroponinTMax
new_dataset$TroponinIMin_inICUStay <- TroponinIMin$TroponinIMin
new_dataset$TroponinTMin_inICUStay <- TroponinTMin$TroponinTMin


normalized_data_T_Max <- scale(TroponinTMax$TroponinTMax)
normalized_data_I_Max <- scale(TroponinIMax$TroponinIMax)

trop_combiniert <- data.frame( "Value" = NaN, "TroponinTMax" = normalized_data_T_Max, "TroponinIMax" = normalized_data_I_Max)
trop_combiniert <- trop_combiniert %>%
  mutate(Value = ifelse(is.na(TroponinIMax), TroponinTMax, TroponinIMax))

new_dataset$Troponin_max_combiniert_inICUStay <- trop_combiniert$Value


normalized_data_T_Min <- scale(TroponinTMin$TroponinTMin)
normalized_data_I_Min <- scale(TroponinIMin$TroponinIMin)

trop_combiniert <- data.frame( "Value" = NaN, "TroponinTMin" = normalized_data_T_Min, "TroponinIMin" = normalized_data_I_Min)
trop_combiniert <- trop_combiniert %>%
  mutate(Value = ifelse(is.na(TroponinIMin), TroponinTMin, TroponinIMin))

new_dataset$Troponin_min_combiniert_inICUStay <- trop_combiniert$Value

#----------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------
MCSType_Values <- import_value("procedures","mcs_ecls_implantation_type")

MCSType_Values$DateTimeEnd <- as.POSIXlt(MCSType_Values$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")
MCSType_Values$DateTime <- as.POSIXlt(MCSType_Values$DateTime, format = "%Y-%m-%d %H:%M:%S")

MCSType <- check_in_range_3("MCSType", MCSType_Values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$MCSType <- MCSType$MCSType

new_dataset$VAECMO <- grepl("va-ecmo",tolower(new_dataset$MCSType))
new_dataset$MCS <- grepl("vad|ecmo|impella",tolower(new_dataset$MCSType))

new_dataset$MCSType <- NULL
#--------------------------------------------------------------------------------------

IABP_values <- import_value("accesssites","IABP")

IABP_values$Value <- TRUE

IABP <- check_in_range_3("IABP", IABP_values, patient_list_dates, as.POSIXct(new_dataset$Datum),as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$IABP <- IABP$IABP

new_dataset$IABP <- grepl("true",tolower(new_dataset$IABP))

#--------------------------------------------------------------------------------------
tamponade_values <- import_event("cs_endpoint","endpoint_tamponade")

tamponade_values$Date2 <- NULL
tamponade_values$Date2 <- tamponade_values$Date1
colnames(tamponade_values)[colnames(tamponade_values) == "Date1"] <- "DateTime"
colnames(tamponade_values)[colnames(tamponade_values) == "Date2"] <- "DateTimeEnd"
tamponade_values$DateTimeEnd <- as.POSIXlt(tamponade_values$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")
tamponade_values$DateTime <- as.POSIXlt(tamponade_values$DateTime, format = "%Y-%m-%d %H:%M:%S")
tamponade_values$Value <- TRUE

HadTamponade <- check_in_range_3("HadTamponade", tamponade_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*250000000000000,as.POSIXct(new_dataset$Datum)+3600*24)

HadTamponade$HadTamponade <- grepl("true",tolower(HadTamponade$HadTamponade))
new_dataset$HadTamponade <- HadTamponade$HadTamponade

TamponadeAdmission <- check_in_range_3("TamponadeAdmission", tamponade_values, patient_list, as.POSIXct(global_patient_data$ICUStay_begin)-3600*24,as.POSIXct(global_patient_data$ICUStay_begin)+3600*24)

TamponadeAdmission$TamponadeAdmission <- grepl("true",tolower(TamponadeAdmission$TamponadeAdmission))

new_dataset <- merge(new_dataset, TamponadeAdmission[, c("ID", "TamponadeAdmission")], by = "ID", all.x = TRUE)


#--------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

RBCUnits_values <- import_value("transfusions","red cell concentrate")
RBCUnitsTaken <- get_sum_values_in_range_patient_list("RBCUnitsTaken", RBCUnits_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*250000000000,as.POSIXct(new_dataset$Datum)+3600*24)
new_dataset$RBCUnitsTaken <- RBCUnitsTaken$RBCUnitsTaken

#-----------------------------------------------------------------------------------------------------------------

PCI_values <- get_pcis("PCIPerformed",patient_list_dates,as.POSIXct(new_dataset$Datum)-3600*24*25000000,as.POSIXct(new_dataset$Datum)+3600*24)   

PCI_values$NumberOfPCIs <- str_count(PCI_values$PCIPerformed, "\\|") + 1
PCI_values$NumberOfPCIs[is.na(PCI_values$NumberOfPCIs)] <- 0

new_dataset$NumberOfPCIs <- PCI_values$NumberOfPCIs

#------------------------------------------------------------------------------------------------------------------

GCS_Admission <- get_first_value_patient_list("GCS_Admission",import_value("admission","admission_hospital_pe_gcs"),patient_list,"factor")
new_dataset <- merge(new_dataset, GCS_Admission[, c("ID", "GCS_Admission")], by = "ID", all.x = TRUE)
new_dataset$GCS_Admission[new_dataset$GCS_Admission == "%%%UNKNOWN%%%"] <- NA


#-----------------------------------------------------------------------------------------------------------------

CABG_values <- import_event("cs_procedures","procedures_cabg")

CABG_values$Date1 <- as.POSIXlt(CABG_values$Date1, format = "%Y-%m-%d %H:%M:%S")
CABG_values$Date2 <- CABG_values$Date1
CABG_values$Value <- TRUE
CABG_values$Date2 <- as.POSIXlt(CABG_values$Date2, format = "%Y-%m-%d %H:%M:%S")

HadCABGOnICU <- check_in_range("HadCABGOnICU", CABG_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*2500000000,as.POSIXct(new_dataset$Datum)+3600*24)

HadCABGOnICU$HadCABGOnICU <- grepl("true",tolower(HadCABGOnICU$HadCABGOnICU))
new_dataset$HadCABGOnICU <- HadCABGOnICU$HadCABGOnICU

#------------------------------------------------------------------------------------------------------------------

Dialysis_OnICU_values <- import_event("cs_procedures","procedures_dialysis")

Dialysis_OnICU_values$Date1 <- as.POSIXlt(Dialysis_OnICU_values$Date1, format = "%Y-%m-%d %H:%M:%S")
Dialysis_OnICU_values$Date2 <- Dialysis_OnICU_values$Date1
Dialysis_OnICU_values$Value <- TRUE
Dialysis_OnICU_values$Date2 <- as.POSIXlt(Dialysis_OnICU_values$Date2, format = "%Y-%m-%d %H:%M:%S")

Had_Dialysis_OnICU <- check_in_range("Had_Dialysis_OnICU", Dialysis_OnICU_values, patient_list_dates, as.POSIXct(new_dataset$Datum)-3600*24*2500000000,as.POSIXct(new_dataset$Datum)+3600*24)

Had_Dialysis_OnICU$Had_Dialysis_OnICU <- grepl("true",tolower(Had_Dialysis_OnICU$Had_Dialysis_OnICU))
new_dataset$Had_Dialysis_OnICU <- Had_Dialysis_OnICU$Had_Dialysis_OnICU



#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------

event_death <- import_event("cs_endpoint","endpoint_death")
death_scd <- import_value("admission","admission_scd_death")
death_initial_event <- import_value("admission","admission_initial_event_death")
all_endpoints <- import_event("cs_endpoint","")

colnames(event_death)[colnames(event_death) == "Date1"] <- "DateTime"
event_death$DateTime <- as.POSIXlt(event_death$DateTime, format = "%Y-%m-%d %H:%M:%S")
event_death$DateTimeEnd <- event_death$DateTime
event_death$DateTimeEnd <- as.POSIXlt(event_death$DateTimeEnd, format = "%Y-%m-%d %H:%M:%S")
event_death$Value <- TRUE

Death_in24h <- check_in_range_3("Death_in24h", event_death, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*48)
Death_inICUStay <- check_in_range_3("Death_inICUStay", event_death, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*250000000000000000)
Death_in3d <- check_in_range_3("Death_in3d", event_death, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*4)
Death_in1w <- check_in_range_3("Death_in1w", event_death, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*8)
Death_in2w <- check_in_range_3("Death_in2w", event_death, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*15)
Death_in3w <- check_in_range_3("Death_in3w", event_death, patient_list_dates, as.POSIXct(new_dataset$Datum)+3600*24,as.POSIXct(new_dataset$Datum)+3600*24*22)

Death_in24h$Death_in24h <- grepl("true",tolower(Death_in24h$Death_in24h))
Death_inICUStay$Death_inICUStay <- grepl("true",tolower(Death_inICUStay$Death_inICUStay))
Death_in3d$Death_in3d <- grepl("true",tolower(Death_in3d$Death_in3d))
Death_in1w$Death_in1w <- grepl("true",tolower(Death_in1w$Death_in1w))
Death_in2w$Death_in2w <- grepl("true",tolower(Death_in2w$Death_in2w))
Death_in3w$Death_in3w <- grepl("true",tolower(Death_in3w$Death_in3w))

new_dataset$Death_in24h <- Death_in24h$Death_in24h
new_dataset$Death_inICUStay <- Death_inICUStay$Death_inICUStay
new_dataset$Death_in3d <- Death_in3d$Death_in3d
new_dataset$Death_in1w <- Death_in1w$Death_in1w
new_dataset$Death_in2w <- Death_in2w$Death_in2w
new_dataset$Death_in3w <- Death_in3w$Death_in3w

new_dataset <- new_dataset %>%
  mutate(Death_inICUStay = ifelse(is.na(Death_inICUStay), FALSE, Death_inICUStay))

#---------------------------------------------------------------------------------------------------

CPConICUDischarge <- get_first_value_patient_list("CPConICUDischarge",import_value("admission","admission_hospital_discharge_cpc"),patient_list,"factor")
new_dataset <- merge(new_dataset, CPConICUDischarge[, c("ID", "CPConICUDischarge")], by = "ID", all.x = TRUE)
new_dataset$CPConICUDischarge[new_dataset$CPConICUDischarge == "%%%UNKNOWN%%%"] <- NA

#-----------------------------------------------------------------------------------------------------

new_dataset <- merge(new_dataset, event_death[, c("PatientID", "DateTime")], by.x = "ID", by.y = "PatientID", all.x = TRUE)
new_dataset <- rename(new_dataset, DeathDate = DateTime)

new_dataset <- merge(new_dataset, global_patient_data[, c("ID", "ICUStay_end")], by = "ID", all.x = TRUE)

#wenn das deathdate spaeter als icu stay end date ist, dann als false machen. wahrscheinlich ist der patient auf general ward oder zuhause gestorben
new_dataset <- new_dataset %>%
  mutate(Death_inICUStay = ifelse(ICUStay_end < DeathDate, FALSE, Death_inICUStay))

#DaysTillICUStay_end berechnen
new_dataset <- new_dataset %>%
  mutate(DaysTillICUStay_end = as.numeric(difftime(ICUStay_end, Datum, units = "days")))

#----------------------------------------------------------------------------------------------

GeneralWardStay_Values <- import_event("cs_admission","admission_ward")
ICUStay_Values <- import_event("cs_admission","admission_hospital")

icu_plus_ward_values <- rbind(GeneralWardStay_Values, ICUStay_Values)

icu_plus_ward_values <- icu_plus_ward_values %>%
  filter(Date2 != "0000-00-00 00:00:00")

icu_plus_ward_values_neu <- check_general_ward(icu_plus_ward_values, patient_list)

LastHospitalStay <- get_last_event_patient_list("LastHospitalStay",icu_plus_ward_values_neu,patient_list_dates)

new_dataset$ICUStay_end <- as.POSIXlt(new_dataset$ICUStay_end, format = "%Y-%m-%d %H:%M:%S")
LastHospitalStay$LastHospitalStay_end <- as.POSIXlt(LastHospitalStay$LastHospitalStay_end, format = "%Y-%m-%d %H:%M:%S")

new_dataset$LastHospitalDate <- new_dataset$ICUStay_end

for (i in 1:nrow(patient_list_dates)) {
  if (!is.na(new_dataset$ICUStay_end[i]) && !is.na(LastHospitalStay$LastHospitalStay_end[i]) &&
      as.POSIXct(new_dataset$ICUStay_end[i]) < as.POSIXct(LastHospitalStay$LastHospitalStay_end[i])) {
    new_dataset$LastHospitalDate[i] <- LastHospitalStay$LastHospitalStay_end[i]
  }
}

new_dataset$LastHospitalDate <- as.POSIXlt(new_dataset$LastHospitalDate, format = "%Y-%m-%d %H:%M:%S")
new_dataset$DeathDate <- as.POSIXlt(new_dataset$DeathDate, format = "%Y-%m-%d %H:%M:%S")

new_dataset <- new_dataset %>%
  mutate(Death_inHospitalStay = ifelse(Death_inICUStay == TRUE | LastHospitalDate >= DeathDate, TRUE, FALSE))

new_dataset$Death_inHospitalStay <- ifelse(is.na(new_dataset$Death_inHospitalStay), FALSE, new_dataset$Death_inHospitalStay)

new_dataset <- new_dataset %>%
  mutate(DaysTillHospitalStay_end = as.numeric(difftime(LastHospitalDate, Datum, units = "days")))

new_dataset$LastHospitalDate <- NULL
new_dataset$ICUStay_end <- NULL
new_dataset$DeathDate <- NULL

#-----------------------------------------------------------------------------------------------

new_dataset$EndotrachealTubeAdmission <- as.logical(new_dataset$EndotrachealTubeAdmission)
new_dataset$IABP <- as.logical(new_dataset$IABP)
new_dataset$Death_in24h <- as.logical(new_dataset$Death_in24h)
new_dataset$Death_inICUStay <- as.logical(new_dataset$Death_inICUStay)

#-------------------------------------------------------------------------------------------------------

new_dataset <- replace(new_dataset, new_dataset == TRUE, 1)
new_dataset <- replace(new_dataset, new_dataset == FALSE, 0)

#--------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------



path <- "D:/"

write.csv(new_dataset, file = file.path(path, paste0("lmu_big_org.csv")), fileEncoding = "UTF-8")


column_names <- colnames(new_dataset)
print(column_names)




