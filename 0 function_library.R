scanning_range_secs <- 0 #24*3600

import_value <- function(db_type,type_ids,lab_source=NULL,no_conv_decimal=FALSE) 
{
  wheres <- c()
 
  pages_type = "cs_df_pages"
  lab_type = "cs_lab_structure"
   
  if(db_type == "lab")
    db_name <- "cs_lab_values"
  else if (db_type == "medication")
    db_name <- "cs_medication_data"
  else if(db_type == "vitalsigns")
    db_name <- "cs_dv_data_vitalsigns"
  else if(db_type == "ventilation")
    db_name <- "cs_dv_data_ventilation"
  else if(db_type == "neurology")
    db_name <- "cs_dv_data_neurology"
  else if(db_type == "procedures")
    db_name <- "cs_df_data_procedures"
  else if(db_type == "echo")
    db_name <- "cs_df_data_echo"
  else if(db_type == "ep")
    db_name <- "cs_df_data_ep"
  else if(db_type == "diagnoses")
    db_name <- "cs_et_diagnoses"
  else if(db_type == "ops")
    db_name <- "cs_et_ops"
  else if(db_type == "transfusion_tests")
    db_name <- "cs_et_transfusion_tests"
  else if(db_type == "transfusions")
    db_name <- "cs_et_transfusions"
  else if(db_type == "accesssites")
    db_name <- "cs_et_accesssites"
  else if(db_type == "mcs")
    db_name <- ("cs_dv_data_mcs")
  else if(db_type == "endpoint")
    db_name <- ("cs_df_data_endpoint")
  else if(db_type == "inclusion")
    db_name <- "cs_df_data_inclusion"
  else if(db_type == "admission")
    db_name <- "cs_df_data_admission"
  else if(db_type == "balances")
    db_name <- "cs_et_balances"
  else if(db_type == "id_tests")
    db_name <- "cs_df_data_id_tests"
  else if(db_type == "muroar_ae")
    db_name <- "muroar_df_data_ae"
  else if(db_type == "muroar_ecg")
    db_name <- "muroar_df_data_ecg_characteristics"
  else if(db_type == "muroar_echo")
    db_name <- "muroar_df_data_echo"
  else if(db_type == "muroar_ep_study")
    db_name <- "muroar_df_data_ep_study"
  else if(db_type == "muroar_genetics")
    db_name <- "muroar_df_data_genetics"
  else if(db_type == "muroar_medication")
    db_name <- "muroar_df_data_medication"
  else if(db_type == "muroar_patient_characteristics")
    db_name <- "muroar_df_data_patient_characteristics"
  else if(db_type == "muroar_lab")
    db_name <- "muroar_lab_values"
  else
    stop(paste("Unknown db_type: ",db_type))
  
  if(grepl("muroar",db_name))
  {
    pages_type = "muroar_df_pages"
    lab_type = "muroar_lab_structure"
  }
  
  for(i in  1:length(type_ids))
  {
    wheres[i] <- paste("vs.Type = '",type_ids[i],"' ")
  }
  
  if(db_type == "lab" || db_type == "muroar_lab")
  {
    if(!is.null(lab_source))
    {
      lab_source_sql <- c()
      for(i in 1:length(lab_source))
      {
        lab_source_sql[i] <- paste("lab_source.Value = '",lab_source[i],"' ",sep="")
      }
      
      sql_string <- paste("SELECT vs.DateTime, vs.PatientID, 
                              REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(vs.Value,'M ',''),'s.Bem.',''),',','.'),'M',''),'f.Pat.',''),'fa.Eti','') AS Value
                              FROM ",db_name," AS vs
                              INNER JOIN ",db_name," AS lab_source ON lab_source.Type = 254 AND lab_source.PatientID = vs.PatientID 
                                AND lab_source.DateTime = vs.DateTime AND (",paste(lab_source_sql, collapse=" OR "),")
                              WHERE (",paste(wheres, collapse=" OR "),") AND vs.Value != ''
                              ORDER BY vs.DateTime ASC
                              ",sep="")
    }
    else
    {
      sql_string <- paste("SELECT vs.DateTime, vs.PatientID, 
                              REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(Value,'M ',''),'s.Bem.',''),',','.'),'M',''),'f.Pat.',''),'fa.Eti','') AS Value
                              FROM ",db_name," AS vs
                              WHERE (",paste(wheres, collapse=" OR "),") AND vs.Value != ''
                              ORDER BY vs.DateTime ASC
                              ")
    }

  }
  else if(db_type == "medication" || db_type == "vitalsigns" || db_type == "ventilation" ||  db_type == "mcs" || db_type == "neurology")
  {
    if(no_conv_decimal)
    {
      sql_string <- paste("SELECT vs.DateTime, vs.PatientID, 
                              vs.Value
                              FROM ",db_name," AS vs
                              WHERE (",paste(wheres, collapse=" OR "),") AND vs.Value != ''
                              ORDER BY vs.DateTime ASC
                              ")
    }
    else
    {
      sql_string <- paste("SELECT vs.DateTime, vs.PatientID, 
                              CAST(REPLACE(Value,',','.') AS DECIMAL(10,5)) AS Value
                              FROM ",db_name," AS vs
                              WHERE (",paste(wheres, collapse=" OR "),") AND vs.Value != ''
                              ORDER BY vs.DateTime ASC
                              ")
    }

  }
  else if(db_type == "diagnoses")
  {
    sql_string <- paste("SELECT vs.PatientID, vs.DateBegin, vs.DateEnd, PrimaryKey1, icd10gm2018.Text, PrimaryKey1 AS Value
                              FROM ",db_name," AS vs
                              LEFT JOIN icd10gm2018 ON icd10gm2018.ID = vs.Code 
                              WHERE icd10gm2018.PrimaryKey1 LIKE '%",type_ids,"%' AND vs.ItemMode != 99
                              ORDER BY vs.DateBegin ASC
                              ",sep="")    
  }
  else if(db_type == "ops")
  {
    sql_string <- paste("SELECT vs.PatientID, vs.DateBegin, vs.DateBegin as DateTime, vs.DateEnd, FirstKey, vs.Text, FirstKey AS Value
                              FROM ",db_name," AS vs
                              LEFT JOIN ops ON ops.ID = vs.`OPS-ID`
                              WHERE ops.FirstKey LIKE '%",type_ids,"%' 
                              ORDER BY vs.DateBegin ASC
                              ",sep="")    
  }
  else if(db_type == "transfusion_tests")
  {
    sql_string <- paste("SELECT vs.PatientID, DATE_FORMAT(vs.Date,'%Y-%m-%d %H:%i:%s') AS DateTime, vs.Test, vs.Value
                              FROM ",db_name," AS vs
                              WHERE vs.Test = '",type_ids,"' AND vs.ItemMode != 99
                              ORDER BY vs.Date ASC
                              ",sep="")    
  }
  else if(db_type == "transfusions" )
  {
    sql_string <- paste("SELECT vs.PatientID, DATE_FORMAT(vs.Date,'%Y-%m-%d %H:%i:%s') AS DateTime, vs.BloodProduct AS BloodProduct, vs.Amount AS Amount, vs.Amount AS Value, vs.Complications AS Complications
                              FROM ",db_name," AS vs
                              WHERE vs.BloodProduct = '",type_ids,"' AND vs.ItemMode != 99
                              ORDER BY vs.Date ASC
                              ",sep="")    
  }
  else if(db_type == "accesssites")
  {
    sql_string <- paste("SELECT vs.PatientID, DATE_FORMAT(vs.DateBegin,'%Y-%m-%d %H:%i:%s') AS DateTime, DATE_FORMAT(vs.DateEnd,'%Y-%m-%d %H:%i:%s') AS DateTimeEnd, vs.Complications, vs.Type as Value
                              FROM ",db_name," AS vs
                              WHERE vs.Type = '",type_ids,"' AND vs.ItemMode != 99
                              ORDER BY vs.DateBegin ASC
                              ",sep="")    
  }
  else if(db_type == "balances")
  {
    sql_string <- paste("SELECT vs.PatientID, DATE_FORMAT(vs.Date,'%Y-%m-%d %H:%i:%s') AS DateTime, vs.Input, vs.Output, vs.Output as Value, vs.Defecation
                              FROM ",db_name," AS vs
                              WHERE vs.ItemMode != 99
                              ORDER BY vs.Date ASC
                              ",sep="")    
  }
  else
  {
    sql_string <- paste("SELECT vs.PatientID, vs.Value, ",pages_type,".Date AS DateTime, ",pages_type,".Date2 AS DateTimeEnd
                              FROM ",db_name," AS vs
                              LEFT JOIN ",pages_type," ON ",pages_type,".ID = vs.Page
                              WHERE Type='",type_ids,"'
                              ORDER BY ",pages_type,".Date ASC
                              ",sep="")
  }
  

  
 # print(sql_string)
  rs = dbSendQuery(mydb,sql_string)
  return(fetch(rs,n=-1))
}

get_values_patient <- function(vector_values,patient_id)
{
  return(vector_values[is.element(vector_values$PatientID,patient_id),])
}

has_diagnosis <- function(vector_values,patient_id)
{
  values <- vector_values[is.element(vector_values$PatientID,patient_id),]
  if(nrow(values) > 0)
    return(TRUE)
  else
    return(FALSE)
}

has_diagnosis_patient_list <- function(name,vector_values,patient_list)
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    list_values <- c(list_values,has_diagnosis(vector_values,patient_list[i,]))
  }
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}

has_value <- function(vector_values,patient_id,value)
{
  values <- get_values_patient(vector_values,patient_id)
  
  if(nrow(values) > 0)
  {
    for(i in 1:nrow(values))
    {
      if(values$Value[i] == value)
        return(TRUE)
    }
  }
  return(FALSE)
}

has_value_in_range <- function(vector_values,patient_id,value,begin_time,end_time)
{
  values <- get_values_in_range(vector_values,patient_id,begin_time,end_time)

  if(nrow(values) > 0)
  {
    for(i in 1:nrow(values))
    {
      if(values$Value[i] == value)
        return(TRUE)
    }
  }
  return(FALSE)
}


get_balances_patient_list_at_timepoint <- function(name,vector_values,patient_list,timepoint)
{
  list_input <- c()
  list_output <- c()
  list_balance <- c()
  for(i in 1:nrow(patient_list))
  {
    if(is.na(timepoint[i]) || toString(timepoint[i]) == "0000-00-00 00:00:00")
    {
      list_input <- c(list_input, NA)
      list_output <- c(list_output, NA)
      list_balance <- c(list_balance, NA)
    }
    else
    {
      my_timepoint <- paste0(format(as.POSIXct(timepoint[i]),format="%Y-%m-%d")," 00:00:00")
      res <- vector_values[vector_values$DateTime == my_timepoint & vector_values$PatientID == patient_list[i,],]
      if(nrow(res) == 1)
      {
        input <- res$Input
        output <- res$Output
        balance <- input - output
      }
      else
      {
        input <- NA
        output <- NA
        balance <- NA
      }
      
      list_input <- c(list_input,input)
      list_output <- c(list_output,output)
      list_balance <- c(list_balance,balance)
      
    }
  }
  #print(paste(length(list_input),length(list_output),length(list_balance)))
  df <- data.frame(ID = patient_list, input = list_input, output = list_output, balance = list_balance )
  names(df)[names(df) == "input"] <- paste0(name,"_input")
  names(df)[names(df) == "output"] <- paste0(name,"_output")
  names(df)[names(df) == "balance"] <- paste0(name,"_balance")
  return(df)
}

get_first_value_patient <- function(vector_values,patient_id)
{
  values <- get_values_patient(vector_values,patient_id)
  
  if(length(values$Value) >= 1)
    return(values[1,]$Value)
  else
    return(NA)
}

get_last_value_patient <- function(vector_values,patient_id)
{
  values <- get_values_patient(vector_values,patient_id)
  
  if(length(values$Value) >= 1)
    return(tail(values$Value,n=1))
  else
    return(NA)
}

get_first_value_patient_list <- function(name,vector_values,patient_list,type="")
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    list_values <- c(list_values,get_first_value_patient(vector_values,patient_list[i,]))
  }
  
  if(type == "numeric")
    list_values <- as.numeric( gsub("[^0-9.-]","",list_values,perl=T))
  else if(type == "factor")
    list_values <- factor(list_values)
  
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}

get_first_value_in_range_patient_list <- function(name,vector_data,patient_list,begin_date,end_date,type="")
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    values <- get_values_in_range(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))

    if(nrow(values) > 0)
      list_values <- c(list_values,values$Value[1])
    else
      list_values <- c(list_values,NA)
  }

  if(type == "numeric")
    list_values <- as.numeric( gsub("[^0-9.-]","",list_values,perl=T))
  else if(type == "factor")
    list_values <- factor(list_values) 
  
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}



get_last_value_in_range_patient_list <- function(name,vector_data,patient_list,begin_date,end_date,type="")
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    values <- get_values_in_range(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))
    
    if(nrow(values) > 0)
      list_values <- c(list_values,tail(values$Value,n=1))
    else
      list_values <- c(list_values,NA)
  }
  
  if(type == "numeric")
    list_values <- as.numeric( gsub("[^0-9.-]","",list_values,perl=T))
  else if(type == "factor")
    list_values <- factor(list_values) 
  
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}


get_first_valuedate_in_range_patient_list <- function(name,vector_data,patient_list,begin_date,end_date)
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    values <- get_values_in_range(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))
    
    if(nrow(values) > 0)
      list_values <- c(list_values,values$DateTime[1])
    else
      list_values <- c(list_values,NA)
  }
  #print(list_values)
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}

get_last_value_patient_list <- function(name,vector_values,patient_list,type="")
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    list_values <- c(list_values,get_last_value_patient(vector_values,patient_list[i,]))
  }
  
  if(type == "numeric")
    list_values <- as.numeric( gsub("[^0-9.-]","",list_values,perl=T))
  else if(type == "factor")
    list_values <- factor(list_values)
  
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}


get_all_values_patient <- function(vector_values,patient_id)
{
  values <- get_values_patient(vector_values,patient_id)
  
  if(length(values$Value) >= 1)
  {
    tmp_array <- c()
    for(i in 1:length(values$Value))
    {
    #  if(!is.na(values$Value[i]) && !is.element(values$Value[i],tmp_array))
        tmp_array <- c(tmp_array,values$Value[i])
    }
    if(length(tmp_array) > 0)
      return(paste(tmp_array,collapse=";"))
    else
      return(NA)
  }
  else
    return(NA)
}


get_all_dates_patient <- function(vector_values,patient_id)
{
  values <- get_values_patient(vector_values,patient_id)
  
  if(length(values$Value) >= 1)
  {
    tmp_array <- c()
    for(i in 1:length(values$Value))
    {
      #  if(!is.na(values$Value[i]) && !is.element(values$Value[i],tmp_array))
      tmp_array <- c(tmp_array,values$DateTime[i])
    }
    if(length(tmp_array) > 0)
      return(paste(tmp_array,collapse=";"))
    else
      return(NA)
  }
  else
    return(NA)
}


get_all_values_in_range <- function(vector_values,patient_id,begin,end)
{
  values <- get_values_in_range(vector_values,patient_id,begin,end)
  
  if(length(values$Value) >= 1)
  {
    tmp_array <- c()
    for(i in 1:length(values$Value))
    {
      #  if(!is.na(values$Value[i]) && !is.element(values$Value[i],tmp_array))
      tmp_array <- c(tmp_array,values$Value[i])
    }
    if(length(tmp_array) > 0)
      return(paste(tmp_array,collapse=";"))
    else
      return(NA)
  }
  else
    return(NA)
}



get_all_values_patient_list <- function(name,vector_values,patient_list)
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    list_values <- c(list_values,get_all_values_patient(vector_values,patient_list[i,]))
  }
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}



get_all_dates_patient_list <- function(name,vector_values,patient_list)
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    list_values <- c(list_values,get_all_dates_patient(vector_values,patient_list[i,]))
  }
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}



get_all_values_and_dates_patient_list <- function(name,vector_values,patient_list)
{
  list_values <- c()
  list_dates <- c()
  for(i in 1:nrow(patient_list))
  {
    
    list_values <- c(list_values,get_all_values_patient(vector_values,patient_list[i,]))
    list_dates <- c(list_dates,get_all_dates_patient(vector_values,patient_list[i,]))
  }
  df <- data.frame(ID = patient_list, placeholder_name = list_values, placeholder_name_date = list_dates )
  names(df)[names(df) == "placeholder_name"] <- name
  names(df)[names(df) == "placeholder_name_date"] <- paste0(name,"_VALDATE")
  return(df)
}



count_occurance_and_firstdate <- function(df,name)
{
  df_new <- data.frame()
  all_values <- c()
  for(i in 1:nrow(df))
  {
    all_values <- c(all_values,unlist(strsplit(toString(df[i,name]),split=";",fixed=TRUE)))
  }
  uniques <- unique(all_values[all_values != "NA"])
  
  for(i in 1:nrow(df))
  {
    split <- unlist(strsplit(toString(df[i,name]),split=";",fixed=TRUE))
    split_date <- unlist(strsplit(toString(df[i,paste0(name,"_VALDATE")]),split=";",fixed=TRUE))
    
    
    df_new[i,"ID"] <- df$ID[i] 
    
    for(uni in uniques)
    {
      df_new[i,paste0(name,"_",uni)] <-  sum(split == uni)
    }
    
    found <- c()
    if(length(split) > 0)
    {
      for(j in 1:length(split))
      {
        if(!is.element(split[j],found))
        {
          found <- c(found,split[j])
          df_new[i,paste0(name,"_",split[j],"_firstDate")] <- split_date[j]
        }
      }
    }
  }
  return(df_new)
}


get_all_values_in_range_patient_list <- function(name,vector_values,patient_list,begin,end)
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    res <- get_all_values_in_range(vector_values,patient_list[i,],begin[i],end[i])
    
    list_values <- c(list_values,res)
  }
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}




get_first_value_in_range <- function(vector_values,patient_id,begin_date,end_date)
{
  values <- get_values_in_range(vector_values,patient_id,begin_date,end_date)
  
  if(length(values$Value) > 1)
    return(values[1,])
  else if(length(values$Value) == 0)
  {
    return(data.frame(PatientID = patient_id, Value = NA))
  }
  else
    return(values)
  
}


get_values_in_range <- function(vector_values,patient_id,begin_time,end_time)
{
  my_values <- vector_values[is.element(vector_values$PatientID,patient_id),]
  my_values <- subset(my_values, as.POSIXct(DateTime, format="%Y-%m-%d %H:%M:%S") <= as.POSIXct(end_time) & as.POSIXct(DateTime, format="%Y-%m-%d %H:%M:%S") >= as.POSIXct(begin_time))

  
   return(my_values)
}


get_values_in_range_tp <- function(vector_values,patient_id,begin_time,end_time)
{
  my_values <- vector_values[is.element(vector_values$PatientID,patient_id),]
  begin_time <- as.POSIXct(begin_time)
  end_time <- as.POSIXct(end_time)
  my_values$DateTime <- as.POSIXct(my_values$DateTime, format="%Y-%m-%d %H:%M:%S")
  
  my_values <- subset(my_values, DateTime <= end_time & DateTime >= begin_time-3600*48)
  my_values <- my_values[order(my_values$DateTime),]

  sub_my_values <- data.frame(DateTime = c(), PatientID = c(), Value = c())
  
  if(nrow(my_values) > 0)
  {
    for(i in 1:nrow(my_values))
    {
      if(i > 1 && my_values$DateTime[i-1] < begin_time && my_values$DateTime[i] > begin_time)
      {
        sub_my_values <- rbind(sub_my_values,data.frame(DateTime = my_values$DateTime[i-1], PatientID = my_values$PatientID[i-1], Value = my_values$Value[i-1]))
      }
      
      if(my_values$DateTime[i] <= end_time && my_values$DateTime[i] >= begin_time)
        sub_my_values <- rbind(sub_my_values,data.frame(DateTime = my_values$DateTime[i], PatientID = my_values$PatientID[i], Value = my_values$Value[i]))
    }
  }

  return(sub_my_values)
}


has_values_in_range <- function(vector_values,patient_id,begin_time,end_time)
{

  my_values <- get_values_in_range(vector_values,patient_id,begin_time,end_time)
  
  if(nrow(my_values) > 0)
    return(TRUE)
  else
    return(FALSE)
}


get_mean_values_in_range <- function(vector_values,patient_id,begin_time,end_time)
{
  begin_timespan <- as.POSIXct(begin_time)
  end_timespan <- as.POSIXct(end_time)
  
  data <- get_values_in_range(vector_values,patient_id,begin_timespan-scanning_range_secs,end_timespan+scanning_range_secs)
  data <- data[order(as.POSIXct(data$DateTime, format="%Y-%m-%d %H:%M:%S")),]
  
  first_value <- TRUE
  values <- c()
  
  #print(data)
  
  if(nrow(data) > 0)
  {
    for(k in 1:nrow(data))
    {
      value_begin <- as.POSIXct(data$DateTime[k], format="%Y-%m-%d %H:%M:%S")
      
      if(is.na(value_begin) || is.na(begin_timespan) || is.na(end_timespan) && value_begin < begin_timespan || value_begin > end_timespan)
        next
      
      if(first_value == TRUE)
      {
        first_value <- FALSE
        if(k > 1)
        {
          values <- c(values,as.numeric(gsub("[^0-9.,-]","",data$Value[k-1])))
        }
      }
      values <- c(values,as.numeric(gsub("[^0-9.,-]","",data$Value[k])))
    }
  }
  
  
  #print(values)
  #print(weights)
  
  if(length(values) == 0)
    return(NA)
  else
    return(mean(values,na.rm = TRUE))
}

get_median_value_in_range_patient_list <- function(name,vector_data,patient_list,begin_date,end_date)
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    values <- get_values_in_range(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))
    if(nrow(values) > 0)
      list_values <- c(list_values,median(as.numeric(gsub("[^0-9.,-]","",values$Value)),na.rm=TRUE))
    else
      list_values <- c(list_values,NA)
  }
  #print(list_values)
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}

get_mean_value_in_range_patient_list <- function(name,vector_data,patient_list,begin_date,end_date)
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    values <- get_values_in_range(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))
    
    if(nrow(values) > 0)
      list_values <- c(list_values,mean(as.numeric(gsub("[^0-9.,-]","",values$Value)),na.rm=TRUE))
                       else
      list_values <- c(list_values,NA)
  }
  #print(list_values)
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}

get_min_value_in_range <- function(vector_values,patient_id,begin_time,end_time)
{
  begin_timespan <- as.POSIXct(begin_time)
  end_timespan <- as.POSIXct(end_time)
  
  data <- get_values_in_range(vector_values,patient_id,begin_timespan-scanning_range_secs,end_timespan+scanning_range_secs)
  data <- data[order(as.POSIXct(data$DateTime, format="%Y-%m-%d %H:%M:%S")),]
  
  first_value <- TRUE
  values <- c()
  
  #print(data)
  
  if(nrow(data) > 0)
  {
    for(k in 1:nrow(data))
    {
      value_begin <- as.POSIXct(data$DateTime[k], format="%Y-%m-%d %H:%M:%S")
      
      if(is.na(value_begin) || is.na(begin_timespan) || is.na(end_timespan) || value_begin < begin_timespan || value_begin > end_timespan)
        next
      
      if(first_value == TRUE)
      {
        first_value <- FALSE
        if(k > 1)
        {
          values <- c(values,as.numeric(gsub("[^0-9.,-]","",data$Value[k-1])))
        }
      }
      values <- c(values,as.numeric(gsub("[^0-9.,-]","",data$Value[k])))
    }
  }
  if(length(values) == 0)
    return(NA)
  else
    return(min(values,na.rm = TRUE))
}

get_min_value_in_range_date <- function(vector_values,patient_id,begin_time,end_time)
{
  begin_timespan <- as.POSIXct(begin_time)
  end_timespan <- as.POSIXct(end_time)
  
  data <- get_values_in_range(vector_values,patient_id,begin_timespan-scanning_range_secs,end_timespan+scanning_range_secs)
  data <- data[order(as.POSIXct(data$DateTime, format="%Y-%m-%d %H:%M:%S")),]
  
  first_value <- TRUE
  values <- data.frame()
  
  #print(data)
  
  if(nrow(data) > 0)
  {
    for(k in 1:nrow(data))
    {
      value_begin <- as.POSIXct(data$DateTime[k], format="%Y-%m-%d %H:%M:%S")
      
      if(is.na(value_begin) && is.na(begin_timespan) && is.na(end_timespan) &&  value_begin < begin_timespan || value_begin > end_timespan)
        next
      
      #if(first_value == TRUE)
      #{
      #  first_value <- FALSE
      #  if(k > 1)
      #  {
      #    values <- c(values,data$Value[k-1])
      #  }
      #}
      values <- rbind(data,values)
    }
  }
  if(length(values) == 0)
    return(NA)
  else
  {
    min_item <- values[which.min(as.numeric(gsub("[^0-9.,-]","",values$Value))),]
    
    if(nrow(min_item) == 0)
      return(NA)
    else
      return(min_item$DateTime)
  }
}



get_max_value_in_range <- function(vector_values,patient_id,begin_time,end_time)
{
  begin_timespan <- as.POSIXct(begin_time)
  end_timespan <- as.POSIXct(end_time)
  
  data <- get_values_in_range(vector_values,patient_id,begin_timespan-scanning_range_secs,end_timespan+scanning_range_secs)
  data <- data[order(as.POSIXct(data$DateTime, format="%Y-%m-%d %H:%M:%S")),]
  
  first_value <- TRUE
  values <- c()
  
  #print(data)
  
  if(nrow(data) > 0)
  {
    for(k in 1:nrow(data))
    {
      value_begin <- as.POSIXct(data$DateTime[k], format="%Y-%m-%d %H:%M:%S")
      
      if(is.na(value_begin) && is.na(begin_timespan) && is.na(end_timespan) && value_begin < begin_timespan || value_begin > end_timespan)
        next
      
      if(first_value == TRUE)
      {
        first_value <- FALSE
        if(k > 1)
        {
          values <- c(values,as.numeric(gsub("[^0-9.,-]","",data$Value[k-1])))
        }
      }
      values <- c(values,as.numeric(gsub("[^0-9.,-]","",data$Value[k])))
    }
  }
  if(length(values) == 0)
    return(NA)
  else
  {
    my_max = max(values,na.rm = TRUE)
    return(my_max)
  }
}


get_median_values_in_range <- function(vector_values,patient_id,begin_time,end_time)
{
  begin_timespan <- as.POSIXct(begin_time)
  end_timespan <- as.POSIXct(end_time)
  
  data <- get_values_in_range(vector_values,patient_id,begin_timespan-scanning_range_secs,end_timespan+scanning_range_secs)
  data <- data[order(as.POSIXct(data$DateTime, format="%Y-%m-%d %H:%M:%S")),]
  
  first_value <- TRUE
  values <- c()
  
  #print(data)
  
  if(nrow(data) > 0)
  {
    for(k in 1:nrow(data))
    {
      value_begin <- as.POSIXct(data$DateTime[k], format="%Y-%m-%d %H:%M:%S")
      
      if(is.na(value_begin) || is.na(begin_timespan) || is.na(end_timespan) || (value_begin < begin_timespan || value_begin > end_timespan))
        next
      
      if(first_value == TRUE)
      {
        first_value <- FALSE
        if(k > 1)
        {    
          values <- c(values,as.numeric(gsub("[^0-9.,-]","",data$Value[k-1])))
        }
      }
      values <- c(values,as.numeric(gsub("[^0-9.,-]","",data$Value[k])))
    }
  }
  
  
  #print(values)
  #print(weights)
  
  if(length(values) == 0)
    return(NA)
  else
    return(median(values,na.rm = TRUE))
}

get_weighted_mean_values_in_range <- function(vector_values,patient_id,begin_time,end_time,set_missing_to_zero = FALSE)
{
  begin_timespan <- as.POSIXct(begin_time)
  end_timespan <- as.POSIXct(end_time)

  data <- get_values_in_range(vector_values,patient_id,begin_timespan-scanning_range_secs,end_timespan+scanning_range_secs)
  data <- data[order(as.POSIXct(data$DateTime, format="%Y-%m-%d %H:%M:%S")),]
  
  first_value <- TRUE
  values <- c()
  weights <- c()
  
  #print(data)
  
  if(nrow(data) > 0)
  {
    for(k in 1:nrow(data))
    {
      value_begin <- as.POSIXct(data$DateTime[k], format="%Y-%m-%d %H:%M:%S")
      
      if(is.na(value_begin) || is.na(begin_timespan) || is.na(end_timespan) || (value_begin < begin_timespan || value_begin > end_timespan))
        next
      
      if(first_value == TRUE)
      {
        first_value <- FALSE
        if(k > 1)
        {
          values <- c(values,as.numeric(gsub("[^0-9.,-]","",data$Value[k-1])))
          weights <- c(weights,difftime(value_begin,begin_timespan,units=c("hours")))
        }
      }
      
      value_end <- end_timespan
      if(k < nrow(data))
        value_end <- as.POSIXct(data$DateTime[k+1], format="%Y-%m-%d %H:%M:%S")
      if(value_end > end_timespan)
        value_end <- end_timespan
      
      value_difftime <- difftime(value_end,value_begin,units=c("hours"))
      
      values <- c(values,as.numeric(gsub("[^0-9.,-]","",data$Value[k])))
      weights <- c(weights,value_difftime)
    }
  }
  
  sum_weights <- sum(weights)
  
  if(set_missing_to_zero == TRUE)
  {
    weight_missing_data <- difftime(end_timespan,begin_timespan,units=c("hours"))-sum_weights
    if(!is.na(weight_missing_data) && weight_missing_data > 0)
    {
      values <- c(values,0)
      weights <- c(weights,weight_missing_data)    
    }
  }

  if(length(values[!is.na(values)]) == 0 || length(weights[!is.na(weights)]) == 0 || sum(weights,na.rm=TRUE) == 0 || 
     length(values[!is.na(values)]) != length(weights[!is.na(weights)]))
    return(NA)
  else
  {
    return(weighted.mean(values,weights[!is.na(weights)],na.rm=TRUE))
  }
}

get_weighted_mean_values_in_range_patient_list <- function(name,vector_data,patient_list,begin_date,end_date,set_missing_to_zero = FALSE)
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    #my_val <- get_all_values_in_range(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))
    
    #if(is.infinite(my_val))
    #  my_val <- NA
    #else
    #{
    my_val <- get_weighted_mean_values_in_range(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))
    #}
    
    list_values <- c(list_values, my_val)  
  }
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}

get_weighted_median_values_in_range <- function(vector_values,patient_id,begin_time,end_time,set_missing_to_zero = FALSE)
{
  begin_timespan <- as.POSIXct(begin_time)
  end_timespan <- as.POSIXct(end_time)
  
  data <- get_values_in_range(vector_values,patient_id,begin_timespan-scanning_range_secs,end_timespan+scanning_range_secs)
  data <- data[order(as.POSIXct(data$DateTime, format="%Y-%m-%d %H:%M:%S")),]
  
  first_value <- TRUE
  values <- c()
  weights <- c()
  
  #print(data)
  
  if(nrow(data) > 0)
  {
    for(k in 1:nrow(data))
    {
      value_begin <- as.POSIXct(data$DateTime[k], format="%Y-%m-%d %H:%M:%S")
      
      if(is.na(value_begin) || is.na(begin_timespan) || is.na(end_timespan) || (value_begin < begin_timespan || value_begin > end_timespan))
        next
      
      if(first_value == TRUE)
      {
        first_value <- FALSE
        if(k > 1)
        {
          values <- c(values,as.numeric(gsub("[^0-9.,-]","",data$Value[k-1])))
          weights <- c(weights,difftime(value_begin,begin_timespan,units=c("hours")))
        }
      }
      
      value_end <- end_timespan
      if(k < nrow(data))
        value_end <- as.POSIXct(data$DateTime[k+1], format="%Y-%m-%d %H:%M:%S")
      if(value_end > end_timespan)
        value_end <- end_timespan
      
      value_difftime <- difftime(value_end,value_begin,units=c("hours"))
      
      values <- c(values,as.numeric(gsub("[^0-9.,-]","",data$Value[k])))
      weights <- c(weights,value_difftime)
    }
  }
  
  sum_weights <- sum(weights)
  
  if(set_missing_to_zero == TRUE)
  {
    weight_missing_data <- difftime(end_timespan,begin_timespan,units=c("hours"))-sum_weights
    if(!is.na(weight_missing_data) && weight_missing_data > 0)
    {
      values <- c(values,0)
      weights <- c(weights,weight_missing_data)  
    }
  }
  
  if(length(values[!is.na(values)]) == 0 || length(weights[!is.na(weights)]) == 0 || sum(weights,na.rm=TRUE) == 0 ||
     length(values[!is.na(values)]) != length(weights[!is.na(weights)]))
    return(NA)
  else
  {
    return(weighted.median(values,weights[!is.na(weights)],na.rm = TRUE))
  }
}


get_weighted_median_values_in_range_patient_list <- function(name,vector_data,patient_list,begin_date,end_date,set_missing_to_zero = FALSE)
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    #my_val <- get_all_values_in_range(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))
    
    #if(is.infinite(my_val))
    #  my_val <- NA
    #else
    #{
    my_val <- get_weighted_median_values_in_range(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))
    #}
    
    list_values <- c(list_values, my_val)  
  }
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}


# EVENTS


import_event <- function(module,page_type)
{
  page_name = "cs_df_pages"
  
  if(grepl("muroar",module))
    page_name = "muroar_df_pages"
  
  if(page_type == "")
    sql_string <- paste("SELECT PatientID, vs.Date AS Date1, vs.Date2 AS Date2
                              FROM ",page_name," AS vs
                              WHERE Module='",module,"' ORDER BY Date1 ASC",sep="")
  else
    sql_string <- paste("SELECT PatientID, vs.Date AS Date1, vs.Date2 AS Date2
                                FROM ",page_name,"  AS vs
                                WHERE Module='",module,"' AND PageType ='",page_type,"' ORDER BY Date1 ASC",sep="")
  rs = dbSendQuery(mydb,sql_string)
  return(fetch(rs,n=-1))
}

get_event_patient <- function(vector_events,patient_id)
{
  return(vector_events[is.element(vector_events$PatientID,patient_id),])
}

get_events_in_range <- function(vector_events,patient_id,begin_time="1900-01-01",end_time="2199-01-01")
{
  my_events <- vector_events[is.element(vector_events$PatientID,patient_id),]

  my_events[which(my_events$Date2 == "0000-00-00 00:00:00"),]$Date2 <- my_events[which(my_events$Date2 == "0000-00-00 00:00:00"),]$Date1
    
  if(!is.na(begin_time) && begin_time != "1900-01-01")
  {
    my_events <- subset(my_events, as.POSIXct(Date1, format="%Y-%m-%d %H:%M:%S") >= as.POSIXct(begin_time) & as.POSIXct(Date2, format="%Y-%m-%d %H:%M:%S") >= as.POSIXct(begin_time))
  }
  if(!is.na(end_time) && end_time != "2199-01-01")
  {
    my_events <- subset(my_events, as.POSIXct(Date1, format="%Y-%m-%d %H:%M:%S") <= as.POSIXct(end_time) & as.POSIXct(Date2, format="%Y-%m-%d %H:%M:%S") <= as.POSIXct(end_time))
  }
  
  return(my_events)
}


get_first_event_patient <- function(vector_events,patient_id)
{
  events <- get_event_patient(vector_events,patient_id)
  
  if(length(events$Date1) >= 1)
  {
    events <- events[order(as.Date(events$Date1, format="%Y-%m-%d %H:%M:%S")),]
    return(events[1,])
  }
  else
    return(NA)
}

get_first_event_in_range <- function(vector_events,patient_id,begin_time="1900-01-01",end_time="2199-01-01")
{
  events <- get_events_in_range(vector_events,patient_id,begin_time,end_time)
  
  if(length(events$Date1) >= 1)
  {
    events <- events[order(as.Date(events$Date1, format="%Y-%m-%d %H:%M:%S")),]
    return(events[1,])
  }
  else
    return(NA)
}


get_last_event_patient <- function(vector_events,patient_id)
{
  events <- get_event_patient(vector_events,patient_id)
  
  if(length(events$Date1) >= 1)
  {
    events <- events[order(as.POSIXct(events$Date1, format="%Y-%m-%d %H:%M:%S"),decreasing=TRUE),]
    return(events[1,])
  }
  else
    return(NA)
}

get_first_event_patient_list <- function(name,vector_events,patient_list)
{
  
  list_date1 <- c()
  list_date2 <- c()
  for(i in 1:nrow(patient_list))
  {

    my_events <- get_event_patient(vector_events,patient_list[i,])
    if(nrow(my_events) > 0)
    {
      list_date1 <- c(list_date1,my_events[1,]$Date1)
      list_date2 <- c(list_date2,my_events[1,]$Date2)
    }
    else
    {
      list_date1 <- c(list_date1,NA)
      list_date2 <- c(list_date2,NA)
    }
  }
  
  df <- data.frame(ID = patient_list, placeholder_name_1 = list_date1, placeholder_name_2 = list_date2 )
  names(df)[names(df) == "placeholder_name_1"] <- paste(name,"_begin",sep="")
  names(df)[names(df) == "placeholder_name_2"] <- paste(name,"_end",sep="")
  return(df)
  
}

get_first_event_patient_list_in_range <- function(name,vector_events,patient_list,begin="1900-01-01",end="2199-01-01")
{
  
  list_date1 <- c()
  list_date2 <- c()
  for(i in 1:nrow(patient_list))
  {
    my_begin <- "1900-01-01"
    my_end <- "2199-01-01"
    
    if(length(begin) > 1)
      my_begin <- begin[i]
    if(length(end) > 1)
      my_end <- end[i]
      
    my_events <- get_events_in_range(vector_events,patient_list[i,],my_begin,my_end)
    if(nrow(my_events) > 0)
    {
      list_date1 <- c(list_date1,my_events[1,]$Date1)
      list_date2 <- c(list_date2,my_events[1,]$Date2)
    }
    else
    {
      list_date1 <- c(list_date1,NA)
      list_date2 <- c(list_date2,NA)
    }
  }

  df <- data.frame(ID = patient_list, placeholder_name_1 = list_date1, placeholder_name_2 = list_date2 )
  names(df)[names(df) == "placeholder_name_1"] <- paste(name,"_begin",sep="")
  names(df)[names(df) == "placeholder_name_2"] <- paste(name,"_end",sep="")
  return(df)
 
}


get_all_events_patient <- function(vector_events,patient_id)
{
  events <- get_event_patient(vector_events,patient_id)
  
  if(length(events$Date1) >= 1)
  {
    tmp_array_1 <- c()
    tmp_array_2 <- c()
    
    for(i in 1:length(events$Date1))
    {
      if(!is.na(events$Date1[i]))
        tmp_array_1 <- c(tmp_array_1,events$Date1[i])
      else
        tmp_array_1 <- c(tmp_array_1, NA)
      
      if(!is.na(events$Date2[i]))
        tmp_array_2 <- c(tmp_array_2,events$Date2[i])
      else
        tmp_array_2 <- c(tmp_array_2, NA)
      
    }
    
    if(length(tmp_array_1) > 0)
    {
      return(c(paste(tmp_array_1,collapse=";"), paste(tmp_array_2,collapse=";")))
    }
      
    else
      return(c(NA,NA))
  }
  else
  {
    return(NA)
  }
}

get_all_events_patient_list_in_range <- function(name,vector_events,patient_list,begin,end)
{
  
  list_date1 <- c()
  list_date2 <- c()
  for(i in 1:nrow(patient_list))
  {
    if(is.na(begin[i]) || is.na(end[i]))
    {
      list_date1 <- c(list_date1,NA)
      list_date2 <- c(list_date2,NA)
      next;
    }

    my_events <- get_events_in_range(vector_events,patient_list[i,],begin[i],end[i])
    if(nrow(my_events) > 0)
    {
      list_date1 <- c(list_date1,paste(my_events$Date1,collapse=";"))
      list_date2 <- c(list_date2,paste(my_events$Date2,collapse=";"))
    }
    else
    {
      list_date1 <- c(list_date1,NA)
      list_date2 <- c(list_date2,NA)
    }
  }
  

  df <- data.frame(ID = patient_list, placeholder_name_1 = list_date1, placeholder_name_2 = list_date2 )
  names(df)[names(df) == "placeholder_name_1"] <- paste(name,"_begin",sep="")
  names(df)[names(df) == "placeholder_name_2"] <- paste(name,"_end",sep="")
  return(df)
  
}



get_all_events_patient_list <- function(name,vector_events,patient_list)
{
  list_date1 <- c()
  list_date2 <- c()
  for(i in 1:nrow(patient_list))
  {
    my_events <- get_all_events_patient(vector_events,patient_list[i,])
    if(length(my_events) > 0)
    {
      list_date1 <- c(list_date1,my_events[1])
      list_date2 <- c(list_date2,my_events[2])
    }
    else
    {
      list_date1 <- c(list_date1,NA)
      list_date2 <- c(list_date2,NA)
    }
  }
  
  df <- data.frame(ID = patient_list, placeholder_name_1 = list_date1, placeholder_name_2 = list_date2 )
  names(df)[names(df) == "placeholder_name_1"] <- paste(name,"_begin",sep="")
  names(df)[names(df) == "placeholder_name_2"] <- paste(name,"_end",sep="")
  return(df)
  
}

count_all_events_patient_list <- function(name,vector_events,patient_list)
{
  list_counter <- c()
  for(i in 1:nrow(patient_list))
  {
    my_events <- get_all_events_patient(vector_events,patient_list[i,])
    if(class(data) == "logical" && is.na(my_events))
      list_counter <- c(list_counter,0)
    else
      list_counter <- c(list_counter,length(my_events))
  }
  df <- data.frame(ID = patient_list, placeholder_name = list_counter )
  names(df)[names(df) == "placeholder_name"] <- paste(name,sep="")
  return(df)
  
}


get_last_event_patient_list <- function(name,vector_events,patient_list)
{
  list_date1 <- c()
  list_date2 <- c()
  for(i in 1:nrow(patient_list))
  {
    my_events <- get_last_event_patient(vector_events,patient_list[i,])
    if(!(class(my_events) == "logical" && is.na(my_events)))
    {
      list_date1 <- c(list_date1,my_events$Date1)
      list_date2 <- c(list_date2,my_events$Date2)
    }
    else
    {
      list_date1 <- c(list_date1,NA)
      list_date2 <- c(list_date2,NA)
    }
  }
  df <- data.frame(ID = patient_list, placeholder_name_1 = list_date1, placeholder_name_2 = list_date2 )
  
  names(df)[names(df) == "placeholder_name_1"] <- paste(name,"_begin",sep="")
  names(df)[names(df) == "placeholder_name_2"] <- paste(name,"_end",sep="")
  return(df)
  
}

get_event_days_patient <- function(vector_events,patient_id)
{
  values = get_event_patient(vector_events,patient_id)
  
  if(nrow(values) > 0)
  {
    count = 0
    for(i in 1:nrow(values))
    {
      date1 <- values$Date1[i]
      date2 <- values$Date2[i]
      
      if(is.na(date1) || date1 == "0000-00-00 00:00:00" ||  
         is.na(date2) || date2 == "0000-00-00 00:00:00")
        return("ongoing")
      
      diff <- as.numeric(difftime(as.POSIXct(date2,format="%Y-%m-%d %H:%M:%S"),as.POSIXct(date1,format="%Y-%m-%d %H:%M:%S"),units="hours"))
      if(diff < 0)
        diff <- 0
      
      count = count + diff
    }
    return(round(count,1))
  }
  else
    return(0)
}


get_event_days_patient_list <- function(name,vector_events,patient_list)
{
  list_event_days <- c()
  for(i in 1:nrow(patient_list))
  {
    
    list_event_days <- c(list_event_days,get_event_days_patient(vector_events,patient_list[i,]))
  }
  
  df <- data.frame(ID = patient_list, placeholder_name = as.numeric(list_event_days ))
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}


get_medication_type <- function(patient_id,date_begin,date_end,type)
{
  date_begin <- format(date_begin,format="%Y-%m-%d %H:%M:%S")
  date_end <- format(date_end,format="%Y-%m-%d %H:%M:%S")
  
  sql_string <- paste("SELECT cs_medication_list.Name AS Name, DateTime, Value FROM cs_medication_data 
                      INNER JOIN cs_medication_list ON cs_medication_data.Type = cs_medication_list.ID 
                      WHERE PatientID=",patient_id," AND DateTime >= '",date_begin,
                      "' AND DateTime <= '",date_end,"' AND cs_medication_data.Type IN (",paste("'",type,"'",collapse=", ",sep=""),")",sep="")  
  print(sql_string)
  rs = dbSendQuery(mydb,sql_string)
  values <- fetch(rs,n=-1)
  diff_medication = unique(values$Name)
  return(paste(diff_medication,collapse=", "))
}

get_sum_values_in_range_patient_list <- function(name,vector_data,patient_list,begin_date,end_date)
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    list_values <- c(list_values,sum(as.numeric(get_values_in_range(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))$Value,na.rm=TRUE),na.rm=TRUE))
  }
  #print(list_values)
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}

get_min_values_in_range_patient_list <- function(name,vector_data,patient_list,begin_date,end_date)
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    my_val <- min(get_values_in_range(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))$Value,na.rm=TRUE)
    
    if(is.infinite(my_val))
      my_val <- NA
    
    list_values <- c(list_values, my_val)  
  }
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}

get_min_values_in_range_tp_patient_list <- function(name,vector_data,patient_list,begin_date,end_date)
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    my_val <- min(get_values_in_range_tp(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))$Value,na.rm=TRUE)
    
    if(is.infinite(my_val))
      my_val <- NA
    
    list_values <- c(list_values, my_val)  
  }
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}


get_max_values_in_range_patient_list <- function(column_name,vector_data,patient_list,begin_date,end_date)
{
  
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {  
    my_val <- max(as.numeric(gsub("[^0-9.,-]", "", get_values_in_range(vector_data, patient_list[i,], as.POSIXct(begin_date[i], optional = TRUE), as.POSIXct(end_date[i], optional = TRUE))$Value)), na.rm = TRUE)
    #my_val <- max(as.numeric(gsub("[^0-9.,-]","", get_values_in_range(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))$Value,na.rm=TRUE))
              
    if(is.infinite(my_val))
      my_val <- NA
    
    list_values <- c(list_values, my_val)
  }
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- column_name
  return(df)
}

get_max_values_in_range_tp_patient_list <- function(name,vector_data,patient_list,begin_date,end_date)
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    my_val <- max(as.numeric(gsub("[^0-9.,-]","",get_values_in_range_tp(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))$Value)),na.rm=TRUE)
    
    if(is.infinite(my_val))
      my_val <- NA
    
    list_values <- c(list_values, my_val)
  }
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}



get_horovitz_index_min_patient_list <- function(name,vector_paO2,vector_fiO2,vector_oxygen_flow,patient_list,begin_date,end_date)
{
  list_values <- c()
  
  for(pat_i in 1:nrow(patient_list))
  {

    data_paO2 <- get_values_in_range(vector_paO2,patient_list[pat_i,],as.POSIXct(begin_date[pat_i],optional=TRUE)-3600*24,as.POSIXct(end_date[pat_i],optional=TRUE)+3600*24)
    data_fiO2 <- get_values_in_range(vector_fiO2,patient_list[pat_i,],as.POSIXct(begin_date[pat_i],optional=TRUE)-3600*24,as.POSIXct(end_date[pat_i],optional=TRUE)+3600*24)
    data_oxygen_flow <- get_values_in_range(vector_oxygen_flow,patient_list[pat_i,],as.POSIXct(begin_date[pat_i],optional=TRUE)-3600*24,as.POSIXct(end_date[pat_i],optional=TRUE)+3600*24)

    
    horovitz_index_array <- c()
    if(nrow(data_paO2) > 0)
    {
      for(i in 1:nrow(data_paO2))
      {
        if(is.na(as.numeric(data_paO2$Value[i])) || as.numeric(data_paO2$Value[i]) <= 0 || as.numeric(data_paO2$Value[i]) >= 1000)
          next
        
        datetime_paO2 <- as.POSIXct(data_paO2$DateTime[i],format="%Y-%m-%d %H:%M:%S")
        last_value_datetime <- as.POSIXct("1900-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S")
        last_value_fiO2 <- NA
        
        if(nrow(data_fiO2) > 0)
        {
          for(j in 1:nrow(data_fiO2))
          {
            datetime_fiO2 <- as.POSIXct(data_fiO2$DateTime[j],format="%Y-%m-%d %H:%M:%S")
            
            if(!is.na(datetime_paO2) &&
               !is.na(datetime_fiO2) &&
               !is.na(data_fiO2$Value[j]) && 
               !is.na(as.numeric(data_fiO2$Value[j])) &&
               as.numeric(data_fiO2$Value[j]) > 0 &&
               as.numeric(data_fiO2$Value[j]) <= 100 &&
               datetime_paO2 >= datetime_fiO2 &&
               datetime_fiO2 > last_value_datetime)
            {
              last_value_datetime <- datetime_fiO2
              last_value_fiO2 <- as.numeric(data_fiO2$Value[j])
            }
          }
        }
        


        if(is.na(last_value_fiO2) && nrow(data_oxygen_flow) > 0)
        {
          last_value_datetime <- as.POSIXct("1900-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S")

          for(j in 1:nrow(data_oxygen_flow))
          {
            datetime_oxygen_flow <- as.POSIXct(data_oxygen_flow$DateTime[j],format="%Y-%m-%d %H:%M:%S")
            
            if(!is.na(datetime_paO2) &&
               !is.na(datetime_oxygen_flow) &&
               !is.na(data_oxygen_flow$Value[j]) && 
#               as.numeric(data_oxygen_flow$Value[j]) != 0 &&
               datetime_paO2 >= datetime_oxygen_flow &&
               datetime_oxygen_flow > last_value_datetime)
            {
              last_value_datetime <- datetime_oxygen_flow
              
              if(as.numeric(data_oxygen_flow$Value[j]) < 1)
              {
                last_value_fiO2 <- 21
              }
              else if(as.numeric(data_oxygen_flow$Value[j]) <= 1)
              {
                last_value_fiO2 <- 25
              } 
              else if(as.numeric(data_oxygen_flow$Value[j]) <= 2)
              {
                last_value_fiO2 <- 29
              }
              else if(as.numeric(data_oxygen_flow$Value[j]) <= 3)
              {
                last_value_fiO2 <- 33
              }
              else if(as.numeric(data_oxygen_flow$Value[j]) <= 4)
              {
                last_value_fiO2 <- 37
              }
              else if(as.numeric(data_oxygen_flow$Value[j]) <= 5)
              {
                last_value_fiO2 <- 41
              }
              else if(as.numeric(data_oxygen_flow$Value[j]) <= 6)
              {
                last_value_fiO2 <- 45
              }
              else
              {
                last_value_fiO2 <- 50
              }
              
            }
          }
        }
        

        
        if(!is.na(last_value_fiO2) && !is.na(data_paO2$Value[i]) && last_value_fiO2 != 0 && 
           (last_value_datetime >= as.POSIXct(begin_date[pat_i],optional=TRUE) || datetime_paO2 >= as.POSIXct(begin_date[pat_i],optional=TRUE) ) &&
           (last_value_datetime <= as.POSIXct(end_date[pat_i],optional=TRUE) || datetime_paO2 <= as.POSIXct(end_date[pat_i],optional=TRUE) )
           
           )
        {
          #print(paste(as.numeric(data_paO2$Value[i]),as.numeric(last_value_fiO2),as.numeric(data_paO2$Value[i])/(as.numeric(last_value_fiO2)/100)))
          horovitz_index_array <- c(horovitz_index_array,as.numeric(data_paO2$Value[i])/(as.numeric(last_value_fiO2)/100))
        }
        
      }   
    }
    
    my_val <- min(horovitz_index_array,na.rm=TRUE)
    
    if(is.infinite(my_val))
      my_val <- NA

    list_values <- c(list_values,my_val)
  }
  #print(list_values)
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)  
}





get_saps2_score_patient_list <- function(name,patient_list,age_list,begin_date,end_date)
{
  list_values <- c()
  

  heartrate <- import_value("vitalsigns",3)
  rr_systolic <- import_value("vitalsigns",1)
  temperature <- import_value("vitalsigns",6)
#paO2 <- import_value("lab",226)
  paO2 <- import_value("lab",226,c("arterial"))
  fiO2 <- import_value("ventilation",10)
  gcs_admission <- import_value("admission","admission_hospital_pe_gcs")
  urea <- import_value("lab",5)
  urin_output <- import_value("balances",c())
  icu_stays <- get_first_event_patient_list_in_range("ICUStay",import_event("cs_admission","admission_hospital"),patient_list,as.POSIXct(global_patient_data$InclusionDate)-3600*24 )
  sodium <- import_value("lab",243)
  potassium <- import_value("lab",244)
  bicarbonate <- import_value("lab",c(228,229))
  bilirubine <- import_value("lab",c(26,251))
  leukocytes <- import_value("lab",c(202))
  metastatic_cancer <- import_value("admission","admission_hospital_medical_history_metastatic")
  hematologic_malignancy <- import_value("admission","admission_hospital_medical_history_malignant_hemat")
  aids <- import_value("admission","admission_hospital_medical_history_aids")

  
  for(pat_i in 1:nrow(patient_list))
  {
    print(paste("Calculate patient",pat_i,patient_list[pat_i,]))
    saps2_score <- 6    # 6 points for medical admission type 
    age <- age_list[pat_i]
    heartrate_min <- get_min_value_in_range(heartrate,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    heartrate_max <- get_max_value_in_range(heartrate,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    systolicRR_min <- get_min_value_in_range(rr_systolic,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    systolicRR_max <- get_max_value_in_range(rr_systolic,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    temperature_max <- get_max_value_in_range(temperature,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    data_paO2 <- get_values_in_range(paO2,patient_list[pat_i,],as.POSIXct(begin_date[pat_i],optional=TRUE)-3600*24,as.POSIXct(end_date[pat_i],optional=TRUE)+3600*24)
    data_fiO2 <- get_values_in_range(fiO2,patient_list[pat_i,],as.POSIXct(begin_date[pat_i],optional=TRUE)-3600*24,as.POSIXct(end_date[pat_i],optional=TRUE)+3600*24)
    gcs_before_sedation <- get_first_value_patient(gcs_admission,patient_list[pat_i,])
    urea_max <- get_max_value_in_range(urea,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    
    if(is.na(begin_date[pat_i]) || is.na(end_date[pat_i]))
    {
      print(paste("No end date for pat_id",patient_list[pat_i,]))
      list_values <- c(list_values,NA)
      next
    }
    
    urin_output_day <- get_values_in_range(urin_output,patient_list[pat_i,],paste(as.Date(begin_date[pat_i],format="%Y-%m-%d"),"00:00:00"),paste(as.Date(end_date[pat_i],format="%Y-%m-%d"),"23:59:59"))
    if(nrow(urin_output_day) == 0)
      urin_output_day <- NA
    else if(nrow(urin_output_day) >= 1)
    {
      time_begin <- as.POSIXct(begin_date[pat_i])
      time_end <- as.POSIXct(paste(as.Date(begin_date[pat_i],format="%Y-%m-%d"),"23:59:59"))
      if(as.POSIXct(icu_stays$ICUStay_end[pat_i]) < time_end)
        time_end <- as.POSIXct(as.POSIXct(icu_stays$ICUStay_end[pat_i]))
      
      urin_output_day <- urin_output_day$Output[1]*(24/as.numeric(difftime(time_end,time_begin,units="hours")))
    }
    
    sodium_min <- get_min_value_in_range(sodium,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    sodium_max <- get_max_value_in_range(sodium,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    potassium_min <- get_min_value_in_range(potassium,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    potassium_max <- get_max_value_in_range(potassium,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])  
    sbc_min <- get_min_value_in_range(bicarbonate,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    bilirubine_max <- get_max_value_in_range(bilirubine,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    leukocytes_min <- get_min_value_in_range(leukocytes,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    leukocytes_max <- get_max_value_in_range(leukocytes,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])  
    
    has_metastatic_cancer <- get_first_value_in_range(metastatic_cancer,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_metastatic_cancer) == 1 && !is.na(has_metastatic_cancer$Value[1]) && has_metastatic_cancer$Value[1] == "yes")
      saps2_score <- saps2_score + 9

    has_hematologic_malignancy <- get_first_value_in_range(hematologic_malignancy,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_hematologic_malignancy) == 1 && !is.na(has_hematologic_malignancy$Value[1]) && has_hematologic_malignancy$Value[1] == "yes")
      saps2_score <- saps2_score + 10
    
    has_aids <- get_first_value_in_range(aids,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_aids) == 1 && !is.na(has_aids$Value[1]) && has_aids$Value[1] == "yes")
      saps2_score <- saps2_score + 17
    
    
    if(age >= 40 && age < 60)
    {
      saps2_score = saps2_score + 7
    }
    else if(age >= 60 && age < 70)
    {
      saps2_score = saps2_score + 12
    }
    else if(age >= 70 && age < 75)
    {
      saps2_score = saps2_score + 15
    }
    else if(age >= 75 && age < 80)
    {
      saps2_score = saps2_score + 16
    }
    else if(age >= 80)
    {
      saps2_score = saps2_score + 18
    }
    

    if(is.na(heartrate_min) || is.na(heartrate_max))
    {
      saps2_score <- NA
      print("no heartrate")
    }
    else if(heartrate_min < 40 && heartrate_min != 0)
    {
      saps2_score = saps2_score + 11
    }
    else if(heartrate_max >= 160)
    {
      saps2_score = saps2_score + 7
    }
    else if(heartrate_max >= 120 && heartrate_max < 160)
    {
      saps2_score = saps2_score + 4
    }
    else if(heartrate_min >= 40 && heartrate_min < 70)
    {
      saps2_score = saps2_score + 2
    }
    
    
    if(is.na(systolicRR_min) || is.na(systolicRR_max))
    {
      saps2_score <- NA
      print("no RR")
    }
    else if(systolicRR_min < 70 && systolicRR_min != 0)
    {
      saps2_score = saps2_score + 13
    }
    else if(systolicRR_min >= 70 && systolicRR_min < 100)
    {
      saps2_score = saps2_score + 5
    }
    else if(systolicRR_max >= 200)
    {
      saps2_score = saps2_score + 2
    }
    
    
    if(is.na(temperature_max))
    {
      saps2_score <- NA
      print("no temperature")
    }
    else if(temperature_max >= 39)
    {
      saps2_score = saps2_score + 3
    }
    

    
    if(is.na(gcs_before_sedation))
    {
      saps2_score <- NA
      print("no gcs before sedation")
    }
    else if(gcs_before_sedation < 6)
    {
      saps2_score = saps2_score + 26
    }
    else if(gcs_before_sedation >= 6 && gcs_before_sedation <= 8)
    {
      saps2_score = saps2_score + 13
    }
    else if(gcs_before_sedation >= 9 && gcs_before_sedation <= 10)
    {
      saps2_score = saps2_score + 7
    }
    else if(gcs_before_sedation >= 11 && gcs_before_sedation <= 13)
    {
      saps2_score = saps2_score + 5
    }
    

    horovitz_index_array <- c()
    if(nrow(data_paO2) > 0)
    {
      for(i in 1:nrow(data_paO2))
      {
        if(is.na(as.numeric(data_paO2$Value[i])) || as.numeric(data_paO2$Value[i]) <= 0 || as.numeric(data_paO2$Value[i]) >= 1000)
          next
        
        datetime_paO2 <- as.POSIXct(data_paO2$DateTime[i],format="%Y-%m-%d %H:%M:%S")
        last_value_datetime <- as.POSIXct("1900-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S")
        last_value_fiO2 <- NA
        
        if(nrow(data_fiO2) > 0)
        {
          for(j in 1:nrow(data_fiO2))
          {
            datetime_fiO2 <- as.POSIXct(data_fiO2$DateTime[j],format="%Y-%m-%d %H:%M:%S")
            
            if(!is.na(datetime_paO2) &&
               !is.na(datetime_fiO2) &&
               !is.na(data_fiO2$Value[j]) && 
               !is.na(as.numeric(data_fiO2$Value[j])) &&
               as.numeric(data_fiO2$Value[j]) > 0 &&
               as.numeric(data_fiO2$Value[j]) <= 100 &&
               datetime_paO2 >= datetime_fiO2 &&
               datetime_fiO2 > last_value_datetime)
            {
              last_value_datetime <- datetime_fiO2
              last_value_fiO2 <- as.numeric(data_fiO2$Value[j])
            }
          }
        }
        
        
        
        if(!is.na(last_value_fiO2) && !is.na(data_paO2$Value[i]) && last_value_fiO2 != 0 && 
           (last_value_datetime >= as.POSIXct(begin_date[pat_i],optional=TRUE) || datetime_paO2 >= as.POSIXct(begin_date[pat_i],optional=TRUE) ) &&
           (last_value_datetime <= as.POSIXct(end_date[pat_i],optional=TRUE) || datetime_paO2 <= as.POSIXct(end_date[pat_i],optional=TRUE) )
           
        )
        {
          #print(paste(as.numeric(data_paO2$Value[i]),as.numeric(last_value_fiO2),as.numeric(data_paO2$Value[i])/(as.numeric(last_value_fiO2)/100)))
          horovitz_index_array <- c(horovitz_index_array,as.numeric(data_paO2$Value[i])/(as.numeric(last_value_fiO2)/100))
        }
        
      }   
    }
    
    horovitz_index_min <- min(horovitz_index_array,na.rm=TRUE)
    
    if(is.na(horovitz_index_min) )
    {
      print("no horovitz index")
      saps2_score <- NA
    }
    else if(horovitz_index_min < 100)
    {
      saps2_score = saps2_score + 11
    }
    else if(horovitz_index_min >= 100 && horovitz_index_min < 200)
    {
      saps2_score = saps2_score + 9
    }
    else if(horovitz_index_min > 200)
    {
      saps2_score = saps2_score + 6
    }
    
    
    if(is.na(urea_max))
    {
      saps2_score <- NA
      print("no urea")
    }
    else if(urea_max >= 84)
    {
      saps2_score = saps2_score + 10
    }
    else if(urea_max >= 28)
    {
      saps2_score = saps2_score + 6
    }
    
    
    if(is.na(urin_output_day))
    {
      saps2_score <- NA
      print("no urin output")
    }
    else if(urin_output_day < 500)
    {
      saps2_score = saps2_score + 11
    }
    else if(urin_output_day >= 500 && urin_output_day < 1000)
    {
      saps2_score = saps2_score + 4
    }
    
    
    if(is.na(sodium_max))
    {
      saps2_score <- NA
      print("no sodium")
    }
    else if(sodium_min < 125)
    {
      saps2_score = saps2_score + 5
    }
    else if(sodium_max >= 145)
    {
      saps2_score = saps2_score + 1
    }
    
    
    if(is.na(potassium_max))
    {
      saps2_score <- NA
      print("no potassium")
    }
    else if(potassium_min < 3)
    {
      saps2_score = saps2_score + 3
    }
    else if(potassium_max >= 5)
    {
      saps2_score = saps2_score + 3
    }
    
    
    if(is.na(sbc_min))
    {
      saps2_score <- NA
      print("no sbc")
    }
    else if(sbc_min < 15)
    {
      saps2_score = saps2_score + 6
    }
    else if(sbc_min >= 15 && sbc_min < 20)
    {
      saps2_score = saps2_score + 3
    }
    
    
    
    if(is.na(bilirubine_max))
    {
      saps2_score <- NA
      print("no bilirubine")
    }
    else if(bilirubine_max >= 6)
    {
      saps2_score = saps2_score + 9
    }
    else if(bilirubine_max >= 4 && bilirubine_max < 6)
    {
      saps2_score = saps2_score + 4
    }
    
    
    if(is.na(leukocytes_max))
    {
      print("no leukocytes")
      saps2_score <- NA
    }
    else if(leukocytes_min < 1)
    {
      saps2_score = saps2_score + 12
    }
    else if(leukocytes_max >= 20)
    {
      saps2_score = saps2_score + 3
    }
    
    
    if(is.infinite(saps2_score))
      saps2_score <- NA
    
    if(is.na(saps2_score))
      print(paste("NA Score for patient",patient_list[pat_i,]))

    list_values <- c(list_values,saps2_score)
  }

  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)  
}

get_charlson_comorbidity_index <- function(name,patient_list,age_list,begin_date,end_date)
{
  
  list_values <- c()
  
  mi <- import_value("admission","admission_hospital_medical_history_mi")
  chf <- import_value("admission","admission_hospital_medical_history_chf")
  pad <- import_value("admission","admission_hospital_medical_history_pad")
  cva <- import_value("admission","admission_hospital_medical_history_stroke")
  dementia <- import_value("admission","admission_hospital_medical_history_dementia")
  copd <- import_value("admission","admission_hospital_medical_history_copd")
  ctd <- import_value("admission","admission_hospital_medical_history_ctd")
  pud <- import_value("admission","admission_hospital_medical_history_pud")
  cirrhosis <- import_value("admission","admission_hospital_medical_history_cirrhosis")
  dm <- import_value("admission","admission_hospital_cv_factors_diabetes")
  dm_eod <- import_value("admission","admission_hospital_cv_factors_diabetes_eod")
  hemiplegia <- import_value("admission","admission_hospital_medical_history_hemiplegia")
  ckd <- import_value("admission","admission_hospital_medical_history_ckd")
  cancer <- import_value("admission","admission_hospital_medical_history_cancer")
  metastatic <- import_value("admission","admission_hospital_medical_history_metastatic")
  leukemia <- import_value("admission","admission_hospital_medical_history_leuk_myel")
  lymphoma <- import_value("admission","admission_hospital_medical_history_lymphoma")
  aids <- import_value("admission","admission_hospital_medical_history_aids")
  
  
  for(pat_i in 1:nrow(patient_list))
  {
    #print(paste("Calculate patient",pat_i,patient_list[pat_i,]))
    cc_index <- 0
    age <- age_list[pat_i]
    
    if(is.na(begin_date[pat_i]) || is.na(end_date[pat_i]))
    {
      print(paste("No end date for pat_id",patient_list[pat_i,]))
      list_values <- c(list_values,NA)
      next
    }
    
    if(age >= 80)
      cc_index = cc_index + 4
    else if(age >= 70)
      cc_index = cc_index + 3
    else if(age >= 60)
      cc_index = cc_index + 2
    else if(age >= 50)
      cc_index = cc_index + 1
    
    has_mi <- get_first_value_in_range(mi,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_mi) == 1 && !is.na(has_mi$Value[1]) && has_mi$Value[1] == "yes")
      cc_index = cc_index+1
    
    has_chf <- get_first_value_in_range(chf,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_chf) == 1 && !is.na(has_chf$Value[1]) && has_chf$Value[1] == "hfref")
      cc_index = cc_index+1
    
    has_pad <- get_first_value_in_range(pad,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_pad) == 1 && !is.na(has_pad$Value[1]) && has_pad$Value[1] == "yes")
      cc_index = cc_index+1
    
    has_cva <- get_first_value_in_range(cva,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_cva) == 1 && !is.na(has_cva$Value[1]) && has_cva$Value[1] != "no")
      cc_index = cc_index+1
  
    has_dementia <- get_first_value_in_range(dementia,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_dementia) == 1 && !is.na(has_dementia$Value[1]) && has_dementia$Value[1] == "yes")
      cc_index = cc_index+1
    
    has_copd <- get_first_value_in_range(copd,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_copd) == 1 && !is.na(has_copd$Value[1]) && has_copd$Value[1] == "yes")
      cc_index = cc_index+1    
    
    has_ctd <- get_first_value_in_range(ctd,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_ctd) == 1 && !is.na(has_ctd$Value[1]) && has_ctd$Value[1] == "yes")
      cc_index = cc_index+1
    
    has_pud <- get_first_value_in_range(pud,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_pud) == 1 && !is.na(has_pud$Value[1]) && has_pud$Value[1] == "yes")
      cc_index = cc_index+1    
    
    has_cirrhosis <- get_first_value_in_range(cirrhosis,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_cirrhosis) == 1 && !is.na(has_cirrhosis$Value[1]) && has_cirrhosis$Value[1] == "yes")
      cc_index = cc_index+3
    
    has_dm <- get_first_value_in_range(dm,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_dm) == 1 && !is.na(has_dm$Value[1]) && has_dm$Value[1] != "no")
      cc_index = cc_index+1
    
    has_dm_eod <- get_first_value_in_range(dm_eod,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_dm_eod) == 1 && !is.na(has_dm_eod$Value[1]) && has_dm_eod$Value[1] == "yes")
      cc_index = cc_index+1    
    
    has_hemiplegia <- get_first_value_in_range(hemiplegia,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_hemiplegia) == 1 && !is.na(has_hemiplegia$Value[1]) && has_hemiplegia$Value[1] == "yes")
      cc_index = cc_index+2
    
    
    has_ckd <- get_first_value_in_range(ckd,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_ckd) == 1 && !is.na(has_ckd$Value[1]) && has_ckd$Value[1] == "g5")
      cc_index = cc_index+1    
    
    has_cancer<- get_first_value_in_range(cancer,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_cancer) == 1 && !is.na(has_cancer$Value[1]) && has_cancer$Value[1] == "yes")
      cc_index = cc_index+2
    
    has_metastatic <- get_first_value_in_range(metastatic,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_metastatic) == 1 && !is.na(has_metastatic$Value[1]) && has_metastatic$Value[1] == "yes")
      cc_index = cc_index+4
    
    has_leukemia <- get_first_value_in_range(leukemia,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_leukemia) == 1 && !is.na(has_leukemia$Value[1]) && has_leukemia$Value[1] == "yes")
      cc_index = cc_index+2
    
    has_lymphoma <- get_first_value_in_range(lymphoma,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_lymphoma) == 1 && !is.na(has_lymphoma$Value[1]) && has_lymphoma$Value[1] == "yes")
      cc_index = cc_index+2
    
    has_aids <- get_first_value_in_range(aids,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_aids) == 1 && !is.na(has_aids$Value[1]) && has_aids$Value[1] == "yes")
      cc_index = cc_index+6    
    
    if(is.na(cc_index))
      print(paste("NA Score for patient",patient_list[pat_i,]))
    
    list_values <- c(list_values,cc_index)
  }
  
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)  
}

updated_get_iabp_shock_2_score <- function(name,patient_list,age_list,begin_date,end_date)
{
  
  list_values <- c()
  
  stroke <- import_value("admission","admission_hospital_medical_history_stroke")
  timi_flow <- import_value("procedures","procedures_cathleft_culprit_timi")
  glucose <- import_value("lab",248)
  lactate <- import_value("lab",249,"arterial")
  creatinine <- import_value("lab",c(6))
  
  #calculate beginn date acoording to each patient: 
  # Now, calculate 'begin_date' and 'end_date' for each row
  global_patient_data$ICUStay_begin <- as.POSIXct(global_patient_data$ICUStay_begin, format="%Y-%m-%d %H:%M:%S")
  global_patient_data$begin_date <- global_patient_data$ICUStay_begin - 6*3600 # 6 hours in seconds
  global_patient_data$end_date <- global_patient_data$ICUStay_begin + 24*3600 # 24 hours in seconds
  
  
  for(pat_i in 1:nrow(patient_list))
  {
    #print(paste("Calculate patient",pat_i,patient_list[pat_i,]))
    cc_index <- 0
    age <- age_list[pat_i]
    
    if(is.na(begin_date[pat_i]) || is.na(end_date[pat_i]))
    {
      print(paste("No end date for pat_id",patient_list[pat_i,]))
      list_values <- c(list_values,NA)
      next
    }
    
    if(age > 73)
      cc_index = cc_index + 1
    
    has_stroke <- get_first_value_in_range(stroke,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_stroke) == 1 && !is.na(has_stroke$Value[1]) && has_stroke$Value[1] == "stroke")
      cc_index = cc_index + 2
    
    has_timi_flow <- get_first_value_in_range(timi_flow,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_timi_flow) == 1 && !is.na(has_timi_flow$Value[1]) && has_timi_flow$Value[1] != "iii")
      cc_index = cc_index + 2
    
    has_glucose <- get_max_value_in_range(glucose,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(!is.na(has_glucose) && has_glucose > 191)
      cc_index = cc_index + 1
    
    has_lactate <- get_max_value_in_range(lactate,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(!is.na(has_lactate) && has_lactate > 5)
      cc_index = cc_index + 2
    
    has_creatinine <- get_max_value_in_range(creatinine,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(!is.na(has_creatinine) && has_creatinine > 1.5)
      cc_index = cc_index + 1
    
    if(is.na(cc_index))
      print(paste("NA Score for patient",patient_list[pat_i,]))
    
    list_values <- c(list_values,cc_index)
  }
  
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)  
}



get_iabp_shock_2_score <- function(name,patient_list,age_list,begin_date,end_date)
{
  
  list_values <- c()
  
  stroke <- import_value("admission","admission_hospital_medical_history_stroke")
  timi_flow <- import_value("procedures","procedures_cathleft_culprit_timi")
  glucose <- import_value("lab",248)
  lactate <- import_value("lab",249,"arterial")
  creatinine <- import_value("lab",c(6))

  
  for(pat_i in 1:nrow(patient_list))
  {
    #print(paste("Calculate patient",pat_i,patient_list[pat_i,]))
    cc_index <- 0
    age <- age_list[pat_i]

    if(is.na(begin_date[pat_i]) || is.na(end_date[pat_i]))
    {
      print(paste("No end date for pat_id",patient_list[pat_i,]))
      list_values <- c(list_values,NA)
      next
    }
    
    if(age > 73)
      cc_index = cc_index + 1
    
    has_stroke <- get_first_value_in_range(stroke,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_stroke) == 1 && !is.na(has_stroke$Value[1]) && has_stroke$Value[1] == "stroke")
      cc_index = cc_index + 2
    
    has_timi_flow <- get_first_value_in_range(timi_flow,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_timi_flow) == 1 && !is.na(has_timi_flow$Value[1]) && has_timi_flow$Value[1] != "iii")
      cc_index = cc_index + 2
    
    has_glucose <- get_max_value_in_range(glucose,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(!is.na(has_glucose) && has_glucose > 191)
      cc_index = cc_index + 1
    
    has_lactate <- get_max_value_in_range(lactate,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(!is.na(has_lactate) && has_lactate > 5)
      cc_index = cc_index + 2
    
    has_creatinine <- get_max_value_in_range(creatinine,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(!is.na(has_creatinine) && has_creatinine > 1.5)
      cc_index = cc_index + 1
    
    if(is.na(cc_index))
      print(paste("NA Score for patient",patient_list[pat_i,]))
    
    list_values <- c(list_values,cc_index)
  }
  
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)  
}




get_sirs_score_patient_list <- function(name,patient_list,age_list,begin_date,end_date)
{
  list_values <- c()
  
  
  rr_systolic <- import_value("vitalsigns",1)
  rr_diastolic <- import_value("vitalsigns",2)
  #paO2 <- import_value("lab",226)
  paO2 <- import_value("lab",226,c("arterial"))
  fiO2 <- import_value("ventilation",10)
  gcs_admission <- import_value("admission","admission_hospital_pe_gcs")
  bilirubine <- import_value("lab",c(26,251))
  platelets <- import_value("lab",c(62,173,300,418,533,534))
  creatinine <- import_value("lab",c(6))
  epinephrine <- import_value("medication",c(8))
  norepinephrine <- import_value("medication",c(9))
  dobutamine <- import_value("medication",c(37))
  
  icu_stays <- get_first_event_patient_list_in_range("ICUStay",import_event("cs_admission","admission_hospital"),patient_list,as.POSIXct(global_patient_data$InclusionDate)-3600*24 )
  
  
  for(pat_i in 1:nrow(patient_list))
  {
    print(paste("Calculate SOFA score for patient",pat_i,patient_list[pat_i,]))
    sofa_score <- 0    # 6 points for medical admission type 
    
    weight <- get_first_value_patient(import_value("vitalsigns",7),patient_list[pat_i,])
    
    systolicRR_min <- get_min_value_in_range(rr_systolic,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    systolicRR_max <- get_max_value_in_range(rr_systolic,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    diastolicRR_min <- get_min_value_in_range(rr_diastolic,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    diastolicRR_max <- get_max_value_in_range(rr_diastolic,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    map_min <- (diastolicRR_min*2 + systolicRR_min)/3
    dobutamine_max <- get_max_value_in_range(dobutamine,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])/60/weight
    epinephrine_max <- get_max_value_in_range(epinephrine,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])/60/weight
    norepinephrine_max <- get_max_value_in_range(norepinephrine,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])/60/weight
    
    if(is.na(dobutamine_max))
      dobutamine_max <- 0
    if(is.na(epinephrine_max))
      epinephrine_max <- 0
    if(is.na(norepinephrine_max))
      norepinephrine_max <- 0
    
    
    data_paO2 <- get_values_in_range(paO2,patient_list[pat_i,],as.POSIXct(begin_date[pat_i],optional=TRUE)-3600*24,as.POSIXct(end_date[pat_i],optional=TRUE)+3600*24)
    data_fiO2 <- get_values_in_range(fiO2,patient_list[pat_i,],as.POSIXct(begin_date[pat_i],optional=TRUE)-3600*24,as.POSIXct(end_date[pat_i],optional=TRUE)+3600*24)
    
    gcs_before_sedation <- get_first_value_patient(gcs_admission,patient_list[pat_i,])
    
    bilirubine_max <- get_max_value_in_range(bilirubine,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    
    platelets_min <- get_min_value_in_range(platelets,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    
    creatinine_max <- get_max_value_in_range(creatinine,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    
    if(is.na(weight))
    {
      print(paste("No weight for pat_id",patient_list[pat_i,]))
      list_values <- c(list_values,NA)
      next;
    }
    
    
    if(is.na(begin_date[pat_i]) || is.na(end_date[pat_i]))
    {
      print(paste("No end date for pat_id",patient_list[pat_i,]))
      list_values <- c(list_values,NA)
      next
    }
    
    
    horovitz_index_array <- c()
    if(nrow(data_paO2) > 0)
    {
      for(i in 1:nrow(data_paO2))
      {
        if(is.na(as.numeric(data_paO2$Value[i])) || as.numeric(data_paO2$Value[i]) <= 0 || as.numeric(data_paO2$Value[i]) >= 1000)
          next
        
        datetime_paO2 <- as.POSIXct(data_paO2$DateTime[i],format="%Y-%m-%d %H:%M:%S")
        last_value_datetime <- as.POSIXct("1900-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S")
        last_value_fiO2 <- NA
        
        if(nrow(data_fiO2) > 0)
        {
          for(j in 1:nrow(data_fiO2))
          {
            datetime_fiO2 <- as.POSIXct(data_fiO2$DateTime[j],format="%Y-%m-%d %H:%M:%S")
            
            if(!is.na(datetime_paO2) &&
               !is.na(datetime_fiO2) &&
               !is.na(data_fiO2$Value[j]) && 
               !is.na(as.numeric(data_fiO2$Value[j])) &&
               as.numeric(data_fiO2$Value[j]) > 0 &&
               as.numeric(data_fiO2$Value[j]) <= 100 &&
               datetime_paO2 >= datetime_fiO2 &&
               datetime_fiO2 > last_value_datetime)
            {
              last_value_datetime <- datetime_fiO2
              last_value_fiO2 <- as.numeric(data_fiO2$Value[j])
            }
          }
        }
        
        
        
        if(!is.na(last_value_fiO2) && !is.na(data_paO2$Value[i]) && last_value_fiO2 != 0 && 
           (last_value_datetime >= as.POSIXct(begin_date[pat_i],optional=TRUE) || datetime_paO2 >= as.POSIXct(begin_date[pat_i],optional=TRUE) ) &&
           (last_value_datetime <= as.POSIXct(end_date[pat_i],optional=TRUE) || datetime_paO2 <= as.POSIXct(end_date[pat_i],optional=TRUE) )
           
        )
        {
          #print(paste(as.numeric(data_paO2$Value[i]),as.numeric(last_value_fiO2),as.numeric(data_paO2$Value[i])/(as.numeric(last_value_fiO2)/100)))
          horovitz_index_array <- c(horovitz_index_array,as.numeric(data_paO2$Value[i])/(as.numeric(last_value_fiO2)/100))
        }
        
      }   
    }
    
    horovitz_index_min <- min(horovitz_index_array,na.rm=TRUE)
    
    if(is.na(horovitz_index_min) )
    {
      print("no horovitz index")
      sofa_score <- NA
    }
    else if(horovitz_index_min < 100)
    {
      sofa_score = sofa_score + 4
    }
    else if(horovitz_index_min < 200)
    {
      sofa_score = sofa_score + 3
    }
    else if(horovitz_index_min < 300)
    {
      sofa_score = sofa_score + 2
    }
    else if(horovitz_index_min < 400)
    {
      sofa_score = sofa_score + 1
    }
    
    #print(paste(horovitz_index_min,gcs_before_sedation,map_min,dobutamine_max,epinephrine_max,norepinephrine_max,bilirubine_max,platelets_min,creatinine_max))
    
    if(is.na(gcs_before_sedation))
    {
      sofa_score <- NA
      print("no gcs before sedation")
    }
    else if(gcs_before_sedation < 6)
    {
      sofa_score = sofa_score + 4
    }
    else if(gcs_before_sedation  < 10)
    {
      sofa_score = sofa_score + 3
    }
    else if(gcs_before_sedation < 13)
    {
      sofa_score = sofa_score + 2
    }
    else if(gcs_before_sedation < 15)
    {
      sofa_score = sofa_score + 1
    }
    
    
    
    if(is.na(map_min))
    {
      sofa_Score <- NA
      print("no RR")
    }
    else if(dobutamine_max > 0.015 | epinephrine_max > 0.0001 | norepinephrine_max > 0.0001)
    {
      sofa_score = sofa_score + 4
    }
    else if(dobutamine_max > 0.005 | epinephrine_max > 0 | norepinephrine_max > 0)
    {
      sofa_score = sofa_score + 3
    }
    else if(dobutamine_max > 0)
    {
      sofa_score = sofa_score + 2
    }
    else if(map_min < 70)
    {
      sofa_score = sofa_score + 1
    }  
    
    
    
    if(is.na(bilirubine_max))
    {
      sofa_score<- NA
      print("no bilirubine")
    }
    else if(bilirubine_max > 12)
    {
      sofa_score = sofa_score + 4
    }
    else if(bilirubine_max >= 6)
    {
      sofa_score = sofa_score + 3
    }
    else if(bilirubine_max >= 2)
    {
      sofa_score = sofa_score + 2
    }
    else if(bilirubine_max >= 1.2)
    {
      sofa_score = sofa_score + 1
    }
    
    
    if(is.na(platelets_min))
    {
      sofa_score<- NA
      print("no platelets")
    }
    else if(platelets_min < 20)
    {
      sofa_score = sofa_score + 4
    }
    else if(platelets_min < 50)
    {
      sofa_score = sofa_score + 3
    }
    else if(platelets_min < 100)
    {
      sofa_score = sofa_score + 2
    }
    else if(platelets_min < 150)
    {
      sofa_score = sofa_score + 1
    }
    
    if(is.na(creatinine_max))
    {
      sofa_score<- NA
      print("no creatinine")
    }
    else if(creatinine_max > 5)
    {
      sofa_score = sofa_score + 4
    }
    else if(creatinine_max >= 3.5)
    {
      sofa_score = sofa_score + 3
    }
    else if(creatinine_max >= 2)
    {
      sofa_score = sofa_score + 2
    }
    else if(creatinine_max >= 1.2)
    {
      sofa_score = sofa_score + 1
    }
    
    
    if(is.infinite(sofa_score))
      sofa_score <- NA
    
    if(is.na(sofa_score))
      print(paste("NA sofa Score for patient",patient_list[pat_i,]))
    
    list_values <- c(list_values,sofa_score)
  }
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)  
}



get_missing_saps2_score_patient_list <- function(name,patient_list,age_list,begin_date,end_date)
{
  list_values <- c()
  
  df_missing <- data.frame(ID=c(0),Type=c(0))
  
  
  heartrate <- import_value("vitalsigns",3)
  rr_systolic <- import_value("vitalsigns",1)
  temperature <- import_value("vitalsigns",6)
  #paO2 <- import_value("lab",226)
  paO2 <- import_value("lab",226,c("arterial"))
  fiO2 <- import_value("ventilation",10)
  gcs_admission <- import_value("admission","admission_hospital_pe_gcs")
  urea <- import_value("lab",5)
  urin_output <- import_value("balances",c())
  icu_stays <- get_first_event_patient_list_in_range("ICUStay",import_event("cs_admission","admission_hospital"),patient_list,as.POSIXct(global_patient_data$InclusionDate)-3600*24 )
  sodium <- import_value("lab",243)
  potassium <- import_value("lab",244)
  bicarbonate <- import_value("lab",c(228,229))
  bilirubine <- import_value("lab",c(26,251))
  leukocytes <- import_value("lab",c(202))
  metastatic_cancer <- import_value("admission","admission_hospital_medical_history_metastatic")
  hematologic_malignancy <- import_value("admission","admission_hospital_medical_history_malignant_hemat")
  aids <- import_value("admission","admission_hospital_medical_history_aids")
  
  
  for(pat_i in 1:nrow(patient_list))
  {
    print(paste("Calculate patient",pat_i,patient_list[pat_i,]))
    saps2_score <- 6    # 6 points for medical admission type 
    age <- age_list[pat_i]
    heartrate_min <- get_min_value_in_range(heartrate,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    heartrate_max <- get_max_value_in_range(heartrate,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    systolicRR_min <- get_min_value_in_range(rr_systolic,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    systolicRR_max <- get_max_value_in_range(rr_systolic,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    temperature_max <- get_max_value_in_range(temperature,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    data_paO2 <- get_values_in_range(paO2,patient_list[pat_i,],as.POSIXct(begin_date[pat_i],optional=TRUE)-3600*24,as.POSIXct(end_date[pat_i],optional=TRUE)+3600*24)
    data_fiO2 <- get_values_in_range(fiO2,patient_list[pat_i,],as.POSIXct(begin_date[pat_i],optional=TRUE)-3600*24,as.POSIXct(end_date[pat_i],optional=TRUE)+3600*24)
    gcs_before_sedation <- get_first_value_patient(gcs_admission,patient_list[pat_i,])
    urea_max <- get_max_value_in_range(urea,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    
    if(is.na(begin_date[pat_i]) || is.na(end_date[pat_i]))
    {
      df_missing[nrow(df_missing)+1,] = list(patient_list[pat_i,],"No end date for stay")
      list_values <- c(list_values,NA)
      next
    }
    
    urin_output_day <- get_values_in_range(urin_output,patient_list[pat_i,],paste(as.Date(begin_date[pat_i],format="%Y-%m-%d"),"00:00:00"),paste(as.Date(end_date[pat_i],format="%Y-%m-%d"),"23:59:59"))
    if(nrow(urin_output_day) == 0)
      urin_output_day <- NA
    else if(nrow(urin_output_day) >= 1)
    {
      time_begin <- as.POSIXct(begin_date[pat_i])
      time_end <- as.POSIXct(paste(as.Date(begin_date[pat_i],format="%Y-%m-%d"),"23:59:59"))
      if(as.POSIXct(icu_stays$ICUStay_end[pat_i]) < time_end)
        time_end <- as.POSIXct(as.POSIXct(icu_stays$ICUStay_end[pat_i]))
      
      urin_output_day <- urin_output_day$Output[1]*(24/as.numeric(difftime(time_end,time_begin,units="hours")))
    }
    
    sodium_min <- get_min_value_in_range(sodium,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    sodium_max <- get_max_value_in_range(sodium,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    potassium_min <- get_min_value_in_range(potassium,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    potassium_max <- get_max_value_in_range(potassium,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])  
    sbc_min <- get_min_value_in_range(bicarbonate,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    bilirubine_max <- get_max_value_in_range(bilirubine,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    leukocytes_min <- get_min_value_in_range(leukocytes,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    leukocytes_max <- get_max_value_in_range(leukocytes,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])  
    
    has_metastatic_cancer <- get_first_value_in_range(metastatic_cancer,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_metastatic_cancer) == 1 && !is.na(has_metastatic_cancer$Value[1]) && has_metastatic_cancer$Value[1] == "yes")
      saps2_score <- saps2_score + 9
    
    has_hematologic_malignancy <- get_first_value_in_range(hematologic_malignancy,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_hematologic_malignancy) == 1 && !is.na(has_hematologic_malignancy$Value[1]) && has_hematologic_malignancy$Value[1] == "yes")
      saps2_score <- saps2_score + 10
    
    has_aids <- get_first_value_in_range(aids,patient_list[pat_i,],begin_date[pat_i],end_date[pat_i])
    if(nrow(has_aids) == 1 && !is.na(has_aids$Value[1]) && has_aids$Value[1] == "yes")
      saps2_score <- saps2_score + 17
    
    
    if(age >= 40 && age < 60)
    {
      saps2_score = saps2_score + 7
    }
    else if(age >= 60 && age < 70)
    {
      saps2_score = saps2_score + 12
    }
    else if(age >= 70 && age < 75)
    {
      saps2_score = saps2_score + 15
    }
    else if(age >= 75 && age < 80)
    {
      saps2_score = saps2_score + 16
    }
    else if(age >= 80)
    {
      saps2_score = saps2_score + 18
    }
    
    
    if(is.na(heartrate_min) || is.na(heartrate_max))
    {
      saps2_score <- NA
      df_missing[nrow(df_missing)+1,] = list(patient_list[pat_i,],"No heart rate documented")
      print("no heartrate")
    }
    else if(heartrate_min < 40 && heartrate_min != 0)
    {
      saps2_score = saps2_score + 11
    }
    else if(heartrate_max >= 160)
    {
      saps2_score = saps2_score + 7
    }
    else if(heartrate_max >= 120 && heartrate_max < 160)
    {
      saps2_score = saps2_score + 4
    }
    else if(heartrate_min >= 40 && heartrate_min < 70)
    {
      saps2_score = saps2_score + 2
    }
    
    
    if(is.na(systolicRR_min) || is.na(systolicRR_max))
    {
      saps2_score <- NA
      df_missing[nrow(df_missing)+1,] = list(patient_list[pat_i,],"No RR documented")
    }
    else if(systolicRR_min < 70 && systolicRR_min != 0)
    {
      saps2_score = saps2_score + 13
    }
    else if(systolicRR_min >= 70 && systolicRR_min < 100)
    {
      saps2_score = saps2_score + 5
    }
    else if(systolicRR_max >= 200)
    {
      saps2_score = saps2_score + 2
    }
    
    
    if(is.na(temperature_max))
    {
      saps2_score <- NA
      df_missing[nrow(df_missing)+1,] = list(patient_list[pat_i,],"No temperature documented")
    }
    else if(temperature_max >= 39)
    {
      saps2_score = saps2_score + 3
    }
    
    
    
    if(is.na(gcs_before_sedation))
    {
      saps2_score <- NA
      df_missing[nrow(df_missing)+1,] = list(patient_list[pat_i,],"No GCS documented")
    }
    else if(gcs_before_sedation < 6)
    {
      saps2_score = saps2_score + 26
    }
    else if(gcs_before_sedation >= 6 && gcs_before_sedation <= 8)
    {
      saps2_score = saps2_score + 13
    }
    else if(gcs_before_sedation >= 9 && gcs_before_sedation <= 10)
    {
      saps2_score = saps2_score + 7
    }
    else if(gcs_before_sedation >= 11 && gcs_before_sedation <= 13)
    {
      saps2_score = saps2_score + 5
    }
    
    
    horovitz_index_array <- c()
    if(nrow(data_paO2) > 0)
    {
      for(i in 1:nrow(data_paO2))
      {
        if(is.na(as.numeric(data_paO2$Value[i])) || as.numeric(data_paO2$Value[i]) <= 0 || as.numeric(data_paO2$Value[i]) >= 1000)
          next
        
        datetime_paO2 <- as.POSIXct(data_paO2$DateTime[i],format="%Y-%m-%d %H:%M:%S")
        last_value_datetime <- as.POSIXct("1900-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S")
        last_value_fiO2 <- NA
        
        if(nrow(data_fiO2) > 0)
        {
          for(j in 1:nrow(data_fiO2))
          {
            datetime_fiO2 <- as.POSIXct(data_fiO2$DateTime[j],format="%Y-%m-%d %H:%M:%S")
            
            if(!is.na(datetime_paO2) &&
               !is.na(datetime_fiO2) &&
               !is.na(data_fiO2$Value[j]) && 
               !is.na(as.numeric(data_fiO2$Value[j])) &&
               as.numeric(data_fiO2$Value[j]) > 0 &&
               as.numeric(data_fiO2$Value[j]) <= 100 &&
               datetime_paO2 >= datetime_fiO2 &&
               datetime_fiO2 > last_value_datetime)
            {
              last_value_datetime <- datetime_fiO2
              last_value_fiO2 <- as.numeric(data_fiO2$Value[j])
            }
          }
        }
        
        
        
        if(!is.na(last_value_fiO2) && !is.na(data_paO2$Value[i]) && last_value_fiO2 != 0 && 
           (last_value_datetime >= as.POSIXct(begin_date[pat_i],optional=TRUE) || datetime_paO2 >= as.POSIXct(begin_date[pat_i],optional=TRUE) ) &&
           (last_value_datetime <= as.POSIXct(end_date[pat_i],optional=TRUE) || datetime_paO2 <= as.POSIXct(end_date[pat_i],optional=TRUE) )
           
        )
        {
          #print(paste(as.numeric(data_paO2$Value[i]),as.numeric(last_value_fiO2),as.numeric(data_paO2$Value[i])/(as.numeric(last_value_fiO2)/100)))
          horovitz_index_array <- c(horovitz_index_array,as.numeric(data_paO2$Value[i])/(as.numeric(last_value_fiO2)/100))
        }
        
      }   
    }
    
    horovitz_index_min <- min(horovitz_index_array,na.rm=TRUE)
    
    if(is.na(horovitz_index_min) )
    {
      print("no horovitz index")
      df_missing[nrow(df_missing)+1,] = list(patient_list[pat_i,],"No paO2 and/or fiO2 documented")
      
      saps2_score <- NA
    }
    else if(horovitz_index_min < 100)
    {
      saps2_score = saps2_score + 11
    }
    else if(horovitz_index_min >= 100 && horovitz_index_min < 200)
    {
      saps2_score = saps2_score + 9
    }
    else if(horovitz_index_min > 200)
    {
      saps2_score = saps2_score + 6
    }
    
    
    if(is.na(urea_max))
    {
      saps2_score <- NA
      print("no urea")
      df_missing[nrow(df_missing)+1,] = list(patient_list[pat_i,],"No urea documented")
    }
    else if(urea_max >= 84)
    {
      saps2_score = saps2_score + 10
    }
    else if(urea_max >= 28)
    {
      saps2_score = saps2_score + 6
    }
    
    
    if(is.na(urin_output_day))
    {
      saps2_score <- NA
      df_missing[nrow(df_missing)+1,] = list(patient_list[pat_i,],"No urine output documented")
      print("no urin output")
    }
    else if(urin_output_day < 500)
    {
      saps2_score = saps2_score + 11
    }
    else if(urin_output_day >= 500 && urin_output_day < 1000)
    {
      saps2_score = saps2_score + 4
    }
    
    
    if(is.na(sodium_max))
    {
      saps2_score <- NA
      df_missing[nrow(df_missing)+1,] = list(patient_list[pat_i,],"No sodium documented")
      print("no sodium")
    }
    else if(sodium_min < 125)
    {
      saps2_score = saps2_score + 5
    }
    else if(sodium_max >= 145)
    {
      saps2_score = saps2_score + 1
    }
    
    
    if(is.na(potassium_max))
    {
      saps2_score <- NA
      df_missing[nrow(df_missing)+1,] = list(patient_list[pat_i,],"No potassium documented")
      print("no potassium")
    }
    else if(potassium_min < 3)
    {
      saps2_score = saps2_score + 3
    }
    else if(potassium_max >= 5)
    {
      saps2_score = saps2_score + 3
    }
    
    
    if(is.na(sbc_min))
    {
      saps2_score <- NA
      df_missing[nrow(df_missing)+1,] = list(patient_list[pat_i,],"No bicarbonate documented")
      print("no sbc")
    }
    else if(sbc_min < 15)
    {
      saps2_score = saps2_score + 6
    }
    else if(sbc_min >= 15 && sbc_min < 20)
    {
      saps2_score = saps2_score + 3
    }
    
    
    
    if(is.na(bilirubine_max))
    {
      saps2_score <- NA
      df_missing[nrow(df_missing)+1,] = list(patient_list[pat_i,],"No bilirubine documented")
      print("no bilirubine")
    }
    else if(bilirubine_max >= 6)
    {
      saps2_score = saps2_score + 9
    }
    else if(bilirubine_max >= 4 && bilirubine_max < 6)
    {
      saps2_score = saps2_score + 4
    }
    
    
    if(is.na(leukocytes_max))
    {
      print("no leukocytes")
      df_missing[nrow(df_missing)+1,] = list(patient_list[pat_i,],"No leukocytes documented")
      saps2_score <- NA
    }
    else if(leukocytes_min < 1)
    {
      saps2_score = saps2_score + 12
    }
    else if(leukocytes_max >= 20)
    {
      saps2_score = saps2_score + 3
    }
    
    
    if(is.infinite(saps2_score))
      saps2_score <- NA
    
    if(is.na(saps2_score))
      print(paste("NA Score for patient",patient_list[pat_i,]))
    
    list_values <- c(list_values,saps2_score)
  }
  
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df_missing)  
}


has_event_in_range_patient_list <- function(name,vector_events,patient_list,begin_time,end_time)
{
  list_event_duration <- c()
  for(i in 1:nrow(patient_list))
  {
    
    my_events <- vector_events[is.element(vector_events$PatientID,patient_list[i,]),]
    ret_pat <- FALSE
    
    patient_begin <- as.POSIXct(begin_time[i],optional=TRUE)
    patient_end <- as.POSIXct(end_time[i],optional=TRUE)
    
    
    if(nrow(my_events) > 0 && !is.na(patient_begin) && ! is.na(patient_end))
    {
      for(j in 1:nrow(my_events))
      {
        date1 <- as.POSIXct(my_events$Date1[j], format="%Y-%m-%d %H:%M:%S")

        if(is.na(date1))
          next

        if(date1 >=patient_begin && date1 <= patient_end)
          ret_pat <- TRUE
      }
    }
    list_event_duration <- c(list_event_duration,ret_pat)
  }
  
  df <- data.frame(ID = patient_list, placeholder_name = as.numeric(list_event_duration ))
  names(df)[names(df) == "placeholder_name"] <- name
  
  return(df)
}


get_individ_range_patient_list <- function(name,vector_values,patient_list,begin_name,end_name,begin_time,end_time) 
{
  names(vector_values)[names(vector_values) == begin_name] <- "Date1"
  names(vector_values)[names(vector_values) == end_name] <- "Date2"
  
  return(get_event_duration_in_range_patient_list(name,vector_values,patient_list,begin_time,end_time))
}

is_invasive_ventilation <- function(value)
{
  if(value == "CPAP" || value == "BIPAP" || value == "MMV" || value == "SIMV" || value == "IPPV" || value == "manually")
    return(TRUE)
  else
    return(FALSE)
}


get_ventilation_duration_from_types_patient_list <- function(name,vector_values,patient_list,begin_time,end_time)
{
  list_event_duration <- c()
  for(i in 1:nrow(patient_list))
  {
    duration <- 0
    patient_begin <- as.POSIXct(begin_time[i],optional=TRUE)
    patient_end <- as.POSIXct(end_time[i],optional=TRUE)
    

    my_events <- vector_values[is.element(vector_values$PatientID,patient_list[i,]),]
    my_events <- my_events[order(my_events$DateTime),]
    
    if(is.na(patient_begin) || is.na(patient_end))
    {
      
    }
    else if(nrow(my_events) == 1)
    {
      my_datetime <- as.POSIXct(my_events$DateTime[1])
      if(my_datetime <= patient_begin)
        my_events$DateTime[1] <- patient_begin

      if(my_datetime <= patient_end && is_invasive_ventilation(my_events$Value[1]))
        duration <- duration + difftime(patient_end,my_datetime,units="hours")
    }
    
    else if(nrow(my_events) > 1)
    {
      last_datetime <- as.POSIXct(my_events$DateTime[1])
      last_value <- my_events$Value[1]
      for(j in 2:nrow(my_events))
      {
        datetime <- as.POSIXct(my_events$DateTime[j])
        
        if(datetime >= patient_begin && last_datetime <= patient_end)
        {
          if(last_datetime  < patient_begin)
            last_datetime <- patient_begin
          if(datetime > patient_end)
            datetime <- patient_end
          
          if(is_invasive_ventilation(last_value))
            duration <- duration + difftime(datetime,last_datetime,units="hours")
        }
        else if(j == nrow(my_events) && datetime < patient_begin)
        {
          if(is_invasive_ventilation(my_events$Value[j]))
            duration <- duration + difftime(patient_end,patient_begin,units="hours")
        }
        
        last_datetime <- as.POSIXct(my_events$DateTime[j])
        last_value <- my_events$Value[j]
      }
    }
    list_event_duration <- c(list_event_duration,duration)
  }
  
  df <- data.frame(ID = patient_list, placeholder_name = as.numeric(list_event_duration ))
  names(df)[names(df) == "placeholder_name"] <- name
  
  return(df)
}




get_medication_info_patient_list <- function(name,vector_values,vector_values_bolus,patient_list,begin_time,end_time,count_zero=FALSE,death_date=NA)
{
  list_event_duration <- c()
  list_event_dosage <- c()
  list_event_full_dosage <- c()
  
  for(i in 1:nrow(patient_list))
  {

    mean <- NA
    
    duration <- 0
    complete_dose <- 0
    weighted_mean_dosage <- c()
    weighted_mean_time <- c()
    patient_begin <- as.POSIXct(begin_time[i],optional=TRUE)
    patient_end <- as.POSIXct(end_time[i],optional=TRUE)
    
    
    my_events <- vector_values[is.element(vector_values$PatientID,patient_list[i,]),]
    my_events <- my_events[order(my_events$DateTime),]
    

   # if(i > 30)
  #  {
  #    list_event_duration <- c(list_event_duration,duration)
   #   list_event_dosage <- c(list_event_dosage, NA)
    #  next
    #}
    
    #print(my_events)

    
    if(!is.na(patient_end) && !is.na(death_date[i]) && as.POSIXct(death_date[i]) < patient_end)
    {
      patient_end <- as.POSIXct(death_date[i])
    }
    
    if(is.na(patient_begin) || is.na(patient_end) || (!is.na(death_date[i]) && as.POSIXct(death_date[i]) <= patient_begin))
    {
    }
    else if(nrow(my_events) == 1)
    {
      my_datetime <- as.POSIXct(my_events$DateTime[1],optional=TRUE)
      if(my_datetime <= patient_begin)
        my_events$DateTime[1] <- patient_begin
      
      if(my_datetime <= patient_end)
      {
        
        if( difftime(patient_end,my_datetime,units="hours") > 0 && !is.na(my_events$Value[1]) && my_events$Value[1] != 0)
        {
          duration <- duration + difftime(patient_end,my_datetime,units="hours")
          weighted_mean_time <- c(difftime(patient_end,my_datetime,units="hours"))
          weighted_mean_dosage <- c(my_events$Value[1])     
        }
      }
    }
    
    else if(nrow(my_events) > 1)
    {
      last_datetime <- as.POSIXct(my_events$DateTime[1],optional=TRUE)
      last_value <- as.numeric(my_events$Value[1])
      for(j in 2:nrow(my_events))
      {
        datetime <- as.POSIXct(my_events$DateTime[j],optional=TRUE)

        if(!is.na(datetime) && !is.na(last_datetime) && datetime >= patient_begin && last_datetime <= patient_end)
        {
          if(last_datetime  < patient_begin)
            last_datetime <- patient_begin
          if(datetime > patient_end)
            datetime <- patient_end
          
          
          if( difftime(datetime,last_datetime,units="hours") > 0 && !is.na(last_value) && last_value != 0)
          {
            duration <- duration + difftime(datetime,last_datetime,units="hours")
           # print(paste("MO",difftime(datetime,last_datetime,units="hours"), last_value))
            weighted_mean_time <- c(weighted_mean_time, difftime(datetime,last_datetime,units="hours") )
            weighted_mean_dosage <- c(weighted_mean_dosage, last_value) 
          }

        }
        else if(!is.na(datetime) && !is.na(last_datetime) && j == nrow(my_events) && datetime < patient_begin)
        {
          if( difftime(datetime,last_datetime,units="hours") > 0 && !is.na(my_events$Value[j]) && my_events$Value[j] != 0)
          {
           # print(paste("MO",difftime(patient_end,patient_begin,units="hours") , my_events$Value[j]))

            duration <- duration + difftime(patient_end,patient_begin,units="hours")
            weighted_mean_time <- c(weighted_mean_time, difftime(patient_end,patient_begin,units="hours") )
            weighted_mean_dosage <- c(weighted_mean_dosage, my_events$Value[j])
          }
        }
        
        last_datetime <- as.POSIXct(my_events$DateTime[j],optional=TRUE)
        last_value <- my_events$Value[j]
      }
    }

    list_event_duration <- c(list_event_duration,duration)

    if(length(weighted_mean_dosage) > 0)
    {
      for(j in 1:length(weighted_mean_dosage))
      {
        complete_dose <- complete_dose + weighted_mean_dosage[j] * weighted_mean_time[j]
      }
      
    }


    
    
    #if(length(list_event_dosage) > 0)
    #{
      bolus_values <- sum(get_values_in_range(vector_values_bolus,patient_list[i,],patient_begin,patient_end)$Value,na.rm=TRUE)
      complete_dose <- complete_dose + bolus_values
      
      if(count_zero && !is.na(patient_end) && !is.na(patient_begin) && difftime(patient_end,patient_begin,units="hours") > duration)
      {
        bolus_values_add <- bolus_values / as.numeric(difftime(patient_end,patient_begin,units="hours"))
        missing_zero <- as.numeric(difftime(patient_end,patient_begin,units="hours")) - duration
        mean <- weighted.mean(c(weighted_mean_dosage,0),c(as.numeric(weighted_mean_time),missing_zero),na.rm=TRUE) + bolus_values_add
      }
      else
      {
        bolus_values_add <- bolus_values / as.numeric(duration)
        
        if(is.nan(bolus_values_add))
          bolus_values_add <- 0
        
        mean <- weighted.mean(weighted_mean_dosage,as.numeric(weighted_mean_time),na.rm=TRUE) + bolus_values_add
      }
      
      if(is.nan(mean))
        list_event_dosage <- c(list_event_dosage, 0)
      else
        list_event_dosage <- c(list_event_dosage, mean )
      
      list_event_full_dosage <- c(list_event_full_dosage, complete_dose)
      
    #}
    #else
    #  list_event_dosage <- c(list_event_dosage, NA)

    
  }
  
  df <- data.frame(ID = patient_list, placeholder_name = as.numeric(list_event_duration ), placeholder_name_2 = as.numeric(list_event_dosage), placeholder_name_3 = list_event_full_dosage)
  names(df)[names(df) == "placeholder_name"] <- paste(name,"_duration",sep="")
  names(df)[names(df) == "placeholder_name_2"] <- paste(name,"_weighted_mean",sep="")
  names(df)[names(df) == "placeholder_name_3"] <- paste(name,"_full_dose",sep="")
  
  return(df)
}





get_pcis <- function(name,patient_list,begin_time,end_time)
{
  list_pci_dates <- c()
  
  for(i in 1:nrow(patient_list))
  {
    pci_dates <- NA
    if(!is.na(begin_time[i]) && !is.na(end_time[i]))
    {
      sql <- paste("SELECT cs_df_data_procedures.PatientID ,Value, cs_df_pages.Date AS Date FROM cs_df_data_procedures 
                LEFT JOIN cs_df_pages ON Page = cs_df_pages.ID

                WHERE Type LIKE '%stent%' 
               AND Value != 0
               AND (cs_df_pages.Date >= '",as.POSIXct(begin_time[i],format="%Y-%m-%d %H:%M:%S") ,"'
                    AND cs_df_pages.Date <= '",as.POSIXct(end_time[i],format="%Y-%m-%d %H:%M:%S") ,"')
               AND cs_df_data_procedures.PatientID = ",patient_list[i,],sep="")
      rs = dbSendQuery(mydb,sql)
      values <- fetch(rs,n=-1)
      
      if(nrow(values))
        pci_dates<- paste(values$Date,collapse="|")
    }
    list_pci_dates <- c(list_pci_dates, pci_dates)
  }
  
  df <- data.frame(ID = patient_list, placeholder_name = list_pci_dates)
  names(df)[names(df) == "placeholder_name"] <- paste(name,sep="")
  
  return(df)
}

has_medication_dose <- function(name,vector_values,patient_list,begin_time,end_time,target_dose)
{
  list_results <- c()
  
  for(i in 1:nrow(patient_list))
  {
    max_dose <- get_max_value_in_range(vector_values,patient_list[i,],begin_time[i],end_time[i])
    list_results <- c(list_results,!is.na(max_dose) & (max_dose >= target_dose))
  }
  df <- data.frame(ID = patient_list, placeholder_name = list_results)
  names(df)[names(df) == "placeholder_name"] <- paste(name,sep="")
  return(df)
}

custom_merge <- function(x,y,by=by)
{
  for(i in 2:length(colnames(y)))
  {
    col_name <- colnames(y)[i]
    if(is.element(col_name, colnames(x)))
    {
      x[col_name] <- NULL
    }
  }
  if(missing(by))
    by <- "ID"
  return(merge(x,y,by=by))
}

custom_merge_new_column <- function(x, y, by = "ID") {
  for (i in 2:length(colnames(y))) {
    col_name <- colnames(y)[i]
    if (is.element(col_name, colnames(x))) {
      x[col_name] <- NULL
    }
  }
  if (missing(by))
    by <- "ID"
  return(merge(x, y, by = by))
}



get_values_array <- function(type, list_names, list_ids)
{
  if(class(list_ids) == "logical" || is.na(list_ids) || length(list_ids) == 0 || length(list_ids) != length(list_names))
    stop("Wrong format for get_values_array")
  
  add_values <- NA
  for(i in 1:length(list_names))
  {
    new_values <-import_value(type,list_ids[i])
    
    if(nrow(new_values) > 0)
    {
      new_values$Name = list_names[i]
      new_values$ID = list_ids[i]
      new_values$Type = type
      
      if(class(add_values) == "logical" && is.na(add_values))
        add_values <- new_values
      else
        add_values <- merge(add_values,new_values,  all=TRUE)   
    }
  }
  return(add_values)
}

#checkt die Dimensionen eine df
check_dimensions <- function(df) {
  dimensions <- dim(df)
  num_rows <- dimensions[1]
  num_cols <- dimensions[2]
  nan_percentage <- colMeans(is.na(df)) * 100
  col_names <- colnames(df)
  #cat("Column Names: ", paste(col_names, collapse = ", "), "\n")
  cat("NaN Percentage: ")
  for (i in 1:num_cols) {
    cat(paste(col_names[i], ": ", nan_percentage[i], "%", sep = " "))
    if (i < num_cols) {
      cat(", ")
    }
  }
  cat("\n")
  cat("Number of Rows: ", num_rows, "\n")
  cat("Number of Columns: ", num_cols, "\n")
}
    



check_in_range <- function(column_name, data, patient_list, begin_date, end_date) {
  values_list <- c()
  
  for (i in 1:nrow(patient_list)) {  
    patient_id <- patient_list[i, "ID"]
    begin <- as.POSIXct(begin_date[i], format = "%Y-%m-%d %H:%M:%S")
    end <- as.POSIXct(end_date[i], format = "%Y-%m-%d %H:%M:%S")
    
    patient_subset <- data[data$PatientID == patient_id, ]
    list_values <- c()
    
    if (nrow(patient_subset) > 0) {
      for (ii in 1:nrow(patient_subset)) {  
        # Pr?fen, ob es ?berlappungen der Zeitintervalle gibt
        overlapping_intervals <- sum((begin <= patient_subset[ii, "Date1"]) & (end >= patient_subset[ii, "Date2"]), na.rm = TRUE)
        
        # ?berpr?fen, ob overlapping_intervals nicht NA ist
        if (!is.na(overlapping_intervals) && overlapping_intervals > 0) {
          list_values <- c(list_values, patient_subset[ii, "Value"])
        } else {
          list_values <- c(list_values, NA)
        }
      }
      gesamtsumme <- paste(list_values, collapse = ", ")
      values_list <- c(values_list, gesamtsumme)
    } else {
      # F?ge ein NA f?r den Patienten hinzu, wenn es keine Daten f?r ihn gibt
      values_list <- c(values_list, NA)
    }
  }
  
  # Dataframe erstellen
  df <- data.frame(ID = patient_list$ID, placeholder_name = values_list)
  names(df)[names(df) == "placeholder_name"] <- column_name
  
  return(df)
}



get_first_ecg <- function(data, patient_list) {
  sum_dataset <- data.frame()  # Erstellen Sie einen leeren Datenrahmen für die Summe
  
  for (i in 1:nrow(patient_list)) {  
    patient_id <- patient_list[i, "ID"]
    patient_subset <- data[data$PatientID == patient_id, ]
    
    if (nrow(patient_subset) > 0) {
      patient_subset <- patient_subset[order(patient_subset$Date1), ]
      new_data <- slice(patient_subset, 1)
    } else {
      new_data <- NA
    }
    sum_dataset <- rbind(sum_dataset, new_data)
  }
  
  return(sum_dataset)
}

get_last_ecg <- function(data, patient_list) {
  sum_dataset <- data.frame()  # Erstellen Sie einen leeren Datenrahmen für die Summe
  
  for (i in 1:nrow(patient_list)) {  
    patient_id <- patient_list[i, "ID"]
    patient_subset <- data[data$PatientID == patient_id, ]
    
    if (nrow(patient_subset) > 0) {
      patient_subset <- patient_subset[order(patient_subset$Date1), ]
      new_data <- tail(patient_subset, 1)
    } else {
      new_data <- NA
    }
    sum_dataset <- rbind(sum_dataset, new_data)
  }
  
  return(sum_dataset)
}



check_in_range_medi <- function(column_name, data, patient_list, begin_date, end_date) {
  values_list <- c()
  
  for (i in 1:nrow(patient_list)) {  
    patient_id <- patient_list[i, "ID"]
    begin <- as.POSIXct(begin_date[i], format = "%Y-%m-%d %H:%M:%S")
    end <- as.POSIXct(end_date[i], format = "%Y-%m-%d %H:%M:%S")
    
    patient_subset <- data[data$PatientID == patient_id, ]
    list_values <- c()
    
    if (nrow(patient_subset) > 0) {
      for (ii in 1:nrow(patient_subset)) {  
        # Pr?fen, ob es ?berlappungen der Zeitintervalle gibt
        overlapping_intervals <- sum((begin <= patient_subset[ii, "DateTimeEnd"]) & (end >= patient_subset[ii, "DateTime"]), na.rm = TRUE)
        
        # ?berpr?fen, ob overlapping_intervals nicht NA ist
        if (!is.na(overlapping_intervals) && overlapping_intervals > 0) {
          list_values <- c(list_values, patient_subset[ii, "Value"])
        } else {
          list_values <- c(list_values, NA)
        }
      }
      gesamtsumme <- sum(list_values, na.rm = TRUE)
      values_list <- c(values_list, gesamtsumme)
    } else {
      # F?ge ein NA f?r den Patienten hinzu, wenn es keine Daten f?r ihn gibt
      values_list <- c(values_list, NA)
    }
  }
  
  # Dataframe erstellen
  df <- data.frame(ID = patient_list$ID, placeholder_name = values_list)
  names(df)[names(df) == "placeholder_name"] <- column_name
  
  return(df)
}



get_last_value_in_range_patient_list_when_2_Values <- function(name,vector_data,patient_list,begin_date,end_date,type="")
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    values <- get_values_in_range(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))
    
    if(nrow(values) > 1)
      list_values <- c(list_values,tail(values$Value,n=1))
    else
      list_values <- c(list_values,NA)
  }
  
  if(type == "numeric")
    list_values <- as.numeric( gsub("[^0-9.-]","",list_values,perl=T))
  else if(type == "factor")
    list_values <- factor(list_values) 
  
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}



get_weighted_mean_values_in_range_ohneAusreisser <- function(vector_values, patient_id, begin_time, end_time, set_missing_to_zero = FALSE) {
  
  begin_timespan <- as.POSIXct(begin_time)
  end_timespan <- as.POSIXct(end_time)
  
  data <- get_values_in_range(vector_values, patient_id, begin_timespan - scanning_range_secs, end_timespan + scanning_range_secs)
  data <- data[order(as.POSIXct(data$DateTime, format = "%Y-%m-%d %H:%M:%S")), ]
  
  first_value <- TRUE
  values <- c()
  weights <- c()
  
  if(nrow(data) > 0) {
    for(k in 1:nrow(data)) {
      value_begin <- as.POSIXct(data$DateTime[k], format = "%Y-%m-%d %H:%M:%S")
      
      if(is.na(value_begin) || is.na(begin_timespan) || is.na(end_timespan) || (value_begin < begin_timespan || value_begin > end_timespan))
        next
      
      if(first_value == TRUE) {
        first_value <- FALSE
        if(k > 1) {
          values <- c(values, as.numeric(gsub("[^0-9.,-]", "", data$Value[k - 1])))
          weights <- c(weights, difftime(value_begin, begin_timespan, units = c("hours")))
        }
      }
      
      value_end <- end_timespan
      if(k < nrow(data))
        value_end <- as.POSIXct(data$DateTime[k + 1], format = "%Y-%m-%d %H:%M:%S")
      if(value_end > end_timespan)
        value_end <- end_timespan
      
      value_difftime <- difftime(value_end, value_begin, units = c("hours"))
      
      values <- c(values, as.numeric(gsub("[^0-9.,-]", "", data$Value[k])))
      weights <- c(weights, value_difftime)
    }
  }
  
  # Remove outliers
  median_value <- median(values, na.rm = TRUE)
  iqr_value <- IQR(values, na.rm = TRUE)
  filtered_values <- values[values >= (median_value - 3 * iqr_value) & values <= (median_value + 3 * iqr_value)]
  filtered_weights <- weights[values >= (median_value - 3 * iqr_value) & values <= (median_value + 3 * iqr_value)]
  
  sum_weights <- sum(filtered_weights)
  
  if(set_missing_to_zero == TRUE) {
    weight_missing_data <- difftime(end_timespan, begin_timespan, units = c("hours")) - sum_weights
    if(!is.na(weight_missing_data) && weight_missing_data > 0) {
      filtered_values <- c(filtered_values, 0)
      filtered_weights <- c(filtered_weights, weight_missing_data)    
    }
  }
  
  if(length(filtered_values[!is.na(filtered_values)]) == 0 || length(filtered_weights[!is.na(filtered_weights)]) == 0 || sum(filtered_weights, na.rm = TRUE) == 0 || 
     length(filtered_values[!is.na(filtered_values)]) != length(filtered_weights[!is.na(filtered_weights)])) {
    return(NA)
  } else {
    return(weighted.mean(filtered_values, filtered_weights[!is.na(filtered_weights)], na.rm = TRUE))
  }
}


get_weighted_mean_values_in_range_patient_list_OhneAusreisser_2 <- function(name,vector_data,patient_list,begin_date,end_date,set_missing_to_zero = FALSE)
{
  list_values <- c()
  for(i in 1:nrow(patient_list))
  {
    #my_val <- get_all_values_in_range(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))
    
    #if(is.infinite(my_val))
    #  my_val <- NA
    #else
    #{
    my_val <- get_weighted_mean_values_in_range_ohneAusreisser(vector_data,patient_list[i,],as.POSIXct(begin_date[i],optional=TRUE),as.POSIXct(end_date[i],optional=TRUE))
    #}
    
    list_values <- c(list_values, my_val)  
  }
  df <- data.frame(ID = patient_list, placeholder_name = list_values )
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)
}


merge_and_concat_text <- function(name, vector_data, patient_list, begin_date, end_date) {
  list_values <- c()
  
  for (i in 1:nrow(patient_list)) {
    # Textwerte im angegebenen Bereich f?r den aktuellen Patienten erhalten
    text_values <- get_values_in_range(vector_data, patient_list[i,], as.POSIXct(begin_date[i], optional = TRUE), as.POSIXct(end_date[i], optional = TRUE))$Value
    
    # Texte zusammenf?hren und Duplikate entfernen
    merged_text <- paste(unique(text_values), collapse = ", ")
    
    # Wert zur Liste hinzuf?gen
    list_values <- c(list_values, merged_text)
  }
  
  # Dataframe erstellen
  df <- data.frame(ID = patient_list, placeholder_name = list_values)
  names(df)[names(df) == "placeholder_name"] <- name
  
  return(df)
}



# Funktion zur Bestimmung der h?chsten Stufe basierend auf den Schl?sselw?rtern
get_highest_level <- function(text) {
  levels <- unique(unlist(lapply(strsplit(text, ", "), function(x) keywords[x])))
  max(levels)
}



ventilation_time_sum <- function(column_name, data, patient_list, begin_date, end_date) {
  values_list <- c()
  
  for (i in 1:nrow(patient_list)) {  
    patient_id <- patient_list[i, "ID"]
    begin <- as.POSIXct(begin_date[i], format = "%Y-%m-%d %H:%M:%S")
    end <- as.POSIXct(end_date[i], format = "%Y-%m-%d %H:%M:%S")
    
    patient_subset <- data[data$PatientID == patient_id, ]
    list_values <- c()
    
    if (nrow(patient_subset) > 1) {
      patient_subset <- patient_subset[order(patient_subset$DateTime), ]
      
      for (ii in 1:nrow(patient_subset)) {  
        
        if (is.na(ii+1)) {
          next_ven_time <- NULL
        } else {
          next_ven_time <- patient_subset[ii+1, "DateTime"]
        }
        
        if (ii > 1) {
          before_ven_time <- patient_subset[ii-1, "DateTime"]
        } else {
          before_ven_time <- NULL
        }
        
        
        overlapping_intervals <- sum((begin <= patient_subset[ii, "DateTimeEnd"]) & (end >= patient_subset[ii, "DateTime"]), na.rm = TRUE)
        if(!is.na(overlapping_intervals) && overlapping_intervals > 0 && !is.na(next_ven_time) && !is.null(next_ven_time)){
          if(!is.null(before_ven_time) && before_ven_time < begin && next_ven_time <= end) {
            extra <- as.numeric(difftime( patient_subset[ii, "DateTime"], begin), units = "hours")
            list_values <- c(list_values, as.numeric(difftime(next_ven_time, patient_subset[ii, "DateTime"]), units = "hours")+extra)
            
          } else {
            if (!is.na(next_ven_time) && !is.null(next_ven_time)){
              if ( next_ven_time <= end ) {
                list_values <- c(list_values, as.numeric(difftime(next_ven_time, patient_subset[ii, "DateTime"]), units = "hours"))
                
              } else if ( next_ven_time > end ) {
                list_values <- c(list_values, as.numeric(difftime(end, patient_subset[ii, "DateTime"]), units = "hours"))
              }
            } else {
              list_values <- c(list_values, NA)
            }
            
          } 
          
        } else {
          list_values <- c(list_values, NA)
          
        }}
      gesamtsumme <- sum(list_values, na.rm = TRUE) # Calculating total sum outside of inner loop
      values_list <- c(values_list, gesamtsumme)
      
      
    } else {
      values_list <- c(values_list, NA)
    }
    
  }
  # Dataframe erstellen
  df <- data.frame(ID = patient_list$ID, placeholder_name = values_list)
  names(df)[names(df) == "placeholder_name"] <- column_name
  
  return(df)
}



check_in_range_3 <- function(column_name, data, patient_list, begin_date, end_date) {
  values_list <- c()
  
  for (i in 1:nrow(patient_list)) {  
    patient_id <- patient_list[i, "ID"]
    begin <- as.POSIXct(begin_date[i], format = "%Y-%m-%d %H:%M:%S")
    end <- as.POSIXct(end_date[i], format = "%Y-%m-%d %H:%M:%S")
    
    patient_subset <- data[data$PatientID == patient_id, ]
    list_values <- c()
    
    if (nrow(patient_subset) > 0) {
      for (ii in 1:nrow(patient_subset)) {  
        # Pr?fen, ob es ?berlappungen der Zeitintervalle gibt
        overlapping_intervals <- sum((begin <= patient_subset[ii, "DateTimeEnd"]) & (end >= patient_subset[ii, "DateTime"]), na.rm = TRUE)
        
        # ?berpr?fen, ob overlapping_intervals nicht NA ist
        if (!is.na(overlapping_intervals) && overlapping_intervals > 0) {
          list_values <- c(list_values, patient_subset[ii, "Value"])
        } else {
          list_values <- c(list_values, NA)
        }
      }
      gesamtsumme <- paste(list_values, collapse = ", ")
      values_list <- c(values_list, gesamtsumme)
    } else {
      # F?ge ein NA f?r den Patienten hinzu, wenn es keine Daten f?r ihn gibt
      values_list <- c(values_list, NA)
    }
  }
  
  # Dataframe erstellen
  df <- data.frame(ID = patient_list$ID, placeholder_name = values_list)
  names(df)[names(df) == "placeholder_name"] <- column_name
  
  return(df)
}



convert_oxygen_flow_to_fiO2 <- function(data, oxygen_flow_column, fiO2_column) {
  data[[fiO2_column]] <- NA  # Erstellen Sie eine leere Spalte für die FiO2-Werte
  
  for (i in 1:nrow(data)) {
    oxygen_flow <- data[[oxygen_flow_column]][i]
    
    if (is.na(oxygen_flow)) {
      data[[fiO2_column]][i] <- NA  # Wenn der Sauerstofffluss fehlt, ist auch der FiO2-Wert nicht verfügbar
    } else {
      if (oxygen_flow < 1) {
        data[[fiO2_column]][i] <- 21
      } else if (oxygen_flow <= 1) {
        data[[fiO2_column]][i] <- 25
      } else if (oxygen_flow <= 2) {
        data[[fiO2_column]][i] <- 29
      } else if (oxygen_flow <= 3) {
        data[[fiO2_column]][i] <- 33
      } else if (oxygen_flow <= 4) {
        data[[fiO2_column]][i] <- 37
      } else if (oxygen_flow <= 5) {
        data[[fiO2_column]][i] <- 41
      } else if (oxygen_flow <= 6) {
        data[[fiO2_column]][i] <- 45
      } else {
        data[[fiO2_column]][i] <- 50
      }
    }
  }
  
  return(data)
}



calculate_horovitz_index <- function(name, data_paO2, data_fiO2) {
  list_values <- c()
  PaO2_DateTimes <- c()
  
  for (i in 1:nrow(data_paO2)) {
    paO2_value <- data_paO2$Value[i]
    patient_id <- data_paO2$PatientID[i]
    PaO2_DateTime <- as.POSIXct(data_paO2$DateTime[i], format = "%Y-%m-%d %H:%M:%S")
    #PaO2_DateTimes <- as.POSIXct(PaO2_DateTimes)
    PaO2_DateTimes <- c(PaO2_DateTimes, PaO2_DateTime)
    #print(PaO2_DateTimes)
    
    if (is.na(paO2_value)) {
      list_values <- c(list_values, NA)  # Wenn PaO2 fehlt, ist auch der Horovitz-Index nicht verfügbar
    } else {
      # Finde den nächsten FiO2-Wert zum aktuellen PaO2-Wert basierend auf der Patienten-ID
      closest_fiO2 <- find_closest_fiO2(PaO2_DateTime, patient_id, data_fiO2)
      
      if (is.na(closest_fiO2)) {
        list_values <- c(list_values, NA)  # Wenn kein FiO2-Wert gefunden wurde, ist auch der Horovitz-Index nicht verfügbar
      } else {
        # Berechne das Horovitz-Verhältnis
        horovitz_index <- paO2_value / (closest_fiO2 / 100)
        list_values <- c(list_values, horovitz_index)
      }
    }
  }
  
  # Erstelle den DataFrame mit den Ergebnissen
  df <- data.frame(PatientID = data_paO2$PatientID, placeholder_name = list_values, DateTime = PaO2_DateTimes)
  names(df)[names(df) == "placeholder_name"] <- name
  return(df)  
}



find_closest_fiO2 <- function(paO2_datetime, patient_id, data_fiO2) {
  closest_fiO2 <- NA
  min_diff <- Inf
  
  # Filtere FiO2-Daten für die entsprechende Patienten-ID
  fiO2_subset <- data_fiO2[data_fiO2$PatientID == patient_id, ]
  #print(paO2_datetime)
  
  # Überprüfen, ob fiO2_subset nicht leer ist
  if (nrow(fiO2_subset) > 0) {
    for (fiO2_row in 1:nrow(fiO2_subset)) {
      fiO2_datetime <- as.POSIXct(fiO2_subset$DateTime[fiO2_row], format = "%Y-%m-%d %H:%M:%S")
      if (!is.na(fiO2_datetime && !is.na(paO2_datetime && "POSIXct" %in% class(paO2_datetime) && "POSIXct" %in% class(fiO2_datetime)))) {
        diff <- abs(difftime(fiO2_datetime, paO2_datetime, units = "secs"))
        #print(diff)
        #print(min_diff)
        if (!is.na(diff) && diff < min_diff) {
          closest_fiO2 <- fiO2_subset$Value[fiO2_row]
          min_diff <- diff
        }
      }
    }
  }
  
  # Überprüfen, ob closest_fiO2 weiterhin NA ist
  if (is.na(closest_fiO2)) {
    # Behandeln Sie den Fall, wenn kein gültiger FiO2-Wert gefunden wurde
    # Hier können Sie je nach Anwendungsfall entscheiden, wie Sie darauf reagieren möchten
    # Zum Beispiel können Sie closest_fiO2 auf einen Standardwert setzen oder NA beibehalten
    closest_fiO2 <- NA  # In diesem Fall behalten wir NA bei
  }
  
  return(closest_fiO2)
}




# Funktion zur Bestimmung der h?chsten Stufe basierend auf den Schl?sselw?rtern
get_highest_level_not_applicable <- function(text) {
  levels <- unique(unlist(lapply(strsplit(text, ", "), function(x) keywords_not_applicable[x])))
  max(levels)
}



check_general_ward <- function(data, patient_list) {
  new_data <- data.frame(PatientID = character(),
                         Date1 = character(),
                         Date2 = character())  # Erstellen Sie einen leeren Datenrahmen mit den gewünschten Spalten
  
  for (ii in 1:nrow(patient_list)) {  
    patient_id <- patient_list[ii, "ID"]
    patient_subset <- data[data$PatientID == patient_id, ]
    
    if ( nrow(patient_subset) > 1) {
      for (i in 1:(nrow(patient_subset) - 1)) {
        patient_subset <- patient_subset[order(patient_subset$Date1), ]
        
        date1_current <- as.Date(patient_subset[i, "Date1"])
        date2_current <- as.Date(patient_subset[i, "Date2"])
        
        date1_next <- as.Date(patient_subset[i + 1, "Date1"])
        
        current_row <- data.frame(PatientID = patient_subset[i, "PatientID"],
                                  Date1 = patient_subset[i, "Date1"],
                                  Date2 = patient_subset[i, "Date2"])
        
        next_row <- data.frame(PatientID = patient_subset[i+1, "PatientID"],
                               Date1 = patient_subset[i+1, "Date1"],
                               Date2 = patient_subset[i+1, "Date2"])
        
        last_row <- patient_subset[nrow(patient_subset), c("PatientID", "Date1", "Date2")]
        
        if (date1_next - date2_current > 10) {
          # Hier können Sie die Aktion ausführen, die Sie für den Fall einer großen Lücke durchführen möchten
          
          new_data <- rbind(new_data, current_row)
          #new_data <- rbind(new_data, next_row)
          ### Ich will hier die schleife brechen und direkt der nächste patient suchen
          #print(unique(patient_subset$PatientID))
          break
        } else {
          new_data <- rbind(new_data, current_row)
          #new_data <- rbind(new_data, next_row)
          
        }
      }
      if (date1_next - date2_current <= 10) {
        new_data <- rbind(new_data, last_row)
      }
      
      
    } else {
      new_row <- data.frame(PatientID = patient_subset[1, "PatientID"],
                            Date1 = patient_subset[1, "Date1"],
                            Date2 = patient_subset[1, "Date2"])
      new_data <- rbind(new_data, new_row)
    }
    #sum_dataset <- rbind(sum_dataset, new_data)
  }
  
  return(new_data)
}


library(parallel)
library(data.table)

check_in_range_3_parallel <- function(column_name, data, patient_list, begin_date, end_date) {
  # Überprüfen, ob die erforderlichen Objekte existieren
  if (!exists("data")) stop("Das Objekt 'data' wurde nicht gefunden")
  if (!exists("patient_list")) stop("Das Objekt 'patient_list' wurde nicht gefunden")
  if (!exists("begin_date")) stop("Das Objekt 'begin_date' wurde nicht gefunden")
  if (!exists("end_date")) stop("Das Objekt 'end_date' wurde nicht gefunden")
  
  # Anzahl der Kerne ermitteln und Cluster erstellen
  cl <- makeCluster(detectCores() - 1)
  
  # Exportiere alle erforderlichen Objekte und Funktionen an die Worker-Cluster
  clusterExport(cl, c("data", "patient_list", "begin_date", "end_date"), envir = environment())
  
  clusterEvalQ(cl, library(data.table))
  
  result_list <- parLapply(cl, 1:nrow(patient_list), function(i) {
    patient_id <- patient_list[i, "ID"]
    begin <- as.POSIXct(begin_date[i], format = "%Y-%m-%d %H:%M:%S")
    end <- as.POSIXct(end_date[i], format = "%Y-%m-%d %H:%M:%S")
    
    patient_subset <- data[data$PatientID == patient_id, ]
    list_values <- c()
    
    if (nrow(patient_subset) > 0) {
      for (ii in 1:nrow(patient_subset)) {  
        # Überprüfen, ob es Überlappungen der Zeitintervalle gibt
        overlapping_intervals <- sum((begin <= patient_subset[ii, "DateTimeEnd"]) & (end >= patient_subset[ii, "DateTime"]), na.rm = TRUE)
        
        # Überprüfen, ob overlapping_intervals nicht NA ist
        if (!is.na(overlapping_intervals) && overlapping_intervals > 0) {
          list_values <- c(list_values, patient_subset[ii, "Value"])
        } else {
          list_values <- c(list_values, NA)
        }
      }
      gesamtsumme <- paste(list_values, collapse = ", ")
      result <- list(patient_id = patient_id, values = gesamtsumme)
    } else {
      # Füge ein NA für den Patienten hinzu, wenn es keine Daten für ihn gibt
      result <- list(patient_id = patient_id, values = NA)
    }
    return(result)
  })
  
  stopCluster(cl)
  
  # Dataframe erstellen
  result_df <- data.table(ID = sapply(result_list, function(x) x$patient_id), 
                          placeholder_name = sapply(result_list, function(x) x$values))
  setnames(result_df, "placeholder_name", column_name)
  
  return(result_df)
}




library(parallel)
library(data.table)

get_last_event_patient_list_parallel <- function(name, vector_events, patient_list) {
  # Überprüfen, ob die erforderlichen Objekte existieren
  if (!exists("vector_events")) stop("Das Objekt 'vector_events' wurde nicht gefunden")
  if (!exists("patient_list")) stop("Das Objekt 'patient_list' wurde nicht gefunden")
  
  # Anzahl der Kerne ermitteln und Cluster erstellen
  cl <- makeCluster(detectCores() - 1)
  
  # Exportiere alle erforderlichen Objekte und Funktionen an die Worker-Cluster
  clusterExport(cl, c("vector_events", "patient_list"), envir = environment())
  
  # Load required libraries in each cluster
  clusterEvalQ(cl, library(data.table))
  
  # Define the function to get the last event date for a patient
  result_list <- parLapply(cl, 1:nrow(patient_list), function(i) {
    patient_id <- patient_list[i,]
    my_events <- vector_events[vector_events$PatientID == patient_id,]
    
    if (nrow(my_events) > 0) {
      last_event <- my_events[order(as.POSIXct(my_events$Date, format="%Y-%m-%d %H:%M:%S"), decreasing = TRUE),][1,]
      return(last_event$Date)
    } else {
      return(NA)
    }
  })
  
  stopCluster(cl)
  
  # Create dataframe
  df <- data.table(ID = patient_list, placeholder_name = unlist(result_list))
  
  # Rename the placeholder column
  setnames(df, "placeholder_name", paste(name, "_last_date", sep = ""))
  
  return(df)
}

library(data.table)

get_last_event_patient_list_hirid <- function(name, vector_events, patient_list) {
  # Ensure Date is in POSIXct format
  vector_events <- vector_events %>%
    mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"))
  
  # Find the last event date for each patient
  last_events <- vector_events %>%
    group_by(PatientID) %>%
    summarize(last_date = max(Date, na.rm = TRUE), .groups = 'drop')
  
  # Ensure all patients in the patient_list are included
  result <- patient_list %>%
    left_join(last_events, by = c("ID" = "PatientID"))
  
  # Rename the last_date column to the desired format
  result <- result %>%
    rename(!!paste(name, "_last_date", sep = "") := last_date)
  
  return(result)
}

get_first_event_patient_list_hirid <- function(name, vector_events, patient_list) {
  # Ensure Date is in POSIXct format
  vector_events <- vector_events %>%
    mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"))
  
  # Find the first event date for each patient
  first_events <- vector_events %>%
    group_by(PatientID) %>%
    summarize(first_date = min(Date, na.rm = TRUE), .groups = 'drop')
  
  # Ensure all patients in the patient_list are included
  result <- patient_list %>%
    left_join(first_events, by = c("ID" = "PatientID"))
  
  # Rename the first_date column to the desired format
  result <- result %>%
    rename(!!paste(name, "_first_date", sep = "") := first_date)
  
  return(result)
}


library(dplyr)
library(purrr)
library(lubridate)
library(parallel)

# Function to get weighted minimum values in a specified time range
get_weighted_min_values_in_range <- function(vector_values, patient_id, begin_time, end_time, set_missing_to_zero = FALSE) {
  begin_timespan <- as.POSIXct(begin_time)
  end_timespan <- as.POSIXct(end_time)
  
  # Fetch values in range (this function should be defined elsewhere)
  data <- get_values_in_range(vector_values, patient_id, begin_timespan - scanning_range_secs, end_timespan + scanning_range_secs)
  
  # Convert to tibble if not already
  data <- as_tibble(data)
  
  # Ensure DateTime is POSIXct
  data <- data %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
    arrange(DateTime)
  
  values <- numeric()
  weights <- numeric()
  first_value <- TRUE
  
  if (nrow(data) > 0) {
    for (k in seq_len(nrow(data))) {
      value_begin <- data$DateTime[k]
      
      if (is.na(value_begin) || is.na(begin_timespan) || is.na(end_timespan) || (value_begin < begin_timespan || value_begin > end_timespan)) {
        next
      }
      
      if (first_value) {
        first_value <- FALSE
        if (k > 1) {
          values <- c(values, as.numeric(gsub("[^0-9.,-]", "", data$Value[k-1])))
          weights <- c(weights, as.numeric(difftime(value_begin, begin_timespan, units = "hours")))
        }
      }
      
      value_end <- if (k < nrow(data)) data$DateTime[k + 1] else end_timespan
      if (value_end > end_timespan) value_end <- end_timespan
      
      value_difftime <- as.numeric(difftime(value_end, value_begin, units = "hours"))
      
      values <- c(values, as.numeric(gsub("[^0-9.,-]", "", data$Value[k])))
      weights <- c(weights, value_difftime)
    }
  }
  
  sum_weights <- sum(weights, na.rm = TRUE)
  
  if (set_missing_to_zero && !is.na(sum_weights)) {
    weight_missing_data <- as.numeric(difftime(end_timespan, begin_timespan, units = "hours")) - sum_weights
    if (!is.na(weight_missing_data) && weight_missing_data > 0) {
      values <- c(values, 0)
      weights <- c(weights, weight_missing_data)
    }
  }
  
  if (length(values[!is.na(values)]) == 0 || sum(weights, na.rm = TRUE) == 0 || length(values[!is.na(values)]) != length(weights[!is.na(weights)])) {
    return(NA)
  } else {
    return(min(values, na.rm = TRUE))
  }
}

# Parallel computation function for weighted minimum values
get_weighted_min_values_in_range_patient_list_parallel <- function(name, vector_data, patient_list, begin_date, end_date, set_missing_to_zero = FALSE) {
  if (!exists("vector_data")) stop("Das Objekt 'vector_data' wurde nicht gefunden")
  if (!exists("patient_list")) stop("Das Objekt 'patient_list' wurde nicht gefunden")
  if (!exists("begin_date")) stop("Das Objekt 'begin_date' wurde nicht gefunden")
  if (!exists("end_date")) stop("Das Objekt 'end_date' wurde nicht gefunden")
  
  # Convert begin_date and end_date to POSIXct if not already
  begin_date <- as.POSIXct(begin_date)
  end_date <- as.POSIXct(end_date)
  
  cl <- makeCluster(detectCores() - 1)
  
  # Export required objects and functions
  clusterExport(cl, c("vector_data", "patient_list", "begin_date", "end_date", "set_missing_to_zero", 
                      "get_weighted_min_values_in_range", "get_values_in_range", "scanning_range_secs"), 
                envir = environment())
  
  clusterEvalQ(cl, {
    library(dplyr)
    library(lubridate)
  })
  
  # Function for parallel processing with error handling
  result_list <- parLapply(cl, seq_len(nrow(patient_list)), function(i) {
    tryCatch({
      begin <- begin_date[i]
      end <- end_date[i]
      my_val <- get_weighted_min_values_in_range(vector_data, patient_list[i, ], begin, end, set_missing_to_zero)
      return(my_val)
    }, error = function(e) {
      message(sprintf("Error for patient %d: %s", i, e$message))
      return(NA)
    })
  })
  
  stopCluster(cl)
  
  df <- tibble(ID = patient_list, placeholder_name = unlist(result_list))
  df <- df %>%
    rename(!!name := placeholder_name)
  return(df)
}



# Function to get weighted maximum values in a specified time range
get_weighted_max_values_in_range <- function(vector_values, patient_id, begin_time, end_time, set_missing_to_zero = FALSE) {
  begin_timespan <- as.POSIXct(begin_time)
  end_timespan <- as.POSIXct(end_time)
  
  # Fetch values in range (this function should be defined elsewhere)
  data <- get_values_in_range(vector_values, patient_id, begin_timespan - scanning_range_secs, end_timespan + scanning_range_secs)
  
  # Convert to tibble if not already
  data <- as_tibble(data)
  
  # Ensure DateTime is POSIXct
  data <- data %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
    arrange(DateTime)
  
  values <- numeric()
  weights <- numeric()
  first_value <- TRUE
  
  if (nrow(data) > 0) {
    for (k in seq_len(nrow(data))) {
      value_begin <- data$DateTime[k]
      
      if (is.na(value_begin) || is.na(begin_timespan) || is.na(end_timespan) || (value_begin < begin_timespan || value_begin > end_timespan)) {
        next
      }
      
      if (first_value) {
        first_value <- FALSE
        if (k > 1) {
          values <- c(values, as.numeric(gsub("[^0-9.,-]", "", data$Value[k-1])))
          weights <- c(weights, as.numeric(difftime(value_begin, begin_timespan, units = "hours")))
        }
      }
      
      value_end <- if (k < nrow(data)) data$DateTime[k + 1] else end_timespan
      if (value_end > end_timespan) value_end <- end_timespan
      
      value_difftime <- as.numeric(difftime(value_end, value_begin, units = "hours"))
      
      values <- c(values, as.numeric(gsub("[^0-9.,-]", "", data$Value[k])))
      weights <- c(weights, value_difftime)
    }
  }
  
  sum_weights <- sum(weights, na.rm = TRUE)
  
  if (set_missing_to_zero && !is.na(sum_weights)) {
    weight_missing_data <- as.numeric(difftime(end_timespan, begin_timespan, units = "hours")) - sum_weights
    if (!is.na(weight_missing_data) && weight_missing_data > 0) {
      values <- c(values, 0)
      weights <- c(weights, weight_missing_data)
    }
  }
  
  if (length(values[!is.na(values)]) == 0 || sum(weights, na.rm = TRUE) == 0 || length(values[!is.na(values)]) != length(weights[!is.na(weights)])) {
    return(NA)
  } else {
    return(max(values, na.rm = TRUE))
  }
}

# Parallel computation function for weighted maximum values
get_weighted_max_values_in_range_patient_list_parallel <- function(name, vector_data, patient_list, begin_date, end_date, set_missing_to_zero = FALSE) {
  if (!exists("vector_data")) stop("Das Objekt 'vector_data' wurde nicht gefunden")
  if (!exists("patient_list")) stop("Das Objekt 'patient_list' wurde nicht gefunden")
  if (!exists("begin_date")) stop("Das Objekt 'begin_date' wurde nicht gefunden")
  if (!exists("end_date")) stop("Das Objekt 'end_date' wurde nicht gefunden")
  
  # Convert begin_date and end_date to POSIXct if not already
  begin_date <- as.POSIXct(begin_date)
  end_date <- as.POSIXct(end_date)
  
  cl <- makeCluster(detectCores() - 1)
  
  # Export required objects and functions
  clusterExport(cl, c("vector_data", "patient_list", "begin_date", "end_date", "set_missing_to_zero", 
                      "get_weighted_max_values_in_range", "get_values_in_range", "scanning_range_secs"), 
                envir = environment())
  
  clusterEvalQ(cl, {
    library(dplyr)
    library(lubridate)
  })
  
  # Function for parallel processing with error handling
  result_list <- parLapply(cl, seq_len(nrow(patient_list)), function(i) {
    tryCatch({
      begin <- begin_date[i]
      end <- end_date[i]
      my_val <- get_weighted_max_values_in_range(vector_data, patient_list[i, ], begin, end, set_missing_to_zero)
      return(my_val)
    }, error = function(e) {
      message(sprintf("Error for patient %d: %s", i, e$message))
      return(NA)
    })
  })
  
  stopCluster(cl)
  
  df <- tibble(ID = patient_list, placeholder_name = unlist(result_list))
  df <- df %>%
    rename(!!name := placeholder_name)
  return(df)
}



########################################von eicu extract 


check_in_range_3_parallel <- function(column_name, data, patient_list, begin_date, end_date) {
  # Überprüfen, ob die erforderlichen Objekte existieren
  if (!exists("data")) stop("Das Objekt 'data' wurde nicht gefunden")
  if (!exists("patient_list")) stop("Das Objekt 'patient_list' wurde nicht gefunden")
  if (!exists("begin_date")) stop("Das Objekt 'begin_date' wurde nicht gefunden")
  if (!exists("end_date")) stop("Das Objekt 'end_date' wurde nicht gefunden")
  
  # Anzahl der Kerne ermitteln und Cluster erstellen
  cl <- makeCluster(detectCores() - 1)
  
  # Exportiere alle erforderlichen Objekte und Funktionen an die Worker-Cluster
  clusterExport(cl, c("data", "patient_list", "begin_date", "end_date"), envir = environment())
  
  clusterEvalQ(cl, library(data.table))
  
  result_list <- parLapply(cl, 1:nrow(patient_list), function(i) {
    patient_id <- patient_list[i, "ID"]
    begin <- as.POSIXct(begin_date[i], format = "%Y-%m-%d %H:%M:%S")
    end <- as.POSIXct(end_date[i], format = "%Y-%m-%d %H:%M:%S")
    
    patient_subset <- data[data$PatientID == patient_id, ]
    list_values <- c()
    
    if (nrow(patient_subset) > 0) {
      for (ii in 1:nrow(patient_subset)) {  
        # Überprüfen, ob es Überlappungen der Zeitintervalle gibt
        overlapping_intervals <- sum((begin <= patient_subset[ii, "DateTimeEnd"]) & (end >= patient_subset[ii, "DateTime"]), na.rm = TRUE)
        
        # Überprüfen, ob overlapping_intervals nicht NA ist
        if (!is.na(overlapping_intervals) && overlapping_intervals > 0) {
          list_values <- c(list_values, patient_subset[ii, "Value"])
        } else {
          list_values <- c(list_values, NA)
        }
      }
      gesamtsumme <- paste(list_values, collapse = ", ")
      result <- list(patient_id = patient_id, values = gesamtsumme)
    } else {
      # Füge ein NA für den Patienten hinzu, wenn es keine Daten für ihn gibt
      result <- list(patient_id = patient_id, values = NA)
    }
    return(result)
  })
  
  stopCluster(cl)
  
  # Dataframe erstellen
  result_df <- data.table(ID = sapply(result_list, function(x) x$patient_id), 
                          placeholder_name = sapply(result_list, function(x) x$values))
  setnames(result_df, "placeholder_name", column_name)
  
  return(result_df)
}


####################################################################################

#von Hirid extract


library(dplyr)
library(purrr)
library(lubridate)  # For date-time operations
library(parallel)

# Function to get weighted mean values in a specified time range
get_weighted_mean_values_in_range <- function(vector_values, patient_id, begin_time, end_time, set_missing_to_zero = FALSE) {
  begin_timespan <- as.POSIXct(begin_time)
  end_timespan <- as.POSIXct(end_time)
  
  # Fetch values in range (this function should be defined elsewhere)
  data <- get_values_in_range(vector_values, patient_id, begin_timespan - scanning_range_secs, end_timespan + scanning_range_secs)
  
  # Convert to tibble if not already
  data <- as_tibble(data)
  
  # Ensure DateTime is POSIXct
  data <- data %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
    arrange(DateTime)
  
  values <- numeric()
  weights <- numeric()
  first_value <- TRUE
  
  if (nrow(data) > 0) {
    for (k in seq_len(nrow(data))) {
      value_begin <- data$DateTime[k]
      
      if (is.na(value_begin) || is.na(begin_timespan) || is.na(end_timespan) || (value_begin < begin_timespan || value_begin > end_timespan)) {
        next
      }
      
      if (first_value) {
        first_value <- FALSE
        if (k > 1) {
          values <- c(values, as.numeric(gsub("[^0-9.,-]", "", data$Value[k-1])))
          weights <- c(weights, as.numeric(difftime(value_begin, begin_timespan, units = "hours")))
        }
      }
      
      value_end <- if (k < nrow(data)) data$DateTime[k + 1] else end_timespan
      if (value_end > end_timespan) value_end <- end_timespan
      
      value_difftime <- as.numeric(difftime(value_end, value_begin, units = "hours"))
      
      values <- c(values, as.numeric(gsub("[^0-9.,-]", "", data$Value[k])))
      weights <- c(weights, value_difftime)
    }
  }
  
  sum_weights <- sum(weights, na.rm = TRUE)
  
  if (set_missing_to_zero && !is.na(sum_weights)) {
    weight_missing_data <- as.numeric(difftime(end_timespan, begin_timespan, units = "hours")) - sum_weights
    if (!is.na(weight_missing_data) && weight_missing_data > 0) {
      values <- c(values, 0)
      weights <- c(weights, weight_missing_data)
    }
  }
  
  if (length(values[!is.na(values)]) == 0 || sum(weights, na.rm = TRUE) == 0 || length(values[!is.na(values)]) != length(weights[!is.na(weights)])) {
    return(NA)
  } else {
    return(weighted.mean(values, weights, na.rm = TRUE))
  }
}

# Parallel computation function
get_weighted_mean_values_in_range_patient_list_parallel <- function(name, vector_data, patient_list, begin_date, end_date, set_missing_to_zero = FALSE) {
  if (!exists("vector_data")) stop("Das Objekt 'vector_data' wurde nicht gefunden")
  if (!exists("patient_list")) stop("Das Objekt 'patient_list' wurde nicht gefunden")
  if (!exists("begin_date")) stop("Das Objekt 'begin_date' wurde nicht gefunden")
  if (!exists("end_date")) stop("Das Objekt 'end_date' wurde nicht gefunden")
  
  # Convert begin_date and end_date to POSIXct if not already
  begin_date <- as.POSIXct(begin_date)
  end_date <- as.POSIXct(end_date)
  
  cl <- makeCluster(detectCores() - 15)
  
  # Export required objects and functions
  clusterExport(cl, c("vector_data", "patient_list", "begin_date", "end_date", "set_missing_to_zero", 
                      "get_weighted_mean_values_in_range", "get_values_in_range", "scanning_range_secs"), 
                envir = environment())
  
  clusterEvalQ(cl, {
    library(dplyr)
    library(lubridate)
  })
  
  # Function for parallel processing with error handling
  result_list <- parLapply(cl, seq_len(nrow(patient_list)), function(i) {
    tryCatch({
      begin <- begin_date[i]
      end <- end_date[i]
      my_val <- get_weighted_mean_values_in_range(vector_data, patient_list[i, ], begin, end, set_missing_to_zero)
      return(my_val)
    }, error = function(e) {
      message(sprintf("Error for patient %d: %s", i, e$message))
      return(NA)
    })
  })
  
  stopCluster(cl)
  
  df <- tibble(ID = patient_list, placeholder_name = unlist(result_list))
  df <- df %>%
    rename(!!name := placeholder_name)
  return(df)
}


get_min_values_in_range_patient_list_parallel <- function(name, vector_data, patient_list, begin_date, end_date) {
  if (!exists("vector_data")) stop("Das Objekt 'vector_data' wurde nicht gefunden")
  if (!exists("patient_list")) stop("Das Objekt 'patient_list' wurde nicht gefunden")
  if (!exists("begin_date")) stop("Das Objekt 'begin_date' wurde nicht gefunden")
  if (!exists("end_date")) stop("Das Objekt 'end_date' wurde nicht gefunden")
  
  # Convert begin_date and end_date to POSIXct if not already
  begin_date <- as.POSIXct(begin_date)
  end_date <- as.POSIXct(end_date)
  
  cl <- makeCluster(detectCores() - 15)
  
  # Export required objects and functions
  clusterExport(cl, c("vector_data", "patient_list", "begin_date", "end_date", 
                      "get_min_values_in_range", "get_values_in_range", "scanning_range_secs"), 
                envir = environment())
  
  clusterEvalQ(cl, {
    library(dplyr)
    library(lubridate)
  })
  
  # Function for parallel processing with error handling
  result_list <- parLapply(cl, seq_len(nrow(patient_list)), function(i) {
    tryCatch({
      begin <- begin_date[i]
      end <- end_date[i]
      my_val <- get_min_values_in_range(vector_data, patient_list[i, ], begin, end)
      return(my_val)
    }, error = function(e) {
      message(sprintf("Error for patient %d: %s", i, e$message))
      return(NA)
    })
  })
  
  stopCluster(cl)
  
  df <- tibble(ID = patient_list, placeholder_name = unlist(result_list))
  df <- df %>%
    rename(!!name := placeholder_name)
  return(df)
}

get_min_values_in_range <- function(vector_values, patient_id, begin_time, end_time) {
  begin_timespan <- as.POSIXct(begin_time)
  end_timespan <- as.POSIXct(end_time)
  
  # Fetch values in range (this function should be defined elsewhere)
  data <- get_values_in_range(vector_values, patient_id, begin_timespan - scanning_range_secs, end_timespan + scanning_range_secs)
  
  # Convert to tibble if not already
  data <- as_tibble(data)
  
  # Ensure DateTime is POSIXct
  data <- data %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
    arrange(DateTime)
  
  # Filter data within the specified time range
  data_in_range <- data %>%
    filter(DateTime >= begin_timespan & DateTime <= end_timespan)
  
  if (nrow(data_in_range) > 0) {
    # Extract the numeric values and calculate the minimum
    numeric_values <- as.numeric(gsub("[^0-9.,-]", "", data_in_range$Value))
    min_value <- min(numeric_values, na.rm = TRUE)
    return(min_value)
  } else {
    return(NA)
  }
}

get_max_values_in_range_patient_list_parallel <- function(name, vector_data, patient_list, begin_date, end_date) {
  if (!exists("vector_data")) stop("Das Objekt 'vector_data' wurde nicht gefunden")
  if (!exists("patient_list")) stop("Das Objekt 'patient_list' wurde nicht gefunden")
  if (!exists("begin_date")) stop("Das Objekt 'begin_date' wurde nicht gefunden")
  if (!exists("end_date")) stop("Das Objekt 'end_date' wurde nicht gefunden")
  
  # Convert begin_date and end_date to POSIXct if not already
  begin_date <- as.POSIXct(begin_date)
  end_date <- as.POSIXct(end_date)
  
  cl <- makeCluster(detectCores() - 15)
  
  # Export required objects and functions
  clusterExport(cl, c("vector_data", "patient_list", "begin_date", "end_date", 
                      "get_max_values_in_range", "get_values_in_range", "scanning_range_secs"), 
                envir = environment())
  
  clusterEvalQ(cl, {
    library(dplyr)
    library(lubridate)
  })
  
  # Function for parallel processing with error handling
  result_list <- parLapply(cl, seq_len(nrow(patient_list)), function(i) {
    tryCatch({
      begin <- begin_date[i]
      end <- end_date[i]
      my_val <- get_max_values_in_range(vector_data, patient_list[i, ], begin, end)
      return(my_val)
    }, error = function(e) {
      message(sprintf("Error for patient %d: %s", i, e$message))
      return(NA)
    })
  })
  
  stopCluster(cl)
  
  df <- tibble(ID = patient_list, placeholder_name = unlist(result_list))
  df <- df %>%
    rename(!!name := placeholder_name)
  return(df)
}

get_max_values_in_range <- function(vector_values, patient_id, begin_time, end_time) {
  begin_timespan <- as.POSIXct(begin_time)
  end_timespan <- as.POSIXct(end_time)
  
  # Fetch values in range (this function should be defined elsewhere)
  data <- get_values_in_range(vector_values, patient_id, begin_timespan - scanning_range_secs, end_timespan + scanning_range_secs)
  
  # Convert to tibble if not already
  data <- as_tibble(data)
  
  # Ensure DateTime is POSIXct
  data <- data %>%
    mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
    arrange(DateTime)
  
  # Filter data within the specified time range
  data_in_range <- data %>%
    filter(DateTime >= begin_timespan & DateTime <= end_timespan)
  
  if (nrow(data_in_range) > 0) {
    # Extract the numeric values and calculate the maximum
    numeric_values <- as.numeric(gsub("[^0-9.,-]", "", data_in_range$Value))
    max_value <- max(numeric_values, na.rm = TRUE)
    return(max_value)
  } else {
    return(NA)
  }
}
