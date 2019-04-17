#' Format user selected data
#' 
#' Format and join data for visualization
#' 
#' @param file_path parent path where the data exists with folder for each participant
#' 
#' @return list object with each component giving compiled data for each data source
#' 
#' @export

data_form <- function(file_path){
  
  # get high level data #
  folder_names = list.files(file_path, full.names = T)
  
  folder_names = folder_names[-c(grep("trelliscope.R", folder_names), grep("Dockerfile", folder_names))]
  # gather data for each subject #
  myair_fulldata = lapply(folder_names, function(x){
    samp_info = unlist(lapply(strsplit(x, "/"), function(x) x[length(x)]))
    
    # get subject specific file names #
    sub_files = list.files(x, full.names = T)
    
    demo_id = grep("demographics", sub_files)
    location_id = grep("locations", sub_files)
    question_id = grep("questionnaires", sub_files)
    spraw_id = grep("spirometers-raw", sub_files)
    spval_id = grep("spirometers-validated", sub_files)
    wb_id = grep("wristbands", sub_files)
    
    if(length(demo_id) == 1){demo_data = read.csv(sub_files[demo_id], check.names = F)}else{demo_data = NULL}
    if(length(location_id) == 1){loc_data = read.csv(sub_files[location_id], check.names = F)}else{loc_data = NULL}
    if(length(question_id) == 1){quest_data = read.csv(sub_files[question_id], check.names = F)}else{quest_data = NULL}
    if(length(spraw_id) == 1){spiroRaw_data = read.csv(sub_files[spraw_id], check.names = F)}else{spiroRaw_data = NULL}
    if(length(spval_id) == 1){spiroVal_data = read.csv(sub_files[spval_id], check.names = F)}else{spiroVal_data = NULL}
    if(length(wb_id) == 1){wristband_data = read.csv(sub_files[wb_id], check.names = F)}else{wristband_data = NULL}
    
    list(sample_info = samp_info, demo_data = demo_data, location_data = loc_data, question_data = quest_data, spiroRaw_data = spiroRaw_data, spiroVal_data = spiroVal_data, wristband_data = wristband_data)

  }
  )
  
  # join data for individual sources #
  full_sample_info = unlist(lapply(myair_fulldata, function(x) x$sample_info))
  
  demo_data_list = lapply(myair_fulldata, function(x) x$demo_data)
  demo_nrow = unlist(lapply(myair_fulldata, function(x) if(!is.null(x$demo_data)){nrow(x$demo_data)}else{0}))
  demo_join = data.frame(Sample_Info = rep(full_sample_info, demo_nrow), do.call(rbind, demo_data_list))
  
  locat_data_list = lapply(myair_fulldata, function(x) x$location_data)
  locat_nrow = unlist(lapply(myair_fulldata, function(x) if(!is.null(x$location_data)){nrow(x$location_data)}else{0}))
  location_join = data.frame(Sample_Info = rep(full_sample_info, locat_nrow), do.call(rbind, locat_data_list))
  
  quest_data_list = lapply(myair_fulldata, function(x) x$question_data)
  quest_nrow = unlist(lapply(myair_fulldata, function(x) if(!is.null(x$question_data)){nrow(x$question_data)}else{0}))
  question_join = data.frame(Sample_Info = rep(full_sample_info, quest_nrow), do.call(rbind, quest_data_list))
  
  spraw_data_list = lapply(myair_fulldata, function(x) x$spiroRaw_data)
  spraw_nrow = unlist(lapply(myair_fulldata, function(x) if(!is.null(x$spiroRaw_data)){nrow(x$spiroRaw_data)}else{0}))
  spraw_join = data.frame(Sample_Info = rep(full_sample_info, spraw_nrow), do.call(rbind, spraw_data_list))
  
  spval_data_list = lapply(myair_fulldata, function(x) x$spiroVal_data)
  spval_nrow = unlist(lapply(myair_fulldata, function(x) if(!is.null(x$spiroVal_data)){nrow(x$spiroVal_data)}else{0}))
  spval_join = data.frame(Sample_Info = rep(full_sample_info, spval_nrow), do.call(rbind, spval_data_list))
  
  wb_data_list = lapply(myair_fulldata, function(x) x$wristband_data)
  wb_nrow = unlist(lapply(myair_fulldata, function(x) if(!is.null(x$wristband_data)){nrow(x$wristband_data)}else{0}))
  wb_join = data.frame(Sample_Info = rep(full_sample_info, wb_nrow), do.call(rbind, wb_data_list))
  
  res = list(demo_joined = demo_join, location_joined = location_join, question_joined = question_join, spiroRaw_joined = spraw_join, spiroVal_joined = spval_join, wristband_joined = wb_join)
  
  return(res)
}
