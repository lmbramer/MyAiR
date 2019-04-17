#' Trelliscope of FEV1 over time
#' 
#' Panel function for FEV1 over time
#' 
#' @param spiro_data data.frame with at minimum columns giving "fev1", "when_time", "Sample_Info", and "when_date"
#' 
#' @return plotly display showing FEV1 over time. If more than one subject (participant and season) is in the data division then subject data is connected and colored differently.
#' 
#' @name fev1_vs_time
#' @rdname fev1_vs_time
#' @export

fev1_vs_time <- function(spiro_data){
  
  # check that the needed columns are present #
  if(sum(c("fev1", "when_time", "Sample_Info", "when_date") %in% names(spiro_data)) != 4) stop("Data does not have the minimum required columns to make plot")
  
  # check to see how many subjects there are #
  num_subjs = length(unique(spiro_data$Sample_Info))
  
  # if one subject only #
  if(num_subjs == 1){
    
  # add POSIX date time column #
    spiro_data$Time_POS = whenTime_format(spiro_data$when_time)
    
  # determine some y value limits #
  # 100 to 300 is "typical" #
  min_obs_fev1 = min(spiro_data$fev1, na.rm = T)
  max_obs_fev1 = max(spiro_data$fev1, na.rm = T)
  
  ymin_val = min(100, min_obs_fev1 - 10)
  ymax_val = max(300, max_obs_fev1 + 10)
  
  p = plot_ly(x = ~Time_POS, y = ~fev1, mode = "lines", data = spiro_data) %>%
    add_trace(mode = "scatter") %>%
    layout(yaxis = list(range=c(ymin_val, ymax_val), title = "FEV1"), xaxis = list(title = ""))
  }else{
    # add POSIX date time column #
    spiro_data$Time_POS = whenTime_format(spiro_data$when_time)
    
    # determine some y value limits #
    # 100 to 300 is "typical" #
    min_obs_fev1 = min(spiro_data$fev1, na.rm = T)
    max_obs_fev1 = max(spiro_data$fev1, na.rm = T)
    
    ymin_val = min(100, min_obs_fev1 - 10)
    ymax_val = max(300, max_obs_fev1 + 10)
    
    spiro_data$participant = as.factor(spiro_data$participant)
    p = plot_ly(x = ~Time_POS, y = ~fev1, mode = "lines", color = ~participant, data = spiro_data) %>%
      add_trace(mode = "scatter") %>%
      layout(yaxis = list(range=c(ymin_val, ymax_val), title = "FEV1"), xaxis = list(title = ""))
    
  }
  return(p)
}

#' Trelliscope cognostics of FEV1 over time
#' 
#' Cognostic function for FEV1 over time
#' 
#' @param spiro_data data.frame with at minimum columns giving "fev1", "when_time", "Sample_Info", and "when_date"
#' 
#' @return tibble containing cognostics for trelliscopejs
#' 
#' @name fev1_vs_time_cog
#' @rdname fev1_vs_time_cog
#' @export
#' 
fev1_vs_time_cog <- function(spiro_data){
  # check to see how many subjects there are #
  num_subjs = length(unique(spiro_data$Sample_Info))
  # add POSIX date time column #
  spiro_data$Time_POS = whenTime_format(spiro_data$when_time)
  
  tibble(
    num_samples = cog(sum(!is.na(spiro_data$fev1)), desc = "Total number of observed FEV1 values"),
    num_subjects = cog(num_subjs, desc = "Number of unique subject and sampling period combinations"),
    firstObs_date = cog(min(spiro_data$Time_POS), desc = "Date of first observation in period", type = "date"),
    lastObs_date = cog(max(spiro_data$Time_POS), desc = "Date of last observation in period", type = "date"),
    min_fev1 = cog(min(spiro_data$fev1, na.rm = T), desc = "Minimum observed FEV1 value"),
    max_fev1 = cog(max(spiro_data$fev1, na.rm = T), desc = "Maximum observed FEV1 value"),
    sd_fev1 = cog(sd(spiro_data$fev1, na.rm = T), desc = "Standard deviation of observed FEV1 values")
  )
}
