#' Format date correctly from the 'when_time' column
#' 
#' Format date to match POSIX accepted formats
#' 
#' @param time_vector current format is DayofWeek_Month_Day_Year_HH:MM:SS_TimeZone
#' 
#' @export

whenTime_format <- function(time_vector){
  
  # turn vector into character string #
  temp = as.character(time_vector)
  
  # lose the time zone info #
  temp2 = unlist(lapply(strsplit(as.character(time_vector), "GMT"), function(x) x[1]))
  
  # lose the day of the week info #
  temp3 = substring(temp, first = 5)
  
  # turn month info into numeric #
  temp_month = substring(temp3, first = 1, last = 3)
  temp_month_num = rep(NA, length(temp_month))
  temp_month_num[which(temp_month == "Jan")] = "01"
  temp_month_num[which(temp_month == "Feb")] = "02"
  temp_month_num[which(temp_month == "Mar")] = "03"
  temp_month_num[which(temp_month == "Apr")] = "04"
  temp_month_num[which(temp_month == "May")] = "05"
  temp_month_num[which(temp_month == "Jun")] = "06"
  temp_month_num[which(temp_month == "Jul")] = "07"
  temp_month_num[which(temp_month == "Aug")] = "08"
  temp_month_num[which(temp_month == "Sep")] = "09"
  temp_month_num[which(temp_month == "Oct")] = "10"
  temp_month_num[which(temp_month == "Nov")] = "11"
  temp_month_num[which(temp_month == "Dec")] = "12"
  
  temp_day = unlist(lapply(strsplit(temp3, " "), function(x) x[2]))

  temp_year = unlist(lapply(strsplit(temp3, " "), function(x) x[3]))
  
  temp_time = unlist(lapply(strsplit(temp3, " "), function(x) x[4]))
  
  res1 = paste(temp_year, temp_month_num, temp_day, sep = "/")
  res2 = paste(res1, temp_time)
  
  as.POSIXlt(res2)
}