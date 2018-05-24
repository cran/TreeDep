#' LAI_deciduous - Generates hourly data of leaf and bark area index for deciduous trees in a specific year
#' @param  Year Year to generate leaf and bark area index (e.g. 2015)
#' @param  BAI_value Bark area index value (e.g. 0.1)
#' @param  LAI_value Maximum value of leaf area index value (e.g. 1.5)
#' @param  day_decay_ini Day of the month leaves start to decay (between 1 and 31; e.g., 15)
#' @param  month_decay_ini Month of the year leaves start to decay (between 1 and 12; e.g., 10)
#' @param  days_duration_decay The duration of leaf decay in number of days (e.g., 50)
#' @param  day_emergence_ini Day of the month leaves start to emerge (between 1 and 31; e.g., 1)
#' @param  month_emergence_ini Month of the year leaves start to emerge (between 1 and 12; e.g., 4)
#' @param  days_duration_emergence The duration of leaf emergence in number of days (e.g., 20)
#' @return A dataframe with LAI and BAI hourly values is generated
#' @export
#' @import "lubridate" "stats"
#' @importFrom "graphics" "axis" "legend" "lines" "mtext" "par" "plot" "polygon"
#' @importFrom "stats" "median" "quantile"
#' @importFrom "utils" "head"
#'
#' @examples
#'
#' LAI_deciduous(Year = 2016,
#' BAI_value = 0.1,
#' LAI_value = 1.5,
#' day_decay_ini = 15,
#' month_decay_ini = 10,
#' days_duration_decay = 100,
#' day_emergence_ini = 1,
#' month_emergence_ini = 4,
#' days_duration_emergence = 20)
#'
LAI_deciduous <- function(Year,
                          BAI_value,
                          LAI_value,
                          day_decay_ini,
                          month_decay_ini,
                          days_duration_decay,
                          day_emergence_ini,
                          month_emergence_ini,
                          days_duration_emergence){

  LAI0_d <- data.frame(Dates =seq(from=as.POSIXct(paste(Year,"-1-1",sep=""), tz="UTC"),
                                  to=as.POSIXct(paste(Year,"-12-31",sep=""), tz="UTC"),
                                  by="day"))
  LAI0_d$month <- lubridate::month(LAI0_d$Dates)
  LAI0_d$day <- lubridate::day(LAI0_d$Dates)
  LAI0_d$month_day <- paste(LAI0_d$month, LAI0_d$day, sep="-")
  Date_decay_ini_d <-  LAI0_d$Dates[LAI0_d$day==day_decay_ini & LAI0_d$month==month_decay_ini]
  if((match(Date_decay_ini_d  , LAI0_d$Dates) + days_duration_decay) > nrow(LAI0_d)){
    ID_decay_fin_d <- (match(Date_decay_ini_d  , LAI0_d$Dates) + days_duration_decay) - nrow(LAI0_d)
    Date_decay_fin_d <-  LAI0_d$Dates[ID_decay_fin_d]
    Date_emergence_ini_d <-  LAI0_d$Dates[LAI0_d$day==day_emergence_ini & LAI0_d$month==month_emergence_ini]
    ID_emergence_fin_d <- match(Date_emergence_ini_d  , LAI0_d$Dates) + days_duration_emergence
    Date_emergence_fin_d <-  LAI0_d$Dates[ID_emergence_fin_d]
    LAI0_d$LAI <- NA
    LAI0_d$LAI[LAI0_d$Dates >= Date_decay_fin_d & LAI0_d$Dates < Date_emergence_ini_d] <- 0
    LAI0_d$LAI[LAI0_d$Dates >= Date_emergence_fin_d & LAI0_d$Dates < Date_decay_ini_d] <- LAI_value + 0
    fit_decay <- lm(c(LAI_value,0)~c(0,days_duration_decay+1))
    LAI0_d$LAI[LAI0_d$Dates >= Date_decay_ini_d]  <-
      fit_decay$coefficients[1]+fit_decay$coefficients[2]*c(1:(days_duration_decay-ID_decay_fin_d+1)) + 0
    LAI0_d$LAI[LAI0_d$Dates < Date_decay_fin_d]  <-
      fit_decay$coefficients[1]+fit_decay$coefficients[2]*c((days_duration_decay-ID_decay_fin_d+2):(days_duration_decay)) + 0
    fit_emergence <- lm(c(LAI_value,0)~c(0,days_duration_emergence+1))
    LAI0_d$LAI[LAI0_d$Dates >= Date_emergence_ini_d & LAI0_d$Dates < Date_emergence_fin_d] <-
      fit_emergence$coefficients[1]+fit_emergence$coefficients[2]*c(days_duration_emergence:1) + 0
  }else{
    ID_decay_fin_d <- match(Date_decay_ini_d  , LAI0_d$Dates) + days_duration_decay
    Date_decay_fin_d <-  LAI0_d$Dates[ID_decay_fin_d]
    Date_emergence_ini_d <-  LAI0_d$Dates[LAI0_d$day==day_emergence_ini & LAI0_d$month==month_emergence_ini]
    ID_emergence_fin_d <- match(Date_emergence_ini_d  , LAI0_d$Dates) + days_duration_emergence
    Date_emergence_fin_d <-  LAI0_d$Dates[ID_emergence_fin_d]
    LAI0_d$LAI <- NA
    LAI0_d$LAI[LAI0_d$Dates >= Date_decay_fin_d | LAI0_d$Dates < Date_emergence_ini_d] <- 0
    LAI0_d$LAI[LAI0_d$Dates >= Date_emergence_fin_d & LAI0_d$Dates < Date_decay_ini_d] <- LAI_value + 0
    fit_decay <- lm(c(LAI_value,0)~c(0,days_duration_decay+1))
    LAI0_d$LAI[LAI0_d$Dates >= Date_decay_ini_d & LAI0_d$Dates < Date_decay_fin_d] <-
      fit_decay$coefficients[1]+fit_decay$coefficients[2]*c(1:days_duration_decay) + 0
    fit_emergence <- lm(c(LAI_value,0)~c(0,days_duration_emergence+1))
    LAI0_d$LAI[LAI0_d$Dates >= Date_emergence_ini_d & LAI0_d$Dates < Date_emergence_fin_d] <-
      fit_emergence$coefficients[1]+fit_emergence$coefficients[2]*c(days_duration_emergence:1) + 0
  }
  lookup <- data.frame(month_day = LAI0_d$month_day, LAI = LAI0_d$LAI)
  LAI0_h <- data.frame(Dates =seq(from=as.POSIXct(paste(Year,"-1-1 0:00",sep=""), tz="UTC"),
                                  to=as.POSIXct(paste(Year,"-12-31 23:00",sep=""), tz="UTC"),
                                  by="hour"))
  LAI0_h$month <- lubridate::month(LAI0_h$Dates)
  LAI0_h$day <- lubridate::day(LAI0_h$Dates)
  LAI0_h$month_day <- paste(LAI0_h$month, LAI0_h$day, sep="-")
  LAI_merge <- merge(lookup, LAI0_h, by='month_day', sort = FALSE)
  LAI <- data.frame(Dates = LAI_merge$Dates, LAI=LAI_merge$LAI)
  LAI$BAI <- BAI_value
  LAI$LAI_Total <- LAI$LAI + LAI$BAI
  plot(LAI$Dates, LAI$LAI_Total, xlab="Date", ylab="LAI + BAI")
  return(LAI)
}

