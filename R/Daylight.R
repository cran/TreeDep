#' Daylight - Generates hourly daylight data ("Night" and "Daylight") in a specific year
#' @param  shortest_day_sunrise Sunrise time in the shortest day in the Northern Hemisphere (December 21) using decimals for minutes (e.g. 8.4)
#' @param  shortest_day_sunset Sunset time in the shortest day in the Northern Hemisphere (December 21) using decimals for minutes (e.g. 17.8)
#' @param  longest_day_sunset0 Sunset time in the longest day in the Northern Hemisphere (June 21) using decimals for minutes (e.g. 21.9)
#' @param  Year Year to generate hourly daylight data (e.g. 2015)
#' @return A dataframe with hourly daylight values is generated
#' @export
#' @import "lubridate" "stats"
#' @importFrom "graphics" "axis" "legend" "lines" "mtext" "par" "plot" "polygon" "points"
#' @importFrom "stats" "median" "quantile"
#' @importFrom "utils" "head"
#'
#' @examples
#'
#' Daylight (shortest_day_sunrise  = 8.4,
#' shortest_day_sunset = 17.8,
#' longest_day_sunset0 = 21.9,
#' Year = 2016)
#'
Daylight <- function(shortest_day_sunrise,
                       shortest_day_sunset,
                       longest_day_sunset0,
                       Year){
  Daily <- seq(as.Date(paste(Year,"/1/1", sep="")),
               as.Date(paste(Year,"/12/31", sep="")), "days")
  longest_day_sunset <- longest_day_sunset0 - 1
  shortest_day_dur <-shortest_day_sunset - shortest_day_sunrise
  longest_day_dur <- 24 - shortest_day_dur
  longest_day_sunrise0 <-  longest_day_sunset0 - longest_day_dur
  longest_day_sunrise <-  longest_day_sunrise0 - 1
  days_distance<- c(c((173-1):0),
                    c(1:(round(length(Daily)/2))),
                    rev(1:(round(length(Daily)/2)))[-1][1:(length(Daily)-173-round(length(Daily)/2))])
  normal_day <-dnorm(days_distance,  mean=0,sd=90)
  st_normal_day <- rep(NA,length(normal_day))
  for(i in 1:length(normal_day)){
    st_normal_day[i] <-  (normal_day[i] - min(normal_day))/ (max(normal_day) - min(normal_day))
  }
  ###sunset
  normal_day <- dnorm(days_distance,mean=0,sd=90)
  st_normal_day <- rep(NA,length(normal_day))
  for(i in 1:length(normal_day)){
    st_normal_day[i] <-  (normal_day[i] - min(normal_day))/ (max(normal_day) - min(normal_day))
  }
  adj_day_sunset_NC <- rep(NA,length(normal_day))
  for(i in 1:length(normal_day)){
    adj_day_sunset_NC[i] <- (longest_day_sunset - shortest_day_sunset) / (max(st_normal_day) - min(st_normal_day)) *
      (st_normal_day[i] - min(st_normal_day)) + shortest_day_sunset
  }
  adjusted_sunset<- c(adj_day_sunset_NC[1:85],adj_day_sunset_NC[86:302]+1,
                      adj_day_sunset_NC[303:length(adj_day_sunset_NC)])
  adjusted_sunset_h <- floor(adjusted_sunset)
  adjusted_sunset_m <-(adjusted_sunset-adjusted_sunset_h)*60

  ###sunrise
  normal_day <- -dnorm(days_distance,mean=0,sd=90)
  st_normal_day <- rep(NA,length(normal_day))
  for(i in 1:length(normal_day)){
    st_normal_day[i] <-  (normal_day[i] - min(normal_day))/ (max(normal_day) - min(normal_day))
  }
  adj_day_sunrise_NC <- rep(NA,length(normal_day))
  for(i in 1:length(normal_day)){
    adj_day_sunrise_NC[i] <- (shortest_day_sunrise - longest_day_sunrise) / (max(st_normal_day) - min(st_normal_day)) *
      (st_normal_day[i] - min(st_normal_day)) + longest_day_sunrise
  }
  adjusted_sunrise<- c(adj_day_sunrise_NC[1:85],adj_day_sunrise_NC[86:302]+1,
                       adj_day_sunrise_NC[303:length(adj_day_sunrise_NC)])
  adjusted_sunrise_h <- floor(adjusted_sunrise)
  adjusted_sunrise_m <-(adjusted_sunrise-adjusted_sunrise_h)*60

  # Duration
  adj_day_dur <- rep(NA,length(normal_day))
  for(i in 1:length(normal_day)){
    adj_day_dur[i] <- (longest_day_dur - shortest_day_dur) / (max(st_normal_day) - min(st_normal_day)) *
      (st_normal_day[i] - min(st_normal_day)) + shortest_day_dur
  }


  Daylight_d <- cbind.data.frame(Dates = seq(as.Date(paste(Year,"/1/1", sep="")),
                                             as.Date(paste(Year,"/12/31", sep="")), "days"),
                                 adjusted_sunrise_h,adjusted_sunrise_m,
                                 adjusted_sunrise,adjusted_sunset_h,adjusted_sunset_m,adjusted_sunset)
  Daylight_d$day <- lubridate::day(Daylight_d$Dates)
  Daylight_d$month <- lubridate::month(Daylight_d$Dates)
  Daylight_d$month_day <- paste(Daylight_d$month, Daylight_d$day, sep="-")

  Daylight_h <-data.frame(Dates = seq(
    from=as.POSIXct(paste(Year,"-1-1 0:00", sep=""), tz="UTC"),
    to=as.POSIXct(paste(Year,"-12-31 23:00", sep=""), tz="UTC"),
    by="hour"))
  Daylight_h$hour <- lubridate::hour(Daylight_h$Dates)
  Daylight_h$day <- lubridate::day(Daylight_h$Dates)
  Daylight_h$month <- lubridate::month(Daylight_h$Dates)
  Daylight_h$month_day <- paste(Daylight_h$month, Daylight_h$day, sep="-")
  Daylight_d_look <- Daylight_d[,c("month_day","adjusted_sunrise","adjusted_sunset")]
  Daylight0_0 <- merge(Daylight_h, Daylight_d_look, by='month_day')
  Daylight0 <- Daylight0_0[order(Daylight0_0$Dates),]
  Daylight0$Daylight <- "Night"
  Daylight0$Daylight[Daylight0$hour >= Daylight0$adjusted_sunrise &
                       Daylight0$hour < Daylight0$adjusted_sunset] <- "Daylight"
  table(Daylight0$Daylight)
  Diff_param <- table(Daylight0$Daylight)[2] - table(Daylight0$Daylight)[1]
  if(Diff_param > 0){
    To_change <- "Night"
    We_need_more <- "Daylight"
  }else{
    To_change <- "Daylight"
    We_need_more <- "Night"
  }
  N_cells_to_change <- round(abs(Diff_param) / 2)
  Daylight0$diff_sunrise <- abs(Daylight0$hour - Daylight0$adjusted_sunrise)
  Daylight0$diff_sunset <- abs(Daylight0$hour - Daylight0$adjusted_sunset)

  md_df <- Daylight0[Daylight0$Daylight==To_change,]
  Lowest_diff <- sort(c(md_df$diff_sunrise,md_df$diff_sunset))[1:N_cells_to_change]
  Dates_to_change0 <- md_df[c(which(md_df$diff_sunrise %in% Lowest_diff),
                              which(md_df$diff_sunset %in% Lowest_diff)),"Dates"]
  if(length(Dates_to_change0) > N_cells_to_change) {
    Remove_last_elements <- length(Dates_to_change0) - N_cells_to_change
    Dates_to_change <- Dates_to_change0[1:(length(Dates_to_change0)-abs(Remove_last_elements))]
  }else{
    Dates_to_change <- Dates_to_change0
  }
  Daylight0[which(Daylight0$Dates %in% Dates_to_change),"Daylight"] <- We_need_more
  table(Daylight0$Daylight)
  Daylight_h$Daylight <- Daylight0
  Daylight0$Sunrise_time <- round(Daylight0$adjusted_sunrise, 2)
  Daylight0$Sunset_time <- round(Daylight0$adjusted_sunset, 2)
  plot(Daylight0$Dates,Daylight0$Sunrise_time, ylim=c(0,24),xlab="Date", ylab="Time",
       main=paste("Sunrise and sunset times -", Year))
  points(Daylight0$Dates,Daylight0$Sunset_time)
  Daylight_final <- Daylight0[,c("Dates", "Daylight")]#, "Sunrise_time", "Sunset_time")]
  return(Daylight_final)
}
