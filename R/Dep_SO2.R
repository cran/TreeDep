#' Dep_SO2 - Calculates hourly deposition of SO2 on vegetation
#' @param  x A data frame containing hourly data of SO2 concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @return Hourly data of deposition of SO2 on vegetation (g m-2 h-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Dep_SO2(x = Bizkaia_data)
Dep_SO2 <- function(x){
  db <- x
  t_s <- 60*60 # s (seconds in 1 hour)
  LAI_Total <-  db$BAI + db$LAI
  LAI_Total_scaled <- LAI_Total
  LAI_Total_scaled[LAI_Total>1] <- 1
  No_rain_event <- rep(NA, length(db$Precip))
  No_rain_event[db$Precip==0] <- 1
  No_rain_event[db$Precip>0] <- 0
  Flux_SO2_hourly0 <- db$SO2/1000000*Dep_vel_SO2(db)[,"Depos_vel"]*t_s*No_rain_event*LAI_Total_scaled # g/m2/h = g/m3 * m/s * s/h * unitless * unitless
  Flux_SO2_hourly <- cbind.data.frame(Dates = db$Dates, Depos_SO2 = Flux_SO2_hourly0)
  return(Flux_SO2_hourly)
}
