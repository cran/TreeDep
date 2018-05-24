#' PM10_flux - Calculates annual deposition of PM10 on vegetation
#' @param  x A data frame containing hourly data of PM10 concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @return Annual value of PM10 deposited (g m-2 yr-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' PM10_flux(x = Bizkaia_data)
#'
PM10_flux <- function(x){
  db <- x
  db$LAI_Total <- db$LAI + db$BAI
  #####time
  t_s <- 60*60 # s (seconds in 1 hour)
  # Events of dry deposition (no rain)
  No_rain_event <- rep(NA, length(db$Precip))
  No_rain_event[db$Precip==0] <- 1
  No_rain_event[db$Precip>0] <- 0
    ##################### Deposition Velocity (Vd)
  Vd_PM10_average <- 0.0064 # m/s
  #LAI_PM10 <- 6
  Vd_PM10 <- Vd_PM10_average#*(BAI_h+LAI_h)/(BAI_h+LAI_PM10)
    ##################### F (Flux hourly)
  Flux_PM10_hourly <- db$PM10/1000000*Vd_PM10*t_s*No_rain_event*db$LAI_Total# g/m2/h = g/m3 * m/s * s/h * unitless * unitless
    ##################### F (Flux annual)
  Flux_PM10_annual <- (sum(Flux_PM10_hourly, na.rm=TRUE)/sum(!is.na(Flux_PM10_hourly)))*nrow(db) # g/m2/yr
  myresults <- c(Flux_PM10_annual)
  names(myresults)  <- c("Flux_PM10")
  return(myresults)
}
