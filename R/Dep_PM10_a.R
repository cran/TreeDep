#' Dep_PM10_a - Calculates the annual value of deposition of PM10 on vegetation
#' @param  x A data frame containing hourly data of PM10 concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @param  z_0 Roughness length value (m)
#' @return Annual value of deposition of PM10 on vegetation (g m-2 yr-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Dep_PM10_a(x = Bizkaia_data, z_0 = 1)
Dep_PM10_a <- function(x, z_0 = 1){
  db <- x
  Flux_PM10_hourly <- Dep_PM10(db, z_0 = z_0)[,"Depos_PM10"]
  Flux_PM10_annual <- (sum(Flux_PM10_hourly, na.rm=TRUE)/sum(!is.na(Flux_PM10_hourly)))*nrow(db) # g/m2/yr
  myresults <- c(Flux_PM10_annual)
  names(myresults)  <- c("Annual_Flux_PM10")
  return(myresults)
}
