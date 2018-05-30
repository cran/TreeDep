#' Dep_NO2_a - Calculates the annual value of deposition of NO2 on vegetation
#' @param  x A data frame containing hourly data of NO2 concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @return Annual value of deposition of NO2 on vegetation (g m-2 yr-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Dep_NO2_a(x = Bizkaia_data)
Dep_NO2_a <- function(x){
  db <- x
  Flux_NO2_hourly <- Dep_NO2(db)[,"Depos_NO2"]
  Flux_NO2_annual <- (sum(Flux_NO2_hourly, na.rm=TRUE)/sum(!is.na(Flux_NO2_hourly)))*nrow(db) # g/m2/yr
  myresults <- c(Flux_NO2_annual)
  names(myresults)  <- c("Annual_Flux_NO2")
  return(myresults)
}
