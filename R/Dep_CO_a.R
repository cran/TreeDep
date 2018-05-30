#' Dep_CO_a - Calculates the annual value of deposition of CO on vegetation
#' @param  x A data frame containing hourly data of CO concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @return Annual value of deposition of CO on vegetation (g m-2 yr-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Dep_CO_a(x = Bizkaia_data)
Dep_CO_a <- function(x){
  db <- x
  Flux_CO_hourly <- Dep_CO(db)[,"Depos_CO"]
  Flux_CO_annual <- (sum(Flux_CO_hourly, na.rm=TRUE)/sum(!is.na(Flux_CO_hourly)))*nrow(db) # g/m2/yr
  myresults <- c(Flux_CO_annual)
  names(myresults)  <- c("Annual_Flux_CO")
  return(myresults)
}
