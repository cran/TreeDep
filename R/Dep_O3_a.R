#' Dep_O3_a - Calculates the annual value of deposition of O3 on vegetation
#' @param  x A data frame containing hourly data of O3 concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @return Annual value of deposition of O3 on vegetation (g m-2 yr-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Dep_O3_a(x = Bizkaia_data)
Dep_O3_a <- function(x){
  db <- x
  Flux_O3_hourly <- Dep_O3(db)[,"Depos_O3"]
  Flux_O3_annual <- (sum(Flux_O3_hourly, na.rm=TRUE)/sum(!is.na(Flux_O3_hourly)))*nrow(db) # g/m2/yr
  myresults <- c(Flux_O3_annual)
  names(myresults)  <- c("Annual_Flux_O3")
  return(myresults)
}
