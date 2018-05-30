#' Dep_SO2_a - Calculates the annual value of deposition of SO2 on vegetation
#' @param  x A data frame containing hourly data of SO2 concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @return Annual value of deposition of SO2 on vegetation (g m-2 yr-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Dep_SO2_a(x = Bizkaia_data)
Dep_SO2_a <- function(x){
  db <- x
  Flux_SO2_hourly <- Dep_SO2(db)[,"Depos_SO2"]
  Flux_SO2_annual <- (sum(Flux_SO2_hourly, na.rm=TRUE)/sum(!is.na(Flux_SO2_hourly)))*nrow(db) # g/m2/yr
  myresults <- c(Flux_SO2_annual)
  names(myresults)  <- c("Annual_Flux_SO2")
  return(myresults)
}
