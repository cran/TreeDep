#' Conc_PM10 - Extracts data of hourly concentration of PM10
#' @param  x A data frame containing hourly data of PM10 concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @return Hourly data of concentration of PM10 (micrograms m-3)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Conc_PM10(x = Bizkaia_data)
Conc_PM10 <- function(x){
  db <- x
  Conc_PM10_hourly <- cbind.data.frame(Dates = db$Dates, Concen_PM10 = db$PM10)
  return(Conc_PM10_hourly)
}
