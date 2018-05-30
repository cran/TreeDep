#' Conc_O3 - Extracts data of hourly concentration of O3
#' @param  x A data frame containing hourly data of O3 concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @return Hourly data of concentration of O3 (micrograms m-3)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Conc_O3(x = Bizkaia_data)
Conc_O3 <- function(x){
  db <- x
  Conc_O3_hourly <- cbind.data.frame(Dates = db$Dates, Concen_O3 = db$O3)
  return(Conc_O3_hourly)
}
