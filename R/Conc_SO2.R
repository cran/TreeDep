#' Conc_SO2 - Extracts data of hourly concentration of SO2
#' @param  x A data frame containing hourly data of SO2 concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @return Hourly data of concentration of SO2 (micrograms m-3)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Conc_SO2(x = Bizkaia_data)
Conc_SO2 <- function(x){
  db <- x
  Conc_SO2_hourly <- cbind.data.frame(Dates = db$Dates, Concen_SO2 = db$SO2)
  return(Conc_SO2_hourly)
}
