#' Conc_CO - Extracts data of hourly concentration of CO
#' @param  x A data frame containing hourly data of CO concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @return Hourly data of concentration of CO (micrograms m-3)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Conc_CO(x = Bizkaia_data)
Conc_CO <- function(x){
  db <- x
  Conc_CO_hourly <- cbind.data.frame(Dates = db$Dates, Concen_CO = db$CO)
  return(Conc_CO_hourly)
}
