#' Res_Tot_NO2 - Calculates hourly total resistance for NO2
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @return Hourly data of total resistance for NO2 (s m-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Res_Tot_NO2(x = Bizkaia_data)
Res_Tot_NO2 <- function(x){
  db <- x
  Rt_NO20 <- Res_aero(db)[,"Resist_aero"] + Res_boun_NO2(db)[,"Resist_bound_NO2"] +
    Res_cano_NO2(db)[,"Resist_cano"]
  Rt_NO2 <- cbind.data.frame(Dates = db$Dates, Resist_Tot = Rt_NO20)
  return(Rt_NO2)
}
