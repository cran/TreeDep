#' Res_cano_CO - Calculates hourly canopy resistance for CO
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @return Hourly data of canopy resistance for CO (s m-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Res_cano_CO(x = Bizkaia_data)
Res_cano_CO <- function(x){
  db <- x
  LAI_Total <-  db$BAI + db$LAI
  Rc_CO_inleaf <- 50000
  Rc_CO_outleaf <- 1000000
  fit_Rc_CO <- lm(c(Rc_CO_inleaf,Rc_CO_outleaf)~c(max(LAI_Total, na.rm=T),min(LAI_Total, na.rm=T)))
  Rc_CO0 <- fit_Rc_CO$coefficients[1]+fit_Rc_CO$coefficients[2]*LAI_Total
  Rc_CO <- cbind.data.frame(Dates = db$Dates, Resist_cano = Rc_CO0)
  return(Rc_CO)
}
