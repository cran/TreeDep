#' Res_meso_O3 - Calculates hourly mesophyll resistance for O3
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @return Hourly data of mesophyll resistance for O3 (s m-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Res_meso_O3(x = Bizkaia_data)
Res_meso_O3 <- function(x){
  db <- x
  Resistance_out_of_leaf <-  NA # 2800
  r_m_O3 <- rep(10 , nrow(db))
  LAI_Total <-  db$BAI + db$LAI
  r_m_O3[LAI_Total <= median(db$BAI)] <- Resistance_out_of_leaf # s/m
  r_meso_O3 <- cbind.data.frame(Dates = db$Dates, Resist_meso = r_m_O3)
  return(r_meso_O3)
}
