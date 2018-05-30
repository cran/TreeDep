#' Res_cuti_O3 - Calculates hourly cuticular resistance for O3
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @return Hourly data of cuticular resistance for O3 (s m-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Res_cuti_O3(x = Bizkaia_data)
Res_cuti_O3 <- function(x){
  db <- x
  Resistance_out_of_leaf <- NA # 2800
  LAI_Total <-  db$BAI + db$LAI
  r_t_O3 <- rep(10000 , nrow(db))
  r_t_O3[LAI_Total <= median(db$BAI) ] <- Resistance_out_of_leaf # s/m
  r_cuti_O3 <- cbind.data.frame(Dates = db$Dates, Resist_cuti = r_t_O3)
  return(r_cuti_O3)
}
