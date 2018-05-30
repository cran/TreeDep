#' Res_cano_NO2 - Calculates hourly canopy resistance for NO2
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @return Hourly data of canopy resistance for NO2 (s m-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Res_cano_NO2(x = Bizkaia_data)
Res_cano_NO2 <- function(x){
  db <- x
  Resistance_out_of_leaf <-  2941
  LAI_Total <-  db$BAI + db$LAI
  Rc_NO20 <- 1/(1/(Res_stom_NO2(db)[,"Resist_stom"] + Res_meso_NO2(db)[,"Resist_meso"])
                +1/Res_soil(db)[,"Resist_soil"]+
                  1/Res_cuti_NO2(db)[,"Resist_cuti"])
  Rc_NO20[LAI_Total <= median(db$BAI) ] <- Resistance_out_of_leaf # s/m

  Rc_NO2 <- cbind.data.frame(Dates = db$Dates, Resist_cano = Rc_NO20)
  return(Rc_NO2)
}
