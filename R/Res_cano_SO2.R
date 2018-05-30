#' Res_cano_SO2 - Calculates hourly canopy resistance for SO2
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @return Hourly data of canopy resistance for SO2 (s m-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Res_cano_SO2(x = Bizkaia_data)
Res_cano_SO2 <- function(x){
  db <- x
  Resistance_out_of_leaf <-  2941
  LAI_Total <-  db$BAI + db$LAI
  Rc_SO20 <- 1/(1/(Res_stom_SO2(db)[,"Resist_stom"] + Res_meso_SO2(db)[,"Resist_meso"])
                +1/Res_soil(db)[,"Resist_soil"]+
                  1/Res_cuti_SO2(db)[,"Resist_cuti"])
  Rc_SO20[LAI_Total <= median(db$BAI) ] <- Resistance_out_of_leaf # s/m

  Rc_SO2 <- cbind.data.frame(Dates = db$Dates, Resist_cano = Rc_SO20)
  return(Rc_SO2)
}
