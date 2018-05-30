#' Res_cano_O3 - Calculates hourly canopy resistance for O3
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @return Hourly data of canopy resistance for O3 (s m-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Res_cano_O3(x = Bizkaia_data)
Res_cano_O3 <- function(x){
  db <- x
  Resistance_out_of_leaf <-  2941
  LAI_Total <-  db$BAI + db$LAI
  Rc_O30 <- 1/(1/(Res_stom_O3(db)[,"Resist_stom"] + Res_meso_O3(db)[,"Resist_meso"])
               +1/Res_soil(db)[,"Resist_soil"]+
                 1/Res_cuti_O3(db)[,"Resist_cuti"])
  Rc_O30[LAI_Total <= median(db$BAI) ] <- Resistance_out_of_leaf # s/m

  Rc_O3 <- cbind.data.frame(Dates = db$Dates, Resist_cano = Rc_O30)
  return(Rc_O3)
}
