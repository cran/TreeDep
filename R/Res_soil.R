#' Res_soil - Calculates soil resistance on an hourly basis
#' @param  x A data frame containing hourly data of weather and other variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @param  r_soil_inleaf Resistance value during in-leaf season
#' @param  r_soil_outleaf Resistance value during in-leaf season
#' @return Hourly data of soil resistance (s m-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Res_soil(x = Bizkaia_data, r_soil_inleaf = 2941, r_soil_outleaf = 2941)
Res_soil <- function(x,
                     r_soil_inleaf = 2941,
                     r_soil_outleaf = 2941){
    db <- x
    #r_soil_inleaf <- 2941 # s/m
    #r_soil_outleaf <- 2941 # 2000 # s/m
    LAI_Total <-  db$BAI + db$LAI

    if((max(LAI_Total, na.rm=T) == min(LAI_Total, na.rm=T)) &
       (r_soil_inleaf == r_soil_outleaf)){
      r_soil0 <- rep(r_soil_inleaf, nrow(db))
      r_soil <- cbind.data.frame(Dates = db$Dates, Resist_soil = r_soil0)
    }else{
      fit_r_soil <- lm(c(r_soil_inleaf,r_soil_outleaf)~c(max(LAI_Total, na.rm=T),min(LAI_Total, na.rm=T)))
      r_soil0 <- fit_r_soil$coefficients[1]+fit_r_soil$coefficients[2]*LAI_Total
      r_soil <- cbind.data.frame(Dates = db$Dates, Resist_soil = r_soil0)
    }
    return(r_soil)
}
