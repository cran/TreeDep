#' Res_boun_O3 - Calculates hourly boundary layer resistance for O3
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @param  z_0 Roughness length value (m)
#' @return Hourly data of  boundary layer resistance for O3 (s m-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Res_boun_O3(x = Bizkaia_data, z_0 = 1)
Res_boun_O3 <- function(x, z_0 = 1){
  db <- x
  k <- 0.41 # von Karman constant
  Sc_O3 <- 1  # Schmidt number
  Pr <- 0.72 # Prandtl number
  u_. <- Fric_vel(db, z_0 = z_0)
  Rb_O3 <- cbind.data.frame(Dates = u_.$Dates,
                            Resist_bound_O3 =
                              2*(Sc_O3^(2/3))*(Pr^(-2/3))*((k*u_.[,"Frict_vel"])^(-1)))
  return(Rb_O3)
}
