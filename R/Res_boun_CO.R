#' Res_boun_CO - Calculates hourly boundary layer resistance for CO
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @return Hourly data of  boundary layer resistance for CO (s m-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Res_boun_CO(x = Bizkaia_data)
Res_boun_CO <- function(x){
  db <- x
  k <- 0.41 # von Karman constant
  Sc_CO <- 0.76 # Schmidt number
  Pr <- 0.72 # Prandtl number
  u_. <- Fric_vel(db)
  Rb_CO <- cbind.data.frame(Dates = u_.$Dates,
                            Resist_bound_CO =
                              2*(Sc_CO^(2/3))*(Pr^(-2/3))*((k*u_.[,"Frict_vel"])^(-1)))
  return(Rb_CO)
}
