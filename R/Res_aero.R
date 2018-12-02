#' Res_aero - Calculates aerodynamic resistance on an hourly basis
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @param  z_0 Roughness length value (m)
#' @return Hourly data of aerodynamic resistance (s m-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Res_aero(x = Bizkaia_data, z_0 = 1)
#'
Res_aero <- function(x, z_0 = 1){
  db <- x
  u_z <- db$Wind # mean wind speed at height z (m/s)
  u_. <- Fric_vel(db, z_0 = z_0)
  Ra <- cbind.data.frame(Dates = u_.$Dates, Resist_aero = u_z/(u_.[,"Frict_vel"]^2))
  return(Ra)
}
