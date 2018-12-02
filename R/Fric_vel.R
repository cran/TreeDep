#' Fric_vel - Calculates friction velocity on an hourly basis
#' @param  x A data frame containing hourly data of weather variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight))
#' @param  z_0 Roughness length value (m)
#' @return Hourly data of friction velocity (m s-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Fric_vel(x = Bizkaia_data, z_0 = 1)
Fric_vel <- function(x, z_0 = 1){
  db <- x

  db$LAI_Total <- db$LAI + db$BAI
  #####time
  t_s <- 60*60 # s (seconds in 1 hour)

  # Events of dry deposition (no rain)
  No_rain_event <- rep(NA, length(db$Precip))
  No_rain_event[db$Precip==0] <- 1
  No_rain_event[db$Precip>0] <- 0
  Pasquill_db <- cbind.data.frame(Day=db$Date,
                                  Light=db$Daylight,
                                  Wind = db$Wind)
  Pasquill_db$Category <- "NA"
  Pasquill_db$Category[Pasquill_db$Light=="Daylight" & Pasquill_db$Wind >=6] <- "D"
  Pasquill_db$Category[Pasquill_db$Light=="Daylight" & (Pasquill_db$Wind >=5 &
                                                          Pasquill_db$Wind < 6)] <- "C"
  Pasquill_db$Category[Pasquill_db$Light=="Daylight" & (Pasquill_db$Wind >=3 &
                                                          Pasquill_db$Wind < 5)] <- "B"
  Pasquill_db$Category[Pasquill_db$Light=="Daylight" & (Pasquill_db$Wind >=2 &
                                                          Pasquill_db$Wind < 3)] <- "B"
  Pasquill_db$Category[Pasquill_db$Light=="Daylight"&Pasquill_db$Wind <2] <- "A"
  Pasquill_db$Category[Pasquill_db$Light=="Night" & Pasquill_db$Wind >=6] <- "D"
  Pasquill_db$Category[Pasquill_db$Light=="Night" & (Pasquill_db$Wind >=5 &
                                                       Pasquill_db$Wind < 6)] <- "D"
  Pasquill_db$Category[Pasquill_db$Light=="Night" & (Pasquill_db$Wind >=3 &
                                                       Pasquill_db$Wind < 5)] <- "E"
  Pasquill_db$Category[Pasquill_db$Light=="Night" & (Pasquill_db$Wind >=2 &
                                                       Pasquill_db$Wind < 3)] <- "E"
  Pasquill_db$Category[Pasquill_db$Light=="Night"&Pasquill_db$Wind <2] <- "F"

  ### Aerodynamic Resistance (Ra)
  k <- 0.41 # von Karman constant
  u_z <- db$Wind # mean wind speed at height z (m/s)
  z <- 5 # height of the weather station (m)
  Canopy_height <- 5 # we assume that there are no trees in the weather station (m)
  d <- 0.67*Canopy_height # (m)
  #z_0 <- 1 # roughness length (see wikipedia values) (m)

  #### L Monin-Obukhov length,
  Pasquill_db$L_MoninObukhov[Pasquill_db$Category=="A"] <- 1/(-0.0875*(z_0^(-0.1029)))
  Pasquill_db$L_MoninObukhov[Pasquill_db$Category=="B"] <- 1/(-0.03849*(z_0^(-0.1714)))
  Pasquill_db$L_MoninObukhov[Pasquill_db$Category=="C"] <- 1/(-0.0807*(z_0^(-0.3049)))
  Pasquill_db$L_MoninObukhov[Pasquill_db$Category=="D"] <- 100000000000000#==1/(0*(z_0^(-0)))
  Pasquill_db$L_MoninObukhov[Pasquill_db$Category=="E"] <- 1/(0.0807*(z_0^(-0.3049)))
  Pasquill_db$L_MoninObukhov[Pasquill_db$Category=="F"] <- 1/(0.03849*(z_0^(-0.1714)))
  #table(Pasquill_db$L_MoninObukhov)

  # Friction velocity ### Neutral atmosphere (L=0)
  u_._neutral <- (k*u_z)/(log((z-(d/Canopy_height)/z_0)))
  Pasquill_db$u_._neutral <- rep(NA, nrow(Pasquill_db))
  Pasquill_db$u_._neutral[Pasquill_db$L_MoninObukhov==0 & !is.na(Pasquill_db$L_MoninObukhov)] <-
    u_._neutral[Pasquill_db$L_MoninObukhov==0 & !is.na(Pasquill_db$L_MoninObukhov)]
  # Friction velocity ### Unstable atmosphere (L<0)
  #1: calculation of x
  x_unstable <- rep(NA,nrow(Pasquill_db))
  #L_unstable <-Pasquill_db$L_MoninObukhov)
  x_unstable <- 1/
    ((1-(28*(z/Pasquill_db$L_MoninObukhov)))^0.25)
  #2: calculation of Stability function for momentum (Psi_M)
  Psi_unstable <- 2 * log((1+x_unstable)/2)+
    log((1+(x_unstable)^2)/2)-
    2*((tan(x_unstable))^-1)+pi/2
  #2: Friction velocity in an Unstable atmosphere (L<0)
  u_._unstable <- (k*u_z) /
    (log(((z-(d/Canopy_height))/z_0)) -
       Psi_unstable *
       ((z-(d/Canopy_height))/Pasquill_db$L_MoninObukhov) +
       Psi_unstable *
       ((z_0)/Pasquill_db$L_MoninObukhov))
  Pasquill_db$u_._unstable <- rep(NA, nrow(Pasquill_db))
  Pasquill_db$u_._unstable[Pasquill_db$L_MoninObukhov<0 & !is.na(Pasquill_db$L_MoninObukhov)] <-
    u_._unstable[Pasquill_db$L_MoninObukhov<0 & !is.na(Pasquill_db$L_MoninObukhov)]

  # Friction velocity ### Stable atmosphere (L>0)
  Temp_Kelvin <- db$Temp +273.15
  Cloud_cover <- rep(NA, nrow(Pasquill_db))
  my_cloud_db <- data.frame(Humidity=c(quantile(db$Hum, 0.10, na.rm=TRUE),quantile(db$Hum, 0.90, na.rm=TRUE)),Cloud_cover = c(0,100))
  fit_clod_hum <-lm(Cloud_cover~Humidity, data=my_cloud_db)
  Cloud_cover <- fit_clod_hum$coefficients[1] + db$Hum * fit_clod_hum$coefficients[2]
  Cloud_cover[db$Hum>=quantile(db$Hum, 0.90, na.rm=TRUE)] <- 100
  Cloud_cover[db$Hum<=quantile(db$Hum, 0.10, na.rm=TRUE)] <- 0
  theta_ <- 0.09 * (1 - 0.5 * (Cloud_cover/100)^2) ## calculated as US EPA (1995)
  Beta_m <- 4.7 # Dimensionless constant
  u0 <- ((Beta_m * z * 9.81 * theta_) / Temp_Kelvin)^0.5
  CDN <- (k) / (log((z/z_0))) #Neutral drag coefficient (Dimensionless)
  u_cr <- ((4 / CDN)^0.5) * u0
  u_.cr <- CDN * u_cr / 2
  u_._stable <- rep(NA, nrow(Pasquill_db))
  u_._stable0a <- CDN * u_z * (1/2 + (1/2) * (1 - ((2 * u0) / ((CDN^0.5) * u_z))^2)^0.5)
  u_._stable0b <- u_.cr * (u_z / u_cr)
  condition <- (2 * u0) / ((CDN * u_z)^0.5)# condition must hold to use this equation
  u_._stable[condition <= 1 & !is.na(condition)] <- u_._stable0a[condition <= 1 & !is.na(condition)]
  u_._stable[condition > 1 & !is.na(condition)] <- u_._stable0b[condition > 1 & !is.na(condition)]
  u_._stable[is.na(u_._stable)] <- u_._stable0b[is.na(u_._stable)]
  Pasquill_db$u_._stable <- rep(NA, nrow(Pasquill_db))
  Pasquill_db$u_._stable[Pasquill_db$L_MoninObukhov>0 & !is.na(Pasquill_db$L_MoninObukhov)] <-
    u_._stable[Pasquill_db$L_MoninObukhov>0 & !is.na(Pasquill_db$L_MoninObukhov)]
  ## # Friction velocity in neutral, stable and unstable atmospheres
  Pasquill_db$u_. <- rowSums(cbind(Pasquill_db$u_._neutral ,Pasquill_db$u_._stable , Pasquill_db$u_._unstable), na.rm=TRUE)
  Pasquill_db$u_.[is.na(Pasquill_db$u_._neutral) & is.na(Pasquill_db$u_._stable) & is.na(Pasquill_db$u_._unstable)] <- NA
  u_. <- cbind.data.frame(Dates = Pasquill_db$Day, Frict_vel = Pasquill_db$u_.) ### # Friction velocity in neutral, stable and unstable atmospheres
  if(z_0 > d){
    warning("Warning: Roughness length (z_0) was too high")
  }
  return(u_.)
}

