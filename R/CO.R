#' CO_flux - Calculates annual deposition of CO on vegetation
#' @param  x A data frame containing hourly data of CO concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @return Annual value of CO deposited (g m-2 yr-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' CO_flux(x = Bizkaia_data)
#'
CO_flux <- function(x){
  db <- x
  db$LAI_Total <- db$LAI + db$BAI
  #####time
  t_s <- 60*60 # s (seconds in 1 hour)
  ##### PAR calculation from Pressure and Radiation data
  theta <- 60 # solar zenith angle
  #E <- Extinction coefficient (= 0.185)
  #m = Optical air mass
  m <- 1 / (cos(theta*pi/180))
  #w = Water absorption in the near infrared for 10 mm of precipitable water
  antilog<-function(lx,base) {
    lbx<-lx/log(exp(1),base=base)
    result<-exp(lbx)
    result
  }
  lx <- (-1.195) + 0.4459 * log(m, base = 10) - 0.0345 * (log(m, base = 10))^2
  W <- 1320 * antilog(lx = lx ,base = 10)
  # P <- Actual pressure (kPa)
  P <- db$Pres #  (kPa)
  #P0 = Sea level pressure (= 101.325 kPa)
  P0 <- 101.325
  # Direct visible component of the solar radiation (RDV) (Weiss and Norman 1985)
  R_DV <- 600 * exp(-0.185 * (P/P0) * m) * cos(theta * pi / 180)
  # Diffuse visible component component of the solar radiation (RdV) (Weiss and Norman 1985)
  R_dv <- 0.4 * (600 - R_DV) * cos(theta * pi / 180)
  # Direct near-infrared component of the solar radiation (RDN) (Weiss and Norman 1985)
  R_DN <- (720 * exp(-0.06 * (P/P0) * m) - W) * cos(theta * pi / 180)
  # Diffuse near-infrared component of the solar radiation (RdN)(Weiss and Norman 1985)
  R_dn <-0.6 * (720 - R_DN - W) * cos(theta * pi / 180)
  # R_V
  R_V <- R_DV + R_dv
  # R_N
  R_N <- R_DN + R_dn
  # R_T Measurement of total incoming solar radiation
  R_T <- db$Rad #(W m^-2)
  ####
  #Fractions of the beams
  A <- 0.9
  B <- 0.7
  C <- 0.88
  D <- 0.68
  RATIO0 <- R_T / (R_V + R_N)
  #summary(RATIO0)
  RATIO1<- replace(RATIO0, RATIO0>A , A)
  #summary(RATIO1)
  RATIO<- replace(RATIO1, RATIO1>C , C)
  #summary(RATIO)
  # Fraction of the visible (near-infrared) beam
  f_V0 <- (R_DV / R_V) * (1 - ((A - RATIO) / B)^(2 / 3))
  f_V<- replace(f_V0, f_V0<0 , 0)
  # Fraction of the near-infrared beam
  f_N0 <- (R_DN / R_N) * (1 - ((C - RATIO) / D)^(2 / 3))
  f_N<- replace(f_N0, f_N0<0 , 0)
  # Fc = Conversion factor from W m
  Fc <- 4.6
  PAR_dir <- f_V * (0.46 * R_T) * Fc ## units: micro Einstein m-2 s-1
  PAR_diff <- (1 - f_V) * (0.46 * R_T) * Fc ## units: micro Einstein m-2 s-1
  ## Assumption that PAR = PAR_dir + PAR_diff
  #PAR <- ((PAR_dir + PAR_diff) / 2) #units: micro E m-2 s-1    / Fc ## units: w m-2
  Convert_PAR <- 0.22 # 1?mol photons m-2s-1 = 0.20 to 0.25 Wm-2
  PAR0 <- 0.46 * R_T  #(Wm-2)
  PAR <- PAR0 / Convert_PAR # 1?mol photons m-2s-1
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
  z_0 <- 1 # roughness length (see wikipedia values) (m)
  #### L Monin-Obukhov length,
  Pasquill_db$L_MoninObukhov[Pasquill_db$Category=="A"] <- 1/(-0.0875*(z_0^(-0.1029)))
  Pasquill_db$L_MoninObukhov[Pasquill_db$Category=="B"] <- 1/(-0.03849*(z_0^(-0.1714)))
  Pasquill_db$L_MoninObukhov[Pasquill_db$Category=="C"] <- 1/(-0.0807*(z_0^(-0.3049)))
  Pasquill_db$L_MoninObukhov[Pasquill_db$Category=="D"] <- 100000000000000#==1/(0*(z_0^(-0)))
  Pasquill_db$L_MoninObukhov[Pasquill_db$Category=="E"] <- 1/(0.0807*(z_0^(-0.3049)))
  Pasquill_db$L_MoninObukhov[Pasquill_db$Category=="F"] <- 1/(0.03849*(z_0^(-0.1714)))
  #table(Pasquill_db$L_MoninObukhov)
  # Friction velocity ### Neutral atmosphere (L=0)
  u_._neutral <- (k*u_z)/(log((z-d/z_0)))
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
    (log(((z-d)/z_0)) -
       Psi_unstable *
       ((z-d)/Pasquill_db$L_MoninObukhov) +
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
  u_. <- Pasquill_db$u_.### # Friction velocity in neutral, stable and unstable atmospheres
  Ra <- u_z/(u_.^2)
  ### Boundary layer Resistance (Rb)
  Sc_CO <- 0.76
  Sc_CO2 <- 1.14
  Pr <- 0.72 # Prandtl number
  Rb_CO <- 2*(Sc_CO^(2/3))*(Pr^(-2/3))*((k*u_.)^(-1))
  Rb_CO2 <- 2*(Sc_CO2^(2/3))*(Pr^(-2/3))*((k*u_.)^(-1))
  ### Canopy Resistance (Rc)
  # Canopy resistance
  Rc_CO_inleaf <- 50000
  Rc_CO_outleaf <- 1000000
  fit_Rc_CO <- lm(c(Rc_CO_inleaf,Rc_CO_outleaf)~c(max(db$LAI_Total, na.rm=T),min(db$LAI_Total, na.rm=T)))
  Rc_CO <- fit_Rc_CO$coefficients[1]+fit_Rc_CO$coefficients[2]*db$LAI_Total
  ### Total Resistance (Rt)
  Rt_CO <- Ra+Rb_CO+Rc_CO
  ##################### Deposition Velocity (Vd)
  Vd_CO <- 1 /Rt_CO
  ##################### F (Flux hourly)
  Flux_CO_hourly <- db$CO/1000000*Vd_CO*t_s*No_rain_event *db$LAI_Total # g/m2/h = g/m3 * m/s * s/h * unitless * unitless
  ##################### F (Flux annual)
  Flux_CO_annual <- (sum(Flux_CO_hourly, na.rm=TRUE)/sum(!is.na(Flux_CO_hourly)))*nrow(db) # g/m2/yr
  myresults <- c(Flux_CO_annual)
  names(myresults)  <- c("Flux_CO")
  return(myresults)
}
