#' SO2_flux - Calculates annual deposition of SO2 on vegetation
#' @param  x A data frame containing hourly data of SO2 concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @return Annual value of SO2 deposited (g m-2 yr-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' SO2_flux(x = Bizkaia_data)
#'
SO2_flux <- function(x){
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
  P <- db$Pres #
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
  Sc_SO2 <- 1.15
  Sc_CO2 <- 1.14
  Pr <- 0.72 # Prandtl number
  Rb_SO2 <- 2*(Sc_SO2^(2/3))*(Pr^(-2/3))*((k*u_.)^(-1))
  Rb_CO2 <- 2*(Sc_CO2^(2/3))*(Pr^(-2/3))*((k*u_.)^(-1))
  ### Canopy Resistance (Rc)
  # Stomata Resistance (r_s)
  Resistance_out_of_leaf <- 2800
  #1. Divide solar radiation above the canopy into visible (PAR) and near-infrared portions
  #2. Divide PAR above the canopy into direct beam and diffuse radiation
  theta <- 60 # solar zenith angle
  #E <- Extinction coefficient (= 0.185)
  E <- 0.185
  #m = Optical air mass
  m <- 1 / (cos(theta * pi / 180))
  #calculate sunlit and shaded leaf areas in each layer of the
  #canopy (Section 2.11.2).
  F_j <- db$LAI_Total
  #Calculate flux density of PAR intercepted by sunlit and shaded leaves in each layer of
  # the canopy
  #C_j = multiple scattering of direct beam radiation
  # beta = solar elevation angle
  beta <- 90 - theta
  C_j = 0.07 * PAR_dir * (1.1 - 0.1 * (F_j - (F_j / 2))) * (exp(-sin(beta * pi / 180)))
  # PAR_shade_j = diffuse PAR (PAR_diff) on shaded leaves
  PAR_shade_j <- PAR_diff * exp(-0.5 * (db$LAI_Total^0.7))+ C_j
  # PAR_sun_j = PAR on sunlit leaves
  alpha <- 60 # angle between the leaf and the sun
  PAR_sun_j <- PAR_dir * ((cos(alpha * pi / 180))/(sin(beta * pi / 180))) + PAR_shade_j
  #Calculate stomatal conductance for sunlit and shaded leaves in each layer of the
  #canopy
  # T_leaf_limit = leaves temperature
  Temp_K <- db$Temp + 273.15
  T_leaf_limit <- 25 + 273.15 # Kelvin
  T_leaf_hot <- rep(NA, length(Temp_K))
  T_leaf_cold <- rep(NA, length(Temp_K))
  Selec_T_leaf_hot <- Temp_K >= T_leaf_limit
  Selec_T_leaf_cold <- Temp_K < T_leaf_limit
  T_leaf_hot[Selec_T_leaf_hot & !is.na(Temp_K)] <- (log(1+
                                                          Temp_K[Selec_T_leaf_hot & !is.na(Temp_K)] -
                                                          T_leaf_limit))^2 + T_leaf_limit
  T_leaf_cold[Selec_T_leaf_cold & !is.na(Temp_K)] <- T_leaf_limit -
    (log(1+T_leaf_limit - Temp_K[Selec_T_leaf_cold & !is.na(Temp_K)]))^2
  T_leaf_df <- cbind.data.frame(T_leaf_hot, T_leaf_cold) # =- T_leaf_cold)
  T_leaf <- rowSums(T_leaf_df, na.rm=T)
  T_leaf[is.na(Temp_K)] <- NA
  T_leaf_df$T_leafdf <- T_leaf
  T_leaf_df$Air_temp <- Temp_K
  # m1 = Dimensionless slope (= 10)
  m1 <- 5
  # Vc(25C)=Carboxylation rate of CO2exchange between leaf and atmosphere (=90)
  Vcmax25 <- 90 #
  # E = Relevant activation energy
  #rh =Relative humidity
  rh <- db$Hum / 100
  # b1 =Zero intercept when A is equal to or less than zero (= 0.02 micro_mol m-2s-1)
  b1 <- 0.02 #(= 0.02 micro_mol m-2s-1)
  # P_O2 = Partial pressures of O2in the intercellular air space (= 210 micro_mol/mol)
  P_O2 <- 210
  # R = Universal gas constant
  R <- 8.3144598 #*1e-6  #(48) J mol-1 K-1 ## From Wikipedia
  #S =constants for Boltzmann distribution temperature function (=710)
  S <- 710
  #H =constants for Boltzmann distribution temperature function (=220,000)
  H <- 220000
  ### kc = Michaelis-Menten coefficients for CO2
  kc25 <- 33.3 # E_rae_kc = Activation energy value for kc	Jmol-1
  E_rae_kc <- 65120
  kc <- kc25 * (exp(((T_leaf - 298) * E_rae_kc)  / (298 * R * T_leaf)))
  ### ko = Michaelis-Menten coefficients for O2(= 295 millibars at 25 C) (29.5 Pa)
  ko25 <- 29.5 # E_rae_ko = Activation energy value for ko	Jmol-1
  E_rae_ko <- 13990
  ko <- ko25 * (exp(((T_leaf - 298) * E_rae_ko)  / (298 * R * T_leaf)))
  ### Vcmax = Carboxylation rate of CO2exchange between leaf and atmosphere (=90)
  Vcmax25	<- 90 # micro_molCO2m-2s-1
  E_rae_Vc <- 64637 # E_rae_Vc = Activation energy value for Vcmax	Jmol-1
  Vcmax <- (Vcmax25 * exp(((T_leaf - 298) * E_rae_Vc)  / (298 * R * T_leaf))) /
    (1 + exp(((S * T_leaf) - H) / (R * T_leaf)))
  ### Jmax = Light-saturated rate of electron transport (= 171 at 25 C for trees)
  Jmax25 <- 171  # micro_mol(e-)m-2s-1
  E_rae_J <- 37000  # Activation energy value for Jmax	Jmol-1
  Jmax <- (Jmax25 * exp(((T_leaf - 298) * E_rae_J)  / (298 * R * T_leaf))) /
    (1 + exp(((S * T_leaf) - H) / (R * T_leaf))) # micro_mol(e-)m-2s-1
  ### Rd = Dark respiration rate of CO2exchange between leaf and atmosphere
  E_rae_Rd <- 51176  # Activation energy value for Rd	Jmol-1
  Rd <- (Vcmax25 * 0.015 * exp(((T_leaf - 298) * E_rae_Rd) / (298 * R * T_leaf))) /
    (1 + exp(1.3 * (T_leaf - 328)))
  # Gamma = CO2 compensation point in the absence of dark respiration
  Gamma <- (0.105 * kc * P_O2) / (ko)
  # alpha0 = Efficiency of light energy conversion on an incident light basis (= 0.22mol electrons/mol photons)
  alpha0 <-	0.22  # mol e-(mol quanta)-1
  J_sun <- (alpha0 * PAR_sun_j) / (sqrt(1 + (((alpha0^2) * (PAR_sun_j^2)) / (Jmax^2)))) # micro_mol(e-)m-2s-1
  J_shade <- (alpha0 * PAR_shade_j) / (sqrt(1 + (((alpha0^2) * (PAR_shade_j^2)) / (Jmax^2)))) # micro_mol(e-)m-2s-1
  J <- (J_sun + J_shade) / 2 # micro_mol(e-)m-2s-1
  ##
  # c_a = Atmosphere's CO2concentration (=360 ppm)
  c_a <- 400 # ppm
  # gb = Conductance across the laminar boundary layer of a leaf (mol m-2s-1) forCO2exchange
  g_b0 <- 1 / (Ra + Rb_CO2)  #(units: m / s)
  g_b1 <- g_b0 * 1e3 #(units: mm / s)
  Converting_factor_conductance0 <- (P * 1e3) / (R * 1e6  * Temp_K) # from (mm s-1) to (mmol m-2 s-1)
  Converting_factor_conductance <- Converting_factor_conductance0 * 1e-3 # from (mmol m-2 s-1) to (mol m-2 s-1)
  g_b2 <- Converting_factor_conductance * g_b1 # (mol m-2 s-1)
  g_b <- g_b2 * 1e6 # from (mol m-2 s-1) to (micro mol m-2 s-1)
  g_b[db$LAI_Total <= median(db$BAI)] <- 0
  alpha2 <- 1 + (b1 / g_b) - (m1 * rh)
  beta2 <- c_a * (g_b * m1 * rh - (2 * b1) - g_b)
  gamma2 <- (c_a^2) * b1 * g_b
  theta2 <- (g_b * m1 * rh) - b1
  # v_c - (0.5 * v_0) = min(w_c, w_j)  * (1 - (Gamma / c_i))
  ####
  #Wc = Carboxylation rate when ribulose bisphosphate (RuBP)
  a2 <- Vcmax
  b2 <- kc * (1 + (P_O2 / ko))
  d2 <- Gamma
  e2 <- 1
  #
  p2 <- (e2 * beta2 + b2 * theta2 - a2 * alpha2 + e2 * alpha2 * Rd) / (e2 * alpha2)
  q2 <- (e2 * gamma2 + b2 * (gamma2 / c_a) - a2 * beta2 + a2 * d2 * theta2 +
           e2 * Rd * beta2 + b2 * theta2 * Rd) / (e2 * alpha2)
  r2 <- ((-a2 * gamma2) + a2 * d2 * (gamma2 / c_a) + e2 * gamma2 * Rd +
           Rd * b2 * (gamma2 / c_a)) / (e2 * alpha2)
  #
  Q2_1 <- ((p2^2) - 3 * q2) / 9
  Q2_1[Q2_1 <= 0] <- 0
  R2_1 <- (2 * (p2^3) - 9 * p2 * q2 + 27 * r2) / 54
  Q2_2 <- Q2_1^3
  Q2_2[Q2_2 <= 0] <- 0
  Q2_3 <- R2_1 / (sqrt(Q2_2))
  #Q2_3[Q2_3 > 1] <- 1
  #Q2_3[Q2_3 < -1] <- -1
  Theta2_1 <- acos(Q2_3)
  # x3 = A_photo
  A_photo <- (-2) * sqrt(Q2_1) * cos((Theta2_1 + 4 * pi) / 3) - (p2 / 3) # Photosynthesis (flux density of a leaf)
  A_photo[A_photo < 0] <- 0 #(micro mol m-2 s-1)
  A_photo[db$LAI_Total <= median(db$BAI)] <- 0 #(micro mol m-2 s-1)
  A_photo[db$Rad <= 0] <- 0 #(micro mol m-2 s-1)
  c_s <- c_a - ((A_photo) / g_b) # c_s=Leaf surface CO2concentration (ppm)
  c_s[c_s < 0 ] <- c_a # (ppm)
  # Stomata conductance
  g_s0 <- ((m1  * A_photo * rh) / (c_s)) + b1 # g_s = stomatal conductance  # micro mol m-2 s-1
  g_s0[g_s0 <= 0] <- b1 # micro mol m-2 s-1
  g_s1 <- g_s0 * 1e-3 # from (micro mol m-2 s-1) to (mmol m-2 s-1)
  g_s2 <- g_s1 / Converting_factor_conductance0  #(= (mmol m-2 s-1) / ((mmol m-2 s-1) / (mm s-1)))
  g_s <- g_s2 * 1e-3 # from (mm s-1) to m s-1
  g_s[db$LAI_Total <= median(db$BAI)] <- 1 / Resistance_out_of_leaf # m s-1
  # Stomata resistance
  r_s <- 1 / g_s  # s m-1
  # Mesophyl resistance (r_m)
  r_m_SO2 <- rep(0 , nrow(db))
  r_m_SO2[db$LAI_Total <= median(db$BAI)] <- Resistance_out_of_leaf # s/m
  # Soil Resistance (r_soil)
  r_soil_inleaf <- 2941 # s/m
  r_soil_outleaf <- 2000 # s/m
  fit_r_soil <- lm(c(r_soil_inleaf,r_soil_outleaf)~c(max(db$LAI_Total, na.rm=T),min(db$LAI_Total, na.rm=T)))
  r_soil <- fit_r_soil$coefficients[1]+fit_r_soil$coefficients[2]*db$LAI_Total
  # Cuticular resistance (r_t)
  r_t_SO2 <- rep(8000 , nrow(db))
  r_t_SO2[db$LAI_Total <= median(db$BAI)] <- Resistance_out_of_leaf # s/m
  # Canopy resistance
  # 1/Rc = 1/(r_s+r_m)+1/r_soil+1/r_t
  Rc_SO2 <- 1/(1/(r_s+r_m_SO2)+1/r_soil+1/r_t_SO2)
  ### Total Resistance (Rt)
  Rt_SO2 <- Ra+Rb_SO2+Rc_SO2
  ##################### Deposition Velocity (Vd)
  Vd_SO2 <- 1 /Rt_SO2
  ##################### F (Flux hourly)
  Flux_SO2_hourly <- db$SO2/1000000*Vd_SO2*t_s*No_rain_event*db$LAI_Total# g/m2/h = g/m3 * m/s * s/h * unitless * unitless
  ##################### F (Flux annual)
  Flux_SO2_annual <- (sum(Flux_SO2_hourly, na.rm=TRUE)/sum(!is.na(Flux_SO2_hourly)))*nrow(db) # g/m2/yr
  myresults <- c(Flux_SO2_annual)
  names(myresults)  <- c("Flux_SO2")
  return(myresults)
}
