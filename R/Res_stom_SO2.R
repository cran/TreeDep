#' Res_stom_SO2 - Calculates stomata resistance on an hourly basis
#' @param  x A data frame containing hourly data of weather and other variables (e.g. Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @param  m2 Dimensionless slope for different air pollutants
#' @param  m3 Dimensionless slope for different species
#' @return Hourly data of stomata resistance (s m-1)
#' @export
#'
#' @examples
#'
#' data(Bizkaia_data)
#' Res_stom_SO2(x = Bizkaia_data)
Res_stom_SO2 <- function(x, m2=1, m3=4){
  db <- x
  db <- x
  LAI_Total <-  db$BAI + db$LAI

  ##### PAR calculation from Pressure and Radiation data
  theta <- 60 # solar zenith angle
  m <- 1 / (cos(theta*pi/180)) #m = Optical air mass
  #w = Water absorption in the near infrared for 10 mm of precipitable water
  antilog<-function(lx,base) {
    lbx<-lx/log(exp(1),base=base)
    result<-exp(lbx)
    result
  }
  lx <- (-1.195) + 0.4459 * log(m, base = 10) - 0.0345 * (log(m, base = 10))^2
  W <- 1320 * antilog(lx = lx ,base = 10)
  P <- db$Pres #  Actual pressure (kPa)
  P0 <- 101.325# #P0 = Sea level pressure  (kPa)
  # Direct visible component of the solar radiation (RDV)
  R_DV <- 600 * exp(-0.185 * (P/P0) * m) * cos(theta * pi / 180) #(W m-2)
  # Diffuse visible component component of the solar radiation (RdV)
  R_dv <- 0.4 * (600 - R_DV) * cos(theta * pi / 180) #(W m-2)
  # Direct near-infrared component of the solar radiation (RDN)
  R_DN <- (720 * exp(-0.06 * (P/P0) * m) - W) * cos(theta * pi / 180) #(W m-2)
  # Diffuse near-infrared component of the solar radiation (RdN)
  R_dn <-0.6 * (720 - R_DN - W) * cos(theta * pi / 180) #(W m-2)
  # R_V
  R_V <- R_DV + R_dv #(W m-2)
  # R_N
  R_N <- R_DN + R_dn #(W m-2)
  # R_T Measurement of total incoming solar radiation
  R_T <- db$Rad #(W m-2)
  ####
  #Fractions of the beams
  A <- 0.9
  B <- 0.7
  C <- 0.88
  D <- 0.68
  RATIO0 <- R_T / (R_V + R_N) #(dimensionless)
  RATIO1<- replace(RATIO0, RATIO0>A , A) #(dimensionless)
  RATIO<- replace(RATIO1, RATIO1>C , C) #(dimensionless)
  # Fraction of the visible (near-infrared) beam
  f_V0 <- (R_DV / R_V) * (1 - ((A - RATIO) / B)^(2 / 3)) #(dimensionless)
  f_V<- replace(f_V0, f_V0<0 , 0) #(dimensionless)
  # Fraction of the near-infrared beam
  f_N0 <- (R_DN / R_N) * (1 - ((C - RATIO) / D)^(2 / 3)) #(dimensionless)
  f_N<- replace(f_N0, f_N0<0 , 0) #(dimensionless)
  Fc <- 4.6 # Fc = Conversion factor from W m-2 to micro Einstein m-2 s-1
  PAR_dir <- f_V * (0.46 * R_T) * Fc ## units: micro Einstein m-2 s-1
  PAR_diff <- (1 - f_V) * (0.46 * R_T) * Fc ## units: micro Einstein m-2 s-1



  Resistance_out_of_leaf <-  NA # 2800
  theta <- 60 # solar zenith angle
  E <- 0.185 #Extinction coefficient (dimensionless)
  m <- 1 / (cos(theta * pi / 180)) # m = Optical air mass (dimensionless)
  F_j <- LAI_Total
  beta <- 90 - theta # beta = solar elevation angle(dimensionless)
  #C_j = multiple scattering of direct beam radiation
  C_j = 0.07 * PAR_dir * (1.1 - 0.1 * (F_j - (F_j / 2))) * (exp(-sin(beta * pi / 180))) ## units: micro Einstein m-2 s-1
  # PAR_shade_j = diffuse PAR (PAR_diff) on shaded leaves
  PAR_shade_j <- PAR_diff * exp(-0.5 * (LAI_Total^0.7))+ C_j ## units: micro Einstein m-2 s-1
  alpha <- 60 # angle between the leaf and the sun
  # PAR_sun_j = PAR on sunlit leaves
  PAR_sun_j <- PAR_dir * ((cos(alpha * pi / 180))/(sin(beta * pi / 180))) + PAR_shade_j ## units: micro Einstein m-2 s-1


  Temp_K <- db$Temp + 273.15
  T_leaf_limit <- 25 + 273.15 # Kelvin # T_leaf_limit = leaves temperature
  T_leaf_hot <- rep(NA, length(Temp_K))
  T_leaf_cold <- rep(NA, length(Temp_K))
  Selec_T_leaf_hot <- Temp_K >= T_leaf_limit
  Selec_T_leaf_cold <- Temp_K < T_leaf_limit
  T_leaf_hot[Selec_T_leaf_hot & !is.na(Temp_K)] <- (log(1+
                                                          Temp_K[Selec_T_leaf_hot & !is.na(Temp_K)] -
                                                          T_leaf_limit))^2 + T_leaf_limit
  T_leaf_cold[Selec_T_leaf_cold & !is.na(Temp_K)] <- T_leaf_limit -
    (log(1+T_leaf_limit - Temp_K[Selec_T_leaf_cold & !is.na(Temp_K)]))^2
  T_leaf_df <- cbind.data.frame(T_leaf_hot, T_leaf_cold)
  T_leaf <- rowSums(T_leaf_df, na.rm=T)
  T_leaf[is.na(Temp_K)] <- NA
  T_leaf_df$T_leafdf <- T_leaf
  T_leaf_df$Air_temp <- Temp_K


  # m3 = 3 Dimensionless slope for different species
  # m2 = 1 Dimensionless slope for different air pollutants
  m1 <- m2 * m3 # m1 = Dimensionless slope
  # Vc(25C)=Carboxylation rate of CO2exchange between leaf and atmosphere
  Vcmax25 <- 90 # (micro_mol m-2s-1)
  #rh =Relative humidity
  rh <- db$Hum / 100 # (dimensionless)
  # b1 =Zero intercept when A is equal to or less than zero (= 0.02 micro_mol m-2s-1)
  b1 <- 0.02 #(= 0.02 micro_mol m-2s-1)
  # P_O2 = Partial pressures of O2in the intercellular air space
  P_O2 <- 210  #(micro_mol/mol)
  # R = Universal gas constant
  R <- 8.3144598 # J mol-1 K-1
  #S =constants for Boltzmann distribution temperature function
  S <- 710 # (dimensionless)
  #H =constants for Boltzmann distribution temperature function
  H <- 220000 # (dimensionless)
  ### kc = Michaelis-Menten coefficients for CO2
  kc25 <- 33.3 # (dimensionless)
  E_rae_kc <- 65120 # E_rae_kc = Activation energy value for kc	Jmol-1
  kc <- kc25 * (exp(((T_leaf - 298) * E_rae_kc)  / (298 * R * T_leaf)))  # (dimensionless)
  ### ko = Michaelis-Menten coefficients for O2
  ko25 <- 29.5 # (dimensionless)
  E_rae_ko <- 13990 # E_rae_ko = Activation energy value for ko	Jmol-1
  ko <- ko25 * (exp(((T_leaf - 298) * E_rae_ko)  / (298 * R * T_leaf))) # (dimensionless)
  ### Vcmax = Carboxylation rate of CO2exchange between leaf and atmosphere
  Vcmax25	<- 90 # micro_molCO2m-2s-1
  E_rae_Vc <- 64637 # E_rae_Vc = Activation energy value for Vcmax	Jmol-1
  Vcmax <- (Vcmax25 * exp(((T_leaf - 298) * E_rae_Vc)  / (298 * R * T_leaf))) /
    (1 + exp(((S * T_leaf) - H) / (R * T_leaf)))  # micro_mol m-2s-1
  ### Jmax = Light-saturated rate of electron transport for trees
  Jmax25 <- 171  # micro_mol(e-)m-2s-1
  E_rae_J <- 37000  # Activation energy value for Jmax	Jmol-1
  Jmax <- (Jmax25 * exp(((T_leaf - 298) * E_rae_J)  / (298 * R * T_leaf))) /
    (1 + exp(((S * T_leaf) - H) / (R * T_leaf))) # micro_mol(e-)m-2s-1
  ### Rd = Dark respiration rate of CO2exchange between leaf and atmosphere
  E_rae_Rd <- 51176  # Activation energy value for Rd	Jmol-1
  Rd <- (Vcmax25 * 0.015 * exp(((T_leaf - 298) * E_rae_Rd) / (298 * R * T_leaf))) /
    (1 + exp(1.3 * (T_leaf - 328)))  # micro_mol m-2s-1
  # Gamma = CO2 compensation point in the absence of dark respiration
  Gamma <- (0.105 * kc * P_O2) / (ko) # (dimensionless)
  # alpha0 = Efficiency of light energy conversion on an incident light basis (= 0.22mol electrons/mol photons)
  alpha0 <-	0.22  # micro_mol(e-) micro Einstein -1
  J_sun <- (alpha0 * PAR_sun_j) / (sqrt(1 + (((alpha0^2) * (PAR_sun_j^2)) / (Jmax^2)))) # micro_mol(e-)m-2s-1
  J_shade <- (alpha0 * PAR_shade_j) / (sqrt(1 + (((alpha0^2) * (PAR_shade_j^2)) / (Jmax^2)))) # micro_mol(e-)m-2s-1
  J <- (J_sun + J_shade) / 2 # micro_mol(e-) m-2 s-1
  ##
  # c_a = Atmosphere's CO2concentration
  c_a <- 400 # ppm
  # gb = Conductance across the laminar boundary layer of a leaf (mol m-2s-1) forCO2exchange
  g_b0 <- 1 / (Res_aero(db)[,"Resist_aero"] + Res_boun_CO2(db)[,"Resist_bound_CO2"])  #(units: m s-1)
  Converting_factor_conductance <- (P * 1e3) / (R  * Temp_K) # from (m s-1) to (micro mol m-2 s-1)
  g_b <- g_b0 * Converting_factor_conductance # (micro mol m-2 s-1)
  g_b[LAI_Total <= median(db$BAI)] <- 0 # (micro mol m-2 s-1)

  alpha2 <- 1 + (b1 / g_b) - (m1 * rh)
  beta2 <- c_a * (g_b * m1 * rh - (2 * b1) - g_b)
  gamma2 <- (c_a^2) * b1 * g_b
  theta2 <- (g_b * m1 * rh) - b1

  a2 <- Vcmax
  b2 <- kc * (1 + (P_O2 / ko))
  d2 <- Gamma
  e2 <- 1

  p2 <- (e2 * beta2 + b2 * theta2 - a2 * alpha2 + e2 * alpha2 * Rd) / (e2 * alpha2)
  q2 <- (e2 * gamma2 + b2 * (gamma2 / c_a) - a2 * beta2 + a2 * d2 * theta2 +
           e2 * Rd * beta2 + b2 * theta2 * Rd) / (e2 * alpha2)
  r2 <- ((-a2 * gamma2) + a2 * d2 * (gamma2 / c_a) + e2 * gamma2 * Rd +
           Rd * b2 * (gamma2 / c_a)) / (e2 * alpha2)

  Q2_1 <- ((p2^2) - 3 * q2) / 9
  Q2_1[Q2_1 <= 0] <- 0
  R2_1 <- (2 * (p2^3) - 9 * p2 * q2 + 27 * r2) / 54
  Q2_2 <- Q2_1^3
  Q2_2[Q2_2 <= 0] <- 0
  Q2_3 <- R2_1 / (sqrt(Q2_2))
  Q2_3[Q2_3 > 1] <- NA
  Q2_3[Q2_3 < -1] <- NA
  Theta2_1 <- acos(Q2_3)

  A_photo <- (-2) * sqrt(Q2_1) * cos((Theta2_1 + 4 * pi) / 3) - (p2 / 3) # Photosynthesis (flux density of a leaf)
  A_photo[A_photo < 0] <- 0 #(micro mol m-2 s-1)
  A_photo[LAI_Total <= median(db$BAI)] <- 0 #(micro mol m-2 s-1)
  A_photo[db$Daylight=="Night"&!is.na(A_photo)] <- 0 #(micro mol m-2 s-1)

  c_s <- c_a - ((A_photo) / g_b) # c_s=Leaf surface CO2concentration (ppm)
  c_s[c_s < 0 ] <- c_a # (ppm)

  # Stomata conductance
  g_s0 <- ((m1  * A_photo * rh) / (c_s)) + b1 # g_s = stomatal conductance  # micro mol m-2 s-1
  g_s0[g_s0 <= 0] <- b1 # micro mol m-2 s-1
  g_s <- g_s0 / Converting_factor_conductance # m s-1
  g_s[LAI_Total <= median(db$BAI)] <- 1 / Resistance_out_of_leaf # m s-1
  # Stomata resistance
  r_s <- cbind.data.frame(Dates = db$Dates, Resist_stom = 1 / g_s)  # s m-1
  return(r_s)
}
