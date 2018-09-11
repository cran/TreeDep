#' TreeDep_plot - Generates a plot for selected variables and dates.
#' @details The variables that can be plotted are: "Hum", "Pres", "Precip", "Rad", "Temp", "Wind", "BAI", "LAI", "Fric_vel", "Res_aero", "Res_boun_CO2", "Res_soil", "Conc_NO2", "Dep_NO2", "Dep_vel_NO2", "Res_boun_NO2", "Res_cuti_NO2", "Res_stom_NO2", "Res_meso_NO2", "Res_cano_NO2", "Res_Tot_NO2", "Conc_O3", "Dep_O3", "Dep_vel_O3", "Res_boun_O3", "Res_cuti_O3", "Res_stom_O3", "Res_meso_O3", "Res_cano_O3", "Res_Tot_O3", "Conc_SO2", "Dep_SO2", "Dep_vel_SO2", "Res_boun_SO2", "Res_cuti_SO2", "Res_stom_SO2", "Res_meso_SO2", "Res_cano_SO2", "Res_Tot_SO2",  "Conc_CO", "Dep_CO", "Dep_vel_CO", "Res_boun_CO", "Res_cano_CO", "Res_Tot_CO", "Conc_PM10", "Dep_PM10", "Dep_vel_PM10".
#' @param  my_data A data frame containing hourly data pollutant concentration and other variables (Dates (e.g. 01/01/2016  00:00:00), Hum (\%), Pres (kPa), Precip (mm), Rad (W m-2), Temp (C), Wind (m s-1), Daylight (Night or Daylight), BAI, LAI)
#' @param  variable1 Variable to be plotted (e.g., "Dep_NO2", "Conc_O3", "Wind", "Temp")
#' @param  variable2 Variable to be plotted (e.g., "Dep_NO2", "Conc_O3", "Wind", "Temp")
#' @param  start_day First day of the month in the plot (between 1 and 31; e.g., 4)
#' @param  stop_day Last day of the month in the plot (between 1 and 31; e.g., 22)
#' @param  start_month First month of the year in the plot (between 1 and 12; e.g., 3)
#' @param  stop_month Last month of the year in the plot (between 1 and 12; e.g., 11)
#' @return A plot with the variables and dates selected
#' @export
#' @import "ggplot2" "stats"
#' @importFrom "graphics" "axis" "legend" "lines" "mtext" "par" "plot" "polygon" "points"
#' @importFrom "stats" "median" "quantile"
#' @importFrom "utils" "head"
#'
#' @examples
#'
#' TreeDep_plot(my_data = Bizkaia_data,
#' variable1 = "Dep_PM10",
#' variable2 = "Wind",
#' start_month = 6,
#' stop_month = 7,
#' start_day = 25,
#' stop_day = 3)
#'

TreeDep_plot <- function(my_data ,
                          variable1 ,
                          variable2 = "Non-existent",
                          start_month ,
                          start_day ,
                          stop_month ,
                          stop_day){
  units_names <- c("Hum", "Pres", "Precip", "Rad", "Temp", "Wind", "Daylight",
                   "BAI", "LAI",
                   "Conc_",
                   "Dep_",
                   "Res_",
                   "Fric_")
  units_names4 <- substr(units_names, 1,4)
  units <- data.frame("%", "kPa", "mm", "W m-2", "Celsius", "m s-1", "unitless",
                      "m2 bark m-2 tree covered area", "m2 leaf m-2 tree covered area",
                      "micrograms m-3",
                      "g m-2 h-1",
                      "s m-1",
                      "m s-1")
  colnames(units) <- units_names4
  if(substr(variable1, 1,7)=="Dep_vel"){
    variable1_units <- "m s-1"
  }else{
    variable1_units <- as.character(units[,substr(variable1, 1,4)])
  }
  if(variable1 %in% units_names[1:9]){
    variable1_values <- my_data[,variable1]
  }else{
    dep_df1 <- do.call(variable1, list(my_data))
    variable1_values <- dep_df1[,2]
  }

  ## if there is only 1 variable
  if(variable2=="Non-existent"){
    my_df <- cbind.data.frame(Dates = my_data[,c("Dates")], variable1_values)
    colnames(my_df)[c(2)] <- c(variable1)
    my_df$year <- substr(my_df$Dates, 1, 4)
    my_df$month <- substr(my_df$Dates, 6, 7)
    my_df$day <- substr(my_df$Dates, 9, 10)
    my_df$hour <- substr(my_df$Dates, 12, 19)
    my_df$my_time <- as.POSIXct(strptime(paste(my_df$year,"-",my_df$month,"-",my_df$day, " ", my_df$hour, sep=""), "%Y-%m-%d %H:%M:%S"))
    my_dates <- c(as.character(start_month), as.character(start_day),
                  as.character(stop_month), as.character(stop_day))
    my_dates[nchar(my_dates)==1] <- paste(0, my_dates[nchar(my_dates)==1], sep="" )
    ID_start_day <- which(as.character(my_df$day)==my_dates[2] & my_df$month==my_dates[1])
    ID_stop_day <- which(as.character(my_df$day)==my_dates[4] & my_df$month==my_dates[3])
    my_df_selected <- my_df[c(ID_start_day[1]:ID_stop_day[length(ID_stop_day)]),]
    ggplot2::ggplot(my_df_selected, aes_string(x = "my_time")) +
      geom_line(aes(y = my_df_selected[,c(variable1)], colour = c(variable1), linetype = c(variable1))) +
      scale_linetype_manual(values = c(2)) +
      scale_colour_manual(values = c("blue")) +
      labs(y = paste(variable1, " (", variable1_units, ")", sep=""),
           x = "Date and time",colour = "Legend", linetype = "Legend")+
      theme(legend.key.height=unit(1, "cm"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))+
      ggtitle(paste("Hourly", variable1)) +
      theme(plot.title = element_text(hjust = 0.5))

  }else{
    ## if there are 2 variables
    if(substr(variable2, 1,7)=="Dep_vel"){
      variable2_units <- "m s-1"
    }else{
      variable2_units <- as.character(units[,substr(variable2, 1,4)])
    }
    if(variable2 %in% units_names[1:9]){
      variable2_values <- my_data[,variable2]
    }else{
      dep_df2 <- do.call(variable2, list(my_data))
      variable2_values <- dep_df2[,2]
    }
    my_df <- cbind.data.frame(Dates = my_data[,c("Dates")], variable1_values, variable2_values)
    colnames(my_df)[c(2:3)] <- c(variable1, variable2)
    my_df$year <- substr(my_df$Dates, 1, 4)
    my_df$month <- substr(my_df$Dates, 6, 7)
    my_df$day <- substr(my_df$Dates, 9, 10)
    my_df$hour <- substr(my_df$Dates, 12, 19)
    my_df$my_time <- as.POSIXct(strptime(paste(my_df$year,"-",my_df$month,"-",my_df$day, " ", my_df$hour, sep=""), "%Y-%m-%d %H:%M:%S"))
    my_dates <- c(as.character(start_month), as.character(start_day),
                  as.character(stop_month), as.character(stop_day))
    my_dates[nchar(my_dates)==1] <- paste(0, my_dates[nchar(my_dates)==1], sep="" )
    ID_start_day <- which(as.character(my_df$day)==my_dates[2] & my_df$month==my_dates[1])
    ID_stop_day <- which(as.character(my_df$day)==my_dates[4] & my_df$month==my_dates[3])
    my_df_selected <- my_df[c(ID_start_day[1]:ID_stop_day[length(ID_stop_day)]),]
    range_var1 <- max(my_df_selected[,c(variable1)], na.rm = T) - min(my_df_selected[,c(variable1)], na.rm = T)
    if(range_var1==0){
      range_var1 <- ceiling(max(my_df_selected[,c(variable1)], na.rm = T))
    }
    range_var2 <- max(my_df_selected[,c(variable2)], na.rm = T) - min(my_df_selected[,c(variable2)], na.rm = T)
    if(range_var2==0){
      range_var2 <- ceiling(max(my_df_selected[,c(variable2)], na.rm = T))
    }
    ratio <- range_var1 / range_var2
    ggplot2::ggplot(my_df_selected, aes_string(x = "my_time")) +
      geom_line(aes(y = my_df_selected[,c(variable1)], colour = c(variable1), linetype = c(variable1))) +
      geom_line(aes(y =my_df_selected[,c(variable2)] * ratio, colour = c(variable2), linetype = c(variable2))) +
      scale_y_continuous(sec.axis = sec_axis(~. / ratio, name = paste(variable2, " (", variable2_units, ")", sep=""))) +
      scale_linetype_manual(values = c(2,1)) +
      scale_colour_manual(values = c("blue", "red")) +
      labs(y = paste(variable1, " (", variable1_units, ")", sep=""), x = "Date and time",colour = "Legend", linetype = "Legend")+
      theme(legend.key.height=unit(1, "cm"), plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))+
      ggtitle(paste("Hourly", variable1, " vs.  Hourly", variable2)) +
      theme(plot.title = element_text(hjust = 0.5))
  }
}
