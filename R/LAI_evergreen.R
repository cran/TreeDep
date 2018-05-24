#' LAI_evergreen - Generates hourly data of leaf and bark area index for evergreen trees in a specific year
#' @param  Year Year to generate leaf and bark area index (e.g. 2015)
#' @param  BAI_value Bark area index value (e.g. 0.1)
#' @param  LAI_value Mean value of leaf area index (e.g. 1.3)
#' @return A dataframe with LAI and BAI hourly values is generated
#' @export
#'
#' @examples
#'
#' LAI_evergreen(Year = 2016,
#' BAI_value = 0.1,
#' LAI_value = 1.3)
#'
LAI_evergreen <- function(Year,
                          LAI_value,
                          BAI_value){
  LAI <- data.frame(Dates =seq(from=as.POSIXct(paste(Year,"-1-1 0:00",sep=""), tz="UTC"),
                               to=as.POSIXct(paste(Year,"-12-31 23:00",sep=""), tz="UTC"),
                               by="hour"))
  LAI$LAI <- (LAI_value)
  LAI$BAI <- BAI_value
  LAI$LAI_Total <- LAI$LAI + LAI$BAI
  plot(LAI$Dates, LAI$LAI_Total, xlab="Date", ylab="LAI + BAI")
  return(LAI)
}
