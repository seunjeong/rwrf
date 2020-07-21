#' Calculate wind speed and direction from U and V winds
#'
#' @param u U component of the wind vector
#' @param v V component of the wind vector
#'
#' @return
#' @export uv_to_wspd_wdir
#'
#' @examples
uv_to_wspd_wdir <- function (u,v){
    wspd=sqrt(u^2+v^2)
    wdir=atan2(v, -u)*180/pi + 90
    wdir[wdir<0]=wdir[wdir<0] + 360
    return(data.frame(wspd,wdir))
}

#' Calculate meteorological wind direction
#'
#' @param u U component of the wind vector
#' @param v V component of the wind vector
#'
#' @return meteorological wdir
#' @export uv_to_meteor_wdir
#' @note This function should result in the same wind direction as uv_to_wspd_wdir.
#' @examples
uv_to_meteor_wdir <- function (u,v) {
    # https://www.ncl.ucar.edu/Document/Functions/Contributed/wind_direction.shtml
    if (v >= 0) ANG = 180
    if (u < 0 & v < 0)  ANG = 0
    if (u >= 0 & v < 0) ANG = 360
    met_direction = (180 / pi) * atan (u / v) + ANG
    return (met_direction)
}


#' Calculate the average wind direction.
#'
#' @param wdir in degrees (0 - 360) where 0 or 360 is from the north.
#'
#' @return
#' @export calc_average_wind_direction
#' @import circular
#' @import dplyr
#' @examples
calc_average_wind_direction <- function (wdir) {
    #> wdir = c(355, 5, 15)
    #> mean(circular(wdir, units = "degrees"))
    #Circular Data:
    #    Type = angles
    #Units = degrees
    #Template = none
    #Modulo = asis
    #Zero = 0
    #Rotation = counter
    #[1] 5

    # The following all works!
    #wdir = 1:180
    #wdir = 90:270
    #wdir = c(270:360, 1:90)
    mean_wdir = mean(circular(wdir, units = "degrees")) %>% as.numeric()
    if (is.na (mean_wdir)) {
        #mean_wdir = NA # theoretically it could be 0
        stop ('winds are from opposite directions for each other')
    } else if (mean_wdir < 0) {
        mean_wdir = mean_wdir + 360
    }
    #print (mean_wdir)
    return (mean_wdir)
}
