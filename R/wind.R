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


#' Plot wind rose
#'
#' @param df
#' @param wspd_var string variable for wind speed
#' @param wdir_var string variable for wind direction
#' @param wspd_breaks breaks for wind speed
#' @param facet_var string variable name used for the facet plot
#' @param legend_tile legend title
#' @param ggtheme
#' @param legend_title_align
#' @param n_col number of columns in the facet plot
#' @import clifro
#' @import dplyr
#' @return
#' @export plot_wind_rose
#'
#' @examples
plot_wind_rose <- function (df, wspd_var, wdir_var, wspd_breaks=c(3, 6, 9), facet_var = NULL,
                            legend_tile ="Wind Speed\n(m/s)", n_col = 1, ggtheme = "bw", legend_title_align = 0.5)
{
    if (!is.null (facet_var)) {
        p =with (df, windrose(get(wspd_var), get(wdir_var),
                              facet = get(facet_var),
                              speed_cuts = wspd_breaks,
                              legend_title = legend_tile,
                              legend.title.align = legend_title_align,
                              ggtheme = ggtheme,
                              #col_pal = "Greys",
                              n_col = n_col))
    } else {
        p =with (df, windrose(get(wspd_var), get(wdir_var),
                              speed_cuts = wspd_breaks,
                              legend_title = legend_tile,
                              legend.title.align = legend_title_align,
                              ggtheme = ggtheme,
                              #col_pal = "Greys",
                              n_col = n_col))
    }

    return (p)
}
