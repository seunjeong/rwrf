#' Find the xy index of WRF grid
#'
#' @param in.ncfile input NetCDF file
#' @param sitelon longitude for (point) site for which WRF variable is extracted
#' @param sitelat longitude for (point) site for which WRF variable is extracted
#' @param VAR variable of interest from WRF output
#' @param CELL_SIZE the horizontal spatial resolution of WRF grid cell.
#'
#' @return
#' @export find_xy_ncdf4
#' @import ncdf4
#' @examples
#'
find_xy_ncdf4 <-function (in.ncfile, sitelon, sitelat, VAR='no.wind', CELL_SIZE = 0.013) {
    # @param VAR: indicate which grid it is looking at. "no.wind" means the
    #variable we are looking at is not wind; it is for mass variables (at the center of the grid cell).
    # Winds are not at the center of the grid
    ncfile= nc_open (in.ncfile)

    if (VAR == 'wind.U') {
        all.xlon=ncvar_get(ncfile, varid='XLONG_U')
        all.xlat=ncvar_get(ncfile, varid='XLAT_U')
    } else if (VAR == 'wind.V') {
        all.xlon=ncvar_get(ncfile, varid='XLONG_V')
        all.xlat=ncvar_get(ncfile, varid='XLAT_V')
    } else if (VAR == 'no.wind'){
        all.xlon=ncvar_get(ncfile, varid='XLONG')
        all.xlat=ncvar_get(ncfile, varid='XLAT')
    } else stop ('Wrong VAR')

    nrow = dim (all.xlon)[1]; ncol = dim (all.xlon)[2]

    dist.array = array (NA, nrow*ncol)
    ct=1
    for (i in 1:nrow) {
        for (j in 1:ncol) {
            dist.array[ct]=sqrt((all.xlon[i,j]-sitelon)^2+(all.xlat[i,j]-sitelat)^2)
            ct=ct+1
        }
    }
    cat('nrow: ', nrow, "\n")
    cat('ncol: ', ncol, "\n")

    tmp.loc.sol = which.min (dist.array)
    cat('tmp.loc.sol: ', tmp.loc.sol, "\n")
    jj = tmp.loc.sol%%ncol

    print (jj)

    if (jj==0) {
        ii = floor(tmp.loc.sol/ncol)
    } else {
        ii = floor(tmp.loc.sol/ncol)+1
    }
    cat('lon: ', all.xlon[ii,jj], "\n"); cat('lat: ', all.xlat[ii,jj], "\n")

    #check
    check.dist = sqrt ((all.xlon[ii,jj]-sitelon)^2+ (all.xlat[ii,jj]-sitelat)^2)
    cat ('Dist check: ', check.dist, '\n')

    if (check.dist>CELL_SIZE) {
        stop('wrong lon or lat')
    }

    # close the file
    nc_close(ncfile)

    ij = list (ii=ii, jj=jj)
    return (ij)
}


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
