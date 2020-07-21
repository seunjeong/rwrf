#' Combine and reformat wind profiler (915 MHz) data in matrix to long format
#' @gmt time in GMT, e.g., 2018100503 (YYYYMMDDHH format)
#' @param height.agl.km height in km a.g.l. in matrix of szie (hour, height_level)
#' @param wind.direction wind direction (meteorological direction) in matrix of size (hour, height_level)
#' @param wind.speed wind speed (m/s) in matrix of size (hour, height_level)
#' @return
#' @export pivot_long_wind_profiler_mat
#' @import tidyverse
#' @note The raw data were downloaded from ftp://ftp1.psl.noaa.gov/psd2/data/realtime/Radar915/CnsWind/ and processed in a matrix.
#' @examples
pivot_long_wind_profiler_mat <- function (gmt, height.agl.km, wind.speed, wind.direction) {

    # check dim
    assert_that (are_equal (dim (height.agl.km), dim (wind.direction)))
    assert_that (are_equal (dim (height.agl.km), dim (wind.speed)))

    profiler_gmt = gmt; rm (gmt)

    lv_name = paste0('L', 1:100)

    height.agl.km = data.frame (height.agl.km) %>% as_tibble ()
    wind.speed = data.frame (wind.speed) %>% as_tibble ()
    wind.direction = data.frame (wind.direction) %>% as_tibble ()
    names (height.agl.km) = lv_name
    names (wind.direction) = lv_name
    names (wind.speed) = lv_name
    height.agl.km$gmt = profiler_gmt
    wind.direction$gmt = profiler_gmt
    wind.speed$gmt = profiler_gmt

    df_hgt = height.agl.km %>% pivot_longer (-gmt, names_to = 'level', values_to = 'km_agl')
    df_wspd = wind.speed %>% pivot_longer (-gmt, names_to = 'level', values_to = 'wspd')
    df_wdir = wind.direction %>% pivot_longer (-gmt, names_to = 'level', values_to = 'wdir')
    assert_that (are_equal(df_wspd$gmt, df_wdir$gmt))
    assert_that (are_equal(df_wspd$gmt, df_hgt$gmt))
    assert_that (are_equal(df_wspd$level, df_wdir$level))
    assert_that (are_equal(df_wspd$level, df_hgt$level))

    #===============================================================================
    # Make one profiler df
    #===============================================================================
    df_prof = cbind (df_hgt, df_wspd %>% dplyr::select (wspd), df_wdir %>% dplyr::select (wdir)) %>%
        as_tibble ()

    return (df_prof)
}
