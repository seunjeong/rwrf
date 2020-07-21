#' Combine and reformat WRF wind data in matrix to long format
#'
#' @param dt time (YYYYMMDDHH format), typically in GMT
#' @param u U component of WRF wind, m/s, with size of (length of dt, length of (num_lv)), e.g., dim (u) 434  50
#' @param v V component of WRF wind, m/s, with size of (length of dt, length of (num_lv)), e.g., dim (u) 434  50
#' @param wrf_lv_m WRF vertical winds in meters, with size of (length of dt, length of (num_lv)), e.g., dim (u) 434  50
#' @param num_lv number of levels, 50 levels by default
#' @param tz time zone, gmt by default
#'
#' @return
#' @export pivot_long_wrf_winds
#' @import dplyr
#' @examples
pivot_long_wrf_winds <- function (dt, u, v, wrf_lv_m, num_lv = 50, tz = 'gmt') {

    # check dim
    assert_that (are_equal (dim (wrf_lv_m), dim (u)))
    assert_that (are_equal (dim (wrf_lv_m), dim (v)))

    lv_name = paste0('L', 1:num_lv)

    df_lv_m = data.frame (wrf_lv_m) %>% as_tibble ()
    df_u = data.frame (u) %>% as_tibble ()
    df_v = data.frame (v) %>% as_tibble ()
    names (df_lv_m) = lv_name
    names (df_u) = lv_name
    names (df_v) = lv_name

    #===============================================================================
    # Add date and time
    #===============================================================================
    assert_that (nrow (df_lv_m) == length(dt))
    df_lv_m [[tz]] = dt
    df_u [[tz]] = dt
    df_v [[tz]] = dt

    #===============================================================================
    # Make it longer
    #===============================================================================
    df_lv_m = df_lv_m %>% pivot_longer (-!!tz, names_to = 'level', values_to = 'm_agl')
    df_uu = df_u %>% pivot_longer (-!!tz, names_to = 'level', values_to = 'u')
    df_vv = df_v %>% pivot_longer (-!!tz, names_to = 'level', values_to = 'v')

    # check!
    assert_that (are_equal(df_uu[[tz]], df_vv[[tz]]))
    assert_that (are_equal(df_uu[[tz]], df_lv_m[[tz]]))
    assert_that (are_equal(df_uu$level, df_vv$level))
    assert_that (are_equal(df_uu$level, df_lv_m$level))

    #===============================================================================
    # Make one df
    #===============================================================================
    df = cbind (df_lv_m, df_uu %>% dplyr::select (u), df_vv %>% dplyr::select (v)) %>%
        as_tibble ()

    #===============================================================================
    # Wind speed
    #===============================================================================
    df$wspd = with (df, sqrt (u^2 + v^2))
    df$wdir = with (df, uv_to_wspd_wdir (u, v)$wdir)
    return (df)
}
