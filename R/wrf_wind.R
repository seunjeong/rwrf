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

#' Merge WRF winds with vertical wind observations matching the height (relative to the ground)
#'
#' @param df_wrf data frame for WRF output
#' @param df_obs data frame for observation
#' @param tz time zone, 'gmt' is the default.
#'
#' @return
#' @export merge_wrf_with_vertical_wind_obs
#' @import dplyr
#' @import assertthat
#' @import FNN
#' @examples
#' Example of df_wrf (m_agl and gmt is required to match WRF to obs)
#'      gmt level m_agl      u     v  wspd  wdir
#'      <dbl> <chr> <dbl>  <dbl> <dbl> <dbl> <dbl>
#'      1 2020020100 L1     12.7 0.427  -1.91  1.96  347.
#'      2 2020020100 L2     39.1 0.433  -2.10  2.15  348.

merge_wrf_with_vertical_wind_obs <- function (df_wrf, df_obs, tz = 'gmt') {

    #===============================================================================
    # Check
    #===============================================================================
    assert_that (!!tz %in% names (df_wrf))
    assert_that (!!tz %in% names (df_obs))
    assert_that ('m_agl' %in% names (df_wrf))
    assert_that ('m_agl' %in% names (df_obs))

    #===============================================================================
    # Unique date and time for iteration
    #===============================================================================
    dt_unique = unique (df_wrf[[tz]])
    assert_that (are_equal(dt_unique, sort(dt_unique)))

    df_all = NULL # storage for results
    for (ii in seq_along (dt_unique)) {
        this_dt = dt_unique [ii]
        # Somehow, "!!tz == this_dt" does not work, so use "get"
        this_wrf = df_wrf %>% dplyr::filter (get(tz) == this_dt)
        this_obs = df_obs %>% dplyr::filter (get(tz) == this_dt)

        #...............................................................................
        # Distance
        # Note: The data is WRF (i.e., this_wrf), and the query is obs.
        #   This means the search is iterated over
        #   the obs while looking for the closest point from WRF.
        #...............................................................................
        dist_vec = FNN::knnx.dist (this_wrf$m_agl, this_obs$m_agl, k=1) %>% as.vector()
        assert_that (length (dist_vec) == nrow (this_obs))
        this_obs$dist_m = dist_vec

        #...............................................................................
        # Index
        #...............................................................................
        idx_vec = FNN::knnx.index (this_wrf$m_agl, this_obs$m_agl, k=1) %>% as.vector()
        #assert_that (sum(duplicated(idx_vec)) == 0)
        assert_that ( length(idx_vec) == nrow (this_obs))
        this_obs$wrf_idx = idx_vec

        # Note: the index is recycled (R's unique functionality)
        this_obs$wrf_m_agl = this_wrf$m_agl [idx_vec]
        this_obs$wrf_wspd = this_wrf$wspd [idx_vec]
        this_obs$wrf_wdir = this_wrf$wdir [idx_vec]

        df_all = rbind (df_all, this_obs)
    }

    return (df_all)
}


