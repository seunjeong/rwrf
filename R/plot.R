#' Plot height (across different vertical levels) versus wind speed with error bars.
#'
#' @param df data frame with wind speed information
#' @param x X-axis variable name
#' @param y Y-axis variable name
#' @param title_txt plot title
#' @param sub_title plot subtitle
#' @param caption_txt plot caption name
#' @param multi_var_for_color whether or not to use multiple colors (with each variable/column using different colors)
#' @param color_var the variable (i.e., column) name to be used for different colors.
#' @param show_error_bar show error bar or not
#' @param error_bar_height the height of both the ends of the error bar.
#'
#' @return
#' @export plot_height_vs_wspd
#' @import dplyr
#' @import ggplot2
#' @note the error bar is horizontally drawn, i.e., a flip version of a typical error bar plot.
#' @examples
plot_height_vs_wspd <- function (
    df, x, y, title_txt = '', sub_title = '', caption_txt = '',
    multi_var_for_color = T, color_var = 'source', show_error_bar = T, error_bar_height = 0.2)
{

    #===============================================================================
    # Make sure "lower" and "upper" are in "df"
    #===============================================================================
    assert_that ('lower' %in% names (df))
    assert_that ('upper' %in% names (df))

    if (multi_var_for_color) {
        p = ggplot(data = df, aes (x = get(x), y = get(y), color = get(color_var))) +
            geom_point()
    } else {
        p = ggplot(data = df, aes (x = get(x), y = get(y))) +
            geom_point()
    }

    if (show_error_bar) {
        p = p + geom_errorbarh(aes(xmin = lower, xmax = upper), height = error_bar_height)
    }

    p = p + labs (x = 'Wind Speed (m/s)', y = 'Height (m agl)', title = title_txt,
                  subtitle = ,
                  caption = caption_txt)
    #scale_x_continuous (breaks = seq(0,12,2), limits = c(0, 12))

    return (p)
}
