% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wind.R
\name{plot_wind_rose}
\alias{plot_wind_rose}
\title{Plot wind rose}
\usage{
plot_wind_rose(
  df,
  wspd_var,
  wdir_var,
  wspd_breaks = c(3, 6, 9),
  facet_var = NULL,
  legend_tile = "Wind Speed\\n(m/s)",
  n_col = 1,
  ggtheme = "bw",
  legend_title_align = 0.5
)
}
\arguments{
\item{wspd_var}{string variable for wind speed}

\item{wdir_var}{string variable for wind direction}

\item{wspd_breaks}{breaks for wind speed}

\item{facet_var}{string variable name used for the facet plot}

\item{legend_tile}{legend title}

\item{n_col}{number of columns in the facet plot}
}
\value{

}
\description{
Plot wind rose
}
