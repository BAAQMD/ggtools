% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggvis_stripchart.R
\name{ggvis_stripchart}
\alias{ggvis_stripchart}
\title{Plot emissions over time, using ggvis}
\usage{
ggvis_stripchart(
  input_data,
  caption = comment(input_data),
  x = ~year,
  y = ~ems_qty,
  stroke = NULL,
  fill = NULL,
  width = 800,
  height = 400,
  ...,
  y_axis_title = NULL,
  y_axis_format = "s",
  verbose = FALSE
)
}
\arguments{
\item{input_data}{data frame}

\item{...}{arguments to \code{ggvis}, including one of the form \code{y = ~var}}
}
\description{
Plot emissions over time, using ggvis
}
\examples{
BY2011_NOx_data <- filter(BY2011_annual, pol_abbr == "NOx", year \%in\% 1990:2030)
BY2011_NOx_by_cnty <- BY2011_NOx_data \%>\% annual_emissions_by(cnty_abbr)
BY2011_NOx_by_cnty \%>\% ggvis_stripchart(stroke = ~cnty_abbr)
BY2011_NOx_by_cnty \%>\% ggvis_stripchart(fill = ~cnty_abbr)

}
