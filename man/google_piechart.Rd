% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/google_piechart.R
\name{google_piechart}
\alias{google_piechart}
\title{Google-style piechart}
\usage{
google_piechart(
  input_data,
  caption,
  colors = NULL,
  ...,
  width = 800,
  height = 600,
  plot = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{input_data}{data frame}

\item{...}{arguments to \link{gvisPieChart}\link{googleVis}}

\item{width}{in pixels}

\item{height}{in pixels}
}
\description{
Google-style piechart
}
\note{
REQUIRES AN INTERNET CONNECTION to display the visualization
}
\examples{
library(inventory)
BY2011_NOx_2015 <- BY2011_annual \%>\% filter(year == 2015, pol_abbr == "NOx")
BY2011_NOx_2015_by_cnty <- BY2011_NOx \%>\% annual_emissions_by(cnty_abbr)
BY2011_NOx_2015_by_cnty \%>\% dplyr::select(cnty_abbr, ems_qty) \%>\% arrange(ems_qty) \%>\% google_piechart(caption = "2015 NOx emissions (Source: BY2011)")
}
