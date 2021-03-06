% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggvis_barchart.R
\name{ggvis_barchart}
\alias{ggvis_barchart}
\title{ggvis-style barchart}
\usage{
ggvis_barchart(input_data, ..., y = ~ems_qty)
}
\arguments{
\item{input_data}{data frame}

\item{...}{further arguments to \link{ggvis}\link{ggvis}}
}
\description{
ggvis-style barchart
}
\examples{
library(inventory)
BY2011_NOx_2015 <- BY2011_annual \%>\% filter(year == 2015, pol_abbr == "NOx")
BY2011_NOx_2015_by_cnty <- BY2011_NOx \%>\% annual_emissions_by(cnty_abbr)
BY2011_NOx_2015_by_cnty \%>\% dplyr::select(cnty_abbr, ems_qty) \%>\% arrange(ems_qty) \%>\% google_piechart(caption = "2015 NOx emissions (Source: BY2011)")

}
