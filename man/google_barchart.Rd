% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/google_barchart.R
\name{google_barchart}
\alias{google_barchart}
\title{Google-style barchart}
\usage{
google_barchart(
  input_data,
  caption = comment(input_data),
  ...,
  stacked = TRUE,
  width = 800,
  height = 600,
  h_axis = list(),
  chart_area = list(left = 100, width = "60\%"),
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
Google-style barchart
}
\note{
REQUIRES AN INTERNET CONNECTION to display the visualization
}
