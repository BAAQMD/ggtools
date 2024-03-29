% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chart_annual_by.R,
%   R/chart_annual_control_factors_by.R, R/chart_annual_emission_factors_by.R,
%   R/chart_annual_emissions_by.R, R/chart_annual_growth_by.R,
%   R/chart_annual_quantities_by.R, R/chart_annual_throughputs_by.R
\name{chart_annual_by}
\alias{chart_annual_by}
\alias{chart_annual_control_factors_by}
\alias{chart_annual_emission_factors_by}
\alias{chart_annual_emissions_by}
\alias{chart_annual_growth_by}
\alias{chart_annual_quantities_by}
\alias{chart_annual_quantities}
\alias{chart_annual_throughputs_by}
\title{Charting Annual Data}
\usage{
chart_annual_control_factors_by(data, ...)

chart_annual_emission_factors_by(data, ...)

chart_annual_emissions_by(data, ...)

chart_annual_growth_by(data, ..., base_year)

chart_annual_quantities_by(data, ...)

chart_annual_throughputs_by(data, ...)
}
\arguments{
\item{data}{tabular, with a column \code{year}}

\item{...}{variables to chart by. Must be present in \code{data}.}

\item{geom}{"line", "point", "col", or "area"}

\item{year_limits}{like \code{CY(1990, 2040)}}

\item{year_breaks}{like \code{seq(1990, 2050, by = 10)}}

\item{year_expand}{see \code{\link[ggplot2:expansion]{ggplot2::expansion()}}}

\item{flag_years}{if provided, will display actual values at these years.}

\item{flag_labels}{controls the information displayed in flags. See
\code{\link[glue:glue]{glue::glue()}}.}

\item{base_year}{required for \code{chart_annual_growth()}; optional otherwise. Displays a mark at the corresponding year.}

\item{title}{passed to \code{\link[ggplot2:labs]{ggplot2::labs()}}}

\item{subtitle}{passed to \code{labs()}}

\item{caption}{passed to \code{labs()}}

\item{verbose}{display messages}
}
\description{
\strong{Please don't use in production code yet! Thank you!} The pollutant-faceting feature of \code{chart_annual_emissions_by()}, in particular, may change.

\strong{\code{chart_annual_quantities_by()}} is the workhorse function. It will
auto-detect the first variable in \code{data} that ends with \verb{_qty}, unless you
supply an explicit \code{qty_var}. It relies on a default \code{\link[=scale_x_annual]{scale_x_annual()}},
which you can tweak by supplying \code{year_breaks}, \code{year_limits}, and/or
\code{year_expand}. You can use it to chart arbitrary annual quantities, like
\code{cf_qty} (control factors), \code{ef_qty} (emission factors), or \code{fee_qty} (fees).

\strong{\code{chart_annual_emissions_by()}} is a specialized variant. It expects to
find \code{ems_qty} and \code{ems_unit} in \code{data}. It relies on \code{scale_y_emissions()}
for the y-axis, and displays both quantities and units in flagged values. It
also facets the resulting chart by pollutant, relying on \code{pol_abbr} by
default.

\strong{\code{chart_annual_throughputs_by(...)}} tries to avoid double-counting. If
there are variables beginning with \code{pol_} or \code{ems_} in your data, it will
first issue a warning. Then, it will try to replace \code{data} with something
very much like \code{distinct(data, year, ..., tput_qty, tput_unit)}.

\strong{\code{chart_annual_growth_by()}} is a specialized variant. Like
\code{chart_annual_quantities_by()}, it will try to find a variable ending with
\verb{_qty} in your \code{data}, unless you supply an explicit \code{qty_var}. It relies on
\code{\link[=scale_y_percentage]{scale_y_percentage()}} for the y-axis, and formats flagged values using
\code{\link[=format_percent_change]{format_percent_change()}}.
}
\section{Functions}{
\itemize{
\item \code{chart_annual_control_factors_by()}: Chart annual control factors.

\item \code{chart_annual_emission_factors_by()}: Chart annual emission factors.

\item \code{chart_annual_emissions_by()}: Chart annual emissions.

\item \code{chart_annual_growth_by()}: Chart annual growth, relative to some base year.

\item \code{chart_annual_quantities_by()}: Chart an arbitrary annual quantity.

\item \code{chart_annual_throughputs_by()}: Chart annual throughputs.

}}
\note{
In \code{chart_annual_growth_by()}, \code{base_year} serves to index the reference year for normalization.
}
