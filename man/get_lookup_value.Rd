% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_lookup_value}
\alias{get_lookup_value}
\title{Retrieve look-up value}
\usage{
get_lookup_value(key, table_name)
}
\arguments{
\item{key}{original name(s) representing keys in look-up table. These are used
to identify the correct value to be outputted by the function}

\item{table_name}{single string identifying internal package look-up tables. Current
options include "oca_missions" and "oca_projects"}
}
\value{
character vector of values from look-up table with equal length as input "key"
arguement
}
\description{
Retrieve look-up value
}
