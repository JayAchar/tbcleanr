% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cavities_fixer.koch6.R
\name{cavities_fixer.koch6}
\alias{cavities_fixer.koch6}
\title{Convert admission x-ray variables to factorised output}
\usage{
\method{cavities_fixer}{koch6}(x, rm_orig = FALSE, ...)
}
\arguments{
\item{x}{data frame containing xray variable data}

\item{rm_orig}{remove original variables - TRUE or FALSE}

\item{...}{further arguments passed to or from other methods}
}
\value{
data frame with two new factor variables indicating
whether a x-ray is normal or abnormal, and whether it shows
a cavity or not
}
\description{
Convert admission x-ray variables to factorised output
}
\seealso{
\code{\link{tbcleanr}}
}
\author{
Jay Achar
}
