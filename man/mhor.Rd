% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CXO_funcs.R
\name{mhor}
\alias{mhor}
\title{Mantel Haenszel Odds Ratio Estimate}
\usage{
mhor(formula, data, digits = 2)
}
\arguments{
\item{formula}{formula for estimation in form Outcome ~ strata/exposure}

\item{data}{input dataframe}

\item{digits}{number of digits for rounding}
}
\value{
Dataframe with OR estimate and 95\% confidence interval
}
\description{
Mantel Haenszel Odds Ratio Estimate
}
\examples{
data(cases)
mhor(Event ~ Id/ex, data=cases, digits=2)

}
