% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/casetimecontrols_tvc.R
\docType{data}
\name{casetimecontrols_tvc}
\alias{casetimecontrols_tvc}
\title{Case time control Dataset with binary time-varying confounder}
\format{
\subsection{\code{casetimecontrols_tvc}}{

A tibble with 2970 rows and 14 columns:
\describe{
\item{Pt_ID}{Person ID}
\item{event}{Binary indicator for the outcome}
\item{ex}{binary indicator for the exposure}
\item{z}{binary time-varying confounder}
\item{period}{period number}
}
}
}
\source{
\url{https://github.com/lan-k/Casecrossover}
}
\usage{
casetimecontrols_tvc
}
\description{
A dataset containing  cases for a case-crossover study from simulated data
}
\keyword{datasets}
