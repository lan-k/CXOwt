% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CXO_funcs.R
\name{CXO_wt_boot}
\alias{CXO_wt_boot}
\title{Weighted conditional logistic regression for case-crossover with bootstrapped CIs}
\usage{
CXO_wt_boot(data, exposure, event, Id, tvc = NULL, B = 500, normal = TRUE)
}
\arguments{
\item{data}{input dataframe}

\item{exposure}{exposure variable}

\item{event}{outcome variable}

\item{Id}{person Id}

\item{tvc}{name of time-varying confounder (optional)}

\item{B}{number of bootstrapped replicates, default is 500. Minimum 200 recommended}

\item{normal}{use normal approximation for bootstrapped CI, default is TRUE
normal = F uses 2.5\% and 97.\% quantiles for 95\% CI}
}
\value{
A list containing:\tabular{ll}{
\code{Variable} \tab Parameter name \cr
\tab \cr
\code{est0} \tab OR from weighted conditional logistic regression \cr
\tab \cr
\code{est} \tab Bootstrapped OR: mean if normal = T, median otherwise \cr
\tab \cr
\code{lower} \tab Lower Bootstrapped 95\% CI: using normal approximation if normal = T, 2.5th quantile otherwise \cr
\tab \cr
\code{upper} \tab Upper Bootstrapped 95\% CI: using normal approximation if normal = T, 97.5th quantile otherwise \cr
}
}
\description{
Weighted conditional logistic regression for case-crossover with bootstrapped CIs
}
\examples{
data(cases)
try(CXO_wt_boot(data=cases, exposure=ex, event = Event, Id = Id, B=3))
# It is recommended that at least 500 bootstrap replications are used
\dontrun{

CXO_wt_boot(data=cases, exposure = ex, event = Event, Id = Id, B = 1000)
}
}
