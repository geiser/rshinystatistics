% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptives.R
\name{get.descriptives}
\alias{get.descriptives}
\title{Descriptive Statistics}
\usage{
get.descriptives(
  data,
  dvs,
  ivs,
  type = "common",
  covar = NULL,
  dv.var = NULL,
  include.global = F,
  symmetry.test = F,
  normality.test = F,
  hide.details = F,
  skewness = c()
)
}
\arguments{
\item{data}{a data.frame in which we will perform the descriptive statistics}

\item{dvs}{numeric columns with the dependent variables}

\item{ivs}{columns with the independent variables (split groups)}

\item{type}{a character string specifying type of summary statistics}

\item{dv.var}{column with the information to classify observations}

\item{include.global}{a boolean value to indicate if descriptive statistics for global data is included}

\item{symmetry.test}{a boolean value to indicate if symmetry test is included}

\item{normality.test}{a boolean value to indicate if normality test is included}

\item{skewness}{a list of transformation to achieve normality}
}
\value{
A data.frame containing the results for the descriptive statistics
}
\description{
Compute summary of descriptive statistics for numeric variables.
}
