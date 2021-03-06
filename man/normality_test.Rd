% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.r
\name{normality_test}
\alias{normality_test}
\title{normality_test}
\usage{
normality_test(data, colums)
}
\arguments{
\item{data}{Object type data.frame}

\item{colums}{Data columns}
}
\value{
data.frame with shapiro.test for selected columns
}
\description{
Runs shapiro.test on the data columns
}
\details{
Run shapiro.test on the data columns and return data.frame with the p_value of each column
}
