\name{plot.TSS}
\alias{plot.TSS}
\title{Generic \code{plot} function for TSS Object}
\usage{
\method{plot}{TSS}(x, columns = c("tss", "sensitivity", "specificity"),
  column.col, ...)
}
\arguments{
  \item{x}{object of class \code{TSS}}

  \item{columns}{columns to include in the plot}

  \item{column.col}{colours to be used for plotting. If not
  provided, \code{rainbow(length(columns))} is used}

  \item{...}{further arguments for the actual plotting. In
  case of a one dimensionsl TSS, i.e. \code{attr(x,
  "dimension")==1}, the \code{plot} function is used, in
  case of a two dimensionsl TSS, i.e. \code{attr(x,
  "dimension")==2} the function \code{wireframe} from the
  package \code{lattice} is used.}
}
\description{
Generic \code{plot} function for TSS Object
}

