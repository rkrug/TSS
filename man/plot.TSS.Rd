\name{plot.TSS}
\alias{plot.TSS}
\title{Generic \code{plot} function for TSS Object}
\usage{
\method{plot}{TSS}(x, columns = c("tss", "sensitivity", "specificity"),
  column.col, ylab, main = NULL, ...)
}
\arguments{
  \item{x}{object of class \code{TSS} to be plotted}

  \item{columns}{names of columns to be plotted from
  \code{x}}

  \item{column.col}{colours for each column to be plotted.
  Default is \code{rainbow(length(columns))}}

  \item{ylab}{y-axis label. Default is paste(columns,
  collapse=" | ")}

  \item{main}{main caption of the plot. Default is NULL}

  \item{...}{additional objects for plot functions. For 1D
  TSS (\code{attr(x, "dimension")==1}) for the
  \code{plot()} function is used, for the 2D (\code{attr(x,
  "dimension")==2}) the \code{wireframe()} function from
  the package \code{lattice} isd used.}
}
\description{
Generic \code{plot} function for TSS Object
}

