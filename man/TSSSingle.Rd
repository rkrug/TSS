\name{TSSSingle}
\alias{TSSSingle}
\title{True skill statistic}
\usage{
TSSSingle(cont, presAbs, thresh, largerPres = TRUE)
}
\arguments{
  \item{cont}{\code{vector} of simulation of any continuous
  variable which has been choosen as a proxy of fitness}

  \item{presAbs}{\code{vector} of presence/absence data,
  can be logical}

  \item{thresh}{\code{vector} containig the reshold value
  in the same unit as cont}

  \item{largerPres}{if \code{TRUE} the \code{cont} values
  larger then the threshold are considered as presence, if
  \code{FALSE} smaller values.}
}
\value{
A TSS object
}
\description{
True skill statistic (TSS) Function
}
\details{
TSS is an index of the goodness-of- fit allowing to assess
a how much a continuous variable simulated by any model fit
presence ??? absence data. It thus allows to translate a
map of any continuous variable into a map of
presence/absence for a given set of spatial simulation at
the species level. This is made possible by finding the
threshold value of the continuous variable used as a procxy
of fitness (e.g. growth, productivity..) Therefore One must
loop the function over a wide range of threshold to find
the optimal one. Note that cont and vec ref must match
spatially (same ranking and same size)
}
\author{
Rainer M. Krug \email{Rainer@krugs.de}
}

