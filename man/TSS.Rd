\name{TSS}
\alias{TSS}
\title{True skill statistic}
\usage{
TSS(cont1, cont2 = NULL, presAbs, thresh1, thresh2, largerPres1 = TRUE,
  largerPres2 = FALSE, link = `&`)
}
\arguments{
  \item{cont1,}{\code{vector} of simulation of any
  continuous variable which has been choosen as a primary
  proxy of fitness}

  \item{cont2}{\code{vector} of simulation of any
  continuous variable which has been choosen as a secondary
  proxy of fitness. If \code{is.null(cont2)}, a simple TSS
  is calculated.}

  \item{presAbs}{\code{vector} of presence/absence data,
  can be logical}

  \item{thresh1}{\code{vector} containig the threshold
  values for \code{cont1}}

  \item{thresh2}{\code{vector} containig the threshold
  values for \code{cont2}}

  \item{largerPres1}{if \code{TRUE}, \code{cont1} values
  larger then the threshold specify presence, if
  \code{FALSE} \code{cont1} values smaller.}

  \item{largerPres2}{if \code{TRUE}, \code{cont2} values
  larger then the threshold specify presence, if
  \code{FALSE} \code{cont2} values smaller.}

  \item{link}{a \bold{vectorised} function returning a
  \code{logical} vector of the same length as the
  \bold{exactly two} \code{logical} input vectors. The
  default is the function \code{&}. It has to be specified
  using backquotes (i.e. "`&`" or "`|`").}
}
\value{
An object of class \code{TSS} TODO
}
\description{
True skill statistic (TSS) Function
}
\details{
TSS is an index of the goodness-of- fit allowing to assess
a how much a continuous variable simulated by any model fit
presence/absence data.  It thus allows to translate a map
of any continuous variable into a map of presence/absence
for a given set of spatial simulation at the species level.
This is made possible by finding the threshold value of the
continuous variable used as a procxy of fitness (e.g.
growth, productivity..).

This version extends the function \code{TSS} to using two
continuous variables (\code{cont1} and \code{cont2}) and
two thresholds (\code{thres1} and \code{thresh2}) to
predict \code{presAbs}

If \code{largerPres1==TRUE} and code{largerPres2==TRUE} the
system represents facilitation, while
\code{largerPres1==TRUE} and code{largerPres2==FALSE}
represents competition.

If \code{is.null(cont2)} the function returns the normal
default TSS.

Note that \code{cont1} and \code{verref} must match
spatially (same ranking and same size)
}
\author{
Rainer M. Krug \email{Rainer@krugs.de}
}

