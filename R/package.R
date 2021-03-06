##' Basic and extended True Skills Statistics (TSS)
##'
##' TSS is an index of the goodness-of- fit allowing to assess a how
##' much a continuous variable simulated by any model fit
##' presence/absence data, details can be found in Allouche et al,
##' 2006. It allows to translate a map of any continuous variable into
##' a map of presence/absence for a given set of spatial
##' simulations. This is made possible by finding the threshold value
##' of the continuous variable.
##'
##' The TSS function loops over a wide range of threshold values and
##' returns indicator values, which can be used to identify the most
##' suitable threshold value.
##'
##' Allouche, O., Tsoar, A., & Kadmon, R. (2006). Assessing the
##' accuracy of species distribution models: prevalence, kappa and the
##' true skill statistic (TSS). Journal of Applied Ecology, 43(6),
##' 1223--1232. doi:10.1111/j.1365-2664.2006.01214.x
##' 
##' \tabular{ll}{
##' Package: \tab TSS\cr
##' Type: \tab Package\cr
##' Version: \tab  0.3.0\cr
##' Date: \tab 2014-03-07_15-43\cr
##' License: \tab GPL (>= 2)\cr
##' LazyLoad: \tab yes\cr
##' }
##'
##' @name TSS-package
##' @docType package
##' @author Rainer M Krug \email{Rainer@@krugs.de}
##' @import lattice
NULL

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
