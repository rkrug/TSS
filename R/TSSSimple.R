## [[file:TSS.org::*Simple%20TSS][Simple\ TSS:1]]
##' True skill statistic (TSS) Function
##' 
##' TSS is an index of the goodness-of- fit allowing to assess 
##' a how much a continuous variable simulated by any model fit presence ??? absence data.
##' It thus allows to translate a map of any continuous variable into a map of  presence/absence 
##' for a given set of spatial simulation at the species level.
##' This is made possible by finding the threshold value of the 
##' continuous variable used as a procxy of fitness (e.g. growth, productivity..) 
##' Therefore One must loop the function over a wide range of threshold to find the optimal one.
##' Note that cont and vec ref must match spatially (same ranking and same size)
##' 
##' @title True skill statistic
##' @param cont \code{vector} of simulation of any continuous variable which has been choosen as a proxy of fitness
##' @param presAbs \code{vector} of presence/absence data, can be logical
##' @param thresh \code{vector} containig the reshold value in the same unit as cont
##' @param largerPres if \code{TRUE} the \code{cont} values larger
##' then the threshold are considered as presence, if \code{FALSE}
##' smaller values.
##' @return A TSS object
##' @author Rainer M. Krug \email{Rainer@@krugs.de}
##'
##' @export
TSSSingle <- function(
    cont,
    presAbs,
    thresh,
    largerPres = TRUE
    ) {
    presAbs <- as.logical(presAbs)
    ## exclude where cont pr verref is NA
    sel <- (!is.na(cont)) & (!is.na(presAbs))
    cont <- cont[sel]
    presAbs <- presAbs[sel]
    ##
    result <- sapply(
        thresh,
        function(tr) {

            ## Classify cont into absence (< tr) and presence (>= tr)
            if (largerPres) {
                vecCont <- cont >= tr
            } else {
                vecCont <- cont <= tr
            }
            ## Presence predicted and Present
            pP <- sum(   vecCont  &   presAbs  )
            ## Presence predicted but Absent
            pA <- sum(   vecCont  & (!presAbs) )
            ## Absence predicted but Presence
            aP <- sum( (!vecCont) &   presAbs  )
            ## Absence predicted and Absent
            aA <- sum( (!vecCont) & (!presAbs) )
            
            ## Sensitivity
            Sens <- pP / (pP + aP)
            
            ## Specificity
            Spe <- aA / (aA + pA)
            
            ## TSS
            TSS <- Sens + Spe - 1
            
            result=c(tr, NA, Sens, Spe, TSS, pP, pA, aA, aP)
            return(result)
        }
        )
    result <- as.data.frame(t(result))
    names(result) <- c("threshold1", "threshold2", "sensitivity", "specificity", "tss", "pP", "pA", "aA", "aP")
    class(result) <- "TSS"
    attr(result, "link") <- NA
    attr(result, "largerPres1") <- largerPres
    attr(result, "largerPres2") <- NA
    attr(result, "threshold1") <- thresh
    attr(result, "threshold2") <- NA
    attr(result, "dimension") <- 1
    return(result)
}
## Simple\ TSS:1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
