## [[file:TSS.org::*TSS][TSS:1]]
##' True skill statistic (TSS) Function
##' 
##' TSS is an index of the goodness-of- fit allowing to assess a how
##' much a continuous variable simulated by any model fit
##' presence/absence data.  It thus allows to translate a map of any
##' continuous variable into a map of presence/absence for a given set
##' of spatial simulation at the species level.  This is made possible
##' by finding the threshold value of the continuous variable used as
##' a procxy of fitness (e.g. growth, productivity..).
##'
##' This version extends the function \code{TSS} to using two
##' continuous variables (\code{cont1} and \code{cont2}) and two
##' thresholds (\code{thres1} and \code{thresh2}) to predict \code{presAbs}
##'
##' If \code{largerPres1==TRUE} and code{largerPres2==TRUE} the system
##' represents facilitation, while \code{largerPres1==TRUE} and
##' code{largerPres2==FALSE} represents competition.
##'
##' If \code{is.null(cont2)} the function returns the normal default
##' TSS.
##' 
##' Note that \code{cont1} and \code{verref} must match spatially (same ranking and
##' same size)
##' 
##' @title True skill statistic
##' @param cont1 \code{vector} of simulation of any continuous variable
##' which has been choosen as a primary proxy of fitness
##' @param cont2 \code{vector} of simulation of any continuous variable
##' which has been choosen as a secondary proxy of fitness. If \code{is.null(cont2)}, a simple TSS is calculated.
##' @param presAbs \code{vector} of presence/absence data, can be logical
##' @param thresh1 \code{vector} containig the threshold values for \code{cont1} 
##' @param thresh2 \code{vector} containig the threshold values for \code{cont2}
##' @param largerPres1 if \code{TRUE}, \code{cont1} values larger then
##' the threshold specify presence, if \code{FALSE} \code{cont1}
##' values smaller. 
##' @param largerPres2 if \code{TRUE}, \code{cont2} values larger then
##' the threshold specify presence, if \code{FALSE} \code{cont2}
##' values smaller. 
##' @param link a \bold{vectorised} function returning a
##' \code{logical} vector of the same length as the \bold{exactly two}
##' \code{logical} input vectors. The default is the function
##' \code{&}. It has to be specified using backquotes (i.e. "`&`" or
##' "`|`").
##' @return An object of class \code{TSS} TODO
##' @author Rainer M. Krug \email{Rainer@@krugs.de}
TSS <- function(
    cont1,
    cont2 = NULL,
    presAbs,
    thresh1,
    thresh2,
    largerPres1 = TRUE,
    largerPres2 = FALSE,
    link = `&`
    ) {
    presAbs <- as.logical(presAbs)
    if (is.null(cont2)) {
        sel <- (!is.na(cont1)) & (!is.na(presAbs))
        cont1 <- cont1[sel]
        presAbs <- presAbs[sel]
        thresh2 <- NA
    } else {
        sel <- (!is.na(cont1)) & (!is.na(cont2)) & (!is.na(presAbs))
        cont1 <- cont1[sel]
        cont2 <- cont2[sel]
        presAbs <- presAbs[sel]
    }
    ##
    result <- sapply(
        thresh1,
        function(tr1) {
            result <- sapply(
                thresh2,
                function(tr2) {
                    ## Classify cont1 and cont2 into absence (< tr)
                    ## and presence (>= tr) depending on largerPres1
                    ## and largerPres2
                    if (largerPres1) {
                        vecCont1 <- cont1 >= tr1
                    } else {
                        vecCont1 <- cont1 <= tr1                        
                    }
                    if (!is.null(cont2)) {
                        if (largerPres2) {
                            vecCont2 <- cont2 >= tr2
                        } else {
                            vecCont2 <- cont2 <= tr2                        
                        }
                        ## Combine vecCont1 and vecCont2 using "link"
                        vecCont <- link(vecCont1, vecCont2)
                    } else {
                        vecCont <- vecCont1
                    }
                    ## Presence predicted and Present
                    pP <- sum(   vecCont &    presAbs  )
                    ## Presence predicted but absent
                    pA <- sum(   vecCont  & (!presAbs) )
                    ## Absence predicted but Present
                    aP <- sum( (!vecCont) &   presAbs  )
                    ## Absence predicted and Absent
                    aA <- sum( (!vecCont) & (!presAbs) )

                    ## --------------------------  
                    ## Sensitivity
                    Sens <- pP / (pP + aP)
                    ## --------------------------  
                    ## Specificity
                    Spe <- aA / (aA + pA)
                    ## --------------------------  
                    ## TSS
                    TSS <- Sens + Spe - 1
                    ## --------------------------  
                    
                    result <- c( tr1, tr2, Sens, Spe, TSS, pP, pA, aA, aP )
                    return(result)
                }
                )
            return(result)
        }
        )
    result <- as.data.frame(t(result))
    names(result) <- c("threshold1", "threshold2", "sensitivity", "specificity", "tss", "pP", "pA", "aA", "aP")
    if (is.null(cont2)) {
        result$threshold2 <- NA
        thresh2 <- NA
        dimension <- 1
    } else {
        dimension <- 2
    }
    class(result) <- "TSS"
    attr(result, "link") <- link
    attr(result, "largerPres1") <- largerPres1
    attr(result, "largerPres2") <- largerPres2
    attr(result, "threshold1") <- thresh1
    attr(result, "threshold2") <- thresh2
    attr(result, "dimension") <- dimension
    return(result)
}
## TSS:1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
