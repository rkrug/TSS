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
##' @param cont1, \code{vector} of simulation of any continuous variable
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
##'
##' @export
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
    thresh <- expand.grid(thresh1, thresh2)
    ##
    n <- sum(presAbs) + sum(!presAbs)
    n2 <- n^2

    no <- rep(NA, nrow(thresh))
    result <- list(
        threshold1 = no,
        threshold2 = no,
        overallAccuracy = no,
        sensitivity = no,
        specificity = no,
        tss = no,
        kappa = no,
        pP = no,
        pA = no,
        aA = no,
        aP = no,
        n = n
        )
    
    
    for ( i in 1:nrow(thresh)) {
        ## Classify cont1 and cont2 into absence (< tr)
        ## and presence (>= tr) depending on largerPres1
        ## and largerPres2
        result$threshold1[i] <- thresh[i, 1]
        if (largerPres1) {
            vecCont1 <- cont1 >= thresh[i, 1]
        } else {
            vecCont1 <- cont1 <= thresh[i, 1]                       
        }
        if (!is.null(cont2)) {
            if (largerPres2) {
                vecCont2 <- cont2 >= thresh[i, 2]
            } else {
                vecCont2 <- cont2 <= thresh[i, 2]                   
            }
            ## Combine vecCont1 and vecCont2 using "link"
            vecCont <- link(vecCont1, vecCont2)
            result$threshold2[i] <- thresh[i, 2]
        } else {
            vecCont <- vecCont1
        }
        
        ## Presence predicted and Present
        pP <- sum(   vecCont  &   presAbs  ) # a
        result$pP[i] <- pP
        ## Presence predicted but absent
        pA <- sum(   vecCont  & (!presAbs) ) # b
        result$pA[i] <- pA
        ## Absence predicted but Present
        aP <- sum( (!vecCont) &   presAbs  ) # c
        result$aP[i] <- aP
        ## Absence predicted and Absent
        aA <- sum( (!vecCont) & (!presAbs) ) # d
        result$aA[i] <- aA

        ## --------------------------  
        ## Sensitivity
        Sens <- pP / (pP + aP)
        result$sensitivity[i] <- Sens
        ## --------------------------  
        ## Specificity
        Spe <- aA / (aA + pA)
        result$specificity[i] <- Spe
        ## --------------------------  
        ## TSS
        TSS <- Sens + Spe - 1
        result$tss[i] <- TSS
        ## --------------------------  
        ## Overall Accuracy
        ovAc <- (aA + pP) / n
        result$overallAccuracy[i] <- ovAc
        ## --------------------------  
        ## kappa
        ## Should this be implemented?
        t1 <- (pP + aA) / n
        t2 <- ( (pP + pA)*(pP + aP)+(aP + aA)*( aA + pA) ) / n2
        kap <- (t1 - t2) / (1 - t2)
        result$kappa[i] <- kap
        ## --------------------------  
    }
    ##
    class(result) <- "TSS"
    attr(result, "link") <- link
    attr(result, "largerPres1") <- largerPres1
    attr(result, "largerPres2") <- largerPres2
    attr(result, "threshold1") <- thresh1
    attr(result, "threshold2") <- thresh2
    attr(result, "dimension") <- ifelse(is.null(cont2), 1, 2)
    return(result)
}
## TSS:1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
