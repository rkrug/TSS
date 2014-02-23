## [[file:TSS.org::*plot%20TSS][plot\ TSS:1]]
##' Generic \code{plot} function for TSS Object
##' 
##' @param x object of class \code{TSS}
##' @param ... further arguments for \code{plot} function
##' 
##' @export
plot.TSS <- function(x, ...) {
    if ( attr(x, "dimension")==1 ) {
        plot(
            x$sensitivity ~ x$threshold1,
            ylim = c(-1, 1),
            type = "l",
            lty  = 1,
            ylab = "Sensitivity | SPe | TSS",
            main = "TSS et al"
            )
        lines(
            x$specificity ~ x$threshold1,
            lty=2
            )
        lines(
            x$tss ~ x$threshold1,
            lwd = 2,
            col = 2
            )
        legend(
            x      = "top",
            legend = c("Sensitivity", "Specificity", "TSS"),
            col    = c(1,1,2),
            lwd    = c(1,1,2),
            lty    = c(1,2,1)
            )
    }
}
## plot\ TSS:1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
