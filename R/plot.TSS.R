## [[file:TSS.org::*plot%20TSS][plot\ TSS:1]]
##' Generic \code{plot} function for TSS Object
##' 
##' @param x object of class \code{TSS}
##' @param columns columns to include in the plot
##' @param ... further arguments for \code{plot} function
##' 
##' @export
plot.TSS <- function(x, columns = c("tss", "sensitivity", "specificity"), ...) {
    if ( attr(x, "dimension")==1 ) {
        cols <- c("red", "green", "blue", "brown", "black")
        ## columns <- c("kappa", "overallAccuracy", "specificity", "sensitivity", "tss")
        plot(
            x[[columns[1]]] ~ x$threshold1,
            ylim = c(-1, 1),
            type = "l",
            lty  = 1,
            col  = cols[1],
            ylab = "Sensitivity | SPe | TSS",
            main = "TSS et al",
            ...
            )
        for (i in 2:length(columns)) {
            lines(
                x[[columns[i]]] ~ x$threshold1,
                lty = i,
                col = cols[i]
                )
        }
        legend(
            x      = "top",
            legend = columns,
            col    = cols[1:length(columns)],
            lty    = 1:length(columns)
            )
        invisible()
    }
}
## plot\ TSS:1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
