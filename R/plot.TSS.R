## [[file:TSS.org::*plot%20TSS][plot\ TSS:1]]
##' Generic \code{plot} function for TSS Object
##' 
##' @param x object of class \code{TSS}
##' @param columns columns to include in the plot
##' @param column.col colours to be used for plotting. If not
##' provided, \code{rainbow(length(columns))} is used
##' @param ... further arguments for the actual plotting. In case of a
##' one dimensionsl TSS, i.e. \code{attr(x, "dimension")==1}, the
##' \code{plot} function is used, in case of a two dimensionsl TSS,
##' i.e. \code{attr(x, "dimension")==2} the function \code{wireframe}
##' from the package \code{lattice} is used.
##' 
##' @export
plot.TSS <- function(
    x,
    columns = c("tss", "sensitivity", "specificity"),
    column.col,
    ...) {
    if (missing(column.col)) {
        column.col <- rainbow(length(columns))
    }
    if ( attr(x, "dimension")==1 ) {
        plot(
            x[[columns[1]]] ~ x$threshold1,
            ylim = c(-1, 1),
            type = "l",
            col  = column.col,
            ylab = "Sensitivity | SPe | TSS",
            main = "TSS et al",
            ...
            )
        for (i in 2:length(columns)) {
            lines(
                x[[columns[i]]] ~ x$threshold1,
                col = column.col[i]
                )
        }
        legend(
            x      = "topright",
            legend = columns,
            fill   = column.col
            )
        invisible()
    } else {
        xp <- rep(x$threshold1, length(columns))
        yp <- rep(x$threshold2, length(columns))
        zp <- NULL
        for (i in 1:length(columns)) {
            zp <- c(zp, x[[columns[i]]])
        }
        grp <- rep(columns, each=length(x$tss))
        plot.new()
        print(
            wireframe(
                zp ~ xp * yp,
                xlab = "threshold 1",
                ylab = "threshold 2",
                zlab = paste(columns, collapse="\n"),
                groups       = grp,
                par.settings = simpleTheme(
                    alpha = 0.7,
                    col = column.col,
                    ),
                scales       = list(arrows = FALSE),
                ...
                )
            )
        legend(
            x      = "topright",
            legend = columns,
            fill   = column.col
            ) 
    }
}
## plot\ TSS:1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
