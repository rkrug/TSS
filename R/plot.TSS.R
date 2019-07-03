## [[file:TSS.org::*plot%20TSS][plot\ TSS:1]]
##' Generic \code{plot} function for TSS Object
##' 
#' @param x object of class \code{TSS} to be plotted

#' @param columns names of columns to be plotted from \code{x}
#' @param column.col colours for each column to be plotted. Default is
#' \code{rainbow(length(columns))}
#' @param ylab y-axis label. Default is paste(columns, collapse=" | ")
#' @param main main caption of the plot. Default is NULL
#' @param ... additional objects for plot functions. For 1D TSS
#' (\code{attr(x, "dimension")==1}) for the \code{plot()} function is
#' used, for the 2D (\code{attr(x, "dimension")==2}) the
#' \code{wireframe()} function from the package \code{lattice} isd
#' used.
#'
#' @export
plot.TSS <- function(
    x,
    columns = c("tss", "sensitivity", "specificity"),
    column.col,
    ylab,
    main = NULL,
    ...) {
    if (missing(column.col)) {
        column.col <- rainbow(length(columns))
    }
    if (missing(ylab)) {
        ylab <- paste(columns, collapse=" | ")
    }
    if ( attr(x, "dimension")==1 ) {
        plot(
            x[[columns[1]]] ~ x$threshold1,
            ylim = c(-1, 1),
            type = "l",
            col  = column.col,
            ylab = ylab,
            xlab = "Threshold",
            main = main,
            ...
            )
        for (i in 2:length(columns)) {
            lines(
                x[[columns[i]]] ~ x$threshold1,
                col = column.col[i]
                )
        }
        m <- which.max(x$tss)
        abline(h=x$tss[m])
        abline(v=x$threshold1[m])
        text(
            x = x$threshold1[m],
            y = 0,
            labels = paste0(
                "(",
                round( x$tss[m], 2),
                ", ",
                round( x$threshold1[m], 2),
                ", ",
                round( x$threshold2[m], 2),
                ")"
                ),
            pos = 2
            )
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
        wireframe(
            zp ~ xp * yp,
            xlab = "threshold 1",
            ylab = "threshold 2",
            zlab = paste(columns, collapse="\n"),
            groups = grp,
            par.settings = simpleTheme(
                alpha = 0.7,
                col = column.col,
                ),
            scales = list(arrows = FALSE),
            autokey = TRUE,
            auto.key = TRUE,
            ...
            )
    }
}
## plot\ TSS:1 ends here

## Local Variables:
## org-babel-tangled-file: t
## buffer-read-only: t
## eval:: (auto-revert-mode)
## End:
