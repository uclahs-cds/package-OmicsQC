get.qc.barplot <- function(barplot.data, filename = NULL) {
    barplot <- BoutrosLab.plotting.general::create.barplot(
        filename = filename,
        Sum ~ Sample,
        barplot.data,
        # Formatting
        yaxis.cex = 0.8,
        xaxis.rot = 90,
        xaxis.cex = 0, # 0.8,
        yaxis.tck = 0,
        xaxis.tck = 0,
        xlab.label = "",
        ylab.label = "Sum of Z (Z < 0)",
        ylab.cex = 1,
        axes.lwd = 2,
        border.col = "black",
        # Lines
        line.func = function(x) {x <- -20},
        line.from = 0,
        line.to = 162,
        line.col = 'darkgrey'
        );

    return(barplot)
    }
