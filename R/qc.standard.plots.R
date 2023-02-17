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


get.qc.heatmap <- function(df, ylabels) {
    heatmap <- BoutrosLab.plotting.general::create.heatmap(
        df,
        # Axes labels
        yaxis.lab = ylabels,
        yaxis.cex = 0.8,
        xaxis.rot = 90,
        xaxis.lab = colnames(df),
        xaxis.cex = 0, # 0.8,
        yaxis.tck = 0,
        xlab.cex = 1,
        xlab.label = "Samples",
        # Clustering
        clustering.method = "none",
        # Colour scheme
        colour.scheme = c("red", "white"),
        colour.centering.value = 0,
        colourkey.labels.at = c(-10:0),
        colourkey.cex = 1,
        at = seq(0, -10, -2),
        same.as.matrix = TRUE,
        # Grid Rows
        grid.row = TRUE,
        row.lines = 26.5 - c(6, 12, 18, 22, 24),
        row.colour = "grey",
        row.lwd = 1,
        axes.lwd = 2
        );
    return(heatmap)
    }

get.qc.multipanelplot <- function(barplot, heatmap, filename) {
    BoutrosLab.plotting.general::create.multipanelplot(
        filename = filename,
        width = 10,
        height = 8,
        plot.objects = list(barplot, heatmap),
        layout.height = 2,
        layout.width = 1,
        plot.objects.heights = c(1, 3),
        y.spacing = -1,
        ylab.axis.padding = -19,
        left.padding = 8,
        main = "QC Summary",
        main.cex = 0,
        );
    }
