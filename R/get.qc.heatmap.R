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