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