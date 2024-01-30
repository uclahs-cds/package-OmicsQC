#' Generates the multipanel plot of heatmap and barplot
#'
#' This function takes the barplot and heatmap and returns the multipanel plot
#' of the two.
#'
#' @param barplot A barplot of the samples aggregated score
#' @param heatmap A heatmap of the sign-corrected scores for each sample
#' @param ... The function can also take any parameter that BoutrosLab.plotting.general::create.multipanelplot takes
#'
#' @inheritParams BoutrosLab.plotting.general::create.multipanelplot
#'
#' @return The multipanelplot or NULL depending if filename is specified
#'
#' @export
get.qc.multipanelplot <- function(
    barplot,
    heatmap,
    filename = NULL,
    width = 10,
    height = 8,
    layout.height = 2,
    layout.width = 1,
    plot.objects.heights = c(1, 3),
    y.spacing = -1,
    ylab.axis.padding = -19,
    left.padding = 8,
    main = 'QC Summary',
    main.cex = 1,
    ...
    ) {
    BoutrosLab.plotting.general::create.multipanelplot(
        filename = filename,
        width = width,
        height = height,
        plot.objects = list(barplot, heatmap),
        layout.height = layout.height,
        layout.width = layout.width,
        plot.objects.heights = plot.objects.heights,
        y.spacing = y.spacing,
        ylab.axis.padding = ylab.axis.padding,
        left.padding = left.padding,
        main = main,
        main.cex = main.cex,
        ...
        );
    }
