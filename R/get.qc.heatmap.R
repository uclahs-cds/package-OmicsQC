#' Generates the standard heatmap of scores for each sample.
#'
#' This function takes the the scores for each sample and each metric,
#' after being sign-corrected, and returns the standard heatmap, if filename
#' is NULL. If filename is not NULL it saves the heatmap to file and returns NULL.
#' The function also takes quality.scores to make sure the samples are ordered correctly, as well as the y labels
#' for the quality metrics.
#' get.qc.heatmap offers a standard template for generating a QC heatmap, but can also take any parameter
#' that BoutrosLab.plotting.general::create.barplot takes for customisability.
#'
#' @param zscores A dataframe of (sign-corrected) z-scores for each sample and test metric, i.e. the output of correct.zscore.signs
#' @param quality.scores A dataframe with columns 'Sum' (of scores) and 'Sample', i.e. the output of accumulate.zscores
#' @param yaxis.lab A vector of metric labels for the y-axis; defaults to column names of z-scores
#' @param xaxis.lab A vector of sample labels for the x-axis; defaults to ordered Sample column elements in quality.scores
#' @param yaxis.cex Size of y-axis tick labels, defaults to 0.8
#' @param xaxis.cex Size of x-axis tick labels, defaults to 0
#' @param xlab.cex Size of x-axis label, defaults to 1
#' @param xlab.label The label for the x-axis, defaults so 'Sample'
#' @param at A vector specifying the breakpoints along the range of x; each interval specified by these breakpoints are assigned to a colour from the palette. Defaults to seq(0, -10, -2), to give a clear discrete display of colours. If x has values outside of the range specified by “at” those values are shown with the colours corresponding to the extreme ends of the colour spectrum and a warning is given.
#' @param grid.row Allow turning off of the interior grid-lines. Default is TRUE.
#' @param ... The function can also take any parameter that BoutrosLab.plotting.general::create.heatmap takes
#'
#' @inheritParams BoutrosLab.plotting.general::create.heatmap
#'
#' @return The heatmap or NULL depending if filename is specified
#'
#' @export
get.qc.heatmap <- function(
    zscores,
    quality.scores,
    yaxis.lab = colnames(zscores),
    xaxis.lab = quality.scores[,'Sample'],
    filename = NULL,
    yaxis.cex = 0.8,
    xaxis.cex = 0,
    xlab.cex = 1,
    xlab.label = 'Samples',
    clustering.method = 'none',
    colour.scheme = c('red', 'white'),
    colour.centering.value = 0,
    colourkey.labels.at = c(-10:0),
    colourkey.cex = 1,
    at = seq(0, -10, -2),
    same.as.matrix = TRUE,
    row.lines = seq(1, ncol(zscores), 1) + 0.5,
    grid.row = TRUE,
    row.colour = 'black',
    row.lwd = 1,
    axes.lwd = 1,
    ...
    ) {

    # Error checking
    zscore.format.check(zscores);
    accumulate.zscores.output.check(quality.scores);
    if (!setequal(rownames(zscores), as.character(quality.scores[,'Sample']))) {
        stop('quality.scores-Sample and zscores-rownames do not contain the same elements');
        }

    heatmap <- BoutrosLab.plotting.general::create.heatmap(
        filename = filename,
        x = t(zscores[as.character(quality.scores[,'Sample']),]),
        # Axes labels
        yaxis.lab = yaxis.lab,
        yaxis.cex = yaxis.cex,
        xaxis.lab = xaxis.lab,
        xaxis.cex = xaxis.cex,
        xlab.cex = xlab.cex,
        xlab.label = xlab.label,
        # Clustering
        clustering.method = clustering.method,
        # Colour scheme
        colour.scheme = colour.scheme,
        colour.centering.value = colour.centering.value,
        colourkey.labels.at = colourkey.labels.at,
        colourkey.cex = colourkey.cex,
        at = at,
        same.as.matrix = same.as.matrix,
        # Grid Rows
        grid.row = grid.row,
        row.lines = row.lines,
        row.colour = row.colour,
        row.lwd = row.lwd,
        axes.lwd = axes.lwd,
        ...
        );
    }
