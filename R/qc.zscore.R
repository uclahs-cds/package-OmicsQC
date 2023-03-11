#' Calculate z-scores for each metric across each sample
#'
#' This function takes a dataframe of QC metrics, and calculcates the
#' the z-scores. If filename is specified, the results will be save to file.
#'
#' @param qc.data A dataframe whose rows are samples and each column a QC metric
#' @param filename A filename where to save data. If NULL data will not be saved to file
#' @return A dataframe of z-scores for each metric
#' @export
zscores.from.metrics <- function(qc.data, filename = NULL) {

    numeric.df.check(qc.data);

    zscores <- apply(qc.data, 2, function(x) scale(x, center = TRUE, scale = TRUE));

    rownames(zscores) <- row.names(qc.data);

    if (!is.null(filename)) {
        write.table(
            x = zscores,
            file = filename,
            quote = FALSE,
            row.names = TRUE,
            col.names = TRUE
            );
        }

    return(zscores);
    }

#' Corrects the z-scores signs according to the metrics
#'
#' For some metrics a high z-score is good, while for others a low
#' one is good. This functions corrects for that so that a negative z-score
#' is a poor score for every metric. It then sets all positive scores
#' to zero, and transposes the dataframe for use in visualisation.
#'
#' @param zscores A dataframe whose rows are samples and each column a QC metric, entries are z-scores
#' @param signs.data A dataframe of two columns, the metric names and the sign of the metric
#' @param metric.col.name The name of the column in signs.data that stores the metric name
#' @param signs.col.name The name of the column in signs.data that stores sign as 'neg' or 'pos'
#' @param filename A filename where to save data. If NULL data will not be saved to file
#' @return A dataframe whose rows are the QC metrics, and columns are samples with the z-scores if they are negative
#' @export
correct.zscore.signs <- function(
    zscores,
    signs.data,
    metric.col.name = 'Metric',
    signs.col.name = 'Sign',
    filename = NULL
    ) {

    numeric.df.check(zscores);

    negative.zscores <- signs.data[[metric.col.name]]['neg' == signs.data[[signs.col.name]]];

    for (i in negative.zscores) {
        zscores[, i] <- -1 * zscores[, i];
        }

    zscores[0 < zscores] <- 0;

    zscores <- t(zscores);

    if (!is.null(filename)) {
        write.table(
            x = zscores,
            file = filename,
            quote = FALSE,
            row.names = TRUE,
            col.names = TRUE
            );
        }

    return(zscores);
    }

#' Sum across sign corrected z-scores for total sample quality score
#'
#' This function takes a dataframe of all the sign corrected scores, thus all negative, and aggregates
#' to get a total sample quality score.
#'
#' @param zscores.corrected A dataframe whose rows are samples and each column a QC metric
#' @param filename A filename where to save data. If NULL data will not be saved to file
#' @return A dataframe of z-scores for each metric
#' @export
accumulate.zscores <- function(zscores.corrected, filename = NULL) {

    numeric.df.check(zscores.corrected);

    quality.scores <- colSums(zscores.corrected);

    quality.scores.df <- data.frame(
        'Sample' = names(quality.scores),
        'Sum' = quality.scores,
        stringsAsFactors = FALSE
        );

    quality.scores.df <- quality.scores.df[order(-quality.scores.df$Sum), ];

    quality.scores.df$Sample <- factor(
        x = quality.scores.df$Sample,
        levels = quality.scores.df$Sample
        );

    if (!is.null(filename)) {
        write.table(
            x = zscores.corrected,
            file = filename,
            quote = FALSE,
            row.names = TRUE,
            col.names = TRUE
            );
        }

    return(quality.scores.df);
    }


#' Test if a dataframe is fully numeric. Raises and error if the dataframe is not.
#'
#' @param dataframe The dataframe one wants to test
#' @noRd
numeric.df.check <- function(dataframe) {

    dataframe.numeric <- all(apply(dataframe, 2, is.numeric));

    if (!dataframe.numeric) {
        stop('dataframe is not fully numeric');
        }

    return(NULL)
    }