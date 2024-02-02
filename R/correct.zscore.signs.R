#' Corrects the z-scores signs according to the metrics
#'
#' For some metrics a high z-score is good, while for others a low
#' one is good. This function corrects for that so that a negative z-score
#' is a poor score for every metric. It then sets all positive scores
#' to zero.
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

    # Error checking
    zscore.format.check(zscores);
    if (!metric.col.name %in% colnames(signs.data) || !signs.col.name %in% colnames(signs.data)) {
        stop('Metric or sign column name not found in signs data');
        }
    if (!setequal(signs.data[,'Metric'], colnames(zscores))) {
        stop('The column names of zscores must contain the same set of elements as the metric column of signs data');
        }
    if (!all(signs.data[,signs.col.name] %in% c('pos', 'neg'))) {
        stop('Incorrect sign specification in signs column');
        }

    negative.zscores <- signs.data[[metric.col.name]]['neg' == signs.data[[signs.col.name]]];

    for (i in negative.zscores) {
        zscores[, i] <- -1 * zscores[, i];
        }

    zscores[0 < zscores] <- 0;

    if (!is.null(filename)) {
        utils::write.table(
            x = zscores,
            file = filename,
            quote = FALSE,
            row.names = TRUE,
            col.names = TRUE
            );
        }

    return(zscores);
    }
