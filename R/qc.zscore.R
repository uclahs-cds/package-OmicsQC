# ' Calculate z-scores for each metric across each sample
# '
# ' This function takes a dataframe of QC metrics, anc calculcates the
# ' the z-score across the margin specified. If filename is specified,
# ' the results will be save to file.
# '
# ' @param qc.data A dataframe whose rows are samples and each column a QC metric
# ' @param margin An integer specifying across which axis to calculate z-scores
# ' @param filename A filename where to save data. If NULL data will not be saved to file
# ' @return A dataframe of z-scores for each metric
# ' @export
zscores.from.metrics <- function(qc.data, filename = NULL) {

    qc.data.numeric <- all(apply(qc.data, 2, is.numeric))

    if (!qc.data.numeric) {
        stop("qc.data is not fully numeric")
        }

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

    return(zscores)
    }

# ' Corrects the z-scores signs according to the metrics
# '
# ' For some metrics a high z-score is good, while for others a low
# ' one is good. This functions corrects for that so that negative
# ' is a poor score for every metric. It then sets all positive scores
# ' to zero, and transposes the dataframe for use in visualisation.
# '
# ' @param zscores A dataframe whose rows are samples and each column a QC metric, entries are z-scores
# ' @param signs.data A dataframe of two columns, the metric names and the sign of the metric
# ' @param metric.col.name The name of the column in signs.data that stores the metric name
# ' @param signs.col.name The name of the column in signs.data that stores sign as "neg" or "pos"
# ' @param filename A filename where to save data. If NULL data will not be saved to file
# ' @return A whose rows are the QC metrics, and columns are samples with the z-scores if they are negative
# ' @export
correct.zscore.signs <- function(zscores, signs.data, metric.col.name, signs.col.name, filename = NULL) {
    neg_z <- signs[[metric.col.name]][which(signs.data[[signs.col.name]] == "neg")];

    for (i in neg_z) {
        zscores[, i] <- -1 * zscores[, i];
        }

    zscores[zscores > 0] <- 0;

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

    return(zscores)
    }