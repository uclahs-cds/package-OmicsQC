# ' Calculate z-scores for each metric across each sample
# '
# ' This function takes a dataframe of QC metrics, anc calculcates the
# ' the z-score across the margin specified. If filename is specified,
# ' the results will be save to file.
# '
# ' @param df A dataframe whose rows are samples and each column a QC metric
# ' @param margin An integer specifying across which axis to calculate z-scores
# ' @param filename A filename where to save data. If NULL data will not be saved to file
# ' @return A dataframe of z-scores for each metric
# ' @export
zscores.from.metrics <- function(df, filename = NULL) {
    df.zscore <- apply(df, 2, function(x) scale(x, center = TRUE, scale = TRUE));

    rownames(df.zscore) <- row.names(df);

    if (!is.null(filename)) {
        write.table(
            x = df.zscore,
            file = filename,
            quote = FALSE,
            row.names = TRUE,
            col.names = TRUE
            );
        }

    return(df.zscore)
    }

# ' Corrects the z-scores signs according to the metrics
# '
# ' For some metrics a high z-score is good, while for others a low
# ' one is good. This functions corrects for that so that negative
# ' is a poor score for every metric. It then sets all positive scores
# ' to zero, and transposes the dataframe for use in visualisation.
# '
# ' @param df A dataframe whose rows are samples and each column a QC metric, entries are z-scores
# ' @param signs.data A dataframe of two columns, the metric names and the sign of the metric
# ' @param metric.col.name The name of the column in signs.data that stores the metric name
# ' @param signs.col.name The name of the column in signs.data that stores sign as "neg" or "pos"
# ' @param filename A filename where to save data. If NULL data will not be saved to file
# ' @return A whose rows are the QC metrics, and columns are samples with the z-scores if they are negative
# ' @export
correct.zscore.signs <- function(df, signs.data, metric.col.name, signs.col.name, filename = NULL) {
    neg_z <- signs.data$metric.col.name[which(signs.data$signs.col.name == "neg")];

    for (i in neg_z) {
        df[, i] <- -1 * df[, i];
        }

    df[df > 0] <- 0;

    df <- t(df);

    if (!is.null(filename)) {
        write.table(
            x = df,
            file = filename,
            quote = FALSE,
            row.names = TRUE,
            col.names = TRUE
            );
        }

    return(df)
    }