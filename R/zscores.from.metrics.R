#' Calculate z-scores for each metric across each sample
#'
#' This function takes a dataframe of QC metrics, and calculates the
#' the z-scores. If filename is specified, the results will be saved to file.
#'
#' @param qc.data A dataframe whose rows are samples and each column a QC metric
#' @param filename A filename where to save data. If NULL data will not be saved to file
#' @return A dataframe of z-scores for each metric
#' @export
zscores.from.metrics <- function(qc.data, filename = NULL) {

    zscore.format.check(qc.data);

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
