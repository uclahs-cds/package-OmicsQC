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

    # Error checking
    numeric.df.check(zscores.corrected);
    if(is.null(rownames(zscores.corrected))){
      stop("Please specify sample IDs by setting the rownames");
    }
    if(any(is.na(zscores.corrected))){
      stop("Current version of OmicsQC does not support missing data in zscore aggregation")
    }

    quality.scores <- rowSums(zscores.corrected);

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
            x = quality.scores.df,
            file = filename,
            quote = FALSE,
            row.names = TRUE,
            col.names = TRUE
            );
        }

    return(quality.scores.df);
    }
