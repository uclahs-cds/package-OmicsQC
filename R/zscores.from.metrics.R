zscores.from.metrics <- function(df, label.index, margin, filename = NULL) {
    df.zscore <- apply(df, margin, function(x) scale(x, center = TRUE, scale = TRUE));

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