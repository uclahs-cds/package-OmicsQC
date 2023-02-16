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

correct.zscore.signs <- function(df, signs, filename = NULL) {
    neg_z <- signs$Metric[which(signs$Sign == "neg")];

    for (i in neg_z) {
        df[, i] <- -1 * df[, i];
        }

    df <- t(df);
    df[df > 0] <- 0;

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