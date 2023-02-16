organise.barplot.data <- function(df, filename = NULL) {

    barplot.data <- colSums(df);

    barplot.df <- data.frame("Sample" = names(barplot.data),
        "Sum" = barplot_data,
        stringsAsFactors = FALSE
        );

    barplot.df <- barplot.df[order(-barplot.df$Sum), ];
    barplot.df$Sample <- factor(barplot.df$Sample, levels = barplot.df$Sample);

    if (!is.null(filename)) {
        write.table(
            x = df,
            file = filename,
            quote = FALSE,
            row.names = TRUE,
            col.names = TRUE
            );
        }

    return(barplot.df)
    }