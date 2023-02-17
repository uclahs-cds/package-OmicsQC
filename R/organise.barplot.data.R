# ' Calculate z-scores for each metric across each sample
# '
# ' This function takes a dataframe of all the negative scores and aggregates
# ' to a form which can be plotted as a a barplot.
# '
# ' @param df A dataframe whose rows are samples and each column a QC metric
# ' @param filename A filename where to save data. If NULL data will not be saved to file
# ' @return A dataframe of z-scores for each metric
# ' @export
organise.barplot.data <- function(df, filename = NULL) {

    barplot.data <- colSums(df);

    barplot.df <- data.frame(
        "Sample" = names(barplot.data),
        "Sum" = barplot_data,
        stringsAsFactors = FALSE
        );

    barplot.df <- barplot.df[order(-barplot.df$Sum), ];

    barplot.df$Sample <- factor(
        x = barplot.df$Sample,
        levels = barplot.df$Sample
        );

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