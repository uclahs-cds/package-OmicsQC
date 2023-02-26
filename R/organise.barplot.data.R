# ' Sign correct statistical scores and sum across for total sample score
# '
# ' This function takes a dataframe of all the negative scores and aggregates
# ' to a form which can be plotted as a a barplot.
# '
# ' @param qc.plotting.data A dataframe whose rows are samples and each column a QC metric
# ' @param filename A filename where to save data. If NULL data will not be saved to file
# ' @return A dataframe of z-scores for each metric
# ' @export
organise.barplot.data <- function(qc.plotting.data, filename = NULL) {

    qc.plot.data.numeric <- all(apply(qc.plotting.data, 2, is.numeric))

    if (!qc.plot.data.numeric) {
        stop("qc.plotting.data is not fully numeric")
        }

    barplot.data <- colSums(qc.plotting.data);

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
            x = qc.plotting.data,
            file = filename,
            quote = FALSE,
            row.names = TRUE,
            col.names = TRUE
            );
        }

    return(barplot.df)
    }