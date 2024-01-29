#' Test if a dataframe is fully numeric. Raises and error if the dataframe is not.
#'
#' @param dataframe The dataframe one wants to test
#' @noRd
numeric.df.check <- function(dataframe) {

    dataframe.numeric <- all(apply(dataframe, 2, is.numeric));

    if (!dataframe.numeric) {
        stop('dataframe is not fully numeric');
        }

    return(NULL);
    }
