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


#' Test if a dataframe is the valid output of accumulate.zscores
#'
#' @param quality.scores The dataframe one wants to test
#' @noRd
accumulate.zscores.output.check <- function(quality.scores) {

    stopifnot(is.data.frame(quality.scores))

    if(!("Sum" %in% colnames(quality.scores)) || !("Sample" %in% colnames(quality.scores))){
        stop("quality.scores must be a data.frame that contains the columns Sum and Sample");
        }

    if(!is.numeric(quality.scores[,"Sum"])){
        stop("The column Sum in quality.scores must be numeric");
        }

    if(!is.character(quality.scores[,"Sample"]) && !is.factor(quality.scores[,"Sample"])){
        stop("Sample ids must be character or factor");
        }

    if(length(quality.scores[,"Sample"]) != length(unique(quality.scores[,"Sample"]))){
        stop("Sample ids must be unique");
        }

    return(NULL);
    }
