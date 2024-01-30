#' Test if the zscore dataframe is correctly formatted. Raises and error if it is not.
#'
#' @param zscores The dataframe one wants to test
#' @noRd
zscore.format.check <- function(zscores) {

    # Numeric data frame
    dataframe.numeric <- all(apply(zscores, 2, is.numeric));

    if (!dataframe.numeric) {
        stop('dataframe is not fully numeric');
        }

    # Column names and row names
    if(is.null(rownames(zscores))){
        stop("Please specify sample IDs by setting the rownames");
        }

    if(is.null(colnames(zscores))){
        stop("Please specify metrics by setting the colnames");
        }

    # Missing data
    if(any(is.na(zscores))){
        stop("Current version of OmicsQC does not support missing data")
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

#' Test if a trim.factor is valid
#'
#' @param trim.factor The trim.factor one wants to test
#' @noRd
check.valid.trim.factor <- function(trim.factor){

    if(!is.numeric(trim.factor) || (trim.factor >= 0) || (trim.factor <= 0.5)) {
        stop("trim.factor must be a numeric in the range of [0, 0.5]");
        }

    return(NULL);
    }


#' Test if a alpha.significant is valid
#'
#' @param alpha.significant The alpha.significant one wants to test
#' @noRd
check.valid.alpha.significant <- function(alpha.significant){

      if(!is.numeric(alpha.significant) || (alpha.significant >= 0) || (alpha.significant <= 1)) {
          stop("alpha.significant must be a numeric in the range of [0, 1]");
          }

      return(NULL);
      }

#' Test if a no.simulations is valid
#'
#' @param no.simulations The no.simulations one wants to test
#' @noRd
check.valid.no.simulations <- function(no.simulations){

    if(!is.numeric(no.simulations) || !(no.simulations > 0)){
        stop("no.simulations must be a positive number");
        }

    return(NULL);
    }
