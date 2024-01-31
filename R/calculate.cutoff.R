#' Calculates the cutoff for what the largest observed value has to be, in order to be considered an outlier.
#'
#' @param theoretical.data.quantile.max The largest theoretical value from quantile estimation
#' @param cos.sim.cutoff The cosine similarity value which is on the threshold to having a significant p-value
#' @noRd
calculate.cutoff <- function(theoretical.data.quantile.max, cos.sim.cutoff) {
    a <- 1 - 2 * cos.sim.cutoff^2;
    b <- 2 * theoretical.data.quantile.max;
    c <- theoretical.data.quantile.max^2 * (1 - 2 * cos.sim.cutoff^2);

    cutoff <- (- b - sqrt(b^2 - 4 * a * c)) / (2 * a);

    return(cutoff);
    }
