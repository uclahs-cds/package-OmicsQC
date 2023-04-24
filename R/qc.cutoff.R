#' Fits the QC data to distributions and returns the KS test result and BIC score
#'
#' This function takes the accumulated QC scores, a vector of distributions
#' and a trimming factor. It then returns the results for each distribution
#' in a dataframe. This function supports the following distributions:
#' * 'weibull'
#' * 'norm'
#' * 'gamma'
#' * 'exp'
#' * 'lnorm'
#' * 'cauchy'
#' * 'logis'
#'
#' @param quality.scores The accumulated QC scores, the output of accumulate.zscores
#' @param distributions A vector of distributions to fit and test
#' @param trim.factor The fraction of extremes on each end to trim before fitting
#' @export
fit.and.evaluate <- function(
    quality.scores,
    distributions = c('weibull', 'norm', 'gamma', 'exp', 'lnorm', 'cauchy', 'logis'),
    trim.factor = 0.05
    )
    {

    distributions.available <- c('weibull', 'norm', 'gamma', 'exp', 'lnorm', 'cauchy', 'logis');

    if (!all(distributions %in% distributions.available)) {
        stop('At least one of the distributions is not available. Please see documentation.')
    }

    no.distributions <- length(distributions);
    KS.rejected <- logical(no.distributions);
    BIC.value <- numeric(no.distributions);

    no.samples <- nrow(quality.scores);
    trim.num <- round(no.samples * trim.factor);

    quality.scores <- quality.scores[-((no.samples - trim.num + 1):no.samples), ];
    quality.scores <- quality.scores[-(1:trim.num), ];

    for (i in seq_len(no.distributions)){

        fit <- fitdistrplus::fitdist(-quality.scores$Sum, distributions[i]);
        gof <- fitdistrplus::gofstat(fit);

        KS.rejected[i] <- gof$kstest == 'rejected';
        BIC.value[i] <- fit$bic;
        }

    results.df <- data.frame(
        distribution = distributions,
        KS.rejected = KS.rejected,
        BIC.value = BIC.value
        );

    results.df <- results.df[order(results.df$BIC.value), ];

    return(results.df);
    }