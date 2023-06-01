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
        stop('At least one of the distributions is not available. Please see documentation.');
    }

    no.distributions <- length(distributions);
    KS.rejected <- logical(no.distributions);
    BIC.value <- numeric(no.distributions);

    no.samples <- nrow(quality.scores);
    trim.num <- round(no.samples * trim.factor);

    quality.scores <- quality.scores[- ((no.samples - trim.num + 1):no.samples), ];
    quality.scores <- quality.scores[- (1:trim.num), ];

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


#' Tests the accumulated quality scores for outliers using cosine similarity
#' 
#' This function takes quality.scores, trims it and fits it to the distribution given.
#' It then iteratively tests the largest datapoint compared a null distribution of size
#' no.simulations. If the largest datapoint has a significant p-value it tests the 2nd largest
#' one and so on. The function supports the following distributions:
#' * 'weibull'
#' * 'norm'
#' * 'gamma'
#' * 'exp'
#' * 'lnorm'
#' * 'cauchy'
#' * 'logis'
#'
#' @param quality.scores The output of accumulate.zscores
#' @param no.simulations The number of datasets to simulate
#' @param trim.factor What fraction of values of each to trim to get parameters without using extremes
#' @param alpha.significant Alpha value for significance
#' @param distribution A distribution to test
#' @export
cosine.similarity.iterative <- function(
    quality.scores,
    no.simulations,
    distribution,
    trim.factor = 0.05,
    alpha.significant = 0.05
    ) {

    distributions.available <- c('weibull', 'norm', 'gamma', 'exp', 'lnorm', 'cauchy', 'logis');

    if (!(distribution %in% distributions.available)) {
        stop('Please use one of the available distributions');
        }

    significant.pvalue <- TRUE;
    no.outliers <- 0;
    outlier.labels <- c();

    observed.data <- quality.scores[order(quality.scores$Sum, decreasing = TRUE), ];
    observed.data$Sum <- -observed.data$Sum

    while (significant.pvalue) {

        no.samples <- nrow(observed.data);
        trim.num <- round(no.samples * trim.factor);

        observed.data.trimmed <- observed.data[-((no.samples - trim.num + 1):no.samples), ];
        observed.data.trimmed <- observed.data.trimmed[-(1:trim.num), ];

        fit <- fitdistrplus::fitdist(observed.data.trimmed$Sum, distribution);

        p <- ppoints(observed.data$Sum);
        args <- as.list(fit$estimate);
        args.q <- rlist::list.append(args, p = p);
        args.r <- rlist::list.append(args, n = no.samples);

        observed.data.quantile <- quantile(
            x = observed.data$Sum,
            prob = p
            );

        theoretical.data.quantile <- do.call(
            what = paste0('q', distribution),
            args = args.q
            );

        cos.similarity.obs <- lsa::cosine(
            x = c(max(observed.data.quantile), max(theoretical.data.quantile)),
            y = c(1, 1)
            );

        theoretical.distributions <- matrix(
            data = NA,
            nrow = no.simulations,
            ncol = no.samples
            );

        for (i in 1:no.simulations) {
            theoretical.distributions[i, ] <- do.call(
                what = paste0('r', distribution),
                args = args.r
                );
            }

        cos.similarity.nulldist <- numeric(no.simulations);

        for (i in 1:no.simulations) {
            simulated.data.quantile <- quantile(
                x = theoretical.distributions[i, ],
                prob = p
                );

            cos.similarity.nulldist[i] <- lsa::cosine(
                x = c(max(simulated.data.quantile), max(theoretical.data.quantile)),
                y = c(1, 1)
                );
            }

        simulated.cos.sim.smaller <- sum(cos.similarity.nulldist < cos.similarity.obs[1, 1]);
        pvalue <- simulated.cos.sim.smaller / no.simulations;
        significant.pvalue <- pvalue < alpha.significant;

        if (significant.pvalue) {
            no.outliers <- no.outliers + 1;
            outlier.labels <- append(
                x = outlier.labels,
                values = as.character(observed.data$Sample[observed.data$Sum == max(observed.data$Sum)])
                );
            observed.data <- observed.data[observed.data$Sum != max(observed.data$Sum), ];
            }
        }

    results <- list(
        "no.outliers" = no.outliers,
        "outlier.labels" = outlier.labels
        );

    return(results);
    }

#' Calculate an outlier cutoff using cosine similarity
#'
#' This function takes quality.scores, trims it and fits it to the distribution given.
#' It then simulates as many datasets as stated by no.simulations, and computes the cosine
#' similarity of each dataset against theoretical distribution. It uses what would correspond
#' to a significant value to then calculate what observed value this would correspond to.
#' The function supports the following distributions:
#' * 'weibull'
#' * 'norm'
#' * 'gamma'
#' * 'exp'
#' * 'lnorm'
#' * 'cauchy'
#' * 'logis'
#'
#' @param quality.scores The output of accumulate.zscores
#' @param no.simulations The number of datasets to simulate
#' @param trim.factor What fraction of values of each to trim to get parameters without using extremes
#' @param alpha.significant Alpha value for significance
#' @param distribution A distribution to test
#' @export
cosine.similarity.cutoff <- function(
    quality.scores,
    no.simulations,
    distribution,
    trim.factor = 0.05,
    alpha.significant = 0.05
    ) {

    distributions.available <- c('weibull', 'norm', 'gamma', 'exp', 'lnorm', 'cauchy', 'logis');

    if (!(distribution %in% distributions.available)) {
        stop('Please use one of the available distributions');
        }

    no.samples <- nrow(quality.scores);
    trim.num <- round(no.samples * trim.factor);

    quality.scores.trimmed <- quality.scores[-((no.samples - trim.num + 1):no.samples), ];
    quality.scores.trimmed <- quality.scores.trimmed[-(1:trim.num), ];

    fit <- fitdistrplus::fitdist(-quality.scores.trimmed$Sum, distribution);
    p <- ppoints(-quality.scores$Sum);
    args <- as.list(fit$estimate);
    args.q <- rlist::list.append(args, p = p);
    args.r <- rlist::list.append(args, n = no.samples);

    theoretical.distributions <- matrix(
        data = NA,
        nrow = no.simulations,
        ncol = no.samples
        );

    for (i in 1:no.simulations) {
        theoretical.distributions[i, ] <- do.call(
            what = paste0('r', distribution),
            args = args.r
            );
        }

    cos.similarity.nulldist <- numeric(no.simulations);

    theoretical.data.quantile <- do.call(
        what = paste0('q', distribution),
        args = args.q
        );

    for (i in 1:no.simulations) {
        simulated.data.quantile <- quantile(
            x = theoretical.distributions[i, ],
            prob = p
            );

        cos.similarity.nulldist[i] <- lsa::cosine(
            x = c(max(simulated.data.quantile), max(theoretical.data.quantile)),
            y = c(1, 1)
            );
        }

    theoretical.data.quantile <- do.call(
        what = paste0('q', distribution),
        args = args.q
        );

    alpha.cutoff.cos.sim <- quantile(
        x = cos.similarity.nulldist,
        prob = c(alpha.significant)
        );

    cutoff <- calculate.cutoff(max(theoretical.data.quantile), alpha.cutoff.cos.sim);

    no.outliers <- sum(-quality.scores$Sum > cutoff);

    results <- list(
        "cutoff" = cutoff,
        "no.outliers" = no.outliers,
        "outlier.labels" = as.character(quality.scores$Sample[-quality.scores$Sum > cutoff])
        );

    return(results);
    }

#' Calculates the cutoff for what the largest observed value have to be considered an outlier.
#'
#' @param theoretical.data.quantile.max The largest theoretical value from quantile estimation
#' @param cos.sim.cutoff The cosine similarity value which is on the threshold to having a signficant p-value
#' @noRd
calculate.cutoff <- function(theoretical.data.quantile.max, cos.sim.cutoff){
    a <- 1 - 2 * cos.sim.cutoff^2;
    b <- 2 * theoretical.data.quantile.max;
    c <- theoretical.data.quantile.max^2 * (1 - 2 * cos.sim.cutoff^2);

    cutoff <- (- b - sqrt(b^2 - 4 * a * c)) / (2 * a);

    return(cutoff);
    }