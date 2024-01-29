
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
#' @param distribution A distribution to test, will default to 'lnorm'
#' @export
cosine.similarity.iterative <- function(
    quality.scores,
    no.simulations,
    distribution = c('lnorm', 'weibull', 'norm', 'gamma', 'exp', 'cauchy', 'logis'),
    trim.factor = 0.05,
    alpha.significant = 0.05
    ) {

    distribution <- match.arg(distribution);

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
        args.q <- c(args, list('p' = p));
        args.r <- c(args, list('n' = no.samples));

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

        simulated.distributions <- matrix(
            data = NA,
            nrow = no.simulations,
            ncol = no.samples
            );

        for (i in 1:no.simulations) {
            simulated.distributions[i, ] <- do.call(
                what = paste0('r', distribution),
                args = args.r
                );
            }

        cos.similarity.nulldist <- numeric(no.simulations);

        for (i in 1:no.simulations) {
            simulated.data.quantile <- quantile(
                x = simulated.distributions[i, ],
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