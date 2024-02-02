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
#' @param quality.scores A dataframe with columns 'Sum' (of scores) and 'Sample', i.e. the output of accumulate.zscores
#' @param no.simulations The number of datasets to simulate
#' @param trim.factor What fraction of values of each to trim to get parameters without using extremes
#' @param alpha.significant Alpha value for significance
#' @param distribution A distribution to test, will default to 'lnorm'
#' @export
cosine.similarity.cutoff <- function(
    quality.scores,
    no.simulations,
    distribution = c('lnorm', 'weibull', 'norm', 'gamma', 'exp', 'cauchy', 'logis'),
    trim.factor = 0.05,
    alpha.significant = 0.05
    ) {

    # Error checking
    accumulate.zscores.output.check(quality.scores);
    check.valid.no.simulations(no.simulations);
    distribution <- match.arg(distribution);
    check.valid.trim.factor(trim.factor);
    check.valid.alpha.significant(alpha.significant);

    # Defining variables
    no.samples <- nrow(quality.scores);
    trim.num <- round(no.samples * trim.factor);

    # Trimming quality scores
    quality.scores.trimmed <- quality.scores[-((no.samples - trim.num + 1):no.samples), ];
    quality.scores.trimmed <- quality.scores.trimmed[-(1:trim.num), ];

    # Fitting distribution and extracting parameters
    fit <- fitdistrplus::fitdist(-quality.scores.trimmed$Sum, distribution);
    p <- stats::ppoints(-quality.scores$Sum);
    args <- as.list(fit$estimate);
    args.q <- c(args, list('p' = p));
    args.r <- c(args, list('n' = no.samples));

    # Simulating data
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

    # Initiating empty vector for cosine similarity
    cos.similarity.nulldist <- numeric(no.simulations);

    # Calculating a theoretical quantile set
    theoretical.data.quantile <- do.call(
        what = paste0('q', distribution),
        args = args.q
        );

    # Comparing each simulated dataset to the theoretical
    # quantile set using cosine similarity
    for (i in 1:no.simulations) {
        simulated.data.quantile <- stats::quantile(
            x = simulated.distributions[i, ],
            prob = p
            );

        cos.similarity.nulldist[i] <- lsa::cosine(
            x = c(max(simulated.data.quantile), max(theoretical.data.quantile)),
            y = c(1, 1)
            );
        }

    # Calculating quantiles of cosine similarity and
    # determining threshold of cosine similarity required to achieve significance
    alpha.cutoff.cos.sim <- stats::quantile(
        x = cos.similarity.nulldist,
        prob = c(alpha.significant)
        );

    # Calculating cutoff and nominating outliers
    cutoff <- calculate.cutoff(max(theoretical.data.quantile), alpha.cutoff.cos.sim);

    no.outliers <- sum(-quality.scores$Sum > cutoff);

    # Returning results
    results <- list(
        'cutoff' = cutoff,
        'no.outliers' = no.outliers,
        'outlier.labels' = as.character(quality.scores$Sample[-quality.scores$Sum > cutoff])
        );

    return(results);
    }
