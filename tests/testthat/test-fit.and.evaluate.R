# Here we want to test
# 1. Input checking
# 2. Whether simulated data is fitted with the correct distribution

# Simulating scores
set.seed(314)
scores <- data.frame(
  "A" = rnorm(100, mean = 2, sd = 3),
  "B" = rnorm(100, mean = 3, sd = 5),
  "C" = rnorm(100, mean = -5, sd = 7)
)
rownames(scores) <- paste0("Patient_", 1:100)

# Signs
signs <- data.frame(
  "Metric" = c("A", "B", "C"),
  "Sign" = c("pos", "neg", "pos")
)

# Data Processing
zscores <- zscores.from.metrics(scores)
corrected_zscores <- correct.zscore.signs(
  zscores = zscores,
  signs.data = signs,
  metric.col.name = "Metric",
  signs.col.name = "Sign"
)
accumulated_scores <- accumulate.zscores(corrected_zscores)

# Tests
test_that("fit.and.evaluate", {

  # Expected output
  # weibull, gamma, exp, lnorm can only accept positive values
  # todo: fix this
  res <- fit.and.evaluate(
    quality.scores = accumulated_scores,
    distributions = c('norm', 'cauchy', 'logis'),
    trim.factor = 0.05
  )
  expect_equal(
    colnames(res),
    c("distribution", "KS.rejected", "BIC.value")
  )
  expect_true(
    is.character(res$distribution)
  )
  expect_true(
    setequal(res$distribution, c('norm', 'cauchy', 'logis'))
  )
  expect_true(
    is.logical(res$KS.rejected)
  )
  expect_true(
    is.numeric(res$BIC.value)
  )

  # Bad trim factor
  expect_error(
    fit.and.evaluate(
      quality.scores = accumulated_scores,
      distributions = c('weibull', 'norm', 'gamma', 'exp', 'lnorm', 'cauchy', 'logis'),
      trim.factor = 0.9
    )
  )

  # No data left after trimming
  expect_error(
    fit.and.evaluate(
      quality.scores = accumulated_scores,
      distributions = c('weibull', 'norm', 'gamma', 'exp', 'lnorm', 'cauchy', 'logis'),
      trim.factor = 0.5
    )
  )

  # Bad input
  bad_input <- accumulated_scores
  bad_input$Sum <- sample(LETTERS, size = nrow(bad_input), replace = T)
  expect_error(
    fit.and.evaluate(
      quality.scores = bad_input,
      distributions = c('norm'),
      trim.factor = 0.05
    )
  )

  bad_input <- accumulated_scores
  colnames(bad_input)[colnames(bad_input) == "Sum"] <- "XX"
  expect_error(
    fit.and.evaluate(
      quality.scores = bad_input,
      distributions = c('norm'),
      trim.factor = 0.05
    )
  )

  # Incorrect distributions
  expect_error(
    fit.and.evaluate(
      quality.scores = accumulated_scores,
      distributions = c('Norm'),
      trim.factor = 0.05
    )
  )


})
