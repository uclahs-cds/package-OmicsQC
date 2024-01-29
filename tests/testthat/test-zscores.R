# Test the z-score related functions
# 1. Accumulate zscores
# 2. Correct zscores
# 3. Metric zscores

# Since these are pretty basic functions, we want to test
# 1. Input is correct
# 2. Output is correct for simple cases

# Simulating scores
set.seed(314)
scores <- data.frame(
  "A" = rnorm(100, mean = 2, sd = 3),
  "B" = rnorm(100, mean = 3, sd = 5),
  "C" = rnorm(100, mean = -5, sd = 7)
)
rownames(scores) <- paste0("Patient_", 1:100)

# Simulating Z scores
zscores <- apply(scores, 2, function(x) scale(x, center = TRUE, scale = TRUE))
rownames(zscores) <- paste0("Patient_", 1:100)

# Signs
signs <- data.frame(
  "Metric" = c("A", "B", "C"),
  "Sign" = c("pos", "neg", "pos")
)

test_that("zscore.from.metrics", {

  # Expected calculation of z-scores
  res <- zscores.from.metrics(scores)
  expect_equal(res, zscores)

  # NA in Input
  na_scores <- scores
  na_scores[1,1] <- NA
  expect_error(
    zscores.from.metrics(na_scores),
    NA
  )

  # Non-dataframe input
  expect_error(
    zscores.from.metrics(rnorm(100)),
    "dim(X) must have a positive length",
    fixed = TRUE
  )

  # Character column
  expect_error(
    zscores.from.metrics(data.frame("A" = LETTERS, "B" = 1:26)),
    "dataframe is not fully numeric"
  )

})

test_that("correct.zscore.signs", {

  # Expected calculation of z-score sign correction

  # Missing columns

  # Extra columns

  # Incorrect specification of 'pos' and 'neg'

})

test_that("accumulate.zscores", {

  # Expected calculation of accumulation of z-scores

  # Missing sample names?

})
