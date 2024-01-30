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

# Corrected signs
correct_signs <- zscores
correct_signs[,"B"] <- -correct_signs[,"B"]
correct_signs[correct_signs > 0 ] <- 0

# Accumulated zscores
accumulated_scores <- data.frame(
  "Sample" = rownames(correct_signs),
  "Sum" = rowSums(correct_signs)
)
accumulated_scores <- accumulated_scores[order(accumulated_scores$Sum, decreasing = T),]
accumulated_scores$Sample <- factor(accumulated_scores$Sample, levels = accumulated_scores$Sample)

test_that("zscore.from.metrics", {

  # Expected calculation of z-scores
  res <- zscores.from.metrics(scores)
  expect_equal(res, zscores)

  # NA in Input
  na_scores <- scores
  na_scores[1,1] <- NA
  expect_error(
    zscores.from.metrics(na_scores)
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
  res <- correct.zscore.signs(zscores = zscores, signs.data = signs, metric.col.name = "Metric", signs.col.name = "Sign")
  expect_equal(res, correct_signs)

  # Missing columns
  missing_columns <- signs[1:2,]
  expect_error(
    correct.zscore.signs(
      zscores = zscores,
      signs.data = missing_columns,
      metric.col.name = "Metric",
      signs.col.name = "Sign"
    )
  )

  # Incorrect column names
  expect_error(
    correct.zscore.signs(
      zscores = zscores,
      signs.data = signs,
      metric.col.name = "M",
      signs.col.name = "Sign"
    )
  )

  # Extra columns
  extra_columns <- rbind(signs, data.frame("Metric" = "K", "Sign" = "pos"))
  expect_error(
    correct.zscore.signs(
      zscores = zscores,
      signs.data = extra_columns,
      metric.col.name = "Metric",
      signs.col.name = "Sign"
    )
  )

  # Incorrect specification of 'pos' and 'neg'
  incorrect_specification <- cbind(signs, "Incorrect" = c("+", "-", "+"))
  expect_error(
    correct.zscore.signs(
      zscores = zscores,
      signs.data = incorrect_specification,
      metric.col.name = "Metric",
      signs.col.name = "Incorrect"
    )
  )

})

test_that("accumulate.zscores", {

  # Expected calculation of accumulation of z-scores
  res <- accumulate.zscores(zscores.corrected = correct_signs)
  expect_equal(res, accumulated_scores)

  # Missing sample names?
  missing_sample_names <- correct_signs
  rownames(missing_sample_names) <- NULL
  expect_error(
    accumulate.zscores(
      zscores.corrected = missing_sample_names
    )
  )

  # NA data - currently does not support missing data
  # Need to be fixed in future iterations
  na_zscores <- correct_signs
  na_zscores[1,1] <- NA
  expect_error(
    accumulate.zscores(na_zscores)
  )

})
