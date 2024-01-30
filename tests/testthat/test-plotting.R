# Mainly checking that the three plotting functions
# 1. get.qc.barplot
# 2. get.qc.heatmap
# 3. get.qc.multipanelplot

# don't run into errors when the input is correct vs. incorrect

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

test_that("get.qc.barplot", {

  # Correct input
  expect_error(
    get.qc.barplot(accumulated_scores),
    NA
  )

  # Incorrect data
  expect_error(
    get.qc.barplot(zscores)
  )

  # Incorrect specification
  incorrect_specification <- accumulated_scores
  incorrect_specification$Sample <- 1:nrow(incorrect_specification)
  expect_error(
    get.qc.barplot(incorrect_specification)
  )

})

test_that("get.qc.heatmap", {

  # Correct input
  expect_error(
    get.qc.heatmap(
      zscores = corrected_zscores,
      quality.scores = accumulated_scores
    ),
    NA
  )

  # zscores - no sample id's
  no_id_zscores <- corrected_zscores
  rownames(no_id_zscores) <- NULL
  expect_error(
    get.qc.heatmap(
      zscores = no_id_zscores,
      quality.scores = accumulated_scores
    )
  )

  # accumulated_scores - wrong column names
  incorrect_accumulated_scores <- accumulated_scores
  colnames(incorrect_accumulated_scores) <- c("A", "B")
  expect_error(
    get.qc.heatmap(
      zscores = corrected_zscores,
      quality.scores = incorrect_accumulated_scores
    )
  )

  # accumulated_scores - sample order (factor/character)
  character_samples <- accumulated_scores
  character_samples$Sample <- as.character(character_samples$Sample)
  expect_error(
    get.qc.heatmap(
      zscores = corrected_zscores,
      quality.scores = character_samples
    ),
    NA
  )

  # mismatched samples in accumulated_scores/zscores
  missing_samples_zscores <- corrected_zscores[1:50,]
  expect_error(
    get.qc.heatmap(
      zscores = missing_samples_zscores,
      quality.scores = accumulated_scores
    )
  )

  # missing data in zscores
  missing_data_zscores <- corrected_zscores
  zscores[1,1] <- NA
  expect_error(
    get.qc.heatmap(
      zscores = missing_data_zscores,
      quality.scores = accumulated_scores
    ),
    NA
  )

})

test_that("get.qc.multipanelplot", {

  # Correct input
  bp <- get.qc.barplot(accumulated_scores)
  hm <- get.qc.heatmap(zscores = corrected_zscores, quality.scores = accumulated_scores)
  expect_error(
    get.qc.multipanelplot(barplot = bp, heatmap = hm),
    NA
  )

})
