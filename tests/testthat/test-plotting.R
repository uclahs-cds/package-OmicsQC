# Mainly checking that the three plotting functions
# 1. get.qc.barplot
# 2. get.qc.heatmap
# 3. get.qc.multipanelplot

# don't run into errors when the input is correct vs. incorrect

# Simulating scores
set.seed(314);
scores <- data.frame(
  'A' = rnorm(100, mean = 2, sd = 3),
  'B' = rnorm(100, mean = 3, sd = 5),
  'C' = rnorm(100, mean = -5, sd = 7)
);
rownames(scores) <- paste0('Patient_', 1:100);

# Signs
signs <- data.frame(
  'Metric' = c('A', 'B', 'C'),
  'Sign' = c('pos', 'neg', 'pos')
);

# Data Processing
zscores <- zscores.from.metrics(scores);
corrected.zscores <- correct.zscore.signs(
  zscores = zscores,
  signs.data = signs,
  metric.col.name = 'Metric',
  signs.col.name = 'Sign'
);
accumulated.scores <- accumulate.zscores(corrected.zscores);

test_that('get.qc.barplot', {

  # Correct input
  expect_error(
    get.qc.barplot(accumulated.scores),
    NA
  );

  # Incorrect data
  expect_error(
    get.qc.barplot(zscores)
  );

  # Incorrect specification
  incorrect.specification <- accumulated.scores;
  incorrect.specification$Sample <- 1:nrow(incorrect.specification);
  expect_error(
    get.qc.barplot(incorrect.specification)
  );

});

test_that('get.qc.heatmap', {

  # Correct input
  expect_error(
    get.qc.heatmap(
      zscores = corrected.zscores,
      quality.scores = accumulated.scores
    ),
    NA
  );

  # zscores - no sample id's
  no.id.zscores <- corrected.zscores;
  rownames(no.id.zscores) <- NULL;
  expect_error(
    get.qc.heatmap(
      zscores = no.id.zscores,
      quality.scores = accumulated.scores
    )
  );

  # accumulated.scores - wrong column names
  incorrect.accumulated.scores <- accumulated.scores;
  colnames(incorrect.accumulated.scores) <- c('A', 'B');
  expect_error(
    get.qc.heatmap(
      zscores = corrected.zscores,
      quality.scores = incorrect.accumulated.scores
    )
  );

  # accumulated.scores - sample order (factor/character)
  character.samples <- accumulated.scores;
  character.samples$Sample <- as.character(character.samples$Sample);
  expect_error(
    get.qc.heatmap(
      zscores = corrected.zscores,
      quality.scores = character.samples
    ),
    NA
  );

  # mismatched samples in accumulated.scores/zscores
  missing.samples.zscores <- corrected.zscores[1:50,];
  expect_error(
    get.qc.heatmap(
      zscores = missing.samples.zscores,
      quality.scores = accumulated.scores
    )
  );

  # missing data in zscores
  missing.data.zscores <- corrected.zscores;
  zscores[1,1] <- NA;
  expect_error(
    get.qc.heatmap(
      zscores = missing.data.zscores,
      quality.scores = accumulated.scores
    ),
    NA
  );

});

test_that('get.qc.multipanelplot', {

  # Correct input
  bp <- get.qc.barplot(accumulated.scores);
  hm <- get.qc.heatmap(zscores = corrected.zscores, quality.scores = accumulated.scores);
  expect_error(
    get.qc.multipanelplot(barplot = bp, heatmap = hm),
    NA
  );

});
