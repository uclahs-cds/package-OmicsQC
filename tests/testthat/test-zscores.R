# Test the z-score related functions
# 1. Accumulate zscores
# 2. Correct zscores
# 3. Metric zscores

# Since these are pretty basic functions, we want to test
# 1. Input is correct
# 2. Output is correct for simple cases

# Simulating scores
set.seed(314);
scores <- data.frame(
  'A' = rnorm(100, mean = 2, sd = 3),
  'B' = rnorm(100, mean = 3, sd = 5),
  'C' = rnorm(100, mean = -5, sd = 7)
);
rownames(scores) <- paste0('Patient_', 1:100);

# Simulating Z scores
zscores <- apply(scores, 2, function(x) scale(x, center = TRUE, scale = TRUE));
rownames(zscores) <- paste0('Patient_', 1:100);

# Signs
signs <- data.frame(
  'Metric' = c('A', 'B', 'C'),
  'Sign' = c('pos', 'neg', 'pos')
);

# Corrected signs
correct.signs <- zscores;
correct.signs[,'B'] <- -correct.signs[,'B'];
correct.signs[correct.signs > 0 ] <- 0;

# Accumulated zscores
accumulated.scores <- data.frame(
  'Sample' = rownames(correct.signs),
  'Sum' = rowSums(correct.signs)
);
accumulated.scores <- accumulated.scores[order(accumulated.scores$Sum, decreasing = T),];
accumulated.scores$Sample <- factor(accumulated.scores$Sample, levels = accumulated.scores$Sample);

test_that('zscore.from.metrics', {

  # Expected calculation of z-scores
  res <- zscores.from.metrics(scores);
  expect_equal(res, zscores);

  # NA in Input
  na.scores <- scores;
  na.scores[1,1] <- NA;
  expect_error(
    zscores.from.metrics(na.scores)
  );

  # Non-dataframe input
  expect_error(
    zscores.from.metrics(rnorm(100)),
    'dim(X) must have a positive length',
    fixed = TRUE
  );

  # Character column
  expect_error(
    zscores.from.metrics(data.frame('A' = LETTERS, 'B' = 1:26)),
    'dataframe is not fully numeric'
  );

});

test_that('correct.zscore.signs', {

  # Expected calculation of z-score sign correction
  res <- correct.zscore.signs(
    zscores = zscores,
    signs.data = signs,
    metric.col.name = 'Metric',
    signs.col.name = 'Sign'
  );
  expect_equal(res, correct.signs);

  # Missing columns
  missing.columns <- signs[1:2,];
  expect_error(
    correct.zscore.signs(
      zscores = zscores,
      signs.data = missing.columns,
      metric.col.name = 'Metric',
      signs.col.name = 'Sign'
    )
  );

  # Incorrect column names
  expect_error(
    correct.zscore.signs(
      zscores = zscores,
      signs.data = signs,
      metric.col.name = 'M',
      signs.col.name = 'Sign'
    )
  );

  # Extra columns
  extra.columns <- rbind(signs, data.frame('Metric' = 'K', 'Sign' = 'pos'));
  expect_error(
    correct.zscore.signs(
      zscores = zscores,
      signs.data = extra.columns,
      metric.col.name = 'Metric',
      signs.col.name = 'Sign'
    )
  );

  # Incorrect specification of 'pos' and 'neg'
  incorrect.specification <- cbind(signs, 'Incorrect' = c('+', '-', '+'));
  expect_error(
    correct.zscore.signs(
      zscores = zscores,
      signs.data = incorrect.specification,
      metric.col.name = 'Metric',
      signs.col.name = 'Incorrect'
    )
  );

});

test_that('accumulate.zscores', {

  # Expected calculation of accumulation of z-scores
  res <- accumulate.zscores(zscores.corrected = correct.signs);
  expect_equal(res, accumulated.scores);

  # Missing sample names?
  missing.sample.names <- correct.signs;
  rownames(missing.sample.names) <- NULL;
  expect_error(
    accumulate.zscores(
      zscores.corrected = missing.sample.names
    )
  );

  # NA data - currently does not support missing data
  # Need to be fixed in future iterations
  na.zscores <- correct.signs;
  na.zscores[1,1] <- NA;
  expect_error(
    accumulate.zscores(na.zscores)
  );

});
