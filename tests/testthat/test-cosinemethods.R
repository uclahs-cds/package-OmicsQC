# Testing cosine similarity cutoff
# Testing cosine iterative
# Testing calculate cutoff

# Data loading
data('example.qc.dataframe')
data('sign.correction')

# Data processing
zscores <- zscores.from.metrics(qc.data = example.qc.dataframe);
corrected.zscores <- correct.zscore.signs(
  zscores = zscores,
  signs.data = sign.correction
);
accumulated.scores <- accumulate.zscores(zscores.corrected = corrected.zscores);

# Distributions
distributions <- c('lnorm', 'weibull', 'norm', 'gamma', 'exp', 'cauchy', 'logis');

# Setting seed to acquire reproducible test results
# todo: fix this in the future with built-in seed
set.seed(314);

test_that('cosine.similarity.cutoff', {

  # Test that this function works for all distributions
  for (distr in distributions) {
    expect_error(
      cosine.similarity.cutoff(
        quality.scores = accumulated.scores,
        no.simulations = 20,
        distribution = distr,
        trim.factor = 0.05,
        alpha.significant = 0.1
      ),
      NA
    )
  };

  # Test output format
  res <- cosine.similarity.cutoff(
    quality.scores = accumulated.scores,
    no.simulations = 100,
    distribution = 'norm',
    trim.factor = 0.05,
    alpha.significant = 0.1
  );
  expect_named(
    res,
    c('cutoff', 'no.outliers', 'outlier.labels')
  );
  expect_true(
    is.numeric(res[['cutoff']])
  );
  expect_true(
    is.numeric(res[['no.outliers']])
  );
  expect_true(
    is.character(res[['outlier.labels']])
  );
  expect_equal(res[['no.outliers']], 6);
  expect_equal(
    res[['outlier.labels']],
    c('CPCG0235', 'CPCG0437', 'CPCG0464', 'CPCG0498', 'CPCG0382', 'CPCG0375')
  );

  # Incorrect number of simulations
  expect_error(
    cosine.similarity.cutoff(
      quality.scores = accumulated.scores,
      no.simulations = 0,
      distribution = 'norm',
      trim.factor = 0.05,
      alpha.significant = 0.1
    )
  );

  # Incorrect trim factor
  expect_error(
    cosine.similarity.cutoff(
      quality.scores = accumulated.scores,
      no.simulations = 10,
      distribution = 'norm',
      trim.factor = 0.51,
      alpha.significant = 0.1
    )
  );

  expect_error(
    cosine.similarity.cutoff(
      quality.scores = accumulated.scores,
      no.simulations = 10,
      distribution = 'norm',
      trim.factor = -0.1,
      alpha.significant = 0.1
    )
  );

  # Incorrect alpha
  expect_error(
    cosine.similarity.cutoff(
      quality.scores = accumulated.scores,
      no.simulations = 10,
      distribution = 'norm',
      trim.factor = 0.1,
      alpha.significant = -0.1
    )
  );

  expect_error(
    cosine.similarity.cutoff(
      quality.scores = accumulated.scores,
      no.simulations = 10,
      distribution = 'norm',
      trim.factor = 0.1,
      alpha.significant = 1.1
    )
  );

});

test_that('cosine.similarity.iterative', {

  # Test that this function works for all distributions
  for (distr in distributions) {
    expect_error(
      cosine.similarity.iterative(
        quality.scores = accumulated.scores,
        no.simulations = 20,
        distribution = distr,
        trim.factor = 0.05,
        alpha.significant = 0.05
      ),
      NA
    )
  };

  # Test output format
  res <- cosine.similarity.iterative(
    quality.scores = accumulated.scores,
    no.simulations = 100,
    distribution = 'norm',
    trim.factor = 0.05,
    alpha.significant = 0.1
  );
  expect_named(
    res,
    c('no.outliers', 'outlier.labels')
  )
  expect_true(
    is.numeric(res[['no.outliers']])
  );
  expect_true(
    is.character(res[['outlier.labels']])
  );
  expect_equal(res[['no.outliers']], 10);
  expect_equal(
    res[['outlier.labels']],
    c('CPCG0375', 'CPCG0382', 'CPCG0498', 'CPCG0464', 'CPCG0437',
      'CPCG0235', 'CPCG0486', 'CPCG0263', 'CPCG0266', 'CPCG0256')
  );

  # Incorrect number of simulations
  expect_error(
    cosine.similarity.iterative(
      quality.scores = accumulated.scores,
      no.simulations = 0,
      distribution = 'norm',
      trim.factor = 0.05,
      alpha.significant = 0.1
    )
  );

  # Incorrect trim factor
  expect_error(
    cosine.similarity.iterative(
      quality.scores = accumulated.scores,
      no.simulations = 10,
      distribution = 'norm',
      trim.factor = 0.51,
      alpha.significant = 0.1
    )
  );

  expect_error(
    cosine.similarity.iterative(
      quality.scores = accumulated.scores,
      no.simulations = 10,
      distribution = 'norm',
      trim.factor = -0.1,
      alpha.significant = 0.1
    )
  );

  # Incorrect alpha
  expect_error(
    cosine.similarity.iterative(
      quality.scores = accumulated.scores,
      no.simulations = 10,
      distribution = 'norm',
      trim.factor = 0.1,
      alpha.significant = -0.1
    )
  );

  expect_error(
    cosine.similarity.iterative(
      quality.scores = accumulated.scores,
      no.simulations = 10,
      distribution = 'norm',
      trim.factor = 0.1,
      alpha.significant = 1.1
    )
  );

});

# test_that('calculate.cutoff', {})
