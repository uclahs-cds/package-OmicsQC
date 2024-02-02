#' QC metrics across 100 samples
#'
#' @name example.qc.dataframe
#'
#' @docType data
#'
#' @usage data(example.qc.dataframe)
#'
#' @format A data frame containing QC data; columns represent QC metrics and rows represent samples
#'
#' @keywords datasets
#'
#'
#' @examples
#' \donttest{
#' data(example.qc.dataframe)
#' zscores.from.metrics(
#'   qc.data = example.qc.dataframe
#'   );
#' }
NULL;

#' Directionality of QC metrics
#'
#' @name sign.correction
#'
#' @docType data
#'
#' @usage data(sign.correction)
#'
#' @format A data frame containing the following columns: Metric, Sign
#' \describe{
#'     \item{Metric}{Quality control metrics, corresponding to the metrics in `example.qc.dataframe`}
#'     \item{Sign}{Directionality of each metric; positive (pos) means a higher metric is better, negative (neg) means a lower metric is better}
#'}
#'
#' @keywords datasets
#'
#'
#' @examples
#' \donttest{
#' data(sign.correction)
#' data(example.qc.dataframe)
#' correct.zscore.signs(
#'   zscores = example.qc.dataframe,
#'   signs.data = sign.correction,
#'   metric.col.name = 'Metric',
#'   signs.col.name = 'Sign',
#'   );
#' }
NULL;

#' Formatted QC metrics labels
#'
#' @name ylabels
#'
#' @docType data
#'
#' @usage data(ylabels)
#'
#' @format A character vector of formatted QC metric labels
#'
#' @keywords datasets
#'
#'
#' @examples
#' \donttest{
#' data(ylabels)
#' data(example.qc.dataframe)
#' data(sign.correction)
#' zscores <- zscores.from.metrics(qc.data = example.qc.dataframe);
#' zscores.corrected <- correct.zscore.signs(
#'   zscores = zscores,
#'   signs.data = sign.correction,
#'   metric.col.name = 'Metric',
#'   signs.col.name = 'Sign'
#' );
#' quality.scores <- accumulate.zscores(zscores.corrected = zscores.corrected);
#' qc.heatmap <- get.qc.heatmap(
#'   zscores = zscores.corrected,
#'   quality.scores = quality.scores,
#'   yaxis.lab = ylabels
#' );
#' }
NULL;
