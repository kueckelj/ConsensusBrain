## agreement_metrics.R
# Functions to quantify agreement among raters on an ordinal scale (1-4)

#' Proportion of ratings in the modal category
#' @param ratings Numeric vector of ratings (length >= 1)
#' @return Numeric proportion in [0,1]
modal_prop <- function(ratings) {
  freqs <- table(ratings)
  max(freqs) / length(ratings)
}

#' Normalized Shannon entropy consensus index
#' @param ratings Numeric vector of ratings (length >= 1)
#' @return Numeric consensus index in [0,1]
shannon_entropy <- function(ratings) {
  freqs <- table(ratings)
  p <- freqs / sum(freqs)
  # Compute entropy (treat 0*log2(0) as 0)
  H <- -sum(p * log2(p), na.rm = TRUE)
  H_max <- log2(length(freqs))
  1 - (H / H_max)
}

#' Mean absolute deviation from the median
#' @param ratings Numeric vector of ratings (length >= 1)
#' @return Numeric mean absolute deviation
dev_from_med <- function(ratings) {
  m <- median(ratings)
  mean(abs(ratings - m))
}

#' Proportion of pairwise exact agreement
#' @param ratings Numeric vector of ratings (length >= 2)
#' @return Numeric proportion in [0,1]
pair_exact_agreement <- function(ratings) {
  n <- length(ratings)
  # Generate all unique pairs
  pairs <- combn(ratings, 2)
  exact <- pairs[1, ] == pairs[2, ]
  mean(exact)
}

#' Weighted pairwise agreement index for ordinal ratings
#'
#' @param ratings Numeric vector of ratings (length >= 2)
#' @param scale_min Integer minimum possible rating (default = 1)
#' @param scale_max Integer maximum possible rating (default = 4)
#' @return Numeric agreement index in [0,1]
#' @details
#' Implements
#' \deqn{w_{ij} = 1 - \frac{|r_i - r_j|}{(scale\_max - scale\_min)}}
#' then averages over all unique pairs. Respects ordinal distances:
#' - \eqn{w=1} if two ratings match exactly
#' - \eqn{w=0} if they differ by the full range
#'
#' @examples
#' ratings <- c(1,2,2,4,3,1,4)
#' weighted_pair_agreement(ratings)
weighted_pair_agreement <- function(ratings, scale_min = 1, scale_max = 4) {
  n <- length(ratings)
  denom <- scale_max - scale_min
  pairs <- combn(ratings, 2)
  diffs <- abs(pairs[1, ] - pairs[2, ])
  weights <- 1 - (diffs / denom)
  mean(weights)
}


compute_voxel_agreement_score <- function(ratings) {
  counts <- table(factor(ratings, levels = 1:4))
  p <- counts / sum(counts)

  entropy <- -sum(p * log(p + 1e-9)) / log(4)  # Normalize to 0–1
  agreement <- 1 - entropy

  bimodality <- 4 * p["1"] * p["4"]
  central_ratings <- p["2"] + p["3"]

  if (bimodality > 0.25 && central_ratings < 0.1) {
    agreement <- agreement - 2 * bimodality  # Penalize for directed disagreement
  }

  return(agreement)
}



