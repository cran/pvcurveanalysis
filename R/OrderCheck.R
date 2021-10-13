#' Order Check
#'
#' Checks for the correct ordering of the data: increasing for date.and.time and time.since start, decreasing for fresh.mass and
#' water.potential. Done separatly for each sample. An individualized warning is printed if not ordered correctly.
#'
#' @param data data frame containing the data to be checked
#' @param sample name of the column containing the sample IDs, if present in data
#' @param fresh.mass name of the column containing the numeric fresh mass values, if present in data
#' @param water.potential name of the column containing the numeric water potential values, if present in data
#' @return no return value
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls




OrderCheck <- function(data,
                       sample = FALSE,
                       fresh.mass = FALSE,
                       water.potential = FALSE) {
  for (i in unique(data[[sample]])) {
    # subset data
    sub.sample <- unique(data[[sample]])[i]
    data_in_subset <- subset(data, sample == i)


    # check for anticipated order
    if (fresh.mass != FALSE) {
      if (any(diff(na.omit(data_in_subset[[fresh.mass]]))  >= 0)) {
        warning(
          paste0("sample ", sub.sample),
          ": Fresh mass values are not strictly descending"
        )
      }
    }

    if (water.potential != FALSE) {
      if (any(diff(na.omit(data_in_subset[[water.potential]]))  >= 0.1)) {
        warning(
          paste0("sample ", sub.sample),
          ": Water potential values do not seem to be in descending order"
        )
      }
    }

  }
}
