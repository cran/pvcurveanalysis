#' Relative Water Deficit (RWD)
#'
#' Calculates relative water deficit (\%)
#'
#' @param data data frame with columns of equal length containing at least columns with the fresh mass (g),
#' the dry mass (g) and the saturated fresh mass (g)
#' @param fresh.mass optional name of the column in data containing
#' the numeric fresh mass values (g); default: fresh.mass
#' @param dry.mass optional name of the column in data containing
#' the numeric dry mass values (g); default: dry.mass
#' @param fresh.mass.saturated optional name of the column in data containing
#' the numeric saturated fresh mass values (g); default: fresh.mass.saturated
#' @details Relative water deficit (\%) is calculated as:
#' \deqn{RWD = 100 - 100 * ((FM - DM) (FMs - DM)^-1)}
#' whereas FM = fresh mass, DM = dry mass and FMs = fresh mass at water saturation.
#' @return the original data frame extended by a numeric column with the relative water deficit (RWD) (\%).
#' @examples
#' # get example data
#' df <- pressure_volume_data
#'
#' # extend df by RWD
#' df_with_RWD <- RelativeWaterDeficit(df)
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls
#'
#' @export

RelativeWaterDeficit <- function(data,
                                 fresh.mass = "fresh.mass",
                                 dry.mass = "dry.mass",
                                 fresh.mass.saturated = "fresh.mass.saturated") {
  # check validity of data
  data_in <-
    ValidityCheck(
      data,
      fresh.mass = fresh.mass,
      dry.mass = dry.mass,
      fresh.mass.saturated = fresh.mass.saturated
    )


  # calculate RWD
  RWD <- 100 - ((data_in[[fresh.mass]] - data_in[[dry.mass]]) /
                  (data_in[[fresh.mass.saturated]] - data_in[[dry.mass]])) *
    100


  return(data.frame(data, RWD))
}
