#' Ensures the validity of the input data
#'
#' @param data data frame containing the data to be checked
#' @param sample name of column containing the sample ID (default: sample)
#' @param dry.mass name of column containing the dry mass (g) (default: dry mass)
#' @param fresh.mass.saturated name of column containing the saturated fresh mass (g) (default: fresh.mass.saturated)
#' @param fresh.mass name of column containing the fresh mass (g) (default: fresh.mass)
#' @param water.potential name of column containing the water potential (MPa) (default: water.potential)
#' @param RWD name of column containing the relative water deficit (default: RWD)
#' @return no return value
#'
#' @import ggplot2
#' @importFrom graphics legend
#' @importFrom stats approx coef confint lm na.omit nls




ValidityCheck <- function(data,
                          sample = FALSE,
                          dry.mass = FALSE,
                          fresh.mass.saturated = FALSE,
                          fresh.mass = FALSE,
                          water.potential = FALSE,
                          RWD = FALSE) {
  # Create dummy sample vector for calculations if not available. put data into new data frame.
  # Check if sample is integer and ordered increasingly
  if(!(sample %in% names(data))) {
    data_in <- cbind(data, sample = rep(1, times = length(data[, 1])))
    if(sample != FALSE){
      warning(paste0("Column ", sample, " is missing in data or is not named as defined. The complete data set is handled as one sample"))
    }
  }else{
    data_in <- data
    if(sample != FALSE){
      if(!(is.integer(data_in[[sample]]))){
        stop(paste0("Column ", sample, " must be of structure integer"))
        if(is.unsorted(data_in[[sample]])){
          stop(paste0("Column ", sample, " must be ordered increasingly"))
        }
      }
    }
  }



  if (fresh.mass != FALSE) {
    ValidityCheckDetail(data_in, fresh.mass)
    if (any(!(round(data_in[[fresh.mass]]) %in% 0:100 |
              (is.na(data_in[[fresh.mass]]))))) {
      warning("Fresh mass (g) values exceed the expected range (0:100) or are missing")
    }
  }

  if (fresh.mass.saturated != FALSE) {
    ValidityCheckDetail(data_in, fresh.mass.saturated)
    if (any(!(round(data_in[[fresh.mass.saturated]]) %in% 0:150))) {
      warning("Saturated fresh mass (g) values exceed the expected range (0:150) or are missing")
    }
  }

  if (dry.mass != FALSE) {
    ValidityCheckDetail(data_in, dry.mass)
    if (any(!(round(data_in[[dry.mass]]) %in% 0:20))) {
      warning("Dry mass (g) values exceed the expected range (0:20) or are missing")
    }
  }

  if (RWD != FALSE) {
    ValidityCheckDetail(data_in, RWD)
    if (any(!(round(data_in[[RWD]]) %in% 0:100 |
              (is.na(data_in[[RWD]])))) |
        all(!(round(data_in[[RWD]]) > 2))) {
      warning("Unexpected values for RWD (%)")
    }
  }

  if (water.potential != FALSE) {
    ValidityCheckDetail(data_in, water.potential)
    if (any(!(round(data_in[[water.potential]]) %in% -7:0)) |
        all(round(data_in[[water.potential]]) > -0.2)) {
      warning("Unexpected or missing values for water potential (MPa)")
    }
  }


  return(data.frame(data_in))
}
