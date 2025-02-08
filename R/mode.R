#' @title Mode
#' @description This function calculates the mode of a set of data
#' @param v A set of data for which the mode should be calculated
#'
#' @returns Mode of the data
#' @export
#'
#' @examples
#' sample_data <- c(1, 2, 2, 3, 3, 3, 4, 4)
#' getmode(sample_data)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
