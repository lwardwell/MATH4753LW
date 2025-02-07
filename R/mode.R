#' Mode
#'
#' @param v A set of data for which the mode should be calculated
#'
#' @returns Mode of the data
#' @export
#'
#' @examples getmode(data$numbers)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
