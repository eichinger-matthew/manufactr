#' A function to calculate the mean or mode of a variable depending on type.
#'
#'
#' This function calculates the mean or mode of a variable by checking whether
#' it is numeric, factor or character, or something else. If a user wants to
#' calculate the mode of an ordinal variable that is coded as an integer, then
#' the variable must be specified as an ordered factor or be manually given
#' its character values. The function automatically breaks bimodal variables
#' by selecting the first mode.
#'
#' @param x A vector of numeric, factor, or character values.
#' @return A scalar indicating the mean or mode of the input vector.
#' @export
#' @examples
#' x <- seq(1, 100, 1)
#' mean_or_mode(x)
#' a <- c("Democrat", "Republican", "Independent")
#' x <- sample(a, 100, replace = T)
#' table(x)
#' mean_or_mode(x)
#'
#' # when two modes are present
#' a <- c("Democrat", "Democrat", "Independent", "Republican", "Republican")
#' mean_or_mode(a)
#'
#' # ordered factors
#' b <- c(1, 2, 3)
#' x <- sample(b, 100, replace = T)
#' mean_or_mode(x)
#' x[x == 1] <- "Agree"
#' x[x == 2] <- "Neutral"
#' x[x == 3] <- "Disagree"
#' table(x)
#' mean_or_mode(x)
#'

mean_or_mode <- function(x) {
  # calculate mean or mode
  if (is.numeric(x)) {
    value <- mean(x, na.rm = TRUE)
  } else if (is.factor(x) | is.character(x)) {
    value <- names(sort(table(x), decreasing = TRUE))[1]
  } else {
    stop("x must be double, integer, factor, or character")
  }
  # return value
  return(value)
}




