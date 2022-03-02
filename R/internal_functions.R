#' @title Round to multiple of any number
#' @param x numeric vector to round
#' @param accuracy number to round to; for POSIXct objects, a number of seconds
#' @param f rounding function: \code{\link{floor}}, \code{\link{ceiling}} or \code{\link{round}}
#' @return Rounded numeric vector
#' @keywords internal
#' @author Hadley Wickham
#' @export
#'
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' @title Back-transform predictor variables from a logit model
#' @param p probability threshold to unlog from \code{model}
#' @param model a \code{glm} model object
#' @return A data frame
#' @keywords internal
#' @export

unlogit <- function(p, model) {
  mean <- unname((log(p/(1 - p)) - coef(model)[1])/coef(model)[2])

  tmp.cis <- suppressMessages(confint(model))

  ci.max <- unname((log(p/(1 - p)) - tmp.cis[1])/tmp.cis[2])
  ci.min <- unname((log(p/(1 - p)) - tmp.cis[3])/tmp.cis[4])

  data.frame(mean = mean, ci.min = ci.min, ci.max = ci.max)
}
