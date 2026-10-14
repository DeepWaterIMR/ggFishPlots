#' @title Round to multiple of any number
#' @param x numeric vector to round
#' @param accuracy number to round to; for POSIXct objects, a number of seconds
#' @param f rounding function: \code{\link{floor}}, \code{\link{ceiling}} or \code{\link{round}}
#' @return Rounded numeric vector
#' @author Hadley Wickham
#' @export
#'
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' @title Back-transform predictor variables from a logit model
#' @description Returns the predictor value at which a binomial logit model reaches probability \code{p} (e.g. L50 for \code{p = 0.5}) with a 95\% delta-method confidence interval.
#' @param p probability threshold to unlog from \code{model}
#' @param model a \code{glm} model object with a single predictor
#' @details The estimate is \eqn{x_p = (logit(p) - b_0) / b_1}. Its variance is approximated by the delta method, \eqn{Var(x_p) = g^T V g}, where \eqn{V} is the variance-covariance matrix of the intercept and slope (\code{vcov(model)}) and \eqn{g = (-1/b_1, -x_p/b_1)} is the gradient of \eqn{x_p}. The confidence interval is \eqn{x_p \pm z_{0.975} SE}. This accounts for the (typically strong negative) correlation between intercept and slope and is equivalent to \code{MASS::dose.p()}.
#' @return A data frame with columns \code{mean}, \code{ci.min} and \code{ci.max}
#' @importFrom stats coef vcov qnorm
#' @export

unlogit <- function(p, model) {
  b <- unname(coef(model)[1:2])
  V <- unname(vcov(model)[1:2, 1:2])

  mean <- (log(p / (1 - p)) - b[1]) / b[2]

  grad <- c(-1 / b[2], -mean / b[2])
  se <- sqrt(as.numeric(t(grad) %*% V %*% grad))
  z <- qnorm(0.975)

  data.frame(mean = mean, ci.min = mean - z * se, ci.max = mean + z * se)
}
