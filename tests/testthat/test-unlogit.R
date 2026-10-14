sim_maturity <- function(n = 800, L50 = 100, slope = 0.15, seed = 1) {
  set.seed(seed)
  length <- runif(n, 40, 160)
  maturity <- rbinom(n, 1, plogis(slope * (length - L50)))
  data.frame(length = length, maturity = maturity)
}

test_that("unlogit returns mean, ci.min and ci.max", {
  dt <- sim_maturity()
  mod <- glm(maturity ~ length, data = dt, family = binomial(link = "logit"))
  res <- unlogit(0.5, mod)

  expect_s3_class(res, "data.frame")
  expect_named(res, c("mean", "ci.min", "ci.max"))
  expect_equal(res$mean, unname(-coef(mod)[1] / coef(mod)[2]))
  expect_lt(res$ci.min, res$mean)
  expect_gt(res$ci.max, res$mean)
})

test_that("unlogit matches the hand-coded delta method", {
  dt <- sim_maturity()
  mod <- glm(maturity ~ length, data = dt, family = binomial(link = "logit"))
  b <- unname(coef(mod))
  V <- unname(vcov(mod))
  p <- 0.75
  x <- (qlogis(p) - b[1]) / b[2]
  g <- c(-1 / b[2], -x / b[2])
  se <- sqrt(drop(t(g) %*% V %*% g))

  res <- unlogit(p, mod)
  expect_equal(res$mean, x)
  expect_equal(res$ci.max - res$ci.min, 2 * qnorm(0.975) * se)
})

test_that("delta-method L50 interval agrees with a bootstrap interval", {
  dt <- sim_maturity()
  mod <- glm(maturity ~ length, data = dt, family = binomial(link = "logit"))
  delta <- unlogit(0.5, mod)

  set.seed(42)
  boot <- vapply(seq_len(500), function(i) {
    d <- dt[sample.int(nrow(dt), replace = TRUE), ]
    m <- suppressWarnings(glm(maturity ~ length, data = d, family = binomial))
    unname(-coef(m)[1] / coef(m)[2])
  }, numeric(1))
  boot.ci <- unname(quantile(boot, c(0.025, 0.975)))

  delta.width <- delta$ci.max - delta$ci.min
  boot.width <- diff(boot.ci)

  # Widths agree within 25% and limits within a quarter of the interval width
  expect_lt(abs(delta.width / boot.width - 1), 0.25)
  expect_lt(abs(delta$ci.min - boot.ci[1]), 0.25 * boot.width)
  expect_lt(abs(delta$ci.max - boot.ci[2]), 0.25 * boot.width)

  # The interval covers the true L50 used in the simulation
  expect_true(delta$ci.min < 100 && delta$ci.max > 100)
})

test_that("plot_maturity default interval equals unlogit and is close to bootstrap", {
  dt <- sim_maturity()
  mod <- glm(maturity ~ length, data = dt, family = binomial(link = "logit"))
  delta <- unlogit(0.5, mod)

  res <- plot_maturity(dt, length = "length", maturity = "maturity")
  expect_equal(res$params$mean, delta$mean)
  expect_equal(res$params$ci.min, delta$ci.min)
  expect_equal(res$params$ci.max, delta$ci.max)

  set.seed(7)
  res.boot <- plot_maturity(dt, length = "length", maturity = "maturity",
                            bootstrap.n = 200)
  boot.width <- res.boot$params$ci.max - res.boot$params$ci.min
  expect_lt(abs((delta$ci.max - delta$ci.min) / boot.width - 1), 0.3)
})
