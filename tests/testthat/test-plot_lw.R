sim_lw <- function(lengths_m) {
  set.seed(1)
  data.frame(length = lengths_m,
             weight = 0.01 * (lengths_m * 100)^3 / 1000 * runif(length(lengths_m), 0.95, 1.05))
}

# Data of the point layer (id, weight, length, outlier)
point_data <- function(r) {
  for (l in r$plot$layers) if (is.data.frame(l$data) && "outlier" %in% names(l$data)) return(l$data)
  NULL
}

test_that("metre-to-cm conversion has no floating-point error", {
  # 0.29 * 100, 0.57 * 100 etc. fall just below a whole centimetre in binary floating point
  dt <- sim_lw(rep(c(0.29, 0.57, 0.58, 1.13, 1.14), 4))
  r <- suppressWarnings(plot_lw(dt, length.unit = "m", weight.unit = "kg",
                                correct.units = TRUE, verbose = FALSE))
  len <- point_data(r)$length
  expect_identical(sort(unique(floor(len))), c(29, 57, 58, 113, 114))
})

test_that("outlier.sd flags only strongly deviating records", {
  dt <- sim_lw(seq(0.2, 1.2, length.out = 200))
  r0 <- suppressWarnings(plot_lw(dt, length.unit = "m", weight.unit = "kg",
                                 outlier.sd = 4, verbose = FALSE))
  expect_equal(sum(point_data(r0)$outlier), 0)
  dt$weight[10] <- dt$weight[10] * 10
  r1 <- suppressWarnings(plot_lw(dt, length.unit = "m", weight.unit = "kg",
                                 outlier.sd = 4, verbose = FALSE))
  expect_equal(which(point_data(r1)$outlier), 10L)
})
