#' @title Plot length-weight relationships
#' @description
#' Plots length-weight relationship for a species and calculates the a and b coefficients used in length-weight conversions.
#' @param length Character argument giving the name of the length column in \code{dt}
#' @param weight Character argument giving the name of the weight column in \code{dt}
#' @param ylab Character giving the x-axis label without unit.
#' @param length.unit Character argument giving the unit of \code{length}. Will be used in the labels of the figure and for conversion of the a coefficient. Allowed values for the conversion: "mm" (millimeters), "cm" (centimeters), and "m" (meters).
#' @param weight.unit Character argument giving the unit of \code{weight}. Will be used in the labels of the figure and for conversion of the a coefficient. Allowed values: "g" (grams), "kg" (kilograms), and "t" (metric tons).
#' @param use.nls Logical indicating whether the parameters should be estimated using the nonlinear least squares (\code{nls; TRUE}) method over the log-log transformed linear model (\code{lm; FALSE}) method. See details.
#' @param init.a,init.b Numeric values giving the starting value for a and b coefficients respectively for non-linear least-squares estimation (i.e. when \code{use.nls = TRUE}). If \code{NULL} (default) starting values are guessed.
#' @param log.axes Logical indicating whether logarithmic axes should be used instead of Cartesian ones.
#' @param outlier.percentile Numeric argument giving the probability as a percent value which should be used to calculate residual quantiles for outlier removal. See details. Values >= 99.5 are recommended. If \code{NULL} (default), no outlier removal is conducted.
#' @param annotate.coefficients Logical indicating whether the a and b coefficients should be annotated into the plot.
#' @param correct.units Logical indicating whether the a and b coefficients should be converted for centimeters and grams as in FishBase.
#' @param point.size Numeric defining the size for data points in the plot. See the \code{size} argument in \link[ggplot2]{geom_point}.
#' @param verbose Logical indicating whether to return warnings and messages.
#' @inheritParams plot_maturity
#' @details The function estimates the a and b coefficients of the length weight relationship, \eqn{weight = a \times length^b}{weight = a*length^b}, and plots the data. The model can be fitted either using the standard log(weight) ~ log(length) regression (\link[stats]{lm}; default) or nonlinear least squares (\link[stats]{nls}) method. The nls method often manages to fit steeper slopes making the b parameter higher and the a parameter lower than the linear \link[stats]{lm} method. This tends to lead to visually more pleasing fits at high lengths and weights.
#'
#' The a and b coefficients are dependent on the units of length and weight. In models, the length and weight units should often match those of the data going into the model, while in comparisons with FishBase, the units of length and weight should be centimeters and grams, respectively. If the units are wrong, the intercept, a, will be off the FishBase scale by orders of magnitude (see \href{https://www.fishbase.se/manual/english/FishBaseThe_LENGTH_WEIGHT_Table.htm}{FishBase}). The \code{correct.units} can be used to correct the data units to the FishBase standard (cm and g). The function also returns a warning when the returned parameters are not within expected bounds for cm and g estimation. It is recommended to compare the a and b coefficients with those in FishBase for the species as a quality assurance.
#'
#' The \code{outlier.percentile} argument enables quick removal of troublesome outliers from the model estimation. The argument is defined as percentile probabilities and used to calculate quantile for absolute residual values from logarithmic regression to identify outliers (\eqn{outlier = |r| > Pr[|r| > outlier.percentile/100]}). These outliers are then removed from model dataset but plotted using crosses in the ggplot output. See Examples.
#' @return A list with three elements: a ggplot object containing the plot, text giving the central statistics that can be pasted to Markdown, and the model parameters (params).
#' @author Mikko Vihtakari // Institute of Marine Research.
#' @import dplyr ggplot2
#' @importFrom stats lm nls quantile residuals
#' @examples
#' data(survey_ghl)
#'
#' # Simple plot
#' plot_lw(survey_ghl, length = "length", weight = "weight")
#' \donttest{
#' # nls
#' plot_lw(survey_ghl, use.nls = TRUE)
#'
#' # Split by sex, annotate coefficients
#' plot_lw(survey_ghl, split.by.sex = TRUE, annotate.coefficients = TRUE)$plot
#'
#' # Outlier removal
#' plot_lw(survey_ghl, outlier.percentile = 99)
#' }
#' @export

# Debug parameters:
# dt = survey_ghl;
# length = "length"; weight = "weight"; sex = "sex"; female.sex = "F"; male.sex = "M"; length.unit = "cm"; weight.unit = "kg"; split.by.sex = FALSE; xlab = "Total length"; ylab = "Weight"; use.nls = FALSE; init.a = NULL; init.b = NULL; log.axes = FALSE; outlier.percentile = 99.5; annotate.coefficients = TRUE; base_size = 8; legend.position = "bottom"; correct.units = FALSE; point.size = 0.5; verbose = TRUE

plot_lw <- function(
  dt,
  length = "length",
  weight = "weight",
  sex = "sex",
  female.sex = "F",
  male.sex = "M",
  length.unit = "cm",
  weight.unit = "kg",
  split.by.sex = FALSE,
  xlab = "Total length",
  ylab = "Weight",
  use.nls = FALSE,
  init.a = NULL,
  init.b = NULL,
  log.axes = FALSE,
  outlier.percentile = NULL,
  annotate.coefficients = FALSE,
  correct.units = FALSE,
  base_size = 8,
  legend.position = "bottom",
  point.size = 0.8,
  verbose = TRUE
) {
  # Add row number ####

  dt$id <- rownames(dt)

  # Fix sex column

  if (split.by.sex) {
    if (is.null(sex)) {
      stop("Sex column has to be specified when split.by.sex = TRUE")
    }
    if (!all(c(female.sex, male.sex) %in% unique(dt[[sex]]))) {
      stop(
        female.sex,
        " or ",
        male.sex,
        " not found from the ",
        sex,
        " column. Check the female.sex and male.sex parameters."
      )
    }

    if (!inherits(dt$sex, class(female.sex))) {
      class(female.sex) <- class(dt$sex)
    }

    if (!inherits(dt$sex, class(male.sex))) {
      class(male.sex) <- class(dt$sex)
    }

    if (length(na.omit(dt[[sex]])) < 10) {
      stop("Either invalid sex column or not enough sex data")
    }

    orig.nrow <- nrow(dt)

    dt <- dt %>%
      dplyr::rename("sex" = !!sex) %>%
      dplyr::filter(!is.na(sex))

    sex.missing <- orig.nrow - nrow(dt)
  }

  # Filter

  if (!exists("orig.nrow")) {
    orig.nrow <- nrow(dt)
  }

  dt <- dt %>%
    dplyr::rename(
      "weight" = !!weight,
      "length" = !!length
    )

  length.missing <- sum(is.na(dt$length))
  weight.missing <- sum(is.na(dt$weight))

  dt <- dt %>% dplyr::filter(!is.na(weight) & !is.na(length))

  ## Select columns

  if (split.by.sex) {
    dt <- dt %>% dplyr::select(id, sex, weight, length)
  } else {
    dt <- dt %>% dplyr::select(id, weight, length)
  }

  ## Convert data

  if (correct.units) {
    if (length.unit == "mm") {
      if (verbose) {
        message("length unit converted from mm to cm")
      }
      dt$length <- dt$length / 100
      length.unit <- "cm"
    }
    if (length.unit == "m") {
      if (verbose) {
        message("length unit converted from m to cm")
      }
      dt$length <- dt$length * 100
      length.unit <- "cm"
    }
    if (weight.unit == "kg") {
      if (verbose) {
        message("weight unit converted from kg to g")
      }
      dt$weight <- dt$weight * 1e3
      weight.unit <- "g"
    }
    if (weight.unit == "t") {
      if (verbose) {
        message("weight unit converted from t to g")
      }
      dt$weight <- dt$weight * 1e6
      weight.unit <- "g"
    }
    if (is.null(init.a)) {
      init.a <- 1e-3
    }
    if (is.null(init.b)) init.b <- 3
  } else {
    if (length.unit == "cm" & weight.unit == "kg") {
      if (is.null(init.a)) {
        init.a <- 1e-6
      }
      if (is.null(init.b)) init.b <- 3
    } else if (length.unit == "m" & weight.unit == "kg") {
      if (is.null(init.a)) {
        init.a <- 1
      }
      if (is.null(init.b)) init.b <- 3
    } else {
      if (is.null(init.a)) {
        init.a <- 1e-3
      }
      if (is.null(init.b)) init.b <- 3
    }
  }

  # Outlier removal

  if (!is.null(outlier.percentile)) {
    if (!is.numeric(outlier.percentile)) {
      stop(
        "The outlier.percentile argument has to be numeric between 0 and 100.
           Values > 99 are recommended."
      )
    }

    if (split.by.sex) {
      dt <- lapply(unique(dt$sex), function(k) {
        tmp <- dt %>% filter(sex == k)
        mod <- lm(log(weight) ~ log(length), data = tmp)
        res <- abs(stats::residuals(mod))
        tmp$outlier <- res > stats::quantile(res, outlier.percentile / 100)
        tmp
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::arrange(id)
    } else {
      mod <- lm(log(weight) ~ log(length), data = dt)
      res <- abs(stats::residuals(mod))
      dt$outlier <- res > stats::quantile(res, outlier.percentile / 100)
    }
  } else {
    dt$outlier <- FALSE
  }

  # Calculate a and b ####

  if (use.nls) {
    if (split.by.sex) {
      lwModPars <-
        dt %>%
        dplyr::filter(!.data$outlier) %>%
        dplyr::group_by(.data$sex) %>%
        tidyr::nest() %>%
        dplyr::mutate(
          mod = purrr::map(
            data,
            ~ nls(
              I(weight) ~ a * length^b,
              .,
              start = list(a = init.a, b = init.b)
            )
          ),
          coeff = purrr::map(mod, broom::tidy, conf.int = TRUE),
          fit = purrr::map(mod, broom::glance)
        ) %>%
        dplyr::select(-.data$data, -.data$mod) %>%
        tidyr::unnest(.data$coeff) %>%
        dplyr::mutate(
          term = dplyr::recode(
            .data$term,
            "(Intercept)" = "a",
            "log(length)" = "b"
          )
        )

      lwModPars <-
        lwModPars %>%
        dplyr::select(-.data$fit) %>%
        dplyr::bind_cols(
          lwModPars$fit %>%
            dplyr::bind_rows() %>%
            dplyr::select(.data$AIC, .data$nobs)
        ) %>%
        dplyr::left_join(
          lapply(unique(lwModPars$sex), function(k) {
            tmp <- dt %>% dplyr::filter(.data$sex == k)

            tibble(
              sex = k,
              length = paste(
                round(min(tmp$length, na.rm = TRUE), 3),
                round(max(tmp$length, na.rm = TRUE), 3),
                sep = " - "
              ),
              length.unit = length.unit,
              weight = paste(
                round(min(tmp$weight, na.rm = TRUE), 4),
                round(max(tmp$weight, na.rm = TRUE), 2),
                sep = " - "
              ),
              weight.unit = weight.unit
            )
          }) %>%
            dplyr::bind_rows(),
          by = dplyr::join_by(sex)
        )
    } else {
      lwModPars <-
        dt %>%
        dplyr::filter(!.data$outlier) %>%
        tidyr::nest() %>%
        dplyr::mutate(
          mod = purrr::map(
            data,
            ~ nls(
              I(weight) ~ a * length^b,
              .,
              start = list(a = init.a, b = init.b)
            )
          ),
          coeff = purrr::map(mod, broom::tidy, conf.int = TRUE),
          fit = purrr::map(mod, broom::glance)
        ) %>%
        dplyr::select(-.data$data, -.data$mod) %>%
        tidyr::unnest(.data$coeff) %>%
        dplyr::mutate(
          term = dplyr::recode(term, "(Intercept)" = "a", "log(length)" = "b")
        )

      lwModPars <-
        lwModPars %>%
        dplyr::select(-.data$fit) %>%
        dplyr::bind_cols(
          lwModPars$fit %>%
            dplyr::bind_rows() %>%
            dplyr::select(.data$AIC, .data$nobs)
        ) %>%
        dplyr::mutate(
          length = paste(
            min(dt$length, na.rm = TRUE),
            max(dt$length, na.rm = TRUE),
            sep = " - "
          ),
          length.unit = length.unit,
          weight = paste(
            round(min(dt$weight, na.rm = TRUE), 4),
            round(max(dt$weight, na.rm = TRUE), 2),
            sep = " - "
          ),
          weight.unit = weight.unit
        )
    }
  } else {
    if (split.by.sex) {
      lwModPars <-
        dt %>%
        dplyr::filter(!.data$outlier) %>%
        dplyr::group_by(.data$sex) %>%
        tidyr::nest() %>%
        dplyr::mutate(
          mod = purrr::map(data, ~ lm(log(weight) ~ log(length), data = .)),
          coeff = purrr::map(mod, broom::tidy, conf.int = TRUE),
          fit = purrr::map(mod, broom::glance)
        ) %>%
        dplyr::select(-.data$data, -.data$mod) %>%
        tidyr::unnest(.data$coeff) %>%
        dplyr::mutate(
          term = dplyr::recode(
            .data$term,
            "(Intercept)" = "a",
            "log(length)" = "b"
          )
        )

      lwModPars <-
        lwModPars %>%
        dplyr::select(-.data$fit) %>%
        dplyr::bind_cols(
          lwModPars$fit %>%
            dplyr::bind_rows() %>%
            dplyr::select(.data$r.squared, .data$AIC, .data$nobs)
        ) %>%
        dplyr::left_join(
          lapply(unique(lwModPars$sex), function(k) {
            tmp <- dt %>% dplyr::filter(.data$sex == k)

            tibble(
              sex = k,
              length = paste(
                round(min(tmp$length, na.rm = TRUE), 3),
                round(max(tmp$length, na.rm = TRUE), 3),
                sep = " - "
              ),
              length.unit = length.unit,
              weight = paste(
                round(min(tmp$weight, na.rm = TRUE), 4),
                round(max(tmp$weight, na.rm = TRUE), 2),
                sep = " - "
              ),
              weight.unit = weight.unit
            )
          }) %>%
            dplyr::bind_rows(),
          by = dplyr::join_by(sex)
        )

      lwModPars[
        lwModPars$term == "a",
        c("estimate", "conf.low", "conf.high")
      ] <- # Transform back to normal space
        exp(lwModPars[
          lwModPars$term == "a",
          c("estimate", "conf.low", "conf.high")
        ])
    } else {
      lwModPars <-
        dt %>%
        dplyr::filter(!.data$outlier) %>%
        tidyr::nest() %>%
        dplyr::mutate(
          mod = purrr::map(data, ~ lm(log(weight) ~ log(length), data = .)),
          coeff = purrr::map(mod, broom::tidy, conf.int = TRUE),
          fit = purrr::map(mod, broom::glance)
        ) %>%
        dplyr::select(-.data$data, -.data$mod) %>%
        tidyr::unnest(.data$coeff) %>%
        dplyr::mutate(
          term = dplyr::recode(
            .data$term,
            "(Intercept)" = "a",
            "log(length)" = "b"
          )
        )

      lwModPars <-
        lwModPars %>%
        dplyr::select(-.data$fit) %>%
        dplyr::bind_cols(
          lwModPars$fit %>%
            dplyr::bind_rows() %>%
            dplyr::select(.data$r.squared, .data$AIC, .data$nobs)
        ) %>%
        dplyr::mutate(
          length = paste(
            min(dt$length, na.rm = TRUE),
            max(dt$length, na.rm = TRUE),
            sep = " - "
          ),
          length.unit = length.unit,
          weight = paste(
            round(min(dt$weight, na.rm = TRUE), 4),
            round(max(dt$weight, na.rm = TRUE), 2),
            sep = " - "
          ),
          weight.unit = weight.unit
        )

      lwModPars[
        lwModPars$term == "a",
        c("estimate", "conf.low", "conf.high")
      ] <- # Transform back to normal space
        exp(lwModPars[
          lwModPars$term == "a",
          c("estimate", "conf.low", "conf.high")
        ])
    }
  }

  ## a tests
  tmp <- log10(lwModPars[lwModPars$term == "a", "estimate"])
  if (
    verbose &
      any(tmp <= -5 | tmp >= -1) &
      length.unit == "cm" &
      weight.unit == "g"
  ) {
    warning(
      "The a parameter appears out of its bounds for cm and g estimation. Check that you got the units right."
    )
  }

  ## b tests
  tmp <- lwModPars[lwModPars$term == "b", "estimate"]
  if (
    verbose &
      any(tmp <= 2.8 | tmp >= 3.8) &
      length.unit == "cm" &
      weight.unit == "g"
  ) {
    warning(
      "The b parameter appears out of its bounds for cm and g estimation. Check that you got the units right."
    )
  }

  # Plot

  if (split.by.sex) {
    tmp <- bind_rows(
      tibble(
        length = seq(
          0,
          max(dt[dt$sex == female.sex, "length"]),
          length.out = 100
        ),
        weight = (lwModPars %>%
          filter(sex == female.sex, term == "a") %>%
          pull(estimate)) *
          length^(lwModPars %>%
            filter(sex == female.sex, term == "b") %>%
            pull(estimate)),
        weight.low = (lwModPars %>%
          filter(sex == female.sex, term == "a") %>%
          pull(conf.low)) *
          length^(lwModPars %>%
            filter(sex == female.sex, term == "b") %>%
            pull(conf.low)),
        weight.high = (lwModPars %>%
          filter(sex == female.sex, term == "a") %>%
          pull(conf.high)) *
          length^(lwModPars %>%
            filter(sex == female.sex, term == "b") %>%
            pull(conf.high)),
        sex = female.sex
      ),
      tibble(
        length = seq(
          0,
          max(dt[dt$sex == male.sex, "length"]),
          length.out = 100
        ),
        weight = (lwModPars %>%
          filter(sex == male.sex, term == "a") %>%
          pull(estimate)) *
          length^(lwModPars %>%
            filter(sex == male.sex, term == "b") %>%
            pull(estimate)),
        weight.low = (lwModPars %>%
          filter(sex == male.sex, term == "a") %>%
          pull(conf.low)) *
          length^(lwModPars %>%
            filter(sex == male.sex, term == "b") %>%
            pull(conf.low)),
        weight.high = (lwModPars %>%
          filter(sex == male.sex, term == "a") %>%
          pull(conf.high)) *
          length^(lwModPars %>%
            filter(sex == male.sex, term == "b") %>%
            pull(conf.high)),
        sex = male.sex
      )
    )

    Plot <- suppressWarnings({
      ggplot() +
        geom_point(
          data = dt,
          aes(
            x = length,
            y = weight,
            color = sex,
            text = paste0("row number: ", id),
            shape = outlier
          ),
          alpha = 0.8,
          size = point.size,
          show.legend = !is.null(outlier.percentile)
        ) +
        scale_shape_manual(values = c(`FALSE` = 21, `TRUE` = 4)) +
        geom_line(
          data = tmp,
          aes(x = length, y = weight.low, color = sex),
          linetype = 2
        ) +
        geom_line(
          data = tmp,
          aes(x = length, y = weight.high, color = sex),
          linetype = 2
        ) +
        facet_wrap(~sex) +
        annotate(
          "line",
          x = tmp[tmp$sex == female.sex, ]$length,
          y = tmp[tmp$sex == female.sex, ]$weight,
          color = "tomato4"
        ) +
        annotate(
          "line",
          x = tmp[tmp$sex == male.sex, ]$length,
          y = tmp[tmp$sex == male.sex, ]$weight,
          color = "dodgerblue4"
        ) +
        {
          if (annotate.coefficients) {
            geom_text(
              data = lwModPars %>%
                dplyr::select(sex, term, estimate) %>%
                tidyr::pivot_wider(
                  names_from = "term",
                  values_from = "estimate"
                ),
              aes(
                x = -Inf,
                y = Inf,
                label = paste(
                  "a = ",
                  round(a, 10),
                  "\nb = ",
                  round(b, 4)
                ),
                vjust = 1,
                hjust = -0.1
              )
            )
          }
        } +
        scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
        scale_y_continuous(n.breaks = 8) +
        labs(
          x = paste0("Total length (", length.unit, ")"),
          y = paste0("Weight (", weight.unit, ")"),
          shape = "Removed"
        ) +
        coord_cartesian(expand = FALSE, clip = "off") +
        theme_fishplots(base_size = base_size) +
        theme(
          legend.position = legend.position,
          text = element_text(size = base_size)
        )
    })

    Text <- paste0(
      if (use.nls) {
        paste0("Nonlinear least squares length-weight model")
      } else {
        paste0("Logarithm transformed linear length-weight model")
      },
      " for females and males, respectively:",
      "  \n a = ",
      lwModPars %>%
        filter(term == "a", sex == female.sex) %>%
        pull(estimate) %>%
        round(., ifelse(weight.unit == "kg", 10, 5)),
      " +/- ",
      lwModPars %>%
        filter(term == "a", sex == female.sex) %>%
        pull(conf.low) %>%
        round(., ifelse(weight.unit == "kg", 10, 5)),
      " - ",
      lwModPars %>%
        filter(term == "a", sex == female.sex) %>%
        pull(conf.high) %>%
        round(., ifelse(weight.unit == "kg", 10, 5)),
      " (95% CIs) and ",
      lwModPars %>%
        filter(term == "a", sex == male.sex) %>%
        pull(estimate) %>%
        round(., ifelse(weight.unit == "kg", 10, 5)),
      " +/- ",
      lwModPars %>%
        filter(term == "a", sex == male.sex) %>%
        pull(conf.low) %>%
        round(., ifelse(weight.unit == "kg", 10, 5)),
      " - ",
      lwModPars %>%
        filter(term == "a", sex == male.sex) %>%
        pull(conf.high) %>%
        round(., ifelse(weight.unit == "kg", 10, 5)),
      " (95% CIs).",

      "  \n b = ",
      lwModPars %>%
        filter(term == "b", sex == female.sex) %>%
        pull(estimate) %>%
        round(., 3),
      " +/- ",
      lwModPars %>%
        filter(term == "b", sex == female.sex) %>%
        pull(conf.low) %>%
        round(., 2),
      " - ",
      lwModPars %>%
        filter(term == "b", sex == female.sex) %>%
        pull(conf.high) %>%
        round(., 2),
      " (95% CIs) and ",
      lwModPars %>%
        filter(term == "b", sex == male.sex) %>%
        pull(estimate) %>%
        round(., 3),
      " +/- ",
      lwModPars %>%
        filter(term == "b", sex == male.sex) %>%
        pull(conf.low) %>%
        round(., 2),
      " - ",
      lwModPars %>%
        filter(term == "b", sex == male.sex) %>%
        pull(conf.high) %>%
        round(., 2),
      " (95% CIs).",

      "  \n Length in ",
      length.unit,
      " and weight in ",
      weight.unit,
      "  \n Number of included specimens = ",
      nrow(dt[dt$sex == female.sex, ]),
      " and ",
      nrow(dt[dt$sex == male.sex, ]),
      "  \n Total number of measured = ",
      orig.nrow,
      "  \n Excluded (data missing): \n Length = ",
      length.missing,
      "; weight = ",
      weight.missing,
      "; sex = ",
      sex.missing,
      "; outlier = ",
      sum(dt$outlier)
    )
  } else {
    tmp <- tibble(
      length = seq(0, max(dt$length), length.out = 100),
      weight = (lwModPars %>% filter(term == "a") %>% pull(estimate)) *
        length^(lwModPars %>% filter(term == "b") %>% pull(estimate)),
      weight.low = (lwModPars %>% filter(term == "a") %>% pull(conf.low)) *
        length^(lwModPars %>% filter(term == "b") %>% pull(conf.low)),
      weight.high = (lwModPars %>% filter(term == "a") %>% pull(conf.high)) *
        length^(lwModPars %>% filter(term == "b") %>% pull(conf.high))
    )

    Plot <- suppressWarnings({
      ggplot() +
        geom_point(
          data = dt,
          aes(
            x = length,
            y = weight,
            text = paste0("row number: ", id),
            color = outlier,
            shape = outlier
          ),
          alpha = 0.8,
          size = point.size,
          show.legend = !is.null(outlier.percentile)
        ) +
        scale_shape_manual(values = c(`FALSE` = 21, `TRUE` = 4)) +
        scale_color_manual(values = c(`FALSE` = "black", `TRUE` = "darkred")) +
        geom_line(
          data = tmp,
          aes(x = length, y = weight.low),
          linetype = 2,
          color = "blue"
        ) +
        geom_line(
          data = tmp,
          aes(x = length, y = weight.high),
          linetype = 2,
          color = "blue"
        ) +
        annotate("line", x = tmp$length, y = tmp$weight, color = "blue") +
        {
          if (annotate.coefficients) {
            annotate(
              "text",
              x = -Inf,
              y = Inf,
              label = paste(
                "a = ",
                lwModPars %>%
                  filter(term == "a") %>%
                  pull(estimate) %>%
                  round(10),
                "\nb = ",
                lwModPars %>%
                  filter(term == "b") %>%
                  pull(estimate) %>%
                  round(4)
              ),
              vjust = 1,
              hjust = -0.1
            )
          }
        } +
        scale_y_continuous(n.breaks = 8) +
        labs(
          x = paste0("Total length (", length.unit, ")"),
          y = paste0("Weight (", weight.unit, ")"),
          color = "Removed",
          shape = "Removed"
        ) +
        coord_cartesian(expand = FALSE, clip = "off") +
        theme_fishplots(base_size = base_size) +
        theme(
          legend.position = legend.position,
          text = element_text(size = base_size)
        )
    })

    Text <- paste0(
      if (use.nls) {
        paste0("Nonlinear least squares length-weight model")
      } else {
        paste0("Logarithm transformed linear length-weight model")
      },
      ". Not splitted by sex:",
      "  \n a = ",
      lwModPars %>%
        filter(term == "a") %>%
        pull(estimate) %>%
        round(., ifelse(weight.unit == "kg", 10, 5)),
      " +/- ",
      lwModPars %>%
        filter(term == "a") %>%
        pull(conf.low) %>%
        round(., ifelse(weight.unit == "kg", 10, 5)),
      " - ",
      lwModPars %>%
        filter(term == "a") %>%
        pull(conf.high) %>%
        round(., ifelse(weight.unit == "kg", 10, 5)),
      " (95% CIs).",

      "  \n b = ",
      lwModPars %>% filter(term == "b") %>% pull(estimate) %>% round(., 3),
      " +/- ",
      lwModPars %>% filter(term == "b") %>% pull(conf.low) %>% round(., 2),
      " - ",
      lwModPars %>% filter(term == "b") %>% pull(conf.high) %>% round(., 2),
      " (95% CIs).",

      "  \n Length in ",
      length.unit,
      " and weight in ",
      weight.unit,
      "  \n Number of included specimens = ",
      nrow(dt),
      "  \n Total number of measured = ",
      orig.nrow,
      "  \n Excluded (data missing): \n Length = ",
      length.missing,
      "; weight = ",
      weight.missing,
      "; outlier = ",
      sum(dt$outlier)
    )
  }

  # Logarithmic axes

  if (log.axes) {
    Plot <- suppressMessages(suppressWarnings({
      Plot +
        scale_y_log10(n.breaks = 8) +
        scale_x_log10(n.breaks = 8)
    }))
  }

  ## Return

  return(list(
    plot = Plot,
    text = Text,
    params = if (exists("lwModPars")) {
      lwModPars
    } else {
      NULL
    }
  ))
}
