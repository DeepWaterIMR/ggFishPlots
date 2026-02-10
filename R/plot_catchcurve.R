#' @title Plot catch curve
#' @description Plot a catch curve to estimate instantaneous total mortality (Z) using age data
#' @inheritParams plot_maturity
#' @inheritParams plot_growth
#' @param time Split analysis by time? If \code{NULL}, all data are assumed to stem from one time point. Using a character argument giving the name of a time column splits the analysis by unique values in that column and produces a faceted plot.
#' @param age.range Defines the age range to be used for Z estimation. If \code{NULL}, all ages are used. If a numeric vector of length 2, the first number defines the minimum age to include and the last number the maximum age. It is also possible to use differing ranges by sex when \code{split.by.sex = TRUE}: use a named list  of length two with names referring to \code{female.sex} and \code{male.sex}. Provide a numeric vector of length 2 to each element (first number defining the minimum age to include and the last number the maximum age). See Examples.
#' @details Calculates and plots the basic log-linearized catch curve to estimate instantaneous mortality. See e.g. \href{https://www.fishbase.se/manual/english/FishBaseThe_LENGTH_WEIGHT_Table.htm}{Ogle (2013)}.
#' @author Mikko Vihtakari // Institute of Marine Research.
#' @import dplyr ggplot2
#' @importFrom scales log_breaks
#' @examples
#' # Catch curve including all ages
#' data(survey_ghl)
#' plot_catchcurve(survey_ghl)
#' \donttest{
#' # Specific ages
#' plot_catchcurve(survey_ghl, age.range = c(10,26))
#' # Split by sex
#' plot_catchcurve(survey_ghl, age.range = c(10,26), split.by.sex = TRUE)
#' # Split by sex, separate age.range
#' plot_catchcurve(survey_ghl,
#' age.range = list("F" = c(13,26), "M" = c(10,26)),
#' split.by.sex = TRUE)
#' }
#' @export

## Debugging parameters
# dt = survey_ghl; age = "age"; sex = "sex"; time = NULL; age.range = list("F" = c(13,26), "M" = c(10,24)); female.sex = "F"; male.sex = "M"; split.by.sex = TRUE; base_size = 8; legend.position = "bottom"
plot_catchcurve <- function(
  dt,
  age = "age",
  sex = "sex",
  time = NULL,
  age.range = NULL,
  female.sex = "F",
  male.sex = "M",
  split.by.sex = FALSE,
  base_size = 8,
  legend.position = "bottom"
) {
  # To avoid modifying the input object
  # dt <- x

  # Checks

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
    if (length(na.omit(dt[[sex]])) < 10) {
      stop("Either invalid sex column or not enough sex data")
    }
    if (!inherits(female.sex, class(male.sex))) {
      stop("female.sex and male.sex are different class.")
    }
  }

  if (!is.null(age.range)) {
    if (inherits(age.range, "list")) {
      if (!split.by.sex) {
        stop("split.by.sex has to be TRUE when supplying lists to age.range")
      }

      if (length(age.range) != 2) {
        stop("length of age.range list has to be two")
      }

      if (!any(names(age.range) %in% c(female.sex, male.sex))) {
        stop(
          "age.range has to be a named list with names ",
          female.sex,
          " and ",
          male.sex
        )
      }

      lapply(age.range, function(k) {
        if (length(k) != 2) {
          stop(
            "age.range has to be a numeric vector of length two. The first number
           defines the minimum age and the second maximum age to be included to
           the Z calculation."
          )

          if (!inherits(k, "numeric")) {
            stop("age.range has to be a numeric vector.")
          }
        }
      })
    } else {
      if (length(age.range) != 2) {
        stop(
          "age.range has to be a numeric vector of length two. The first number
           defines the minimum age and the second maximum age to be included to
           the Z calculation."
        )
      }

      if (!inherits(age.range, "numeric")) {
        stop("age.range has to be a numeric vector.")
      }
    }
  }

  # Data manipulation ####

  if ("age" %in% colnames(dt) && age != "age") {
    dt <- dt %>% dplyr::select(-age)
  }

  ## Select and rename columns ####

  dt <- dt %>%
    dplyr::rename("age" = !!age) %>%
    dplyr::filter(!is.na(age)) %>%
    dplyr::mutate(age = as.numeric(age))

  ## Sex data ####

  if (split.by.sex) {
    ### Split by sex case ####

    if ("sex" %in% colnames(dt) && sex != "sex") {
      dt <- dt %>% dplyr::select(-sex)
    }

    if (is.null(time)) {
      dt <- dt %>%
        dplyr::rename("sex" = !!sex) %>%
        dplyr::filter(!is.na(sex)) %>%
        dplyr::group_by(sex, age) %>%
        dplyr::count()
    } else {
      dt <- dt %>%
        dplyr::rename("sex" = !!sex) %>%
        dplyr::filter(!is.na(sex)) %>%
        dplyr::group_by(time = get(time), sex, age) %>%
        dplyr::count()
    }
  } else {
    ### No sex split ####
    if (is.null(time)) {
      dt <- dt %>%
        dplyr::mutate(sex = "both") %>%
        dplyr::group_by(sex, age) %>%
        dplyr::count()
    } else {
      dt <- dt %>%
        dplyr::mutate(sex = "both") %>%
        dplyr::group_by(time = get(time), sex, age) %>%
        dplyr::count()
    }
  }

  # Total mortality regression ####

  ## Data to include ####

  if (is.null(age.range)) {
    dt <- dt %>% dplyr::mutate(include = TRUE)
  } else {
    if (inherits(age.range, "list")) {
      dt <- lapply(unique(dt$sex), function(k) {
        if (k == female.sex) {
          ar <- age.range[[female.sex]]
          dt %>%
            dplyr::filter(.data$sex == k) %>%
            dplyr::mutate(include = .data$age >= ar[1] & .data$age <= ar[2])
        } else if (k == male.sex) {
          ar <- age.range[[male.sex]]
          dt %>%
            dplyr::filter(.data$sex == k) %>%
            dplyr::mutate(include = .data$age >= ar[1] & .data$age <= ar[2])
        } else {
          stop("k is not female.sex nor male.sex. Don't know what to do.")
        }
      }) %>%
        dplyr::bind_rows()
    } else {
      dt <- dt %>%
        dplyr::mutate(
          include = .data$age >= age.range[1] & .data$age <= age.range[2]
        )
    }
  }

  ## Model ####

  mod <- lapply(unique(dt$sex), function(k) {
    if (is.null(time)) {
      broom::tidy(
        lm(
          log(n) ~ age,
          data = dt %>% dplyr::filter(.data$include, .data$sex == k)
        ),
        conf.int = TRUE
      ) %>%
        dplyr::mutate(sex = k, .before = 1)
    } else {
      dt %>%
        dplyr::filter(.data$include, .data$sex == k) %>%
        dplyr::group_by(.data$time) %>%
        dplyr::reframe(broom::tidy(lm(log(n) ~ age, data = dplyr::cur_data()), conf.int = TRUE)) %>%
        dplyr::mutate(sex = k, .before = 1)
    }
  }) %>%
    dplyr::bind_rows()

  # Plot ####

  if (split.by.sex) {
    modf <- mod %>%
      dplyr::filter(sex == female.sex) %>%
      dplyr::mutate(term = ifelse(term == "(Intercept)", "a", "b")) %>%
      tidyr::pivot_wider(
        id_cols = c(sex, time),
        names_from = c(term),
        values_from = c(estimate, conf.low, conf.high)
      )
    modm <- mod %>%
      dplyr::filter(sex == male.sex) %>%
      dplyr::mutate(term = ifelse(term == "(Intercept)", "a", "b")) %>%
      tidyr::pivot_wider(
        id_cols = c(sex, time),
        names_from = c(term),
        values_from = c(estimate, conf.low, conf.high)
      )

    p <- ggplot() +
      geom_point(
        data = dt,
        aes(x = age, y = n, shape = .data$include, color = sex)
      ) +
      geom_smooth(
        data = dt %>% dplyr::filter(.data$include),
        aes(x = age, y = n, color = sex),
        method = "lm",
        se = FALSE,
        formula = 'y ~ x',
        linewidth = 0.7 / 2.13
      ) +
      geom_text(
        data = modf,
        aes(
          x = Inf,
          y = Inf,
          label = paste0(
            "b = ",
            round(.data$estimate_b, 3),
            " (",
            round(.data$conf.low_b, 3),
            ",",
            round(.data$conf.high_b, 3),
            ")\n",
            "a = ",
            round(.data$estimate_a, 3),
            " (",
            round(.data$conf.low_a, 3),
            ",",
            round(.data$conf.high_a, 3),
            ")\n"
          )
        ),
        vjust = 1,
        hjust = 1,
        size = base_size / 2.845276,
        color = "#FF5F68"
      ) +
      geom_text(
        data = modm,
        aes(
          x = Inf,
          y = Inf,
          label = paste0(
            "b = ",
            round(.data$estimate_b, 3),
            " (",
            round(.data$conf.low_b, 3),
            ",",
            round(.data$conf.high_b, 3),
            ")\n",
            "a = ",
            round(.data$estimate_a, 3),
            " (",
            round(.data$conf.low_a, 3),
            ",",
            round(.data$conf.high_a, 3),
            ")\n"
          )
        ),
        vjust = 2,
        hjust = 1,
        size = base_size / 2.845276,
        color = "#449BCF"
      ) +
      labs(x = "Age (years)", y = "Abundance (Ln-scale)") +
      {
        if (!is.null(time)) facet_wrap(~time)
      } +
      scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
      scale_shape_manual(
        "Included to Z regression",
        values = c(21, 19)
      ) +
      coord_cartesian(expand = FALSE, clip = "off") +
      scale_y_continuous(trans = "log", breaks = scales::log_breaks(n = 10)) +
      scale_x_continuous(n.breaks = 5) +
      theme_fishplots(base_size = base_size) +
      theme(
        legend.position = legend.position,
        text = element_text(size = base_size)
      )

    if (!is.null(time)) {
      modf <- modf %>%
        dplyr::group_by(sex) %>%
        dplyr::summarise(
          conf.low_a = min(.data$estimate_a),
          conf.high_a = max(.data$estimate_a),
          estimate_a = mean(.data$estimate_a),
          conf.low_b = min(.data$estimate_b),
          conf.high_b = max(.data$estimate_b),
          estimate_b = mean(.data$estimate_b),
        )

      modm <- modm %>%
        dplyr::group_by(sex) %>%
        dplyr::summarise(
          conf.low_a = min(.data$estimate_a),
          conf.high_a = max(.data$estimate_a),
          estimate_a = mean(.data$estimate_a),
          conf.low_b = min(.data$estimate_b),
          conf.high_b = max(.data$estimate_b),
          estimate_b = mean(.data$estimate_b),
        )
    }

    Text <- paste0(
      "Instantenous total mortality (Z) estimated using a catch curve and\nage range ",
      ifelse(
        inherits(age.range, "list"),
        paste0(
          paste(age.range[["female"]], collapse = "-"),
          " for females and ",
          paste(age.range[["male"]], collapse = "-"),
          " for males.\n\n"
        ),
        paste0(paste(age.range, collapse = "-"), " for both sexes.\n\n")
      ),
      "Females:\n",
      if (!is.null(time)) {
        "Mean "
      },
      "Z = ",
      round(-modf$estimate_b, 3),
      " (",
      round(-modf$conf.high_b, 3),
      "-",
      round(-modf$conf.low_b, 3),
      ifelse(is.null(time), paste(" 95% CIs)\n"), paste(" range)\n")),
      if (!is.null(time)) {
        "Mean "
      },
      "N at age 0 = ",
      round(exp(modf$estimate_a)),
      " (",
      round(exp(modf$conf.low_a)),
      "-",
      round(exp(modf$conf.high_a)),
      ifelse(is.null(time), paste(" 95% CIs)\n"), paste(" range)\n")),
      if (!is.null(time)) {
        "Mean l"
      } else {
        "L"
      },
      "longevity = ",
      round(-modf$estimate_a / modf$estimate_b, 1),
      " years (",
      round(-modf$conf.low_a / modf$conf.low_b, 1),
      " - ",
      round(-modf$conf.high_a / modf$conf.high_b, 1),
      ifelse(is.null(time), paste(" 95% CIs)\n\n"), paste(" range)\n\n")),
      "Males:\n",
      if (!is.null(time)) {
        "Mean "
      },
      "Z = ",
      round(-modm$estimate_b, 3),
      " (",
      round(-modm$conf.high_b, 3),
      "-",
      round(-modm$conf.low_b, 3),
      ifelse(is.null(time), paste(" 95% CIs)\n"), paste(" range)\n")),
      if (!is.null(time)) {
        "Mean "
      },
      "N at age 0 = ",
      round(exp(modm$estimate_a)),
      " (",
      round(exp(modm$conf.low_a)),
      "-",
      round(exp(modm$conf.high_a)),
      ifelse(is.null(time), paste(" 95% CIs)\n"), paste(" range)\n")),
      if (!is.null(time)) {
        "Mean l"
      } else {
        "L"
      },
      "longevity = ",
      round(-modm$estimate_a / modm$estimate_b, 1),
      " years (",
      round(-modm$conf.low_a / modm$conf.low_b, 1),
      " - ",
      round(-modm$conf.high_a / modm$conf.high_b, 1),
      ifelse(is.null(time), paste(" 95% CIs)"), paste(" range)"))
    )
  } else {
    modb <- mod %>%
      dplyr::mutate(term = ifelse(term == "(Intercept)", "a", "b")) %>%
      tidyr::pivot_wider(
        id_cols = c(sex, time),
        names_from = c(term),
        values_from = c(estimate, conf.low, conf.high)
      )

    p <- ggplot() +
      geom_point(data = dt, aes(x = age, y = n, shape = .data$include)) +
      geom_smooth(
        data = dt %>% dplyr::filter(.data$include),
        aes(x = age, y = n),
        method = "lm",
        se = FALSE,
        formula = 'y ~ x',
        color = "black",
        linewidth = 0.7 / 2.13
      ) +
      geom_text(
        data = modb,
        aes(
          x = Inf,
          y = Inf,
          label = paste0(
            "b = ",
            round(.data$estimate_b, 3),
            " (",
            round(.data$conf.low_b, 3),
            ",",
            round(.data$conf.high_b, 3),
            ")\n",
            "a = ",
            round(.data$estimate_a, 3),
            " (",
            round(.data$conf.low_a, 3),
            ",",
            round(.data$conf.high_a, 3),
            ")\n"
          )
        ),
        vjust = 1,
        hjust = 1,
        size = base_size / 2.845276
      ) +
      labs(x = "Age (years)", y = "Abundance (Ln-scale)") +
      {
        if (!is.null(time)) facet_wrap(~time)
      } +
      scale_shape_manual(
        "Included to Z regression",
        values = c(21, 19)
      ) +
      coord_cartesian(expand = FALSE, clip = "off") +
      scale_y_continuous(trans = "log", breaks = scales::log_breaks(n = 10)) +
      scale_x_continuous(n.breaks = 5) +
      theme_fishplots(base_size = base_size) +
      theme(
        legend.position = legend.position,
        text = element_text(size = base_size)
      )

    if (!is.null(time)) {
      modb <- modb %>%
        dplyr::group_by(sex) %>%
        dplyr::summarise(
          conf.low_a = min(.data$estimate_a),
          conf.high_a = max(.data$estimate_a),
          estimate_a = mean(.data$estimate_a),
          conf.low_b = min(.data$estimate_b),
          conf.high_b = max(.data$estimate_b),
          estimate_b = mean(.data$estimate_b),
        )
    }

    Text <- paste0(
      "Instantenous total mortality (Z) estimated using a catch curve and\nage range ",
      paste(age.range, collapse = "-"),
      ".\n\n",
      if (!is.null(time)) {
        "Mean "
      },
      "Z = ",
      round(-modb$estimate_b, 3),
      " (",
      round(-modb$conf.high_b, 3),
      "-",
      round(-modb$conf.low_b, 3),
      ifelse(is.null(time), paste(" 95% CIs)\n"), paste(" range)\n")),
      if (!is.null(time)) {
        "Mean "
      },
      "N at age 0 = ",
      round(exp(modb$estimate_a)),
      " (",
      round(exp(modb$conf.low_a)),
      "-",
      round(exp(modb$conf.high_a)),
      ifelse(is.null(time), paste(" 95% CIs)\n"), paste(" range)\n")),
      if (!is.null(time)) {
        "Mean l"
      } else {
        "L"
      },
      "longevity = ",
      round(-modb$estimate_a / modb$estimate_b, 1),
      " years (",
      round(-modb$conf.low_a / modb$conf.low_b, 1),
      " - ",
      round(-modb$conf.high_a / modb$conf.high_b, 1),
      ifelse(is.null(time), paste(" 95% CIs)\n\n"), paste(" range)\n\n"))
    )
  }

  # Return ####

  return(list(
    plot = suppressWarnings(p),
    text = Text,
    params = mod,
    definitions = list(age.range = age.range)
  ))
}
