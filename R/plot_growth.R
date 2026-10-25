# Debug parameters:
# length = "length"; age = "age"; sex = "sex"; female.sex = "F"; male.sex = "M"; length.unit = "cm"; split.by.sex = FALSE; growth.model = 1; force.zero.group.length = NA; force.zero.group.strength = 10; force.zero.group.cv = 0; show.Linf = TRUE; boxplot = TRUE; return_model = FALSE; return_data = FALSE; base_size = 8; legend.position = "bottom"

# Internal helper: delta-method SE/CI for L0 from fitted growth model
compute_L0_row <- function(fit_model, growth_model_internal, n_obs) {
  L0 <- try(as.numeric(predict(fit_model, newdata = data.frame(age = 0))), silent = TRUE)
  if (inherits(L0, "try-error") || length(L0) == 0 || !is.finite(L0)) {
    return(tibble::tibble(
      term = "L0", estimate = NA_real_, std.error = NA_real_,
      statistic = NA_real_, p.value = NA_real_, conf.low = NA_real_, conf.high = NA_real_
    ))
  }

  se_L0 <- NA_real_; stat_L0 <- NA_real_; pval_L0 <- NA_real_
  ci_low <- NA_real_; ci_high <- NA_real_

  vcov_mat <- try(vcov(fit_model), silent = TRUE)
  if (!inherits(vcov_mat, "try-error") && all(is.finite(vcov_mat))) {
    pars <- coef(fit_model)
    Sinf <- pars[["Sinf"]]; K <- pars[["K"]]; t0 <- pars[["t0"]]
    eKt0 <- exp(K * t0)

    grad <- if (growth_model_internal == "vout") {
      c(1 - eKt0, -Sinf * t0 * eKt0, -Sinf * K * eKt0)
    } else if (growth_model_internal == "gout") {
      en <- exp(-eKt0)
      c(en, -Sinf * t0 * eKt0 * en, -Sinf * K * eKt0 * en)
    } else {
      d <- 1 + eKt0
      c(1 / d, -Sinf * t0 * eKt0 / d^2, -Sinf * K * eKt0 / d^2)
    }

    var_L0 <- as.numeric(t(grad) %*% vcov_mat %*% grad)
    if (is.finite(var_L0) && var_L0 > 0) {
      se_L0   <- sqrt(var_L0)
      df      <- n_obs - length(pars)
      stat_L0 <- L0 / se_L0
      pval_L0 <- 2 * pt(-abs(stat_L0), df)
      ci_low  <- L0 - qt(0.975, df) * se_L0
      ci_high <- L0 + qt(0.975, df) * se_L0
    }
  }

  tibble::tibble(
    term = "L0", estimate = L0, std.error = se_L0,
    statistic = stat_L0, p.value = pval_L0, conf.low = ci_low, conf.high = ci_high
  )
}

#' @title Plot age-length relationships and growth curves
#' @description Fits and plots a von Bertalanffy, Gompertz, or Logistic growth curve to age-length data. Returns the plot, a text summary, and estimated parameters.
#' @param length Character argument giving the name of the length column in \code{dt}
#' @param age Character argument giving the name of the age column in \code{dt}
#' @param growth.model Integer defining the growth model. 1 = von Bertalanffy, 2 = Gompertz, 3 = Logistic.
#' @param force.zero.group.length Numeric indicating the length to which 0-group should be forced. Use \code{NA} ignore the forcing.
#' @param force.zero.group.cv Numeric indicating the coefficient of variation for the forced 0-group length. Resulting lengths will be randomly generated from a normal distribution.
#' @param force.zero.group.strength Numeric indicating how many percent of total fish should be added to the specified \code{force.zero.group.length}.
#' @inheritParams plot_maturity
#' @param boxplot Logical indicating whether boxplots (\code{TRUE}) should be used to show data over points (\code{FALSE})
#' @param return_model Logical indicating whether the growth models should be returned together with other output.
#' @param return_data Logical indicating whether the data used to make the growth models should be returned together with other output.
#' @param show.Linf Logical indicating whether Linf values should be shown as dashed vertical lines.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme}.
#' @details Uses the \code{fishmethods::growth} function to calculate the growth curves. Zero group length can be forced to the growth functions using the \code{force.zero.group.*} parameters.
#' @return A named list with \code{plot} (a ggplot2 object), \code{text} (a text summary),
#'   \code{params} (the estimated growth parameters), \code{model} (the fitted models
#'   when \code{return_model = TRUE}, otherwise \code{NULL}), and \code{data} (the
#'   prepared data when \code{return_data = TRUE}, otherwise \code{NULL}).
#' @author Mikko Vihtakari // Institute of Marine Research.
#' @import dplyr ggplot2
#' @importFrom fishmethods growth
#' @importFrom stats pt qt rnorm setNames vcov
#' @examples
#' # Simple plot. Note that a list is returned.
#' data(survey_ghl)
#' plot_growth(survey_ghl, length = "length", age = "age")
#' \donttest{
#' # Split by sex
#' plot_growth(survey_ghl, split.by.sex = TRUE)$plot
#' # Data as points. Forcing zero group to 10 cm
#' plot_growth(survey_ghl, force.zero.group.length = 10, boxplot = FALSE)$plot
#' }
#' @export
plot_growth <- function(
  dt,
  length = "length",
  age = "age",
  sex = "sex",
  female.sex = "F",
  male.sex = "M",
  length.unit = "cm",
  split.by.sex = FALSE,
  growth.model = 1,
  force.zero.group.length = NA,
  force.zero.group.strength = 10,
  force.zero.group.cv = 0,
  show.Linf = TRUE,
  boxplot = TRUE,
  return_model = FALSE,
  return_data = FALSE,
  base_size = 8,
  legend.position = "bottom"
) {
  # Growth model

  if (!growth.model %in% 1:3) {
    stop("growth.model has to be an integer between 1 and 3")
  }

  modName <- c(
    "von Bertalanffy" = "vout",
    "Gompertz" = "gout",
    "Logistic" = "lout"
  )
  mod.name <- names(modName[growth.model])
  growth.model <- unname(modName[growth.model])

  # Add row number

  dt$id <- rownames(dt)
  orig.nrow <- nrow(dt)

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
    if (length(na.omit(dt[[sex]])) < 10) {
      stop("Either invalid sex column or not enough sex data")
    }

    dt <- dt %>%
      dplyr::rename("sex" = !!sex) %>%
      dplyr::filter(!is.na(sex))

    sex.missing <- orig.nrow - nrow(dt)
  }

  # Filter

  dt <- dt %>%
    dplyr::rename(
      "age" = !!age,
      "length" = !!length
    )

  length.missing <- sum(is.na(dt$length))
  age.missing <- sum(is.na(dt$age))

  dt <- dt %>% dplyr::filter(!is.na(age) & !is.na(length))

  # Select columns and add zero group if requested

  if (split.by.sex) {
    dt <- dt %>% dplyr::select(id, sex, age, length)

    if (!is.na(force.zero.group.length)) {
      dt <- dt %>%
        tibble::add_column(type = "data") %>%
        dplyr::bind_rows(
          tibble::tibble(
            id = NA,
            sex = female.sex,
            age = 0,
            length = rnorm(
              sum(dt$sex == female.sex) * (force.zero.group.strength / 100),
              force.zero.group.length,
              force.zero.group.length * force.zero.group.cv
            ),
            type = "made"
          ),
          tibble::tibble(
            id = NA,
            sex = male.sex,
            age = 0,
            length = rnorm(
              sum(dt$sex == male.sex) * (force.zero.group.strength / 100),
              force.zero.group.length,
              force.zero.group.length * force.zero.group.cv
            ),
            type = "made"
          )
        )
    }
  } else {
    dt <- dt %>% dplyr::select(id, age, length)

    if (!is.na(force.zero.group.length)) {
      dt <- dt %>%
        tibble::add_column(type = "data") %>%
        dplyr::bind_rows(
          tibble::tibble(
            id = NA,
            age = 0,
            length = rnorm(
              nrow(dt) * (force.zero.group.strength / 100),
              force.zero.group.length,
              force.zero.group.length * force.zero.group.cv
            ),
            type = "made"
          )
        )
    }
  }

  ################
  ## The Plot ####

  laModpars <- NULL

  # Plot sexed data

  if (split.by.sex) {
    # FSA::vbStarts(length~age, data = dt %>% dplyr::filter(sex == female.sex))

    laModF <- fishmethods::growth(
      age = dt %>% dplyr::filter(sex == female.sex) %>% dplyr::pull(age),
      size = dt %>% dplyr::filter(sex == female.sex) %>% dplyr::pull(length),
      Sinf = dt %>%
        dplyr::filter(sex == female.sex) %>%
        dplyr::pull(length) %>%
        max,
      K = 0.1,
      t0 = 0,
      graph = FALSE
    )

    laModM <- fishmethods::growth(
      age = dt %>% dplyr::filter(sex == male.sex) %>% dplyr::pull(age),
      size = dt %>% dplyr::filter(sex == male.sex) %>% dplyr::pull(length),
      Sinf = dt %>%
        dplyr::filter(sex == male.sex) %>%
        dplyr::pull(length) %>%
        max,
      K = 0.1,
      t0 = 0,
      graph = FALSE
    )

    FfitFailed <- all(
      eval(parse(text = paste0("laModF$", growth.model))) == "Fit failed"
    )
    MfitFailed <- all(
      eval(parse(text = paste0("laModM$", growth.model))) == "Fit failed"
    )

    if (FfitFailed & MfitFailed) {
      Plot <- ggplot(data = dt, aes(x = age, y = length, color = sex)) +
        {
          if (boxplot) {
            geom_boxplot(
              aes(group = interaction(age, sex)),
              alpha = 0.5,
              outlier.size = 0.5
            )
          }
        } +
        {
          if (!boxplot) {
            geom_point(
              aes(text = paste0("row number: ", id)),
              alpha = 0.5,
              shape = 21
            )
          }
        } +
        annotation_custom(
          grid::textGrob(
            "Failed to fit growth models\nConsider adding force.zero.group.length\nand/or not splitting by sex.",
            gp = grid::gpar(fontsize = base_size, fontface = "bold")
          ),
          xmin = -Inf,
          xmax = Inf,
          ymin = -Inf,
          ymax = Inf
        ) +
        scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
        ylab(paste0("Total length (", length.unit, ")")) +
        xlab("Age (years)") +
        coord_cartesian(
          expand = FALSE,
          clip = "off",
          xlim = c(0, NA),
          ylim = c(0, NA)
        ) +
        theme_fishplots(base_size = base_size) +
        theme(
          legend.position = legend.position,
          text = element_text(size = base_size)
        )

      Text <- paste0(
        "Failed to fit growth models:",
        "\n Number of included specimens = ",
        sum(dt$sex == female.sex),
        " females and ",
        sum(dt$sex == male.sex),
        " males"
      )

      Models <- NA
    } else {
      # Females
      if (FfitFailed) {
        laModFpred <- data.frame(age = 0:max(dt$age), length = NA)
        laModparsF <- data.frame(
          sex = female.sex,
          term = NA,
          estimate = NA,
          std.error = NA,
          statistic = NA,
          p.value = NA,
          conf.low = NA,
          conf.high = NA
        )

        Fmodels <- NA
      } else {
        tmpF <- predict(
          eval(parse(text = paste0("laModF$", growth.model))),
          newdata = data.frame(age = 0:max(dt$age))
        )
        laModFpred <- data.frame(age = 0:max(dt$age), length = tmpF)

        model_tidy <- try(
          {
            broom::tidy(
              eval(parse(text = paste0("laModF$", growth.model))),
              conf.int = TRUE
            )
          },
          silent = TRUE
        )

        if (inherits(model_tidy, "try-error")) {
          laModparsF <- dplyr::bind_cols(
            sex = female.sex,
            broom::tidy(
              eval(parse(text = paste0("laModF$", growth.model))),
              conf.int = FALSE
            )
          )
          Fmodels <- laModF
        } else {
          laModparsF <- dplyr::bind_cols(sex = female.sex, model_tidy)
          Fmodels <- laModF
        }

        L0_row_F <- compute_L0_row(
          eval(parse(text = paste0("laModF$", growth.model))),
          growth.model,
          sum(dt$sex == female.sex)
        )
        if (!"conf.low" %in% names(laModparsF)) {
          L0_row_F <- dplyr::select(L0_row_F, -conf.low, -conf.high)
        }
        laModparsF <- dplyr::bind_rows(
          laModparsF,
          dplyr::mutate(L0_row_F, sex = female.sex, .before = 1)
        )
      }

      # Males
      if (MfitFailed) {
        laModMpred <- data.frame(age = 0:max(dt$age), length = NA)
        laModparsM <- data.frame(
          sex = male.sex,
          term = NA,
          estimate = NA,
          std.error = NA,
          statistic = NA,
          p.value = NA,
          conf.low = NA,
          conf.high = NA
        )
        Mmodels <- NA
      } else {
        tmpM <- predict(
          eval(parse(text = paste0("laModM$", growth.model))),
          newdata = data.frame(age = 0:max(dt$age))
        )
        laModMpred <- data.frame(age = 0:max(dt$age), length = tmpM)

        model_tidy <- try(
          {
            broom::tidy(
              eval(parse(text = paste0("laModM$", growth.model))),
              conf.int = TRUE
            )
          },
          silent = TRUE
        )

        if (inherits(model_tidy, "try-error")) {
          laModparsM <- dplyr::bind_cols(
            sex = male.sex,
            broom::tidy(
              eval(parse(text = paste0("laModM$", growth.model))),
              conf.int = FALSE
            )
          )
          Mmodels <- laModM
        } else {
          laModparsM <- dplyr::bind_cols(sex = male.sex, model_tidy)
          Mmodels <- laModM
        }

        L0_row_M <- compute_L0_row(
          eval(parse(text = paste0("laModM$", growth.model))),
          growth.model,
          sum(dt$sex == male.sex)
        )
        if (!"conf.low" %in% names(laModparsM)) {
          L0_row_M <- dplyr::select(L0_row_M, -conf.low, -conf.high)
        }
        laModparsM <- dplyr::bind_rows(
          laModparsM,
          dplyr::mutate(L0_row_M, sex = male.sex, .before = 1)
        )
      }

      laModpars <- dplyr::bind_rows(laModparsF, laModparsM)
      Models <- setNames(list(Fmodels, Mmodels), c(female.sex, male.sex))

      ## Plot

      Plot <-
        suppressWarnings({
          ggplot(data = dt, aes(x = age, y = length, color = sex)) +
            {
              if (boxplot) {
                geom_boxplot(
                  aes(group = interaction(age, sex)),
                  alpha = 0.5,
                  outlier.size = 0.5
                )
              }
            } +
            {
              if (!boxplot) {
                geom_point(
                  aes(text = paste0("row number: ", id)),
                  alpha = 0.5,
                  shape = 21
                )
              }
            } +
            {
              if (show.Linf & !FfitFailed) {
                geom_hline(
                  yintercept = laModparsF$estimate[1],
                  linetype = 2,
                  color = "#FF5F68",
                  alpha = 0.5
                )
              }
            } +
            {
              if (show.Linf & !MfitFailed) {
                geom_hline(
                  yintercept = laModparsM$estimate[1],
                  linetype = 2,
                  color = "#449BCF",
                  alpha = 0.5
                )
              }
            } +
            {
              if (!FfitFailed) {
                geom_path(
                  data = laModFpred,
                  aes(x = age, y = length),
                  color = "#FF5F68",
                  linewidth = 2 / 2.13
                )
              }
            } +
            {
              if (FfitFailed) {
                annotate(
                  "text",
                  x = -Inf,
                  y = Inf,
                  label = "Fit failed for females",
                  color = "#FF5F68",
                  size = base_size / 2.85,
                  vjust = 3,
                  hjust = -0.5
                )
              }
            } +
            {
              if (!MfitFailed) {
                geom_path(
                  data = laModMpred,
                  aes(x = age, y = length),
                  color = "#449BCF",
                  linewidth = 2 / 2.13
                )
              }
            } +
            {
              if (MfitFailed) {
                annotate(
                  "text",
                  x = -Inf,
                  y = Inf,
                  label = "Fit failed for males",
                  color = "#449BCF",
                  size = base_size / 2.85,
                  vjust = 2,
                  hjust = -0.5
                )
              }
            } +
            scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
            labs(
              y = paste0("Total length (", length.unit, ")"),
              x = "Age (years)"
            ) +
            coord_cartesian(
              expand = FALSE,
              clip = "off",
              xlim = c(0, NA),
              ylim = c(0, NA)
            ) +
            theme_fishplots(base_size = base_size) +
            theme(
              legend.position = legend.position,
              text = element_text(size = base_size)
            )
        })

      Text <- paste0(
        mod.name,
        " growth function coefficients for females and males, respectively:  \n Linf (asymptotic average length) = ",
        round(laModparsF$estimate[1], 1),
        " ",
        length.unit,
        " +/- ",
        if ("conf.low" %in% names(laModparsF)) {
          paste0(
            round(laModparsF$conf.low[1], 1),
            " - ",
            round(laModparsF$conf.high[1], 1),
            " (95% CIs) and "
          )
        } else {
          paste0("no CIs and ")
        },
        round(laModparsM$estimate[1], 1),
        " ",
        length.unit,
        " +/- ",
        if ("conf.low" %in% names(laModparsM)) {
          paste0(
            round(laModparsM$conf.low[1], 1),
            " - ",
            round(laModparsM$conf.high[1], 1),
            " (95% CIs)"
          )
        } else {
          paste0("no CIs")
        },
        "  \n K (growth rate coefficient) = ",
        round(laModparsF$estimate[2], 4),
        " ",
        length.unit,
        " +/- ",
        if ("conf.low" %in% names(laModparsF)) {
          paste0(
            round(laModparsF$conf.low[2], 3),
            " - ",
            round(laModparsF$conf.high[2], 3),
            " (95% CIs) and "
          )
        } else {
          paste0("no CIs and ")
        },
        round(laModparsM$estimate[2], 4),
        " ",
        length.unit,
        " +/- ",
        if ("conf.low" %in% names(laModparsM)) {
          paste0(
            round(laModparsM$conf.low[2], 3),
            " - ",
            round(laModparsM$conf.high[2], 3),
            " (95% CIs)"
          )
        } else {
          paste0("no CIs")
        },
        "  \n t0 (age at length 0) = ",
        round(laModparsF$estimate[3], 2),
        " (years) +/- ",
        if ("conf.low" %in% names(laModparsF)) {
          paste0(
            round(laModparsF$conf.low[3], 1),
            " - ",
            round(laModparsF$conf.high[3], 1),
            " (95% CIs) and "
          )
        } else {
          paste0("no CIs and ")
        },
        round(laModparsM$estimate[3], 2),
        " (years) +/- ",
        if ("conf.low" %in% names(laModparsM)) {
          paste0(
            round(laModparsM$conf.low[3], 3),
            " - ",
            round(laModparsM$conf.high[3], 3),
            " (95% CIs)"
          )
        } else {
          paste0("no CIs")
        },
        "  \n L0 (length at age 0) = ",
        round(laModparsF$estimate[4], 1),
        " ",
        length.unit,
        " +/- ",
        if ("conf.low" %in% names(laModparsF) && !is.na(laModparsF$conf.low[4])) {
          paste0(
            round(laModparsF$conf.low[4], 1),
            " - ",
            round(laModparsF$conf.high[4], 1),
            " (95% CIs) and "
          )
        } else {
          paste0("no CIs and ")
        },
        round(laModparsM$estimate[4], 1),
        " ",
        length.unit,
        " +/- ",
        if ("conf.low" %in% names(laModparsM) && !is.na(laModparsM$conf.low[4])) {
          paste0(
            round(laModparsM$conf.low[4], 1),
            " - ",
            round(laModparsM$conf.high[4], 1),
            " (95% CIs)"
          )
        } else {
          paste0("no CIs")
        },
        "  \n tmax (life span; t0 + 3/K) = ",
        round(laModparsF$estimate[3] + 3 / laModparsF$estimate[2], 1),
        " and ",
        round(laModparsM$estimate[3] + 3 / laModparsM$estimate[2], 1),
        " years",
        "  \n Number of included specimens = ",
        nrow(dt[dt$sex == female.sex, ]),
        " and ",
        nrow(dt[dt$sex == male.sex, ]),
        "  \n Total number of measured = ",
        orig.nrow,
        "  \n Excluded (length or age missing): \n Length = ",
        length.missing,
        "; age = ",
        age.missing,
        "; sex = ",
        sex.missing
      )
    }
  } else {
    # Plot non-sex split data

    laMod <- fishmethods::growth(
      age = dt$age,
      size = dt$length,
      Sinf = max(dt$length),
      K = 0.1,
      t0 = 0,
      graph = FALSE
    )

    if (
      all(eval(parse(text = paste0("laMod$", growth.model))) == "Fit failed")
    ) {
      #if(eval(parse(text = paste0("laMod$", growthModelSwitch))) == "Fit failed") {

      Plot <- ggplot(dt, aes(x = age, y = length)) +
        {
          if (boxplot) {
            geom_boxplot(
              data = dt,
              aes(x = age, y = length, group = age),
              outlier.size = 0.5,
              alpha = 0.5
            )
          }
        } +
        {
          if (!boxplot) {
            geom_point(
              data = dt,
              aes(x = age, y = length, text = paste0("row number: ", id)),
              shape = 21,
              alpha = 0.5
            )
          }
        } +
        annotation_custom(
          grid::textGrob(
            "Failed to fit a growth model\nConsider adding force.zero.group.length",
            gp = grid::gpar(
              fontsize = base_size,
              fontface = "bold",
              col = "red"
            )
          ),
          xmin = -Inf,
          xmax = Inf,
          ymin = -Inf,
          ymax = Inf
        ) +
        ylab(paste0("Total length (", length.unit, ")")) +
        xlab("Age (years)") +
        coord_cartesian(
          expand = FALSE,
          clip = "off",
          xlim = c(0, NA),
          ylim = c(0, NA)
        ) +
        theme_fishplots(base_size = base_size) +
        theme(
          legend.position = legend.position,
          text = element_text(size = base_size)
        )

      Text <- paste0(
        "Failed to fit a growth model:",
        "  \n Number of included specimens = ",
        nrow(dt)
      )

      Models <- laMod
    } else {
      laModpred <- data.frame(
        age = 0:max(dt$age),
        length = predict(
          eval(parse(text = paste0("laMod$", growth.model))),
          newdata = data.frame(age = 0:max(dt$age))
        )
      )

      model_tidy <- try(
        {
          broom::tidy(
            eval(parse(text = paste0("laMod$", growth.model))),
            conf.int = TRUE
          )
        },
        silent = TRUE
      )

      if (any(class(model_tidy) == "try-error")) {
        laModpars <- dplyr::bind_cols(broom::tidy(
          eval(parse(text = paste0("laMod$", growth.model))),
          conf.int = FALSE
        ))
      } else {
        laModpars <- dplyr::bind_cols(model_tidy)
      }

      L0_row <- compute_L0_row(
        eval(parse(text = paste0("laMod$", growth.model))),
        growth.model,
        nrow(dt)
      )
      if (!"conf.low" %in% names(laModpars)) {
        L0_row <- dplyr::select(L0_row, -conf.low, -conf.high)
      }
      laModpars <- dplyr::bind_rows(laModpars, L0_row)

      Plot <-
        suppressWarnings({
          ggplot(data = dt, aes(x = age, y = length)) +
            {
              if (boxplot) {
                geom_boxplot(aes(group = age), outlier.size = 0.5, alpha = 0.5)
              }
            } +
            {
              if (!boxplot) {
                geom_point(
                  aes(text = paste0("row number: ", id)),
                  shape = 21,
                  alpha = 0.5
                )
              }
            } +
            # expand_limits(x = c(0, round_any(max(dt$age), 2, ceiling)), y = c(0, round_any(max(dt$length), 5, ceiling))) + # c(0, max(pretty(c(0, max(dt$length)))))
            {
              if (show.Linf) {
                geom_hline(
                  yintercept = laModpars$estimate[1],
                  linetype = 2,
                  color = "blue",
                  alpha = 0.5
                )
              }
            } +
            geom_path(
              data = laModpred,
              aes(x = age, y = length),
              color = "blue"
            ) +
            labs(
              y = paste0("Total length (", length.unit, ")"),
              x = "Age (years)"
            ) +
            coord_cartesian(
              expand = FALSE,
              clip = "off",
              xlim = c(0, NA),
              ylim = c(0, NA)
            ) +
            theme_fishplots(base_size = base_size) +
            theme(
              legend.position = legend.position,
              text = element_text(size = base_size)
            )
        })

      Text <- paste0(
        mod.name,
        " growth function coefficients:  \n Linf (asymptotic average length) = ",
        round(laModpars$estimate[1], 1),
        " ",
        length.unit,
        " +/- ",
        if ("conf.low" %in% names(laModpars)) {
          paste0(
            round(laModpars$conf.low[1], 1),
            " - ",
            round(laModpars$conf.high[1], 1),
            " (95% CIs)"
          )
        } else {
          paste0("no CIs")
        },
        "  \n K (growth rate coefficient) = ",
        round(laModpars$estimate[2], 4),
        " +/- ",
        if ("conf.low" %in% names(laModpars)) {
          paste0(
            round(laModpars$conf.low[2], 3),
            " - ",
            round(laModpars$conf.high[2], 3),
            " (95% CIs)"
          )
        } else {
          paste0("no CIs")
        },
        "  \n t0 (age at length 0) = ",
        round(laModpars$estimate[3], 2),
        " (years) +/- ",
        if ("conf.low" %in% names(laModpars)) {
          paste0(
            round(laModpars$conf.low[3], 3),
            " - ",
            round(laModpars$conf.high[3], 3),
            " (95% CIs)"
          )
        } else {
          paste0("no CIs")
        },
        "  \n L0 (length at age 0) = ",
        round(laModpars$estimate[4], 1),
        " ",
        length.unit,
        " +/- ",
        if ("conf.low" %in% names(laModpars) && !is.na(laModpars$conf.low[4])) {
          paste0(
            round(laModpars$conf.low[4], 1),
            " - ",
            round(laModpars$conf.high[4], 1),
            " (95% CIs)"
          )
        } else {
          paste0("no CIs")
        },
        "  \n tmax (life span; t0 + 3/K) = ",
        round(laModpars$estimate[3] + 3 / laModpars$estimate[2], 1),
        " years",
        "  \n Number of included specimens = ",
        nrow(dt),
        "  \n Total number of measured = ",
        orig.nrow,
        "  \n Excluded (length or age missing):  \n Length = ",
        length.missing,
        "; age = ",
        age.missing
      )

      Models <- laMod
    }
  }

  ## Return

  return({
    list(
      plot = Plot,
      text = Text,
      params = laModpars,
      model = if (return_model) {
        Models
      } else {
        NULL
      },
      data = if (return_data) {
        dt
      } else {
        NULL
      }
    )
  })
}
