#' @title Plot maturity ogive
#' @description Plots an estimate of length or age at 50\% mature for a dataset
#' @param dt A data.frame, tibble or data.table
#' @param length Character argument giving the name of the length (or age) column in \code{dt}
#' @param maturity Character argument giving the name of the maturity column in \code{dt}. Should be either logical (\code{TRUE == mature, FALSE == immature}) or integer (\code{1 == mature, 0 == immature}).
#' @param sex Character argument giving the name of the sex column in \code{dt}. Ignored if \code{split.by.sex == FALSE}.
#' @param length.unit A character argument giving the unit of \code{length}. Will be used in the labels of the figure.
#' @param length.bin.width Numeric specifying the increment (delta length) by which length data should be binned to calculate maturity proportions. Use \code{NULL} to remove from the plot.
#' @param split.by.sex Logical indicating whether the result should be split by sex.
#' @param female.sex,male.sex A character or integer denoting female and male sex in the \code{sex} column of \code{dt}, respectively.
#' @param bootstrap.n Integer defining the number of bootstrap replicates to be used to calculate 95\% confidence intervals for the mean 50\% mature estimate. If \code{NA} (default), the confidence intervals are calculated from the glm object using the delta method without bootstrapping. See Details.
#' @param bootstrap.cluster Character argument giving the name of a column in \code{dt} that identifies the sampling unit (e.g. haul, station or trip ID) for a cluster bootstrap. If \code{NULL} (default), individual fish are resampled. If given, whole clusters are resampled with replacement instead, which accounts for fish from the same haul being more alike than fish from different hauls. Requires \code{bootstrap.n}. See Details.
#' @param force.zero.group.length Numeric indicating the length to which 0-group (all immatures) should be forced. Use \code{NA} ignore the forcing.
#' @param force.zero.group.cv Numeric indicating the coefficient of variation for the forced 0-group (all immature) length. Resulting lengths will be randomly generated from a normal distribution.
#' @param force.zero.group.strength Numeric indicating how many percent of total fish should be added to the specified \code{force.zero.group.length}. Cannot be used simultaneously with \code{force.zero.group.n}
#' @param force.zero.group.n Numeric indicating how many observations should be added to the specified \code{force.zero.group.length}. If \code{split.by.sex = TRUE}, use a named vector of length two with names referring to \code{female.sex} and \code{male.sex}. Cannot be used simultaneously with \code{force.zero.group.strength}
#' @param xlab Character giving the x-axis label without unit
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme}.
#' @param legend.position Position of the ggplot legend as a character. See \link[ggplot2]{theme}.
#' @param ... Additional arguments passed to \link[ggridges]{geom_density_ridges}.
#' @return A named list with \code{plot} (a ggplot2 object), \code{text} (a text summary),
#'   and \code{params} (the estimated maturity-model parameters).
#' @details The 95\% confidence intervals for the mean 50\% mature estimate are calculated by default with the delta method from the fitted \link[stats]{glm} (see \code{\link{unlogit}}): L50 = -intercept/slope, and its standard error is approximated from the variance-covariance matrix of the intercept and slope, so the strong negative correlation between the two is accounted for. The interval is L50 \eqn{\pm} 1.96 SE and is equivalent to \code{MASS::dose.p()}. Versions of ggFishPlots before 0.4.5 combined the marginal profile-likelihood limits of the intercept and slope, which ignored this correlation and produced far too wide intervals.
#'
#' The delta-method interval might not be optimal when zero group fish are added, because the added data inflate the apparent sample size. Hence, the function contains an option to bootstrap confidence intervals using \emph{the same number of data than observations} (i.e. excluding the added data from the number of randomly resampled rows). Adding an integer to the \code{bootstrap.n} argument turns on this feature; the interval is then given by the 2.5\% and 97.5\% quantiles of the bootstrapped L50 estimates. Without added zero group data, the bootstrapped and delta-method intervals are usually similar.
#'
#' Both the delta-method and the individual bootstrap intervals assume that fish are independent observations. Survey and fishery samples are usually clustered: fish caught in the same haul tend to be of similar size and maturity, so the effective sample size is smaller than the number of fish and these intervals are too narrow. Giving the haul (or other sampling unit) identifier in \code{bootstrap.cluster} turns on a cluster bootstrap: each replicate draws as many clusters as there are in the data, with replacement, and refits the model to all fish in the drawn clusters. Added zero group data are appended unchanged to each replicate. Fish with a missing cluster identifier are removed. The cluster bootstrap needs a reasonable number of clusters (preferably several tens) per sex to be reliable.
#' @author Mikko Vihtakari // Institute of Marine Research.
#' @import dplyr ggplot2
#' @importFrom ggridges geom_density_ridges
#' @importFrom stats na.omit binomial coef glm predict rnorm quantile
#' @importFrom ggrepel geom_text_repel
#' @examples
#' # Simple L50 plot
#' data(survey_ghl)
#' plot_maturity(survey_ghl, length = "length", maturity = "maturity")
#' \donttest{
#' # Bootstrapped CIs
#' plot_maturity(survey_ghl, bootstrap.n = 10)
#'
#' # Cluster (haul-level) bootstrap. survey_ghl has no haul ID, so a
#' # made-up one is used here for illustration only
#' ghl <- survey_ghl
#' ghl$haul <- sample(1:50, nrow(ghl), replace = TRUE)
#' plot_maturity(ghl, bootstrap.n = 10, bootstrap.cluster = "haul")
#'
#' # A50 plot, split by sex
#' plot_maturity(survey_ghl, length = "age", length.unit = "years",
#' xlab = "Age", length.bin.width = 1, split.by.sex = TRUE)$plot
#'
#' # Add juveniles
#' plot_maturity(survey_ghl, length = "age", length.unit = "years",
#' xlab = "Age", length.bin.width = 1, split.by.sex = TRUE,
#' force.zero.group.length = 0,
#' force.zero.group.strength = 100)$plot
#' }
#' @export

# Debug parameters
# length = "length"; maturity = "maturity"; sex = "sex"; split.by.sex = FALSE; female.sex = "F"; male.sex = "M"; length.unit = "cm"; length.bin.width = 2; bootstrap.n = NA; bootstrap.cluster = NULL; force.zero.group.length = NA; force.zero.group.strength = NA; force.zero.group.n = NA; force.zero.group.cv = 0; xlab = "Total length"; base_size = 8; legend.position = "bottom"

plot_maturity <- function(
  dt,
  length = "length",
  maturity = "maturity",
  sex = "sex",
  split.by.sex = FALSE,
  female.sex = "F",
  male.sex = "M",
  length.unit = "cm",
  length.bin.width = 2,
  bootstrap.n = NA,
  bootstrap.cluster = NULL,
  force.zero.group.length = NA,
  force.zero.group.strength = NA,
  force.zero.group.n = NA,
  force.zero.group.cv = 0,
  xlab = "Total length",
  base_size = 8,
  legend.position = "bottom",
  ...
) {
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

  if (!is.null(bootstrap.cluster)) {
    if (!is.character(bootstrap.cluster) || length(bootstrap.cluster) != 1 ||
        !bootstrap.cluster %in% colnames(dt)) {
      stop("bootstrap.cluster has to be the name of a column in dt")
    }
    if (is.na(bootstrap.n)) {
      stop("bootstrap.cluster requires bootstrap.n. Give the number of bootstrap replicates.")
    }
  }

  if (!is.na(force.zero.group.length)) {
    if (is.na(force.zero.group.strength) & any(is.na(force.zero.group.n))) {
      stop(
        "You want to use force.zero.group? Specify either force.zero.group.strength or force.zero.group.n"
      )
    }

    if (!is.na(force.zero.group.strength) & !all(is.na(force.zero.group.n))) {
      stop(
        "force.zero.group.strength and force.zero.group.n cannot be used simultaneously. Set either of these to NA"
      )
    }

    if (all(!is.na(force.zero.group.n)) & split.by.sex) {
      if (length(force.zero.group.n) != 2) {
        stop(
          "force.zero.group.n has to be a numeric vector of length 2 when split.by.sex = TRUE"
        )
      }
      if (is.null(names(force.zero.group.n))) {
        stop(
          "force.zero.group.n has to be a named vector when split.by.sex = TRUE. Use same names than female.sex and male.sex"
        )
      }
      if (any(!names(force.zero.group.n) %in% c(female.sex, male.sex))) {
        stop(
          "Names of force.zero.group.n have to equal those of female.sex and male.sex"
        )
      }
    }
  }

  # Data manipulation ####

  if ("length" %in% colnames(dt) && length != "length") {
    dt <- dt %>% dplyr::select(-length)
  }

  if ("maturity" %in% colnames(dt) && maturity != "maturity") {
    dt <- dt %>% dplyr::select(-maturity)
  }

  if (!is.null(bootstrap.cluster)) {
    if ("cluster" %in% colnames(dt) && bootstrap.cluster != "cluster") {
      dt <- dt %>% dplyr::select(-"cluster")
    }
    dt <- dt %>% dplyr::rename("cluster" = !!bootstrap.cluster)

    if (any(is.na(dt$cluster))) {
      warning(
        sum(is.na(dt$cluster)),
        " rows with missing ", bootstrap.cluster,
        " removed before cluster bootstrapping."
      )
      dt <- dt %>% dplyr::filter(!is.na(.data$cluster))
    }
    dt$cluster <- as.character(dt$cluster)
  } else {
    dt$cluster <- NA_character_
  }

  ## Select and rename columns ####

  dt <- dt %>%
    dplyr::rename(
      "maturity" = !!maturity,
      "length" = !!length
    ) %>%
    dplyr::filter(
      !is.na(maturity) &
        !is.na(length)
    ) %>%
    dplyr::mutate(maturity = as.integer(maturity))

  ## Sex data ####

  if (split.by.sex) {
    ### Split by sex case ####

    if ("sex" %in% colnames(dt) && sex != "sex") {
      dt <- dt %>% dplyr::select(-sex)
    }

    dt <- dt %>%
      dplyr::rename("sex" = !!sex) %>%
      dplyr::filter(!is.na(sex)) %>%
      dplyr::select(length, sex, maturity, "cluster")

    if (inherits(dt$sex, "numeric")) {
      dt$sex <- as.character(dt$sex)
    }

    ## More checks
    # if(!inherits(female.sex, class(dt$sex))) stop("female.sex (or male.sex) is not the same class as dt[[sex]].")
  } else {
    ### No sex split ####
    dt <- dt %>%
      dplyr::mutate(sex = "both") %>%
      dplyr::select(length, sex, maturity, "cluster")
  }

  ## Force zero group length ####

  if (!is.na(force.zero.group.length)) {
    if (split.by.sex) {
      dt <- dt %>%
        tibble::add_column(type = "data") %>%
        dplyr::bind_rows(
          tibble::tibble(
            length = if (!is.na(force.zero.group.strength)) {
              rnorm(
                sum(dt$sex == female.sex) * (force.zero.group.strength / 100),
                force.zero.group.length,
                force.zero.group.length * force.zero.group.cv
              )
            } else {
              rnorm(
                force.zero.group.n[[female.sex]],
                force.zero.group.length,
                force.zero.group.length * force.zero.group.cv
              )
            },
            sex = female.sex,
            maturity = 0,
            type = "made"
          ),
          tibble::tibble(
            length = if (!is.na(force.zero.group.strength)) {
              rnorm(
                sum(dt$sex == male.sex) * (force.zero.group.strength / 100),
                force.zero.group.length,
                force.zero.group.length * force.zero.group.cv
              )
            } else {
              rnorm(
                force.zero.group.n[[male.sex]],
                force.zero.group.length,
                force.zero.group.length * force.zero.group.cv
              )
            },
            sex = male.sex,
            maturity = 0,
            type = "made"
          )
        )
    } else {
      dt <- dt %>%
        tibble::add_column(type = "data") %>%
        dplyr::bind_rows(
          tibble::tibble(
            length = if (!is.na(force.zero.group.strength)) {
              rnorm(
                sum(dt$sex == "both") * (force.zero.group.strength / 100),
                force.zero.group.length,
                force.zero.group.length * force.zero.group.cv
              )
            } else {
              rnorm(
                force.zero.group.n,
                force.zero.group.length,
                force.zero.group.length * force.zero.group.cv
              )
            },
            sex = "both",
            maturity = 0,
            type = "made"
          )
        )
    }
  } else {
    dt <- dt %>%
      tibble::add_column(type = "data")
  }

  ## Length bin width ####

  if (!is.null(length.bin.width)) {
    mat.pr.dt <- dt %>%
      dplyr::mutate(
        bin = ggplot2::cut_interval(x = length, length = length.bin.width)
      ) %>%
      dplyr::group_by(sex, bin) %>%
      dplyr::summarise(
        mat.pr = sum(maturity == 1) / (length(maturity)),
        .groups = "keep"
      ) %>%
      dplyr::mutate(
        bin1 = as.numeric(gsub(
          "\\D",
          "",
          sapply(strsplit(as.character(bin), "\\,"), "[", 1)
        )),
        bin2 = as.numeric(gsub(
          "\\D",
          "",
          sapply(strsplit(as.character(bin), "\\,"), "[", 2)
        ))
      )
  }

  ## 0.5 maturity modelling ####

  modDat <- lapply(unique(dt$sex), function(k) {
    tmp <- dt %>%
      dplyr::filter(sex == k) %>%
      dplyr::group_by(maturity) %>%
      dplyr::summarise(mean = mean(length), .groups = "keep")

    if (!all(c(0L, 1L) %in% tmp$maturity)) {
      warning(
        "Sex group '", k, "' has only one maturity class in the data. ",
        "Unable to calculate L50 reliably."
      )
      return(
        tibble::tibble(mean = NA, ci.min = NA, ci.max = NA, sex = k) %>%
          dplyr::mutate(dplyr::across(c("mean", "ci.min", "ci.max"), as.numeric))
      )
    }

    if (tmp[tmp$maturity == 0, ]$mean > tmp[tmp$maturity == 1, ]$mean) {
      warning(
        "Mean size of ",
        k,
        " immature fish larger than mature fish. Unable to calculate L50 reliably"
      )
      return({
        tibble::tibble(mean = NA, ci.min = NA, ci.max = NA, sex = k) %>%
          dplyr::mutate(dplyr::across(
            c("mean", "ci.min", "ci.max"),
            as.numeric
          ))
      })
    } else {
      if (!is.na(bootstrap.n)) {
        ### Bootstrap option ####

        dk <- dt %>% dplyr::filter(.data$sex == k)
        n.data <- sum(dk$type == "data")

        if (!is.null(bootstrap.cluster)) {
          dk.data <- dk %>% dplyr::filter(.data$type == "data")
          dk.made <- dk %>% dplyr::filter(.data$type != "data")
          clusters <- split(seq_len(nrow(dk.data)), dk.data$cluster)

          if (length(clusters) < 10) {
            warning(
              "Only ", length(clusters), " clusters (", bootstrap.cluster,
              ") for ", k, ". The cluster bootstrap interval is unreliable."
            )
          }
        }

        dat <- lapply(1:bootstrap.n, function(i) {
          boot.dt <- if (is.null(bootstrap.cluster)) {
            dplyr::sample_n(dk, size = n.data, replace = TRUE)
          } else {
            idx <- unlist(
              sample(clusters, length(clusters), replace = TRUE),
              use.names = FALSE
            )
            dplyr::bind_rows(dk.data[idx, ], dk.made)
          }

          mod <- suppressWarnings(glm(
            maturity ~ length,
            data = boot.dt,
            family = binomial(link = "logit")
          ))

          dat <- unlogit(0.5, mod)
          dat$sex <- k
          dat %>% dplyr::mutate(rep = i, .before = 1)
        }) %>%
          dplyr::bind_rows() %>%
          dplyr::summarise(
            Mean = mean(mean, na.rm = TRUE),
            ci.min = quantile(mean, 0.025, na.rm = TRUE),
            ci.max = quantile(mean, 0.975, na.rm = TRUE)
          ) %>%
          dplyr::rename("mean" = "Mean")

        mod <- glm(
          maturity ~ length,
          data = dt %>% dplyr::filter(sex == k),
          family = binomial(link = "logit")
        )

        if (broom::tidy(mod)$p.value[2] > 0.05) {
          warning(
            "The length term in the ",
            k,
            " L50 logistic model is non-siginificant. This indicates problems with the underlying data."
          )
        }

        dat$sex <- k
        dat$intercept <- coef(mod)[1]
        dat$slope <- coef(mod)[2]

        return(dat)
      } else {
        ### Standard option ####

        mod <- glm(
          maturity ~ length,
          data = dt %>% dplyr::filter(sex == k),
          family = binomial(link = "logit")
        )

        if (broom::tidy(mod)$p.value[2] > 0.05) {
          warning(
            "The length term in the ",
            k,
            " L50 logistic model is non-siginificant. This indicates problems with the underlying data."
          )
        }

        dat <- unlogit(0.5, mod)
        dat$sex <- k
        dat$intercept <- coef(mod)[1]
        dat$slope <- coef(mod)[2]

        return(dat)
      }
    }
  }) %>%
    dplyr::bind_rows()

  modDat <- dplyr::left_join(
    modDat,
    dt %>% dplyr::group_by(sex) %>% dplyr::count(),
    by = "sex"
  )

  ###########
  # Plot ####

  if (split.by.sex) {
    p <-
      ggplot() +
      #facet_wrap(~sex, ncol = 1) +
      {
        if (!is.null(length.bin.width)) {
          geom_step(
            data = mat.pr.dt,
            aes(x = bin1, y = mat.pr, color = sex, group = sex),
            alpha = 0.5
          )
        }
      } +
      ggridges::geom_density_ridges(
        data = dt,
        aes(x = length, y = maturity, group = paste(sex, maturity), fill = sex),
        scale = 0.3,
        linewidth = 0.5 / 2.13,
        alpha = 0.5,
        ...
      ) +
      geom_segment(
        data = modDat,
        aes(x = mean, xend = mean, y = 0, yend = 0.5, color = sex),
        linetype = 3,
        linewidth = 0.7 / 2.13
      ) +
      geom_segment(
        data = modDat,
        aes(x = -Inf, xend = mean, y = 0.5, yend = 0.5, color = sex),
        linetype = 3,
        linewidth = 0.7 / 2.13
      ) +
      geom_errorbar(
        data = modDat,
        aes(xmin = ci.min, xmax = ci.max, y = 0.5, color = sex),
        orientation = "y",
        width = 0.1
      ) +
      ggrepel::geom_text_repel(
        data = modDat,
        aes(
          x = mean,
          y = -0.07,
          label = paste0(round(mean, 1), " ", length.unit, "\n(n = ", n, ")"),
          color = sex
        ),
        size = 0.8 * base_size / 2.845276,
        direction = "x",
        min.segment.length = 100,
        force = 4
      ) +
      stat_smooth(
        data = dt,
        aes(x = length, y = maturity, color = sex, group = sex),
        method = "glm",
        formula = y ~ x,
        method.args = list(family = "binomial"),
        linewidth = 1 / 2.13
      ) +
      scale_x_continuous(
        paste0(xlab, " (", length.unit, ")"),
        expand = c(0, 0)
      ) +
      scale_y_continuous("Maturity", breaks = seq(0, 1, 0.2)) +
      coord_cartesian(xlim = c(0, ceiling(max(dt$length)))) +
      scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
      scale_fill_manual("Sex", values = c("#FF5F68", "#449BCF")) +
      guides(color = guide_legend(override.aes = list(fill = NA))) +
      theme_fishplots(base_size = base_size) +
      theme(
        legend.position = legend.position,
        text = element_text(size = base_size)
      )
  } else {
    p <- ggplot() +
      {
        if (!is.null(length.bin.width)) {
          geom_step(data = mat.pr.dt, aes(x = bin1, y = mat.pr), alpha = 0.5)
        }
      } +
      ggridges::geom_density_ridges(
        data = dt,
        aes(x = length, y = maturity, group = maturity),
        scale = 0.3,
        linewidth = 0.5 / 2.13,
        alpha = 0.5,
        ...
      ) +
      geom_segment(
        data = modDat,
        aes(x = mean, xend = mean, y = 0, yend = 0.5),
        linetype = 3,
        linewidth = 0.7 / 2.13
      ) +
      geom_segment(
        data = modDat,
        aes(x = -Inf, xend = mean, y = 0.5, yend = 0.5),
        linetype = 3,
        linewidth = 0.7 / 2.13
      ) +
      geom_errorbar(
        data = modDat,
        aes(xmin = ci.min, xmax = ci.max, y = 0.5),
        orientation = "y",
        width = 0.1
      ) +
      geom_text(
        data = modDat,
        aes(
          x = mean,
          y = -0.03,
          label = paste0(round(mean, 1), " ", length.unit, " (n = ", n, ")")
        ),
        size = 0.8 * base_size / 2.845276
      ) +
      stat_smooth(
        data = dt,
        aes(x = length, y = maturity),
        method = "glm",
        formula = y ~ x,
        method.args = list(family = "binomial"),
        linewidth = 1 / 2.13
      ) +
      scale_x_continuous(
        paste0(xlab, " (", length.unit, ")"),
        expand = c(0, 0)
      ) +
      scale_y_continuous("Maturity", breaks = seq(0, 1, 0.2)) +
      coord_cartesian(xlim = c(0, ceiling(max(dt$length)))) +
      guides(color = guide_legend(override.aes = list(fill = NA))) +
      theme_fishplots(base_size = base_size) +
      theme(
        legend.position = legend.position,
        text = element_text(size = base_size)
      )
  }

  ############
  ## Text ####

  ci.text <- if (is.na(bootstrap.n)) {
    ".\n Confidence intervals estimated from the glm object using the delta method."
  } else {
    paste0(
      "\n\n Confidence intervals estimated using ",
      bootstrap.n,
      " bootstrap replicates",
      if (!is.null(bootstrap.cluster)) {
        paste0(" resampling ", bootstrap.cluster, " clusters")
      },
      "."
    )
  }

  if (split.by.sex) {
    Text <-
      paste0(
        "50% maturity at ",
        ifelse(
          length.unit %in%
            c("mm", "cm", "m", "meter", "metre", "meters", "in", "inches"),
          "length (L50)",
          ifelse(
            length.unit %in% c("year", "years"),
            "age (A50)",
            "length or age (L50 or A50)"
          )
        ),
        " based on logit regressions:",
        "\n\n Females: ",
        round(modDat[modDat$sex == female.sex, "mean"], 3),
        " ",
        length.unit,
        ". 95% confidence intervals: ",
        round(modDat[modDat$sex == female.sex, "ci.min"], 3),
        " - ",
        round(modDat[modDat$sex == female.sex, "ci.max"], 3),
        "\n Number of specimens: ",
        modDat[modDat$sex == female.sex, "n"],
        "\n\n Males: ",
        round(modDat[modDat$sex == male.sex, "mean"], 3),
        " ",
        length.unit,
        ". 95% confidence intervals: ",
        round(modDat[modDat$sex == male.sex, "ci.min"], 3),
        " - ",
        round(modDat[modDat$sex == male.sex, "ci.max"], 3),
        "\n Number of specimens: ",
        modDat[modDat$sex == male.sex, "n"],
        ci.text
      )
  } else {
    Text <-
      paste0(
        "50% maturity at ",
        ifelse(
          length.unit %in%
            c("mm", "cm", "m", "meter", "metre", "meters", "in", "inches"),
          "length (L50)",
          ifelse(
            length.unit %in% c("year", "years"),
            "age (A50)",
            "length or age (L50 or A50)"
          )
        ),
        " based on logit regressions:",
        "\n",
        round(modDat$mean, 3),
        " ",
        length.unit,
        ". 95% confidence intervals: ",
        round(modDat$ci.min, 3),
        " - ",
        round(modDat$ci.max, 3),
        "\n  Number of specimens: ",
        modDat$n,
        ci.text
      )
  }

  ##############
  ## Return ####

  return(list(
    plot = suppressWarnings(p),
    text = Text,
    params = modDat
  ))
}
