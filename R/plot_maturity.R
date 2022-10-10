#' @title Plot maturity ogive
#' @description Plots an estimate of length or age at 50\% mature for a dataset
#' @param dt A data.frame, tibble or data.table
#' @param length Character argument giving the name of the length (or age) column in \code{dt}
#' @param maturity Character argument giving the name of the maturity column in \code{dt}. Should be either logical (\code{TRUE == mature, FALSE == immature}) or integer (\code{1 == mature, 0 == immature}).
#' @param sex Character argument giving the name of the sex column in \code{dt}. Ignored if \code{split.by.sex == FALSE}.
#' @param length.unit A character argument giving the unit of \code{length}. Will be used in the labels of the figure.
#' @param length.bin.width Numeric specifying the increment (delta length) by which length data should be binned to calculate maturity proportions. Use \code{NULL} to remove from the plot.
#' @param split.by.sex Logical indicating whether the result should be split by sex.
#' @param female.sex A character denoting female sex in the \code{sex} column of \code{dt}
#' @param male.sex A character denoting male sex in the \code{sex} column of \code{dt}
#' @param force.zero.group.length Numeric indicating the length to which 0-group (all immatures) should be forced. Use \code{NA} ignore the forcing.
#' @param force.zero.group.cv Numeric indicating the coefficient of variation for the forced 0-group (all immature) length. Resulting lengths will be randomly generated from a normal distribution.
#' @param force.zero.group.strength Numeric indicating how many percent of total fish should be added to the specified \code{force.zero.group.length}.
#' @param xlab Character giving the x-axis label without unit
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{ggtheme}.
#' @param legend.position Position of the ggplot legend as a character. See \link[ggplot2]{ggtheme}.
#' @param ... Additional arguments passed to \link[ggridges]{geom_density_ridges}.
#' @return Returns a ggplot2 or tibble depending on the \code{plot} argument showing the maturity ogives.
#' @details Depends on the tidyverse and ggridges packages. The dplyr and ggplot2 packages must be loaded into the workspace.
#' @author Mikko Vihtakari // Institute of Marine Research.
#' @import dplyr ggplot2
#' @importFrom ggridges geom_density_ridges
#' @importFrom stats na.omit binomial coef glm predict rnorm
#' @examples
#' # Simple L50 plot
#' data(survey_ghl)
#' plot_maturity(survey_ghl, length = "length", maturity = "maturity")
#'
#' # A50 plot, split by sex
#' plot_maturity(survey_ghl, length = "age", length.unit = "years",
#' xlab = "Age", length.bin.width = 1, split.by.sex = TRUE)$plot
#' @export

# Debug parameters
# dt = survey_ghl; length = "length"; maturity = "maturity"; sex = "sex"; female.sex = "F"; male.sex = "M"; length.unit = "cm"; length.bin.width = 2; split.by.sex = T; filter.exp = NULL; xlab = "Total length"; plot = TRUE; base_size = 8
# dt = x; length = "Length"; maturity = "Mature"; sex = "Sex"; split.by.sex = F; female.sex = "F"; male.sex = "M"; length.unit = "cm"; length.bin.width = 2; force.zero.group.length = 0; force.zero.group.strength = 10; force.zero.group.cv = 0; xlab = "Total length";  base_size = 8; legend.position = "bottom"

plot_maturity <- function(dt, length = "length", maturity = "maturity", sex = "sex", split.by.sex = FALSE, female.sex = "F", male.sex = "M", length.unit = "cm", length.bin.width = 2, force.zero.group.length = NA, force.zero.group.strength = 10, force.zero.group.cv = 0, xlab = "Total length", base_size = 8, legend.position = "bottom", ...) {

  # Checks

  if(split.by.sex) {
    if(is.null(sex)) stop("Sex column has to be specified when split.by.sex = TRUE")
    if(!all(c(female.sex, male.sex) %in% unique(dt[[sex]]))) {
      stop(female.sex, " or ", male.sex, " not found from the ", sex,
           " column. Check the female.sex and male.sex parameters.")
    }
    if(dt %>% dplyr::pull(!!enquo(sex)) %>% na.omit() %>% length() < 10) {
      stop("Either invalid sex column or not enough sex data")
    }
  }

  if("length" %in% colnames(dt) && length != "length"){
    dt <- dt %>% dplyr::select(-length)
  }

  if("maturity" %in% colnames(dt) && maturity != "maturity"){
    dt <- dt %>% dplyr::select(-maturity)
  }

  # Select and rename columns

  dt <- dt %>%
    dplyr::rename("maturity" = tidyselect::all_of(maturity),
                  "length" = tidyselect::all_of(length)) %>%
    dplyr::filter(!is.na(maturity) &
                    !is.na(length)) %>%
    dplyr::mutate(maturity = as.integer(maturity))


  # Data manipulation when split by sex

  if(split.by.sex) {

    if("sex" %in% colnames(dt) && sex != "sex"){
      dt <- dt %>% dplyr::select(-sex)
    }

    dt <- dt %>%
      dplyr::rename("sex" = tidyselect::all_of(sex)) %>%
      dplyr::filter(!is.na(sex)) %>%
      dplyr::select(length, sex, maturity)

    ## More checks

    if(!inherits(female.sex, class(male.sex))) stop("female.sex and male.sex are different class.")
    if(!inherits(female.sex, class(dt$sex))) stop("female.sex (or male.sex) is not the same class as dt[[sex]].")

    if(!is.na(force.zero.group.length)) {
      dt <- dt %>%
        tibble::add_column(type = "data") %>%
        bind_rows(
          tibble::tibble(
            length = rnorm(sum(dt$sex == female.sex)*(force.zero.group.strength/100),
                           force.zero.group.length,
                           force.zero.group.length*force.zero.group.cv),
            sex = female.sex,
            maturity = 0,
            type = "made"
          ),
          tibble::tibble(
            length = rnorm(sum(dt$sex == male.sex)*(force.zero.group.strength/100),
                           force.zero.group.length,
                           force.zero.group.length*force.zero.group.cv),
            sex = male.sex,
            maturity = 0,
            type = "made"
          )
        )
    }

    if(!is.null(length.bin.width)) {
      mat.pr.dt <- dt %>%
        mutate(bin = ggplot2::cut_interval(x = length, length = length.bin.width)) %>%
        group_by(sex, bin) %>%
        summarise(mat.pr = sum(maturity == 1)/(length(maturity)), .groups = "keep") %>%
        mutate(
          bin1 =
            as.numeric(gsub("\\D", "",
                            sapply(strsplit(as.character(bin), "\\,"), "[", 1))),
          bin2 =
            as.numeric(gsub("\\D", "",
                            sapply(strsplit(as.character(bin), "\\,"), "[", 2)))
        )
    }

    tmp <- dt %>% group_by(sex, maturity) %>% summarise(mean = mean(length), .groups = "keep")

    if(tmp[tmp$sex == female.sex & tmp$maturity == 0,]$mean > tmp[tmp$sex == female.sex & tmp$maturity == 1, ]$mean) {

      warning("Mean size of female immature fish larger than mature fish. Unable to calculate L50 reliably")
      Fdat <- tibble(mean = NA, ci.min = NA, ci.max = NA, sex = female.sex) %>% mutate(across(c("mean", "ci.min", "ci.max"), as.numeric))

    } else {
      modF <- glm(maturity ~ length, data = dt %>% dplyr::filter(sex == female.sex),
                  family = binomial(link = "logit"))

      if(broom::tidy(modF)$p.value[2] > 0.05) {
        warning("The length term in the female L50 logistic model is non-siginificant. This indicates problems with the underlying data.")
      }

      Fdat <- unlogit(0.5, modF)
      Fdat$sex <- female.sex
      Fdat$intercept <- coef(modF)[1]
      Fdat$slope <- coef(modF)[2]
    }

    if(tmp[tmp$sex == male.sex & tmp$maturity == 0, "mean"] > tmp[tmp$sex == male.sex & tmp$maturity == 1, "mean"]) {
      warning("Mean size of male immature fish larger than mature fish. Unable to calculate L50 reliably")
      Mdat <- tibble(mean = NA, ci.min = NA, ci.max = NA, sex = male.sex) %>% mutate(across(c("mean", "ci.min", "ci.max"), as.numeric))
    } else {
      modM <- glm(maturity ~ length, data = dt %>% dplyr::filter(sex == male.sex),
                  family = binomial(link = "logit"))

      Mdat <- unlogit(0.5, modM)
      Mdat$sex <- male.sex
      Mdat$intercept <- coef(modM)[1]
      Mdat$slope <- coef(modM)[2]
    }

    modDat <- dplyr::bind_rows(Fdat, Mdat)
    modDat <- dplyr::left_join(modDat, dt %>% dplyr::group_by(sex) %>% count, by = "sex")

  } else {

    # Data manipulation when not split by sex

    dt <- dt %>%
      dplyr::filter(!is.na(sex)) %>%
      dplyr::select(length, maturity)

    if(!is.na(force.zero.group.length)) {
      dt <- dt %>%
        tibble::add_column(type = "data") %>%
        bind_rows(
          tibble::tibble(
            length = rnorm(nrow(dt)*(force.zero.group.strength/100),
                           force.zero.group.length,
                           force.zero.group.length*force.zero.group.cv),
            maturity = 0,
            type = "made"
          )
        )
    }

    tmp <- dt %>% dplyr::group_by(maturity) %>% dplyr::summarise(mean = mean(length))

    if(!is.null(length.bin.width)) {
      mat.pr.dt <- dt %>%
        dplyr::mutate(bin = ggplot2::cut_interval(x = length, length = length.bin.width)) %>%
        dplyr::group_by(bin) %>%
        dplyr::summarise(mat.pr = sum(maturity == 1)/(length(maturity)), .groups = "keep") %>%
        dplyr::mutate(bin1 = as.numeric(gsub("\\D", "", sapply(strsplit(as.character(bin), "\\,"), "[", 1))),
                      bin2 = as.numeric(gsub("\\D", "", sapply(strsplit(as.character(bin), "\\,"), "[", 2))))
    }

    if(tmp[tmp$maturity == 0, "mean"] > tmp[tmp$maturity == 1, "mean"]) {
      warning("Mean size of immature fish larger than mature fish. Unable to calculate L50 reliably")
      modDat <- tibble::tibble(mean = NA, ci.min = NA, ci.max = NA, n = nrow(dt)) %>%
        dplyr::mutate_all(., as.numeric)
    } else {
      mod <- glm(maturity ~ length, data = dt,
                 family = binomial(link = "logit"))
      modDat <- unlogit(0.5, mod)
      modDat <- dplyr::bind_cols(modDat, n = nrow(dt))
    }
  }

  ############
  ## Plot ####

  if(split.by.sex) {
    p <-
      ggplot() +
      #facet_wrap(~sex, ncol = 1) +
      {if(!is.null(length.bin.width)) geom_step(data = mat.pr.dt, aes(x = bin1, y = mat.pr, color = sex), alpha = 0.5)} +
      ggridges::geom_density_ridges(
        data = dt,
        aes(x = length, y = maturity, group = paste(sex, maturity), fill = sex),
        scale = 0.3, size = 0.5/2.13, alpha = 0.5, ...) +
      geom_segment(data = modDat,
                   aes(x = mean, xend = mean, y = 0, yend = 0.5, color = sex),
                   linetype = 3, size = 0.7/2.13) +
      geom_segment(data = modDat,
                   aes(x = -Inf, xend = mean, y = 0.5, yend = 0.5, color = sex),
                   linetype = 3, size = 0.7/2.13) +
      geom_errorbarh(data = modDat,
                     aes(xmin = ci.min, xmax = ci.max, y = 0.5, color = sex),
                     height = 0.1) +
      geom_text(data = modDat,
                aes(x = mean*c(1.15,0.85),
                    y = -0.07, label =
                      paste0(round(mean, 1), " ", length.unit, "\n(n = ", n, ")"),
                    color = sex), size = base_size/2.845276) +
      stat_smooth(data = dt, aes(x = length, y = maturity, color = sex),
                  method = "glm", formula = y ~ x,
                  method.args = list(family = "binomial"), size = 1/2.13) +
      scale_x_continuous(paste0(xlab, " (", length.unit, ")"), expand = c(0,0)) +
      scale_y_continuous("Maturity", breaks = seq(0,1,0.2)) +
      coord_cartesian(xlim = c(0, ceiling(max(dt$length)))) +
      scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
      scale_fill_manual("Sex", values = c("#FF5F68", "#449BCF")) +
      # theme_bw(base_size = 8) +
      guides(color=guide_legend(override.aes=list(fill=NA))) +
      theme_fishplots(base_size = base_size) +
      theme(legend.position = legend.position,
            text = element_text(size = base_size))

  } else {
    p <- ggplot() +
      {if(!is.null(length.bin.width)) geom_step(data = mat.pr.dt, aes(x = bin1, y = mat.pr), alpha = 0.5)} +
      ggridges::geom_density_ridges(data = dt,
                                    aes(x = length, y = maturity, group = maturity),
                                    scale = 0.3, size = 0.5/2.13, alpha = 0.5, ...) +
      geom_segment(data = modDat,
                   aes(x = mean, xend = mean, y = 0, yend = 0.5), linetype = 3,
                   size = 0.7/2.13) +
      geom_segment(data = modDat,
                   aes(x = -Inf, xend = mean, y = 0.5, yend = 0.5), linetype = 3,
                   size = 0.7/2.13) +
      geom_errorbarh(data = modDat,
                     aes(xmin = ci.min, xmax = ci.max, y = 0.5), height = 0.1) +
      geom_text(data = modDat,
                aes(x = mean, y = -0.03, label =
                      paste0(round(mean, 1), " ", length.unit, " (n = ", n, ")")),
                size = base_size/2.845276) +
      stat_smooth(data = dt, aes(x = length, y = maturity),
                  method = "glm", formula = y ~ x,
                  method.args = list(family = "binomial"), size = 1/2.13) +
      scale_x_continuous(paste0(xlab, " (", length.unit, ")"), , expand = c(0,0)) +
      scale_y_continuous("Maturity", breaks = seq(0,1,0.2)) +
      coord_cartesian(xlim = c(0, ceiling(max(dt$length)))) +
      guides(color=guide_legend(override.aes=list(fill=NA))) +
      theme_fishplots(base_size = base_size) +
      theme(legend.position = legend.position,
            text = element_text(size = base_size))
  }

  ############
  ## Text ####

  if(split.by.sex) {
    Text <-
      paste0(
        "50% maturity at ",
        ifelse(length.unit %in% c("mm", "cm", "m", "meter", "metre", "meters", "in", "inches"), "length (L50)", ifelse(length.unit %in% c("year", "years"), "age (A50)", "length or age (L50 or A50)")),
        " based on logit regressions:",
        "\n\n Females: ", round(modDat[modDat$sex == female.sex, "mean"], 3), " ", length.unit, ". 95% confidence intervals: ", round(modDat[modDat$sex == female.sex, "ci.min"], 3), " - ", round(modDat[modDat$sex == female.sex, "ci.max"], 3),
        "\n Number of specimens: ", modDat[modDat$sex == female.sex, "n"],
        "\n\n Males: ", round(modDat[modDat$sex == male.sex, "mean"], 3), " ", length.unit, ". 95% confidence intervals: ", round(modDat[modDat$sex == male.sex, "ci.min"], 3), " - ", round(modDat[modDat$sex == male.sex, "ci.max"], 3),
        "\n Number of specimens: ", modDat[modDat$sex == male.sex, "n"]
      )

  } else {

    Text <-
      paste0(
        "50% maturity at ",
        ifelse(length.unit %in% c("mm", "cm", "m", "meter", "metre", "meters", "in", "inches"), "length (L50)", ifelse(length.unit %in% c("year", "years"), "age (A50)", "length or age (L50 or A50)")),
        " based on logit regressions:",
        "\n", round(modDat$mean, 3), " ", length.unit, ". 95% confidence intervals: ", round(modDat$ci.min, 3), " - ", round(modDat$ci.max, 3),
        "\n  Number of specimens: ", modDat$n
      )
  }


  ##############
  ## Return ####

  return(list(plot = suppressWarnings(p), text = Text, params = if(exists("modDat")) {modDat} else {NULL}))
}

