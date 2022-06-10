#' @title Plot age-length relationships and growth curves
#' @param length Character argument giving the name of the length column in \code{dt}
#' @param age Character argument giving the name of the age column in \code{dt}
#' @param growth.model Integer defining the growth model. 1 = von Bertalanffy, 2 = Gompertz, 3 = Logistic.
#' @param force.zero.group.length Numeric indicating the length to which 0-group should be forced. Use \code{NA} ignore the forcing.
#' @param force.zero.group.cv Numeric indicating the coefficient of variation for the forced 0-group length. Resulting lengths will be randomly generated from a normal distribution.
#' @param force.zero.group.strength Numeric indicating how many percent of total fish should be added to the specified \code{force.zero.group.length}.
#' @inheritParams plot_maturity
#' @param boxplot Logical indicating whether boxplots (\code{TRUE}) should be used to show data over points (\code{FALSE})
#' @param show.Linf Logical indicating whether Linf values should be shown as dashed vertical lines.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{ggtheme}.
#' @details Uses the \code{fishmethods::growth} function to calculate the growth curves. Zero group length can be forced to the growth functions using the \code{force.zero.group.*} parameters.
#' @return A list containing the \code{plot}, \code{text} for Rmarkdown and Shiny applications, and estimated parameters (\code{params}).
#' @author Mikko Vihtakari // Institute of Marine Research.
#' @import dplyr ggplot2
#' @importFrom fishmethods growth
#' @importFrom stats rnorm
#' @examples
#' # Simple plot. Note that a list is returned.
#' data(survey_ghl)
#' plot_growth(survey_ghl, length = "length", age = "age")
#' # Split by sex
#' plot_growth(survey_ghl, split.by.sex = TRUE)$plot
#' # Data as points. Forcing zero group to 10 cm
#' plot_growth(survey_ghl, force.zero.group.length = 10, boxplot = FALSE)$plot
#' @export

# Debug parameters:
# dt = survey_ghl; length = "length"; age = "age"; sex = "sex"; female.sex = "F"; male.sex = "M"; length.unit = "cm"; split.by.sex = FALSE; growth.model = 1; force.zero.group.length = NA; force.zero.group.strength = 10; force.zero.group.cv = 0; show.Linf = TRUE; boxplot = FALSE; base_size = 8; legend.position = "bottom"
# dt = x; length = "Length"; age = "Age"; sex = "Sex"; female.sex = "F"; male.sex = "M"; length.unit = "cm"; filter.exp = NULL; split.by.sex = FALSE; growth.model = 1; force.zero.group.length = NA; force.zero.group.strength = 10
plot_growth <- function(dt, length = "length", age = "age", sex = "sex", female.sex = "F", male.sex = "M", length.unit = "cm", split.by.sex = FALSE, growth.model = 1, force.zero.group.length = NA, force.zero.group.strength = 10, force.zero.group.cv = 0, show.Linf = TRUE, boxplot = TRUE, base_size = 8, legend.position = "bottom") {

  # Growth model

  if(!growth.model %in% 1:3) stop("growth.model has to be an integer between 1 and 3")

  modName <- c("von Bertalanffy" = "vout", "Gompertz" = "gout", "Logistic" = "lout")
  mod.name <- names(modName[growth.model])
  growth.model <- unname(modName[growth.model])

  # Add row number

  dt$id <- rownames(dt)

  # Fix sex column

  if(split.by.sex) {
    if(is.null(sex)) stop("Sex column has to be specified when split.by.sex = TRUE")
    if(!all(c(female.sex, male.sex) %in% unique(dt[[sex]]))) stop(female.sex, " or ", male.sex, " not found from the ", sex,
                                                                  " column. Check the female.sex and male.sex parameters.")
    if(dt %>% dplyr::pull(!!rlang::enquo(sex)) %>% na.omit() %>% length() < 10) stop("Either invalid sex column or not enough sex data")

    orig.nrow <- nrow(dt)

    dt <- dt %>%
      dplyr::rename("sex" = tidyselect::all_of(sex)) %>%
      dplyr::filter(!is.na(sex))

    sex.missing <- orig.nrow - nrow(dt)
  }

  # Filter

  if(!exists("orig.nrow")) orig.nrow <- nrow(dt)

  dt <- dt %>%
    dplyr::rename("age" = tidyselect::all_of(age),
                  "length" = tidyselect::all_of(length)
    )

  length.missing <- sum(is.na(dt$length))
  age.missing <- sum(is.na(dt$age))

  dt <- dt %>% dplyr::filter(!is.na(age) & !is.na(length))

  # Select columns and add zero group if requested

  if(split.by.sex){

    dt <- dt %>% dplyr::select(id, sex, age, length)

    if(!is.na(force.zero.group.length)) {

      dt <- dt %>%
        tibble::add_column(type = "data") %>%
        dplyr::bind_rows(
          tibble::tibble(
            id = NA,
            sex = female.sex,
            age = 0,
            length = rnorm(sum(dt$sex == female.sex)*(force.zero.group.strength/100),
                           force.zero.group.length,
                           force.zero.group.length*force.zero.group.cv),
            type = "made"
          ),
          tibble::tibble(
            id = NA,
            sex = male.sex,
            age = 0,
            length = rnorm(sum(dt$sex == male.sex)*(force.zero.group.strength/100),
                           force.zero.group.length,
                           force.zero.group.length*force.zero.group.cv),
            type = "made"
          )
        )
    }
  } else {

    dt <- dt %>% dplyr::select(id, age, length)

    if(!is.na(force.zero.group.length)) {
      dt <-  dt %>%
        tibble::add_column(type = "data") %>%
        dplyr::bind_rows(
          tibble::tibble(
            id = NA,
            age = 0,
            length = rnorm(nrow(dt)*(force.zero.group.strength/100),
                           force.zero.group.length,
                           force.zero.group.length*force.zero.group.cv),
            type = "made"
          )
        )
    }
  }

  ################
  ## The Plot ####

  # Plot sexed data

  if(split.by.sex) {

    laModF <- fishmethods::growth(
      age = dt %>% dplyr::filter(sex == female.sex) %>% dplyr::pull(age),
      size = dt %>% dplyr::filter(sex == female.sex) %>% dplyr::pull(length),
      Sinf = dt %>% dplyr::filter(sex == female.sex) %>% dplyr::pull(length) %>% max,
      K = 0.1, t0 = 0, graph = FALSE)

    laModM <- fishmethods::growth(
      age = dt %>% dplyr::filter(sex == male.sex) %>% dplyr::pull(age),
      size = dt %>% dplyr::filter(sex == male.sex) %>% dplyr::pull(length),
      Sinf = dt %>% dplyr::filter(sex == male.sex) %>% dplyr::pull(length) %>% max,
      K = 0.1, t0 = 0, graph = FALSE)

    FfitFailed <- all(eval(parse(text = paste0("laModF$", growth.model))) == "Fit failed")
    MfitFailed <- all(eval(parse(text = paste0("laModM$", growth.model))) == "Fit failed")

    if(FfitFailed & MfitFailed) {

      Plot <- ggplot() +
        {if(boxplot) geom_boxplot(data = dt, aes(x = age, y = length, color = sex, group = interaction(age, sex)), alpha = 0.5, outlier.size = 0.5)} +
        {if(!boxplot) geom_point(data = dt, aes(x = age, y = length, color = sex, text = paste0("row number: ", id)), alpha = 0.5, shape = 21)} +
        annotation_custom(
          grid::textGrob("Failed to fit growth models\nConsider adding force.zero.group.length\nand/or not splitting by sex.",
                         gp = grid::gpar(fontsize = 8, fontface = "bold")),
          xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
        ylab(paste0("Total length (", length.unit, ")")) +
        xlab("Age (years)") +
        coord_cartesian(expand = FALSE, clip = "off", xlim = c(0,NA), ylim = c(0,NA)) +
        theme_fishplots(base_size = base_size) +
        theme(legend.position = legend.position,
              text = element_text(size = base_size))

      Text <- paste0(
        "Failed to fit growth models:",
        "\n Number of included specimens = ", sum(dt$sex == female.sex), " females and ", sum(dt$sex == male.sex), " males"
      )

    } else {

      # Females
      if(FfitFailed) {
        laModFpred <- data.frame(age = 0:max(dt$age), length = NA)
        laModparsF <- data.frame(sex = female.sex, term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA, conf.low = NA, conf.high = NA)
      } else {
        tmpF <- predict(eval(parse(text = paste0("laModF$", growth.model))), newdata = data.frame(age = 0:max(dt$age)))
        laModFpred <- data.frame(age = 0:max(dt$age), length = tmpF)

        tryshit <- try(broom::tidy(eval(parse(text = paste0("laModF$", growth.model))), conf.int = TRUE))

        if(any(class(tryshit) == "try-error")) {
          laModparsF <- dplyr::bind_cols(sex = female.sex, broom::tidy(eval(parse(text = paste0("laModF$", growth.model))), conf.int = FALSE))
        } else {
          laModparsF <- dplyr::bind_cols(sex = female.sex, tryshit)
        }
      }

      # Males
      if(MfitFailed) {
        laModMpred <- data.frame(age = 0:max(dt$age), length = NA)
        laModparsM <- data.frame(sex = male.sex, term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA, conf.low = NA, conf.high = NA)
      } else {
        tmpM <- predict(eval(parse(text = paste0("laModM$", growth.model))), newdata = data.frame(age = 0:max(dt$age)))
        laModMpred <- data.frame(age = 0:max(dt$age), length = tmpM)

        tryshit <- try(broom::tidy(eval(parse(text = paste0("laModM$", growth.model))), conf.int = TRUE))

        if(any(class(tryshit) == "try-error")) {
          laModparsM <- dplyr::bind_cols(sex = male.sex, broom::tidy(eval(parse(text = paste0("laModM$", growth.model))), conf.int = FALSE))
        } else {
          laModparsM <- dplyr::bind_cols(sex = male.sex, tryshit)
        }
      }

      laModpars <- dplyr::bind_rows(laModparsF, laModparsM)

      ## Plot

      Plot <-
        suppressWarnings({
          ggplot() +
            {if(boxplot) geom_boxplot(data = dt, aes(x = age, y = length, color = sex, group = interaction(age, sex)), alpha = 0.5, outlier.size = 0.5)} +
            {if(!boxplot) geom_point(data = dt, aes(x = age, y = length, color = sex, text = paste0("row number: ", id)), alpha = 0.5, shape = 21)} +
            {if(show.Linf & !FfitFailed) geom_hline(yintercept = laModparsF$estimate[1], linetype = 2, color = "#FF5F68", alpha = 0.5)} +
            {if(show.Linf & !MfitFailed) geom_hline(yintercept = laModparsM$estimate[1], linetype = 2, color = "#449BCF", alpha = 0.5)} +
            {if(!FfitFailed) geom_path(data = laModFpred, aes(x = age, y = length), color = "#FF5F68", size = 2/2.13)} +
            {if(FfitFailed) annotate("text", x = -Inf, y = Inf, label = "Fit failed for females", color = "#FF5F68", size = base_size/2.85, vjust = 3, hjust = -0.5)} +
            {if(!MfitFailed) geom_path(data = laModMpred, aes(x = age, y = length), color = "#449BCF", size = 2/2.13)} +
            {if(MfitFailed) annotate("text", x = -Inf, y = Inf, label = "Fit failed for males", color = "#449BCF", size = base_size/2.85, vjust = 2, hjust = -0.5)} +
            expand_limits(x = c(0, round_any(max(dt$age), 2, ceiling)), y = c(0, round_any(max(dt$length), 5, ceiling))) +
            # scale_x_continuous(breaks = seq(0,100,2)) +
            # scale_y_continuous(breaks = seq(0,200,5)) +
            scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
            labs(y = paste0("Total length (", length.unit, ")"),  x = "Age (years)") +
            coord_cartesian(expand = FALSE, clip = "off") +
            theme_fishplots(base_size = base_size) +
            theme(legend.position = legend.position,
                  text = element_text(size = base_size))
        })

      Text <- paste0(
        mod.name, " growth function coefficients for females and males, respectively:  \n Linf (asymptotic average length) = ",
        round(laModparsF$estimate[1], 1), " ", length.unit, " +/- ",
        if("conf.low" %in% names(laModparsF)) {paste0(round(laModparsF$conf.low[1], 1), " - ", round(laModparsF$conf.high[1], 1), " (95% CIs) and ")} else {paste0("no CIs and ")},
        round(laModparsM$estimate[1], 1), " ", length.unit, " +/- ",
        if("conf.low" %in% names(laModparsM)) {paste0(round(laModparsM$conf.low[1], 1), " - ", round(laModparsM$conf.high[1], 1), " (95% CIs)")} else {paste0("no CIs")},
        "  \n K (growth rate coefficient) = ",
        round(laModparsF$estimate[2], 4), " ", length.unit, " +/- ",
        if("conf.low" %in% names(laModparsF)) {paste0(round(laModparsF$conf.low[2], 3), " - ", round(laModparsF$conf.high[2], 3), " (95% CIs) and ")} else {paste0("no CIs and ")},
        round(laModparsM$estimate[2], 4), " ", length.unit, " +/- ",
        if("conf.low" %in% names(laModparsM)) {paste0(round(laModparsM$conf.low[2], 3), " - ", round(laModparsM$conf.high[2], 3), " (95% CIs)")} else {paste0("no CIs")},
        "  \n t0 (age at length 0) = ",
        round(laModparsF$estimate[3], 2), " (years) +/- ",
        if("conf.low" %in% names(laModparsF)) {paste0(round(laModparsF$conf.low[3], 1), " - ", round(laModparsF$conf.high[3], 1), " (95% CIs) and ")} else {paste0("no CIs and ")},
        round(laModparsM$estimate[3], 2), " (years) +/- ",
        if("conf.low" %in% names(laModparsM)) {paste0(round(laModparsM$conf.low[3], 3), " - ", round(laModparsM$conf.high[3], 3), " (95% CIs)")} else {paste0("no CIs")},
        "  \n tmax (life span; t0 + 3/K) = ", round(laModparsF$estimate[3] + 3 / laModparsF$estimate[2], 1), " and ", round(laModparsM$estimate[3] + 3 / laModparsM$estimate[2], 1), " years",
        "  \n Number of included specimens = ", nrow(dt[dt$sex == female.sex,]), " and ", nrow(dt[dt$sex == male.sex,]),
        "  \n Total number of measured = ", orig.nrow,
        "  \n Excluded (length or age missing): \n Length = ", length.missing, "; age = ", age.missing, "; sex = ", sex.missing
      )

    }

  } else {
    # Plot non-sex split data

    laMod <- fishmethods::growth(age = dt$age, size = dt$length, Sinf = max(dt$length), K = 0.1, t0 = 0, graph = FALSE)

    if(all(eval(parse(text = paste0("laMod$", growth.model))) == "Fit failed")) {

      #if(eval(parse(text = paste0("laMod$", growthModelSwitch))) == "Fit failed") {

      Plot <- ggplot(dt, aes(x = age, y = length)) +
        {if(boxplot) geom_boxplot(data = dt, aes(x = age, y = length, group = age), outlier.size = 0.5, alpha = 0.5)} +
        {if(!boxplot) geom_point(data = dt, aes(x = age, y = length, text = paste0("row number: ", id)), shape = 21, alpha = 0.5)} +
        annotation_custom(
          grid::textGrob("Failed to fit a growth model\nConsider adding force.zero.group.length",
                         gp = grid::gpar(fontsize = 8, fontface = "bold", col = "red")),
          xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        ylab(paste0("Total length (", length.unit, ")")) +
        xlab("Age (years)") +
        coord_cartesian(expand = FALSE, clip = "off", xlim = c(0,NA), ylim = c(0,NA)) +
        theme_fishplots(base_size = base_size) +
        theme(legend.position = legend.position,
              text = element_text(size = base_size))

      Text <- paste0(
        "Failed to fit a growth model:",
        "  \n Number of included specimens = ", nrow(dt)
      )

    } else {

      laModpred <- data.frame(age = 0:max(dt$age), length = predict(eval(parse(text = paste0("laMod$", growth.model))), newdata = data.frame(age = 0:max(dt$age))))

      laModpars <- broom::tidy(eval(parse(text = paste0("laMod$", growth.model))), conf.int = TRUE)

      Plot <-
        suppressWarnings({
          ggplot() +
            {if(boxplot) geom_boxplot(data = dt, aes(x = age, y = length, group = age), outlier.size = 0.5, alpha = 0.5)} +
            {if(!boxplot) geom_point(data = dt, aes(x = age, y = length, text = paste0("row number: ", id)), shape = 21, alpha = 0.5)} +
            expand_limits(x = c(0, round_any(max(dt$age), 2, ceiling)), y = c(0, round_any(max(dt$length), 5, ceiling))) + # c(0, max(pretty(c(0, max(dt$length)))))
            {if(show.Linf) geom_hline(yintercept = laModpars$estimate[1], linetype = 2, color = "blue", alpha = 0.5)} +
            # scale_x_continuous(breaks = seq(0,100,2)) +
            # scale_y_continuous(breaks = seq(0,200,5)) +
            geom_path(data = laModpred, aes(x = age, y = length), color = "blue") +
            labs(y = paste0("Total length (", length.unit, ")"),  x = "Age (years)") +
            coord_cartesian(expand = FALSE, clip = "off") +
            theme_fishplots(base_size = base_size) +
            theme(legend.position = legend.position,
                  text = element_text(size = base_size))
        })

      Text <- paste0(
        mod.name, " growth function coefficients:  \n Linf (asymptotic average length) = ",
        round(laModpars$estimate[1], 1), " ", length.unit, " +/- ", round(laModpars$conf.low[1], 1), " - ", round(laModpars$conf.high[1], 1), " (95% CIs)",
        "  \n K (growth rate coefficient) = ",
        round(laModpars$estimate[2], 4), " +/- ", round(laModpars$conf.low[2], 3), " - ", round(laModpars$conf.high[2], 3), " (95% CIs)",
        "  \n t0 (age at length 0) = ",
        round(laModpars$estimate[3], 2), " (years) +/- ", round(laModpars$conf.low[3], 1), " - ", round(laModpars$conf.high[3], 1), " (95% CIs)",
        "  \n tmax (life span; t0 + 3/K) = ", round(laModpars$estimate[3] + 3 / laModpars$estimate[2], 1), " years",
        "  \n Number of included specimens = ", nrow(dt),
        "  \n Total number of measured = ", orig.nrow,
        "  \n Excluded (length or age missing):  \n Length = ", length.missing, "; age = ", age.missing
      )

    }

  }

  ## Return

  return(list(plot = Plot, text = Text, params = if(exists("laModpars")) {laModpars} else {NULL}))
}
