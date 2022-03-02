#' @title Plot length-weight relationships
#' @param length Character argument giving the name of the length column in \code{dt}
#' @param weight Character argument giving the name of the age column in \code{dt}
#' @param ylab Character giving the x-axis label without unit.
#' @param weight.unit Character argument giving the unit of \code{weight}. Will be used in the labels of the figure.
#' @param use.nls Logical indicating whether the parameters should be calculated using the nonlinear least squares (\code{nls; TRUE}) method over the log-log transformed linear model (\code{lm; FALSE}) method.
#' @param log.axes Logical indicating whether logarithmic axes should be used instead of cartesian ones.
#' @inheritParams plot_maturity
#' @return A ggplot together with the a and b parameters.
#' @author Mikko Vihtakari // Institute of Marine Research.
#' @import dplyr ggplot2
#' @importFrom stats lm nls
#' @examples
#' data(survey_ghl)
#'
#' # Simple plot
#' plot_lw(survey_ghl, length = "length", weight = "weight")
#'
#' # Split by sex
#' plot_lw(survey_ghl, split.by.sex = TRUE)$plot
#' @export

# Debug parameters:
# dt = survey_ghl; length = "length"; weight = "weight"; sex = "sex"; female.sex = "F"; male.sex = "M"; length.unit = "cm"; weight.unit = "kg"; split.by.sex = FALSE; filter.exp = NULL; xlab = "Total length"; ylab = "Weight"; base_size = 8; use.nls = TRUE; log.axes = FALSE
plot_lw <- function(dt, length = "length", weight = "weight", sex = "sex", female.sex = "F", male.sex = "M", length.unit = "cm", weight.unit = "kg", split.by.sex = FALSE, filter.exp = NULL, xlab = "Total length", ylab = "Weight", base_size = 8, use.nls = FALSE, log.axes = FALSE) {

  # Add row number ####

  dt$id <- rownames(dt)

  # Fix sex column

  if(split.by.sex) {
    if(is.null(sex)) stop("Sex column has to be specified when split.by.sex = TRUE")
    if(dt %>% dplyr::pull(!!rlang::enquo(sex)) %>% na.omit() %>% length() < 10) stop("Either invalid sex column or not enough sex data")

    orig.nrow <- nrow(dt)

    dt <- dt %>%
      dplyr::rename("sex" = tidyselect::all_of(sex)) %>%
      dplyr::filter(!is.na(sex))

    sex.missing <- orig.nrow - nrow(dt)
  }

  # Filter

  if(is.null(filter.exp)) {
    if(!exists("orig.nrow")) orig.nrow <- nrow(dt)

    dt <- dt %>%
      dplyr::rename("weight" = tidyselect::all_of(weight),
                    "length" = tidyselect::all_of(length)
      )

    length.missing <- sum(is.na(dt$length))
    weight.missing <- sum(is.na(dt$weight))

    dt <- dt %>% dplyr::filter(!is.na(weight) & !is.na(length))

  } else {
    if(!exists("orig.nrow")) orig.nrow <- nrow(dt)

    dt <- dt %>%
      dplyr::filter(!!rlang::parse_expr(filter.exp)) %>%
      dplyr::rename("weight" = tidyselect::all_of(weight),
                    "length" = tidyselect::all_of(length)
      )

    length.missing <- sum(is.na(dt$length))
    weight.missing <- sum(is.na(dt$weight))

    dt <- dt %>% dplyr::filter(!is.na(weight) & !is.na(length))

  }

  ## Select columns

  if(split.by.sex) {
    dt <- dt %>% dplyr::select(id, sex, weight, length)
  } else {
    dt <- dt %>% dplyr::select(id, weight, length)
  }

  # Calculate a and b ####

  if(use.nls) {

    if(split.by.sex) {
      lwModPars <-
        dt %>%
        dplyr::group_by(sex) %>%
        dplyr::do(mod = broom::tidy(nls(I(weight)~a*length^b,., start = list(a = 1e-6, b = 3)), conf.int = TRUE)) %>%
        tidyr::unnest(mod)
    } else {
      lwModPars <-
        dt %>%
        dplyr::do(mod = broom::tidy(nls(I(weight)~a*length^b,., start = list(a = 1e-6, b = 3)), conf.int = TRUE)) %>%
        tidyr::unnest(mod)
    }

  } else {

    if(split.by.sex) {
      lwModPars <-
        dt %>%
        dplyr::group_by(sex) %>%
        dplyr::do(mod = broom::tidy(lm(log(weight) ~ log(length), data = .), conf.int = TRUE)) %>%
        tidyr::unnest(mod) %>%
        dplyr::mutate(term = dplyr::recode(term, "(Intercept)" = "a", "log(length)" = "b"))

      lwModPars[lwModPars$term == "a", c("estimate", "conf.low", "conf.high")] <-  # Transform back to normal space
        exp(lwModPars[lwModPars$term == "a", c("estimate", "conf.low", "conf.high")])
    } else {
      lwModPars <-
        dt %>%
        dplyr::do(mod = broom::tidy(lm(log(weight) ~ log(length), data = .), conf.int = TRUE)) %>%
        tidyr::unnest(mod) %>%
        dplyr::mutate(term = dplyr::recode(term, "(Intercept)" = "a", "log(length)" = "b"))

      lwModPars[lwModPars$term == "a", c("estimate", "conf.low", "conf.high")] <-  # Transform back to normal space
        exp(lwModPars[lwModPars$term == "a", c("estimate", "conf.low", "conf.high")])
    }
  }

  # Plot

  if(split.by.sex) {

    tmp <- bind_rows(
      tibble(length = 0:max(dt[dt$sex == female.sex, "length"]),
             weight = (lwModPars %>% filter(sex == female.sex, term == "a") %>% pull(estimate))*length^
               (lwModPars %>% filter(sex == female.sex, term == "b") %>% pull(estimate)),
             weight.low = (lwModPars %>% filter(sex == female.sex, term == "a") %>% pull(conf.low))*length^
               (lwModPars %>% filter(sex == female.sex, term == "b") %>% pull(conf.low)),
             weight.high = (lwModPars %>% filter(sex == female.sex, term == "a") %>% pull(conf.high))*length^
               (lwModPars %>% filter(sex == female.sex, term == "b") %>% pull(conf.high)),
             sex = female.sex),
      tibble(length = 0:max(dt[dt$sex == male.sex, "length"]),
             weight = (lwModPars %>% filter(sex == male.sex, term == "a") %>% pull(estimate))*length^
               (lwModPars %>% filter(sex == male.sex, term == "b") %>% pull(estimate)),
             weight.low = (lwModPars %>% filter(sex == male.sex, term == "a") %>% pull(conf.low))*length^
               (lwModPars %>% filter(sex == male.sex, term == "b") %>% pull(conf.low)),
             weight.high = (lwModPars %>% filter(sex == male.sex, term == "a") %>% pull(conf.high))*length^
               (lwModPars %>% filter(sex == male.sex, term == "b") %>% pull(conf.high)),
             sex = male.sex)
    )

    Plot <- suppressWarnings({
      ggplot() +
        geom_point(data = dt, aes(x = length, y = weight, color = sex, text = paste0("row number: ", id)), shape = 21, alpha = 0.8, size = 0.5) +
        geom_line(data = tmp, aes(x = length, y = weight.low, color = sex), linetype = 2) +
        geom_line(data = tmp, aes(x = length, y = weight.high, color = sex), linetype = 2) +
        facet_wrap(~sex) +
        annotate("line", x = tmp[tmp$sex == female.sex,]$length, y = tmp[tmp$sex == female.sex,]$weight, color = "tomato4") +
        annotate("line", x = tmp[tmp$sex == male.sex,]$length, y = tmp[tmp$sex == male.sex,]$weight, color = "dodgerblue4") +
        scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
        scale_y_continuous(n.breaks = 8) +
        labs(x = paste0("Total length (", length.unit, ")"), y = paste0("Weight (", weight.unit, ")")) +
        coord_cartesian(expand = FALSE, clip = "off") +
        theme_fishplots(base_size = base_size) +
        theme(legend.position = "none",
              text = element_text(size = base_size))
    })

    Text <- paste0(
      if(use.nls) {paste0("Nonlinear least squares length-weight model")} else {paste0("Logarithm transformed linear length-weight model")},
      " for females and males, respectively:",
      "  \n a = ", lwModPars %>% filter(term == "a", sex == female.sex) %>% pull(estimate) %>% round(.,4),
      " +/- ", lwModPars %>% filter(term == "a", sex == female.sex) %>% pull(conf.low) %>% round(.,3), " - ",
      lwModPars %>% filter(term == "a", sex == female.sex) %>% pull(conf.high) %>% round(.,3), " (95% CIs) and ",
      lwModPars %>% filter(term == "a", sex == male.sex) %>% pull(estimate) %>% round(.,4),
      " +/- ", lwModPars %>% filter(term == "a", sex == male.sex) %>% pull(conf.low) %>% round(.,3), " - ",
      lwModPars %>% filter(term == "a", sex == male.sex) %>% pull(conf.high) %>% round(.,3), " (95% CIs).",

      "  \n b = ", lwModPars %>% filter(term == "b", sex == female.sex) %>% pull(estimate) %>% round(.,3),
      " +/- ", lwModPars %>% filter(term == "b", sex == female.sex) %>% pull(conf.low) %>% round(.,2), " - ",
      lwModPars %>% filter(term == "b", sex == female.sex) %>% pull(conf.high) %>% round(.,2), " (95% CIs) and ",
      lwModPars %>% filter(term == "b", sex == male.sex) %>% pull(estimate) %>% round(.,3),
      " +/- ", lwModPars %>% filter(term == "b", sex == male.sex) %>% pull(conf.low) %>% round(.,2), " - ",
      lwModPars %>% filter(term == "b", sex == male.sex) %>% pull(conf.high) %>% round(.,2), " (95% CIs).",

      "  \n Number of included specimens = ", nrow(dt[dt$sex == female.sex,]), " and ", nrow(dt[dt$sex == male.sex,]),
      "  \n Total number of measured = ", orig.nrow,
      "  \n Excluded (data missing): \n Length = ", length.missing, "; weight = ", weight.missing, "; sex = ", sex.missing
    )

  } else {

    tmp <- tibble(length = 0:max(dt$length),
                  weight = (lwModPars %>% filter(term == "a") %>% pull(estimate))*length^
                    (lwModPars %>% filter(term == "b") %>% pull(estimate)),
                  weight.low = (lwModPars %>% filter(term == "a") %>% pull(conf.low))*length^
                    (lwModPars %>% filter(term == "b") %>% pull(conf.low)),
                  weight.high = (lwModPars %>% filter(term == "a") %>% pull(conf.high))*length^
                    (lwModPars %>% filter(term == "b") %>% pull(conf.high))
    )

    Plot <- suppressWarnings({
      ggplot() +
        geom_point(data = dt, aes(x = length, y = weight, text = paste0("row number: ", id)), shape = 21, alpha = 0.8, size = 0.5) +
        geom_line(data = tmp, aes(x = length, y = weight.low), linetype = 2, color = "blue") +
        geom_line(data = tmp, aes(x = length, y = weight.high), linetype = 2, color = "blue") +
        annotate("line", x = tmp$length, y = tmp$weight, color = "blue") +
        scale_y_continuous(n.breaks = 8) +
        labs(x = paste0("Total length (", length.unit, ")"), y = paste0("Weight (", weight.unit, ")")) +
        coord_cartesian(expand = FALSE, clip = "off") +
        theme_fishplots(base_size = base_size) +
        theme(legend.position = "none",
              text = element_text(size = base_size))
    })

    Text <- paste0(
      if(use.nls) {paste0("Nonlinear least squares length-weight model")} else {paste0("Logarithm transformed linear length-weight model")},
      ". Not splitted by sex:",
      "  \n a = ", lwModPars %>% filter(term == "a") %>% pull(estimate) %>% round(.,4),
      " +/- ", lwModPars %>% filter(term == "a") %>% pull(conf.low) %>% round(.,3), " - ",
      lwModPars %>% filter(term == "a") %>% pull(conf.high) %>% round(.,3), " (95% CIs).",

      "  \n b = ", lwModPars %>% filter(term == "b") %>% pull(estimate) %>% round(.,3),
      " +/- ", lwModPars %>% filter(term == "b") %>% pull(conf.low) %>% round(.,2), " - ",
      lwModPars %>% filter(term == "b") %>% pull(conf.high) %>% round(.,2), " (95% CIs).",

      "  \n Number of included specimens = ", nrow(dt),
      "  \n Total number of measured = ", orig.nrow,
      "  \n Excluded (data missing): \n Length = ", length.missing, "; weight = ", weight.missing
    )

  }

  # Logarithmic axes

  if(log.axes) {
    Plot <- suppressMessages(suppressWarnings({
      Plot +
        scale_y_log10(n.breaks = 8) +
        scale_x_log10(n.breaks = 8)
    }))
  }

  ## Return

  return(list(plot = Plot, text = Text, params = if(exists("lwModPars")) {lwModPars} else {NULL}))

}
