#' @title Plot catch curve
#' @description Plot a catch curve to estimate instantaneous total mortality (Z) using age data
#' @inheritParams plot_maturity
#' @inheritParams plot_growth
#' @param age.range Defines the age range to be used for Z estimation. If \code{NULL}, all ages are used. If a numeric vector of length 2, the first number defines the minimum age to include and the last number the maximum age. It is also possible to use differring ranges by sex when \code{split.by.sex = TRUE}: use a named list with 'female' and 'male' as names. Provide a numeric vector of length 2 to each element. See Examples.
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
#' age.range = list(female = c(13,26), male = c(10,26)),
#' split.by.sex = TRUE)
#' }
#' @export


# dt = survey_ghl; age = "age"; sex = "sex"; age.range = list(female = c(13,26), male = c(10,24)); female.sex = "F"; male.sex = "M"; split.by.sex = TRUE; base_size = 8; legend.position = "bottom"
plot_catchcurve <- function(dt, age = "age", sex = "sex", age.range = NULL, female.sex = "F", male.sex = "M", split.by.sex = FALSE, base_size = 8, legend.position = "bottom") {

# To avoid modifying the input object
# dt <- x

# Checks

if(split.by.sex) {
  if(is.null(sex)) {
    stop("Sex column has to be specified when split.by.sex = TRUE")
  }
  if(!all(c(female.sex, male.sex) %in% unique(dt[[sex]]))) {
    stop(female.sex, " or ", male.sex, " not found from the ", sex,
         " column. Check the female.sex and male.sex parameters.")
  }
  if(dt %>% dplyr::pull(!!enquo(sex)) %>% na.omit() %>% length() < 10) {
    stop("Either invalid sex column or not enough sex data")
  }
  if(!inherits(female.sex, class(male.sex))) {
    stop("female.sex and male.sex are different class.")
  }
}

if(!is.null(age.range)) {

  if(inherits(age.range, "list")) {

    if(!split.by.sex) stop("split.by.sex has to be TRUE when supplying lists to age.range")

    if(length(age.range) != 2) stop("length of age.range list has to be two")

    if(!any(names(age.range) %in% c("female", "male"))) {
      stop("age.range has to be a named list with names female and male")
    }

    lapply(age.range, function(k) {
      if(length(k) != 2) {
        stop("age.range has to be a numeric vector of length two. The first number
           defines the minimum age and the second maximum age to be included to
           the Z calculation.")

        if(!inherits(k, "numeric")) {
          stop("age.range has to be a numeric vector.")
        }
      }
    })

  } else {
    if(length(age.range) != 2) {
      stop("age.range has to be a numeric vector of length two. The first number
           defines the minimum age and the second maximum age to be included to
           the Z calculation.")
    }

    if(!inherits(age.range, "numeric")) {
      stop("age.range has to be a numeric vector.")
    }
  }
}

# Data manipulation ####

if("age" %in% colnames(dt) && age != "age"){
  dt <- dt %>% dplyr::select(-age)
}

## Select and rename columns ####

dt <- dt %>%
  dplyr::rename("age" = tidyselect::all_of(age)) %>%
  dplyr::filter(!is.na(age)) %>%
  dplyr::mutate(age = as.numeric(age))

## Sex data ####

if(split.by.sex) {
  ### Split by sex case ####

  if("sex" %in% colnames(dt) && sex != "sex") {
    dt <- dt %>% dplyr::select(-sex)
  }

  dt <- dt %>%
    dplyr::rename("sex" = tidyselect::all_of(sex)) %>%
    dplyr::filter(!is.na(sex)) %>%
    dplyr::group_by(sex, age) %>%
    dplyr::count()

} else {
  ### No sex split ####
  dt <- dt %>%
    dplyr::mutate(sex = "both") %>%
    dplyr::group_by(sex, age) %>%
    dplyr::count()
}

# Total mortality regression ####

## Data to include ####

if(is.null(age.range)) {
  dt <- dt %>% dplyr::mutate(include = TRUE)
} else {
  if(inherits(age.range, "list")) {
    dt <- lapply(unique(dt$sex), function(k) {
      if(k == female.sex) {
        ar <- age.range[["female"]]
        dt %>%
          dplyr::filter(.data$sex == k) %>%
          dplyr::mutate(include = .data$age >= ar[1] & .data$age <= ar[2])
      } else if(k == male.sex) {
        ar <- age.range[["male"]]
        dt %>%
          dplyr::filter(.data$sex == k) %>%
          dplyr::mutate(include = .data$age >= ar[1] & .data$age <= ar[2])
      } else {
        stop("k is not female.sex nor male.sex. Don't know what to do.")
      }
    }) %>% dplyr::bind_rows()
  } else {
    dt <- dt %>% dplyr::mutate(include = .data$age >= age.range[1] & .data$age <= age.range[2])
  }
}

## Model ####

mod <- lapply(unique(dt$sex), function(k) {
  broom::tidy(
    lm(log(n) ~ age,
       data = dt %>% dplyr::filter(.data$include, .data$sex == k)),
    conf.int = TRUE) %>%
    dplyr::mutate(sex = k, .before = 1)
}) %>% dplyr::bind_rows()

# Plot ####

if(split.by.sex) {

  modf <- mod %>% dplyr::filter(sex == female.sex)
  modm <- mod %>% dplyr::filter(sex == male.sex)

  p <- ggplot() +
    geom_point(data = dt, aes(x = age, y = n, shape = .data$include, color = sex)) +
    geom_smooth(
      data = dt %>% dplyr::filter(.data$include), aes(x = age, y = n, color = sex),
      method = "lm", se = FALSE, formula = 'y ~ x', linewidth = 0.7/2.13) +
    annotate(
      "text", x = Inf, y = Inf,
      label = paste0(
        "b = ", round(modf[2,"estimate"], 3), " (",
        round(modf[2,"conf.low"], 3), ",",
        round(modf[2,"conf.high"], 3), ")\n",
        "a = ", round(modf[1,"estimate"], 3), " (",
        round(modf[1,"conf.low"], 3), ",",
        round(modf[1,"conf.high"], 3), ")\n"
      ),
      vjust = 1, hjust = 1, size = base_size/2.845276, color = "#FF5F68") +
    annotate(
      "text", x = Inf, y = Inf,
      label = paste0(
        "b = ", round(modm[2,"estimate"], 3), " (",
        round(modm[2,"conf.low"], 3), ",",
        round(modm[2,"conf.high"], 3), ")\n",
        "a = ", round(modm[1,"estimate"], 3), " (",
        round(modm[1,"conf.low"], 3), ",",
        round(modm[1,"conf.high"], 3), ")\n"
      ),
      vjust = 2, hjust = 1, size = base_size/2.845276, color = "#449BCF") +
    labs(x = "Age (years)", y = "Abundance (Ln-scale)") +
    scale_color_manual("Sex", values = c("#FF5F68", "#449BCF")) +
    scale_shape_manual(
      "Included to Z regression",
      values = c(21, 19)) +
    coord_cartesian(expand = FALSE, clip = "off") +
    scale_y_continuous(trans = "log", breaks = scales::log_breaks(n = 10)) +
    scale_x_continuous(n.breaks = 5) +
    theme_fishplots(base_size = base_size) +
    theme(legend.position = legend.position,
          text = element_text(size = base_size))

  Text <- paste0(
    "Instantenous total mortality (Z) estimated using a catch curve and\nage range ",
    ifelse(inherits(age.range, "list"),
           paste0(paste(age.range[["female"]], collapse = "-"), " for females and ",
                  paste(age.range[["male"]], collapse = "-"), " for males.\n\n"),
           paste0(paste(age.range, collapse = "-"), " for both sexes.\n\n")),
    "Females:\n",
    "Z = ", round(-modf[2,"estimate"], 3), " (",
    round(-modf[2,"conf.high"], 3), "-",
    round(-modf[2,"conf.low"], 3), " 95% CIs)\n",
    "N at age 0 = ", round(exp(modf[1,"estimate"])), " (",
    round(exp(modf[1,"conf.low"])), "-",
    round(exp(modf[1,"conf.high"])), " 95% CIs)\n",
    "Longevity = ", round(-modf[1,"estimate"]/modf[2,"estimate"], 1), " (",
    round(-modf[1,"conf.low"]/modf[2,"conf.low"], 1), "-",
    round(-modf[1,"conf.high"]/modf[2,"conf.high"], 1), " 95% CIs)\n\n",
    "Males:\n",
    "Z = ", round(-modm[2,"estimate"], 3), " (",
    round(-modm[2,"conf.high"], 3), "-",
    round(-modm[2,"conf.low"], 3), " 95% CIs)\n",
    "N at age 0 = ", round(exp(modm[1,"estimate"])), " (",
    round(exp(modm[1,"conf.low"])), "-",
    round(exp(modm[1,"conf.high"])), " 95% CIs)\n",
    "Longevity = ", round(-modm[1,"estimate"]/modm[2,"estimate"], 1), " (",
    round(-modm[1,"conf.low"]/modm[2,"conf.low"], 1), "-",
    round(-modm[1,"conf.high"]/modm[2,"conf.high"], 1), " 95% CIs)\n\n"
  )

} else {
  p <- ggplot() +
    geom_point(data = dt, aes(x = age, y = n, shape = .data$include)) +
    geom_smooth(
      data = dt %>% dplyr::filter(.data$include), aes(x = age, y = n),
      method = "lm", se = FALSE, formula = 'y ~ x',
      color = "black", linewidth = 0.7/2.13) +
    annotate(
      "text", x = Inf, y = Inf,
      label = paste0(
        "b = ", round(mod[2,"estimate"], 3), " (",
        round(mod[2,"conf.low"], 3), ",",
        round(mod[2,"conf.high"], 3), ")\n",
        "a = ", round(mod[1,"estimate"], 3), " (",
        round(mod[1,"conf.low"], 3), ",",
        round(mod[1,"conf.high"], 3), ")\n"
      ),
      vjust = 1, hjust = 1, size = base_size/2.845276) +
    labs(x = "Age (years)", y = "Abundance (Ln-scale)") +
    scale_shape_manual(
      "Included to Z regression",
      values = c(21, 19)) +
    coord_cartesian(expand = FALSE, clip = "off") +
    scale_y_continuous(trans = "log", breaks = scales::log_breaks(n = 10)) +
    scale_x_continuous(n.breaks = 5) +
    theme_fishplots(base_size = base_size) +
    theme(legend.position = legend.position,
          text = element_text(size = base_size))

  Text <- paste0(
    "Instantenous total mortality (Z) estimated using a catch curve and\nage range ",
    paste(age.range, collapse = "-"), ".\n\n",
    "Z = ", round(-mod[2,"estimate"], 3), " (",
    round(-mod[2,"conf.high"], 3), "-",
    round(-mod[2,"conf.low"], 3), " 95% CIs)\n",
    "N at age 0 = ", round(exp(mod[1,"estimate"])), " (",
    round(exp(mod[1,"conf.low"])), "-",
    round(exp(mod[1,"conf.high"])), " 95% CIs)\n",
    "Longevity = ", round(-mod[1,"estimate"]/mod[2,"estimate"], 1), " (",
    round(-mod[1,"conf.low"]/mod[2,"conf.low"], 1), "-",
    round(-mod[1,"conf.high"]/mod[2,"conf.high"], 1), " 95% CIs)\n\n"
  )
}

# Return ####

return(list(plot = suppressWarnings(p), text = Text, params = mod))

}
