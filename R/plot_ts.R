#' Plot disease cases or incidence on a map
#'
#' @param infile A number
#' @param popdata A number
#' @param UF_shp A number
#' @param muni_shp A number
#' @param type A number
#' @param incidence A number
#' @param age A number
#' @param gender A number
#' @param timestep A number
#' @param outfile A number
#' @return A cleaned dataframe
#' @examples
#' add(1, 1)
#' add(10, 1)

packages  <- c("foreign", "ggplot2", "dplyr")
invisible(install.packages(setdiff(packages, rownames(installed.packages()))))
# invisible(lapply(packages, function(pkg) suppressMessages(require(pkg, character.only = TRUE))))

plot_ts <- function(infile, comparison, area = NULL, timestep = NULL,
                    age = NULL, gender = NULL, title = NULL) {
  #load data
  data <- foreign::read.dbf(infile)

  # select area
  if (!is.null(area)){
    if (all(area %in% unique(data$area_code))){
      data <- data[data$area_code %in% area, ]
    } else {
      print(paste0("Your defined area codes (",
                   paste(area, collapse = ", "),
                   ") do not match with the area codes in the data (",
                   paste(unique(data$area_code), collapse = ", "),
                   ")"))
    }
  }

  # select timestep
  if (!is.null(timestep)) {
    if (all(timestep %in% unique(data$t_step))) {
      data <- data[data$t_step %in% timestep, ]
    } else {
      print(paste0("Your defined timesteps (",
                   paste(area, collapse = ", "),
                   ") do not match with the time steps in the data (",
                   paste(unique(data$t_step), collapse = ", "),
                   ")"))
    }
  }

  #select age group
  if (!is.null(age)) {
    if (!("age_grp" %in% colnames(data))) {
      print("WARNING: The data will not be aggregated by age because
            column 'age_grp' is missing.")
    } else if (all(age %in% unique(data$age_grp))) {
      data <- data[data$age_grp %in% age, ]
    } else {
      print(paste0("Your defined age groups (",
                   paste(area, collapse = ", "),
                   ") do not match with the age groups in the data (",
                   paste(unique(data$age_grp), collapse = ", "),
                   ")"))
    }
  }

  # select gender
  if (!is.null(gender)){
    if (!("gender" %in% colnames(data))){
      print("WARNING: The data will not be aggregated by gender
            because column 'gender' is missing.")
    } else if (all(gender %in% unique(data$gender))) {
      data <- data[data$gender %in% gender, ]
    } else {
      print(paste0("Your defined genders (",
                   paste(area, collapse = ", "),
                   ") do not match with the genders in the data (",
                   paste(unique(data$gender), collapse = ", "),
                   ")"))
    }
  }

  #aggregate
  ctype <- list(L = "area_code", A = "age_grp", G = "gender")
  ctype2 <- list(L = "", A = "Age groups", G = "Gender")

  if (comparison %in% c("L", "A", "G", "N")) {
    by <- list(t_step = data$t_step)
    if(comparison %in% c("L", "A", "G")) {
      by <- append(by, list(comp = data[[ctype[[comparison]]]]))
    } else if (comparison == "N") {
      comp <- 1
    }
  } else {
    print(paste0("Your input for comparison (", comparison,
                 ") is not valid. It has to be 'L','A','G' or 'N'."))
  }
  data <- aggregate(list(freq = data$freq),
                    by = by,
                    FUN = sum)

  #plot
  plot <- ggplot2::ggplot(data) +
    ggplot2::geom_line(aes(x = t_step, y = freq, group = comp, color = comp), size = 1.5) + 
    ggplot2::xlab("Time") +
    ggplot2::ylab("Number of cases") +
    ggplot2::labs(title = title, color = ctype2[[comparison]])
  
  return(plot)
}