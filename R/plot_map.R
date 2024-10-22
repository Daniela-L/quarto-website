#' Plot disease cases or incidence on a map
#'
#' @param infile A dbf file with disease cases
#' @param popdata A csv file with population data. Can be output from get_br_popdata.
#' @param UF_shp A shapefile containing the Federative Units (UF)
#' @param muni_shp A shapefile containing the municipalities
#' @param type Should be either  'abs' for absolute disease cases or
#'  'inc' for diseases incidence
#' @param incidence Mandatory if type is 'inc'. A number indicating
#' the denominator for the incidence.
#' @param age Optional.
#' @param gender Optional. What gender should be displayed.
#' Can be 'F', 'M', 'I', 'all'
#' @param timestep A date included in infile indicating which timestep
#' should be visualised. Or can be the "sum" to dispaly the total disease cases.
#' @param outfile Optional. Path and name of where the plot should be saved.
#' @return A png plot
#' @examples
#' plot_map(infile = "Data/Disease/Dengue/dengue_2014_aggregated.dbf",
#'          popdata = "Data/Demographic/pop_2015.csv",
#'          UF_shp = "Data/Maps/BR_UF.shp", type = "inc",
#'          age = "0-25", gender = "F", timestep = "02-2014")
#' plot_map(infile = "Data/Disease/Malaria/malaria_2014_2016_aggregated.dbf",
#'          muni_shp = "Data/Maps/BR_muni.shp", type = "abs", gender = "F",
#'          timestep = "sum", outfile= "Figure/malaria_2014_2016_plot.png")


packages  <- c("ggplot2", "sf", "foreign", "dplyr")
invisible(install.packages(setdiff(packages, rownames(installed.packages()))))
# invisible(lapply(packages, function(pkg) suppressMessages(require(pkg, character.only = TRUE))))

plot_map <- function(infile, popdata, UF_shp, muni_shp, type = "abs", timestep,
                     incidence = 1000, gender = NULL, age = NULL, outfile) {

  # check data requirements
  if (missing(infile) | missing(UF_shp) & missing(muni_shp)) {
    stop("You are missing a required variable")
  }
  if (type == "inc") {
    stopifnot("'popdata' must be a string of the path + filename." = is.character(popdata))
  }
  stopifnot("`type` must be either 'abs' (absolute values) or 'inc'(incidence)." = (type %in% c("abs", "inc")))
  stopifnot("`incidence` must be a number." = is.numeric(incidence))
  stopifnot("`age` must be a vector of numbers." = is.character(age) | is.null(age))
  stopifnot("`gender` must be a 'F', 'M', 'I' or 'all'." = (gender %in% c('F', 'M', 'I', 'all')))

  #load data
  data <- foreign::read.dbf(infile)

  #select correct age group
  if (!is.null(age)) {
    if ("age_grp" %in% colnames(data)) {
      if (age %in% unique(data$age_grp)) {
        data <- data[data["age_grp"] == age, ]
      } else {
        stop(paste0("The provided age group is not contained in the dataset, try: ",
                    paste(unique(data$age_grp), collapse = " ")))
      }
    } else {
      stop("There are no age groups contained in the data.")
    }
  }

  #select correct sex
  if (!is.null(gender)) {
    if ("gender" %in% colnames(data)) {
      if  (gender %in% unique(data$gender)) {
        data <- data[data["gender"] == gender, ]
      } else {
        stop(paste0("The provided gender is not contained in the dataset, try: ",
                    paste(unique(data$gender), collapse = " ")))
      }
    } else {
      stop("The provided gender is not contained in the data.")
    }
  }

  #calculate sum of timesteps
  if (timestep == "sum") {
    data <- aggregate(list(freq = data$freq),
                      by = list(area_code = data$area_code),
                      FUN = sum) #############!!!!!!!!!!!!!!!!!!
  } else if (timestep %in% data$t_step) {
    data <- data[data["t_step"] == timestep, ]
  } else {
    print("The provided timestep is not available in the data.")
  }

  #add incidence
  if (type == "inc"){
    pop <- read.csv(popdata)
    if (nchar(as.character(data["area_code"][1, 1])) == 2) {
      pop <- aggregate(list(pop = pop$pop),
                       by = list(area_code = as.factor(pop$UF_code)),
                       FUN = sum) ### test this
    } else if (nchar(as.character(data["area_code"][1, 1])) == 6) {
      pop$area_code <- as.factor(pop$MUNI_code)
    }
    data <- dplyr::left_join(data, pop)
    data$inci <- data$freq / data$pop * incidence
  }

  #add spatial aspect
  if (nchar(as.character(data["area_code"][1, 1])) == 2) {
    br <- sf::st_read(UF_shp)
  } else if (nchar(as.character(data["area_code"][1, 1])) == 6) {
    br <- sf::st_read(muni_shp)
  }
  br <- br[, 1]
  names(br)[1] <- 'area_code'
  br$area_code <- as.factor(br$area_code)
  data <- dplyr::left_join(br, data)

  # set breaks and colors
  breaks2 <- c(0, 1, 2, 5, 10, 200)
  cols <- c("green", "yellow", "orange", "red", "black")
  btype <- list(inc = "inci", abs = "freq") ### CHeck btype

  # plot the maps
  plot <- ggplot2::ggplot(data) + 
    ggplot2::ggtitle("Incidence of dengue notifications in 2015") +
    ggplot2::geom_sf(aes(fill = freq), size = .15, show.legend = TRUE) +
    ggplot2::scale_fill_stepsn(breaks = breaks2,
                               colours = cols,
                               values = scales::rescale(breaks2))
  #save as png
  if (!missing(outfile)) {
    png(outfile)
    print(plot)
    dev.off()
  }

  return(plot)
}

### END
