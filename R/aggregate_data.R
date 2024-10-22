#' Aggregates the data spatiotemporally
#'
#' @param infile A dbf file with disease cases. Can be output of clean_data.
#' @param gender 'F','M', 'I' or 'all'.
#' @param age A vector of numbers
#' @param itime A number indicating initial year.
#' @param ftime A number indicating final year.
#' @param timetype 'sym' to use date of first symptoms or
#' 'noti' to use notification date.
#' @param tunit 'week', 'month', or 'year'
#' @param spacetype  'N', 'R', 'H', or 'I'
#' @param sunit 'MUNI', 'UF'
#' @param sloc 'all' or 'amazon'
#' @param crit 'lab', 'clin', 'both' or 'sus'
#' @param amazon_file A csv file containing the municipality within
#' the amazones. Can be the output from get_amazon_adm.
#' @param output Path and name of the output file
#' @return An Aggregated dataframe in dbf format
#' @examples
#' aggregate_data(infile = "Data/Disease/Dengue/dengue_2014_cleaned.dbf",
#'                itime = 2014, ftime = 2014, sloc = "all", tunit = "week",
#'                sunit = "UF", spacetype = "N", timetype = "noti",
#'                crit = "both", age = c(0, 25, 50, 100, 125), gender = "all",
#'                output = "Data/Disease/Dengue/dengue_2014_aggregated.dbf")
#' aggregate_data(infile = "Data/Disease/Malaria/malaria_2014_2016_cleaned.dbf",
#'                itime = 2014, ftime = 2015, sloc = "all", tunit = "month",
#'                sunit = "MUNI", spacetype = "N", timetype = "noti",
#'                crit = "sus", gender = "F",
#'                output = "Data/Disease/Malaria/malaria_2014_2016_aggregated.dbf")

packages  <- c("foreign")
invisible(install.packages(setdiff(packages, rownames(installed.packages()))))
invisible(lapply(packages, function(pkg) suppressMessages(require(pkg, character.only = TRUE))))


aggregate_data <- function(infile, gender = NULL, age = NULL, itime, ftime,
                           timetype = "noti", tunit = "week", sunit = "muni",
                           sloc = "all", spacetype = "N", crit = "both",
                           amazon_file, output) {

  #check data requirements
  if (missing(ftime) | missing(itime) | missing(infile)) {
    stop("You are missing a required variable")
  }
  stopifnot("`gender` must be a 'F','M', 'I' or 'all'." = (gender %in% c('F', 'M', 'I', 'all')))
  stopifnot("`age` must be a vector of numbers." = is.numeric(age) | missing(age))
  stopifnot("`itime` must be a year." = is.numeric(itime))
  stopifnot("`ftime` must be a year." = is.numeric(ftime))
  stopifnot("`timetype` must be either 'sym' or ǹoti." = (timetype %in% c("sym", "noti")) )
  stopifnot("`tunit` must be either 'week', 'month', 'year'." = (tunit %in% c("week", "month", "year")))
  stopifnot("`sunit` must be either 'MUNI', 'UF'." = (sunit %in% c("MUNI", "UF")))
  stopifnot("`sloc` must be either 'all' or 'amazon'." = (sloc %in% c("all", "amazon"))| missing(sloc))
  stopifnot("`spacetype` must be either 'N', 'R', 'H', or 'I'." = (spacetype %in% c("N", "R", "H", "I")))
  stopifnot("`crit` must be in 'lab', 'clin', 'both' or 'sus'." = (crit %in% c("lab", "clin", "both", "sus")))

  #load file
  data <- foreign::read.dbf(infile)

  #get only desired gender
  if (!is.null(gender)) {
    if (!("SEX" %in% colnames(data))) {
      print("WARNING: The data will not be aggregated by gender
            because column 'SEX' is missing.")
    } else if (gender %in% c("F", "M", "I")) {
      data <- data[data["SEX"] == gender, ]
    } else if (gender != "all") {
      print("WARNING: The data will not be aggregated by gender
            because the provided gender is incorrect.")
    }
  }

  #get moment of time
  ttype <- list(sym = "SYM", noti = "NOTI")

  #select temporal boundaries
  data <- data[which(data[,paste0("DT_", ttype[[timetype]])] >= paste0(itime, "-01-01") & 
                    data[,paste0("DT_", ttype[[timetype]])] <= paste0(ftime, "-12-31")), ]

  #get type of data
  stype <- list(N = "NOTI", R = "RESI", H = "HOSP", I = "INF")

  # select spatial boundaries
  if (sloc == "amazon") {
    am_muni <- read.csv(amazon_file)
    if (sunit == "MUNI") {
      data <- data[which(data[, paste0("MUNI_", stype[[spacetype]])] %in% am_muni[, 1]), ]
    } else if (sunit == "UF") {
      print("The legal amazonian can only be used for municipalities (for now)")
    }
  } else if (sloc != "all") {
    data <- data[which(data[, paste0(sunit, "_", stype[[spacetype]])] %in% sloc), ]
  }

  # select confirmation criteria
  if (crit == "lab") {
    data <- data[which((data[,"CLASS"] %in% c(1, 10, 11, 12)) & (data[,"CRIT"] == 1)), ]
  } else if (crit == "clin") {
    data <- data[which((data[,"CLASS"] %in% c(1, 10, 11, 12))& (data[, "CRIT"] == 2)), ]
  } else if (crit == "both") {
    data <- data[which((data[,"CLASS"] %in% c(1, 10, 11, 12))& (data[, "CRIT"] %in% c(1, 2))), ]
  }

  # add respective timesteps, depending on choosen time unit
  if (tunit == "week") {
    data$T_STEP <- format(as.Date(data[[paste0("DT_", ttype[[timetype]])]]), "%V-%G") 
  } else if (tunit == "month") {
    data$T_STEP <- format(as.Date(data[[paste0("DT_", ttype[[timetype]])]]), "%m-%Y") 
  } else if (tunit == "year") {
    data$T_STEP <- format(as.Date(data[[paste0("DT_", ttype[[timetype]])]]), "%Y")
  }

  # add age group
  if (!is.null(age)) {
    if (!("AGE" %in% colnames(data))){
      print("WARNING: The data will not be aggregated by age
            because column 'AGE' is missing.")
    } else if (is.numeric(age)) {
      age_groups <- c()
      for (i in 1:length(age) - 1) {
        age_groups[i] <- paste0(age[i], "-", age[i + 1])
      }
      data$AGE_GRP <- cut(data$AGE, breaks = age, labels = age_groups)
    } else if (age != "all") {
      print("WARNING: The data will not be aggregated by age 
            because the provided ages are incorrect.")
    }
  }

  #prepare labels for output
  collabels <- c("area_code", "t_step")
  if (!(is.null(age))) {collabels <- append(collabels, "age_grp")}
  if (!(is.null(gender))) {collabels <- append(collabels, "gender")}

  #group together?
  data <- as.data.frame(do.call(table, c(list(data[[paste0(sunit, "_", stype[[spacetype]])]]),
                                         list(data[["T_STEP"]]),
                                         list(data[["AGE_GRP"]])[!(is.null(age))],
                                         list(data[["SEX"]] )[!(is.null(gender))],
                                         list(dnn = collabels))),
                        responseName = "freq")

  #save output
  foreign::write.dbf(data, output)
}

### END
