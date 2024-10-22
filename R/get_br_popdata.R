#' Get brazilian population data
#'
#' @param year A character specyfing the path and name of the
#' resulting output file.
#' @param outpath A character specyfing the path and name of the
#' resulting output file.
#' @param load Should the output be loaded into the environment?
#' Default is TRUE.
#' @param save Should the output be saved to disk? Default is TRUE.
#' @return A cleaned dataframe
#' @examples
#' get_br_pop_data(2015, "Data/Demographic/")
#' get_br_pop_data(2016, "Data/Demographic/", load = F)
#' get_br_pop_data(2017, save = F)
#' get_br_pop_data(c(2018, 2019), save = F)

get_br_pop_data <- function(year, outpath, load = TRUE, save = TRUE) {

  # if data is not saved nor loaded, stop function
  if (save == FALSE & load == FALSE) {
    stop("Error: the data is not loaded nor saved.
          Change either load or save to TRUE.")
  }

  # if outpath is missing and save=T, give warning set outpath to ""
  if (missing(outpath) & save == TRUE) {
    print("Warning: No outpath was provided,
           file will be saved in root directory.")
    outpath <- ""
  }

  # for every year in "year", read population data online
  for (y in year){

    # if year is not between 1992 and 2022, stop function
    if (y <= 1992 & y >= 2022) {
      stop(paste0("Error: Year ", y, " is either out of range or not numeric."))
    }

    pop_data <- read.csv(paste0("https://sidra.ibge.gov.br/geratabela?format=us.csv&name=tabela6579.csv&terr=NC&rank=-&query=t/6579/n6/all/v/all/p/", y, "/l/v,p,t"),
                         header = FALSE, sep = ",") %>%
                setNames(c("code_muni", "name_muni", "pop")) %>%
                filter(grepl("^[0-9]{7}$", code_muni)) %>%
                mutate(
                  code_muni = str_sub(code_muni, end = 6),
                  code_state = str_sub(code_muni, end = 2)
                )
    # write a csv if wanted
    if (save == TRUE) {
      write.csv(pop_data, file = paste0(outpath, "pop_", y, ".csv"), row.names = FALSE)
    }
    # return
    if (load == TRUE){
      assign(paste("pop_", y, sep = ""), pop_data, envir = .GlobalEnv)
    }
  }
}

### END
