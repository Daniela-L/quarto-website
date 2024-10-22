#' Get dataframe with the municipalities located wihtin the amazon
#'
#' @param level A number specifying the % of the municipalities that
#' needs to be within the amazones to be counted here. Default is 50.
#' @param year A character specyfing the path and name of the
#' resulting output file.
#' @param outfile A character specyfing the path and name of the
#' resulting output file.
#' @param load Should the output be loaded into the environment?
#' Default is TRUE.
#' @param save Should the output be saved to disk? Default is TRUE.
#' @return A cleaned dataframe
#' @examples
#' get_br_adm_data("UF", 2015, load = TRUE, save = FALSE)
#' get_br_adm_data("UF", 2020, "Data/BR_muni.shp", load = FALSE)
packages  <- c("sf")
invisible(install.packages(setdiff(packages, rownames(installed.packages()))))
# invisible(lapply(packages, function(pkg) suppressMessages(require(pkg, character.only = TRUE))))

get_br_adm_data <- function(level, year, outfile, load = TRUE, save = TRUE) {

  # if data is not saved nor loaded, stop function
  if (save == FALSE & load == FALSE) {
    stop("Error: the data is not loaded nor saved.
          Change either load or save to TRUE.")
  }

  # if outpath is missing and save=T, give warning set outpath to ""
  if (missing(outfile) & save == TRUE) {
    print("Warning: No outpath was provided,
           file will be saved in root directory.")
    outfile <- "amazonas_mun.shp"
  }

  # check if year is over 2015
  if (missing(year) | !is.numeric(year) | year < 2015 | year > year(Sys.time())) {
    stop("Error: The provided 'year' is not valid.")
  }

  #check if level is either "muni" or "UF
  if (!(level %in% c("muni", "UF"))) {
    stop("The provided level should be either 'muni' or 'UF'.")
  }

  # Create url
  url <-  paste0("https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_",year,"/Brasil/BR/")
  if (level == "muni") {
    if (year %in% seq(2015, 2018)) {
      url <- paste0(url, "br_municipios.zip")
    } else if (year == 2019) {
      url <- paste0(url, "br_municipios_20200807.zip")
    } else if (year %in% seq(2020, 2022)) {
      url <- paste0(url, "BR_Municipios_", year, ".zip")
    } else {
      stop("Year is not valid.")
    }
  } else if (level == "UF") {
    if (year %in% seq(2015, 2019)) {
      url <- paste0(url, "br_unidades_da_federacao.zip")
    } else if (year %in% seq(2020, 2022)) {
      url <- paste0(url, "BR_UF_", year,  ".zip")
    } else {
      stop("Year is not valid.")
    } 
  } else {
    stop("The level must either be 'muni' or 'UF'")
  }

  # Download the zip file and save to 'temp'
  temp <- tempfile()
  temp2 <- tempfile()
  download.file(url, temp)
  unzip(zipfile = temp, exdir = temp2)

  # write a csv if wanted
  if (save == TRUE) {
    #delete unnecessary data, format, and save
    temp2 %>%
      sf::read_sf() %>%
      select(1) %>%
      setNames(c("area_code", "geometry")) %>%
      mutate(area_code = as.factor(stringr::str_sub(area_code, end = 6))) %>%
      sf::st_write(outfile)
  }
  # return
  if (load == TRUE){
    #delete unnecessary data, format, and save
    shapefile <- temp2 %>%
      read_sf() %>%
      select(1) %>%
      setNames(c("area_code", "geometry")) %>%
      mutate(area_code = as.factor(stringr::str_sub(area_code, end = 6)))
    assign(paste0("shp_", year), shapefile, envir = .GlobalEnv)
  }
}

### END
