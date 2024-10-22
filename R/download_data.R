#' Plot disease cases or incidence on a map
#'
#' @param disease A number
#' @param iyear A number
#' @param fyear A number
#' @param outfile A number
#' @return A cleaned dataframe
#' @examples
#' add(1, 1)
#' add(10, 1)
# 1. Download data from SINAN

# Do you think we need to do a function using another function from microdatus? Is it not better 
# to use directely the microdatasus? To convert the file in another extention I think it's good.
# Maybe, the best is save the database in csv.
download_data <- function(disease, iyear, fyear,
                          outfile = paste0(disease, "_", iyear, "_", fyear)) {
  data <- fetch_datasus(year_start = iyear,
                        year_end = fyear,
                        information_system = paste0("SINAN-", toupper(disease)))
  foreign::write.dbf(data, outfile)
}

### END