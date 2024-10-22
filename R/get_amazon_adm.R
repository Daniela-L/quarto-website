#' Get dataframe with the municipalities located wihtin the amazon
#'
#' @param perc A number specifying the % of the municipalities that
#' needs to be within the amazones to be counted here. Default is 50.
#' @param outfiles A character specyfing the path and name of the
#' resulting output file.
#' @return A cleaned dataframe
#' @examples
#' get_amazon_adm(outfile="Data/AmazoniaLegal.csv")
#' get_amazon_adm(20, "Data/AmazoniaLegal.csv")


packages  <- c("dplyr", "openxlsx", "stringr")
invisible(install.packages(setdiff(packages, rownames(installed.packages()))))
# invisible(lapply(packages, function(pkg) suppressMessages(require(pkg, character.only = TRUE))))


# 0. Get Legal Amazon municipalities
get_amazon_adm <- function(perc = 50,  outfile){

  stopifnot("`erc' must be numeric and between 0-100" = (is.numeric(perc) & perc >=0 & perc <= 100))

  url <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/amazonia_legal/2021/lista_de_municipios_Amazonia_Legal_2021.xlsx"

  openxlsx::read.xlsx(url) %>%
    dplyr::select(CD_MUN, PERC_INT) %>%
    dplyr::mutate(CD_MUN = stringr::str_sub(CD_MUN, end = 6)) %>%
    filter(PERC_INT > perc) %>%
    select(CD_MUN) %>%
    write.csv(file = outfile, row.names = FALSE)
}

### END
