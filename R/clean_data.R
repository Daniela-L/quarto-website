#' Clean health data
#'
#' @param infiles A dbf, dbc, or csv file  with disease cases
#' @param outfiles A cleaned dbf file
#' @return A cleaned dataframe
#' @examples
#' clean_data(infiles = "Data/Disease/Dengue/dengue_2014_raw.dbf",
#'            outfiles = "Data/Disease/Dengue/dengue_2014_cleaned")
#' clean_data(infiles = "Data/Disease/Malaria/malaria_2014_2016_raw.dbf",
#'            outfiles = "Data/Disease/Malaria/malaria_2014_2016_cleaned")
#' 
packages  <- c("foreign", "read.dbc")
invisible(install.packages(setdiff(packages, rownames(installed.packages()))))
# invisible(lapply(packages, function(pkg) suppressMessages(require(pkg, character.only = TRUE))))

# 2. Clean the data by fixing age and unifying column names
clean_data <- function(infiles, outfiles) {

  # create a table with the columnnames of differrent datasets
  column_names <- list(UF_RESI   = c("SG_UF", "UF_RESID"),
                       MUNI_RESI = c("ID_MN_RESI", "MUN_RESI"),
                       UF_HOSP   = c("UF"),
                       MUNI_HOSP = c("MUNICIPIO"),
                       DISEASE   = "ID_AGRAVO",
                       DT_NOTI   = c("DT_NOTIFIC", "DT_NOTIF"),
                       UF_NOTI   = c("SG_UF_NOT", "UF_NOTIF"),
                       MUNI_NOTI = c("ID_MUNICIP", "MUN_NOTI"),
                       AGE       = c("AGE","NU_IDADE_N", c("ID_DIMEA","ID_PACIE")),
                       SEX       = c("CS_SEXO", "SEXO"),
                       RACE      = c("CS_RACA", "RACA"),
                       UF_INF    = c("COUFINF", "UF_INFEC"),
                       MUNI_INF  = c("COMUNINF", "MUN_INFE"),
                       CLASS     = "CLASSI_FIN",
                       CRIT      = "CRITERIO",
                       DT_SYM    = c("DT_SIN_PRI", "DT_SINTO"))

  #create empty dataframe
  data <- data.frame()

  #load data, select required variables and bind to dataframe
  for (file in 1:infiles) {
    #load data
    if (sub(".*\\.", "", file) == "dbc") {
      new <- read.dbc::read.dbc(paste0(file))
    } else if (sub(".*\\.", "", file) == "dbf") {
      new <- foreign::read.dbf(paste0(file), as.is = TRUE)
    } else if (sub(".*\\.", "", file) == "csv") {
      new <- read.csv(paste0(file))
    }

    # fix the age   # RETHINK here
    if ("NU_IDADE_N" %in% colnames(new)) {
      new  <- new %>%
        mutate(AGE = case_when(NU_IDADE_N < 120 ~ as.numeric(NU_IDADE_N),
                               NU_IDADE_N >= 120 & NU_IDADE_N <1000 ~ NA_real_,
                               NU_IDADE_N >= 1000 & NU_IDADE_N < 2366 ~ 0,
                               NU_IDADE_N >= 2366 & NU_IDADE_N < 3000 ~ NA_real_,
                               NU_IDADE_N >= 3000 & NU_IDADE_N < 3013 ~ 0,
                               NU_IDADE_N >= 3013 & NU_IDADE_N < 4000 ~ NA_real_,
                               NU_IDADE_N >= 4000 & NU_IDADE_N < 4120 ~ as.numeric(NU_IDADE_N - 4000),
                               NU_IDADE_N >= 4120 ~ NA_real_,
                               TRUE ~ NA_real_)) %>%
        select(-NU_IDADE_N)
    } else if ("ID_PACIE" %in% colnames(new)){
      new  <- new %>%
        mutate(AGE = case_when(ID_DIMEA == "D" ~ 0, 
                               ID_DIMEA == "M" & ID_PACIE < 12 ~ 0,
                               ID_DIMEA == "M" & ID_PACIE < 24 ~ 1, # There are notifications with ID_DIMEA == "M and ID_PACIE >= 48.
                               # 1 year is up to < 24. 24 is 2 years. I changed here and in zero years. The database from 2003 to 2022
                               # I'm working on, there are age in months up to 271 months. Note sure if it mak sense to consider it 
                               # or if the best is just cut off to the database.
                               ID_DIMEA == "A"  ~ as.numeric(ID_PACIE),
                               TRUE ~ NA_real_)) %>%
        select(-ID_PACIE, -ID_DIMEA)
    }

    # create the list of variables that will be extracted
    sel_var <- c()
    new_var <- c()
    for (i in 1:length(column_names)){
      if (any(colnames(new) %in% column_names[[i]])){
        sel_var <- append(sel_var, colnames(new)[which(colnames(new) %in% column_names[[i]])])
        new_var <- append(new_var, names(column_names)[i])
      } else {
        cat(paste0("There was no variable found for", names(column_names)[i], ". 
                     \nShould the variable be ignored or do you want to enter it manually? 
                     \n('i' for ignore or enter the column name)."))
        args <- readline()
        if (args != "i") {
          sel_var <- append(sel_var, args)
          new_var <- append(new_var, names(column_names)[i])
        } else {
          print(paste0(names(column_names)[i], " will not be included."))
        }
      }
    }

    #bind to dataframe
    new <- new %>%
      select(all_of(sel_var)) %>%
      setNames(new_var)

    data <- rbind(data, new)
    rm(new)
    print(paste0("Loaded ", file, " of ", length(infiles), " files."))
  }

  #save output
  foreign::write.dbf(data, outfiles)
}

### END
