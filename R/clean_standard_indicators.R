#' Clean Annual Standard TB Indicators
#'
#' This function takes a data frame of imported data from  
#' a MSF OCA Annual Standard Indicators Excel file.
#' The function will clean, transpose, subset TB data and standardise renaming
#' missions and projects using internal package look up tables. These look up 
#' tables can be accessed via tbcleanr:::oca_missions and tbcleanr:::oca_projects.
#' 
#' @param file data frame of raw compiled project data. 
#'
#' @param year string defining year of data collection
#' 
#' @return list containing a data frame of cleaned data 
#' @importFrom stringr str_extract str_detect
#' @importFrom readxl read_excel
#' @importFrom assertthat assert_that
#' @export


clean_standard_indicators <- function(file,
                                      year) {
  
  # check args
  assert_that(is.data.frame(file),
              is.character(year),
              length(year) == 1)
  
  # remvoe first three rows if not done on data import
  if (file[4, 1] == "Country:") {
    df <- file[-c(1:3), ]
  }
  
  # find columns with no data
  empty <- str_detect(df[2,], "^Project\\d")
  
  # convert NA to FALSE
  empty[is.na(empty)] <- FALSE
  
    # check that 'empty' is a logical with some TRUE
    assert_that(is.logical(empty),
                mean(empty) > 0)
  
  # remove columns with no data
  df_trim <- df[, !empty]
  
  # use lookup table to standardise project names
  df_trim[2, ] <- get_lookup_value(as.character(df_trim[2, ]), oca_projects)
  
  # assign project name to colnames
  colnames(df_trim) <- df_trim[2, ]
  
  # use lookup table to standardise mission names
  df_trim[1, ] <- get_lookup_value(as.character(df_trim[1, ]), oca_missions)
  
  # assign names to columns with NA
  if (all(is.na(df_trim[2, 1:7]))) {
    print("Correct missing column names have been added.")
    add_names <- c("var",
                   "total",
                   "projects",
                   "min",
                   "max",
                   "tot_excl",
                   "proj_excl")
    colnames(df_trim)[1:7] <- add_names
  }
  
  # names of output have all been filled
  if (any(is.na(names(df_trim)) != F)) {
    stop("Column names must all be assigned")
  }
  
  # extract TB data from data frame object
  df_tb <- extract_tb_data(df_trim)
  
  # transpose and clean TB data from data frame object
  df_tb <- transpose_tb_data(df_tb,
                             year = year)
  
  # check output 
  assert_that(is.data.frame(df_tb),
              is.character(year))
  
  # output data frame and year integer
  df_tb
  
}

