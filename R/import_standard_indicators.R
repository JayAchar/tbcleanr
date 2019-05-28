#' Custom import Annual Standard Indicators
#'
#' This function takes a single character vector identifying 
#' a MSF OCA Annual Standard Indicators Excel file of medical data.
#' The function will import, transpose, subset TB data and standardise renaming
#' missions and projects. 
#' 
#' @param file character vector identifying the path and filename of an Excel 
#' file of MSF OCA Standard Indicators
#'
#' @return list containing a data frame of cleaned data and an integer indicating
#' the year of the raw data. 
#' @importFrom stringr str_extract str_detect
#' @importFrom readxl read_excel
#' @importFrom assertthat assert_that
#' @export


import_standard_indicators <- function(file) {
  
  # check args
  assert_that(is.character(file))
  
  # extract year from filename
  year <- str_extract(file, "\\d{4}")
  
  # import data
  df <- read_excel(path = file, 
                   sheet = "projects compiled", 
                   skip = 3)
  
  # find columns with no data
  empty <- str_detect(df[2,], "^Project\\d")
  
  # convert NA to FALSE
  empty[is.na(empty)] <- FALSE
  
  # remove columns with no data
  df_trim <- df[, !empty]
  
  # use lookup table to standardise project names
  pos <- match(df_trim[2, ], look_up[["old_proj"]])
  df_trim[2, ] <- look_up[["new_proj"]][pos]
  
  # assign project name to colnames
  colnames(df_trim) <- df_trim[2, ]
  
  # use lookup table to standardise mission names
  mis <- match(df_trim[1, ], look_up[["old_mission"]])
  df_trim[1, ] <- look_up[["new_mission"]][mis]
  
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
  if (all(is.na(names(df_trim)) != F)) {
    stop("Column names must all be assigned")
  }
  
  # check output 
  assert_that(is.data.frame(df_trim),
              is.numeric(year))
  
  # output data frame and year integer
  list(df_trim, 
       year)
  
}

