#' Retrieve look-up value
#'
#' @param key original name(s) representing keys in look-up table. These are used 
#' to identify the correct value to be outputted by the function
#' @param table_name single string identifying internal package look-up tables. Current
#' options include "oca_missions" and "oca_projects"
#' 
#' @importFrom assertthat assert_that
#'
#' @return character vector of values from look-up table with equal length as input "key"
#' arguement
#'

get_lookup_value <- function(key, 
                             table_name) {
  
   # check args
  assert_that(is.character(key),
              length(table_name) > 1 & is.character(table_name))
  

  # available lookup tables in tbcleanr
  lookup_tables <- c("oca_missions", "oca_projects")
  # convert lookup table object name to string
  table_name_string <- deparse(substitute(table_name))

  if (! (table_name_string %in% lookup_tables)) {
    stop("Look-up table not found - respecify in table_name arg")
  }

  # remove vector name from output
  unname(table_name[key])
  
}
