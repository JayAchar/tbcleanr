#' Confirm correct lab data import
#'
#' By default, readr parses the first 1000 rows to define column class.
#' However, lab data is frequently very long with more recent test types, 
#' e.g. Hain MTBDRsl, only being available in more recent years. Thus, if not 
#' specified within the guess_max option, variables may be miss classified on import
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export

lab_import_checkr <- function(x, ...) {
  
  # check input
  assert_that(is.data.frame(x))
  
  UseMethod("lab_import_checkr", x)
  
}  
