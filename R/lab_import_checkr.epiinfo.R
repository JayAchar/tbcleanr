#' Confirm correct lab data import
#'
#' @param x data frame containing variables
#'
#' @return unchanged data frame or error highlighting parsing errors on data import
#' @importFrom purrr map_lgl
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom stringr str_detect
#' @export 

lab_import_checkr.epiinfo <- function(x) {
  
  # if specific variables are class == logical must check if data imported correctly
  # define key variables
  check_vars <- c("RAYON", "CULRES2", "RES02", "CULRES3", "RES03", "CULRES4", "RES04",
                  "MGITRES", "RESULT", "MGITRES2", "RESULT02", "MGITRES3", "RESULT03", 
                  "MGITRES4", "RESULT04", "HAIN", "CAPILIA","MGITH", "MGITE", "MGITR",
                  "MGITS", "MGITZ", "HAINH", "HAINR", "MFX1")
  
  # new xpert variable names added in late 2018
  xpert_additions <- xpert_variable_detector(x)
  
  # add to keep variables
  check_vars <- c(check_vars, xpert_additions)
  
  # check if variables have been incorrectly parsed as logicals
  class_check <- map_lgl(x[, check_vars], .f = ~ class(.x) == "logical")
  
  if (any(class_check)) stop("Lab variables incorrectly parsed as logicals - check data read args - suggest guess_max adjustment in readr, or data set is incomplete")
  
  x
}
