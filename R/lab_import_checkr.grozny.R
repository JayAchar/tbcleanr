#' Confirm correct lab data import
#'
#' @param x data frame containing variables
#'
#' @return unchanged data frame or error highlighting parsing errors on data import
#' @importFrom purrr map_lgl
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export 

lab_import_checkr.grozny <- function(x) {

  # if specific variables are class == logical must check if data imported correctly
  # define key variables
  check_vars <- c("xpert2err", "mcm", "mam", "mlfx", "mcs", "meth",
                  "ljz", "ctmfx", "ctmfx2", "ctlzd")
  
  # check if variables have been incorrectly parsed as logicals
  class_check <- map_lgl(x[, check_vars], .f = ~ class(.x) == "logical")
  
  if (any(class_check)) stop("Lab variables incorrectly parsed as logicals - check data read args - suggest guess_max adjustment in readr")
  
  x
}
