#' Confirm correct lab data import
#'
#' @inheritParams lab_import_checkr
#' @return unchanged data frame or error highlighting parsing errors on data import
#' @importFrom purrr map_lgl
#' @author Jay Achar
#' @seealso \code{\link{tbcleanr}}
#' @importFrom stringr str_detect
#' @export 

lab_import_checkr.koch6 <- function(x) {
  
  # if specific variables are class == logical must check if data imported correctly
  # define key variables
  check_vars <- c("RegistrationNb", "Id_Patient", "Id_Bacterio", "Imported", 
                  "Admission", "FollowUp", "PostFollowUp", "Samplecollectiondate", 
                  "Monthofttr", "SmearResult", "SmearNbLab", "CultResult", "CultLabNb", 
                  "GeneXpertResult", "HainResult", "DSTLab1", "DSTLabNumber1", 
                  "DSTMethode1", "DSTResultDate1", "E1", "H1", "R1", "Z1", 
                  "Cm1", "Km1", "S1", "DSTLabNumber2", "DSTMethode2", "DSTResultDate2", 
                  "DSTLab3", "DSTLabNumber3", 
                  "DSTMethode3", "DSTResultDate3")
  
  # check if variables have been incorrectly parsed as logicals
  class_check <- map_lgl(x[, check_vars], .f = ~ class(.x)[1] == "logical")
  
  if (any(class_check)) stop("Lab variables incorrectly parsed as logicals - check data read args - suggest guess_max adjustment in readr, or data set is incomplete")
  
  x
}
