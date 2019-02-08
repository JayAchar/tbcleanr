#' Treatment response weight cleanr
#'
#' @param x adherence data frame including monthly weight measurements
#'
#' @return data frame with treatment month and weight per patient. Duplicate
#' monthly measurements are removed with weights closest to the preceeding month
#' retained. 
#' @seealso \code{\link{tbcleanr}}
#' @author Jay Achar 
#' @export
#'

response_weight_cleanr.default <- function(x) {
  
  k6_bmi_names <- c("RegistrationNb", "RxMonth", "Id_BMI", "weight", "BMI")
  
  if (all(k6_bmi_names %in% names(x))) {

    class(x) <- c(class(x), "koch6")
    response_weight_cleanr(x)
    
  } else {
    
    message("No BMI object class detected: response_weight_cleanr() not applied.")  
    
  }

}
