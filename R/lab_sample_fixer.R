#' Lab sample fixer
#'
#' Consolidate mycobacterial sample types in Chechen lab data
#' @param x data frame containing sample date variables
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom assertthat assert_that

lab_sample_fixer <- function(x, rm_orig = TRUE, ...) {
  
  # check input
  assert_that(is.data.frame(x))
  
  UseMethod("lab_sample_fixer", x)

}