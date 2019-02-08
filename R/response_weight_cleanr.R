#' Treatment response weight cleanr
#'
#' @param x adherence data frame including monthly weight measurements
#'
#' @return data frame with treatment month and weight per patient. Duplicate
#' monthly measurements are removed with weights closest to the preceeding month
#' retained. 
#' @seealso \code{\link{tbcleanr}}
#' @author Jay Achar 
#' @importFrom assertthat assert_that
#' @export
#'

response_weight_cleanr <- function(x) {
  
  assert_that(is.data.frame(x))
  
  UseMethod("response_weight_cleanr", x)
}
