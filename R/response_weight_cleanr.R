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

  epiinfo_names <- c("FOLAFT", "STARTTRE", "MEDTTD", "PHASE", "WEIGHT")

  if (all(epiinfo_names %in% names(x))) {

    class(x) <- c("epiinfo", class(x))
    response_weight_cleanr(x)

  } else {

    message("No BMI object class detected: response_weight_cleanr() not applied.")

  }

}
