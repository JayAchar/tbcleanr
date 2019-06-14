#' Clean drug adherence data
#'
#' Take drug adherence data from routine TB programmes and clean for
#' further analysis.
#'
#' @param x data frame containing drug adherence data
#' @author Jay Achar
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export

adhere_cleanr <- function(x) {

  # check input
  assert_that(is.data.frame(x))

  # apply useMethod approach
  UseMethod("adhere_cleanr", x)
}


#' Default method for adhere_cleanr()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @author Jay Achar
#' @seealso \code{\link{tbcleanr}}
#' @export

adhere_cleanr.default <- function(x) {

  # check whether adm object class can be applied
  y <- adhere_classr(x)

  # recycle if class sucessfully applied
  if (length(class(y)) > length(class(x))) {

    adhere_cleanr(y, adm = adm)

  } else {

    message("No adherence object class detected: adhere_cleanr() not applied.")
    x
  }


}
