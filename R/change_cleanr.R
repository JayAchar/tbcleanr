#' Clean drug change data
#'
#' Take drug change data from routine TB programmes and clean for
#' further analysis.
#'
#' @param x data frame containing drug variables
#' @param add string of variables to include in output without cleaning
#' @author Jay Achar
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export

change_cleanr <- function(x) {

  # check input
  assert_that(is.data.frame(x))

  # apply useMethod approach
  UseMethod("change_cleanr", x)
}



#' Default method for change_cleanr()
#'
#' Allow data frames with unspecified object class to pass through
#' @inheritParams change_cleanr
#' @author Jay Achar
#' @seealso \code{\link{tbcleanr}}
#' @export

change_cleanr.default <- function(x) {


  k6_names <- c("RegistrationNb", "changedate", "SEdescrip", "Datepi")
  epiinfo_names <- c("APID", "DACHAN", "STARTTRE", "MEDTT")

  if (all(k6_names %in% names(x))) {

    class(x) <- c(class(x), "koch6")
    change_cleanr(x)

  } else if (all(epiinfo_names %in% names(x))) {
    class(x) <- c(class(x), "epiinfo")
    change_cleanr(x)

  } else {

    message("No change object class detected: change_cleanr() not applied.")
    x
  }

}
