#' Split admission ID number
#'
#' Take admission ID number, check that it is unique and generate additional information
#' @param x data frame containing admission ID variable
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom assertthat assert_that

id_detangle <- function(x, ...) {

    # check input
    assert_that(is.data.frame(x))

    # UseMethod
    UseMethod("id_detangle", x)
}


#' Default method for id_detangle()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing admission ID variable
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar
#' @seealso \code{\link{tbcleanr}}
#' @export

id_detangle.default <- function(x, ...) {

  # check whether adm object class can be applied
  y <- adm_classr(x)

  # recycle if class sucessfully applied
  if (length(class(y)) > length(class(x))) {

    id_detangle(y)

  } else {

    message("No adm object class detected: id_detangle() not applied.")
    x

  }

}
