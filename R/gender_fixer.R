#' Factorise gender variable
#'
#' Take gender variable from data frame and factorise. Requires that variable names
#' have been pre-adjusted.
#' @param x data frame containing Koch 6 admission variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom assertthat assert_that

gender_fixer <- function(x, ...) {

# check input
    assert_that(is.data.frame(x))

# UseMethod
    UseMethod("gender_fixer", x)
}


#' Default method for gender_fixer()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar
#' @seealso \code{\link{tbcleanr}}
#' @export

gender_fixer.default <- function(x, ...) {

  # check whether adm object class can be applied
  y <- adm_classr(x)

  # recycle if class sucessfully applied
  if (length(class(y)) > length(class(x))) {

    adm_subset(y)

  } else {
    message("No adm object class detected: gender_fixer() not applied.")
    x

  }

}
