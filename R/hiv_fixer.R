#' Factorise HIV variable
#'
#' Take HIV variables from data frame and factorise. Use software
#' to define which data base is being used as the input
#' @param x data frame containing Koch 6 admission variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export

hiv_fixer <- function(x, ...) {

# check input
  assert_that(is.data.frame(x))

# call correct method
    UseMethod("hiv_fixer", x)
x
}


#' Default method for hiv_fixer()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar
#' @seealso \code{\link{tbcleanr}}
#' @export

hiv_fixer.default <- function(x, ...) {

  # check whether adm object class can be applied
  y <- adm_classr(x)

  # recycle if class sucessfully applied
  if (length(class(y)) > length(class(x))) {

    adm_subset(y)

  } else {

    message("No adm object class detected: hiv_fixer() not applied.")
    x
  }

}
