#' Subset adm variables
#'
#' Subset pre-specified TB admission variables
#' @param x data frame containing variables
#' @param add string of any additional variables to keep
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export

adm_subset <- function(x, add = NULL, ...) {

# check arg
    assert_that(is.character(add) | is.null(add))
    assert_that(is.data.frame(x))

    UseMethod("adm_subset", x)
}


#' Default method for adm_subset()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @param add string of any additional variables to keep
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar
#' @seealso \code{\link{tbcleanr}}
#' @export

adm_subset.default <- function(x, add = NULL, ...) {

  # check whether adm object class can be applied
  y <- adm_classr(x)

  # recycle if class sucessfully applied
  if (length(class(y)) > length(class(x))) {

    adm_subset(y, add = add)

  } else {

    message("No adm object class detected: adm_subset() not applied.")
    x
  }
}
