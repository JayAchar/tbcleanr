#' Convert admission binary variables to factorised output
#'
#' Take admission data frame with binary variables and output
#' binary factorised variables
#' @param x data frame containing admission binary variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export


binary_fixer <- function(x, ...) {

# check input
    assert_that(is.data.frame(x))

# useMethod
    UseMethod("binary_fixer", x)
}


#' Default method for binary_fixer()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar
#' @seealso \code{\link{tbcleanr}}

binary_fixer.default <- function(x, ...) {

  # check whether adm object class can be applied
  y <- adm_classr(x)

  # recycle if class sucessfully applied
  if (length(class(y)) > length(class(x))) {

    binary_fixer(y)

  } else {

    message("No adm object class detected: binary_fixer() not applied.")
    x

  }
}
