#' Fix TB treatment outcomes
#'
#' Combine treatment outcome variables to leave one
#' factorised, labelled variable
#' @param x data frame containing outcome variables
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar
#' @importFrom assertthat assert_that is.flag
#' @seealso \code{\link{tbcleanr}}
#' @export

outcome_fixer <- function(x,
                          rm_orig = TRUE,
                          ...) {

    # check input
    assert_that(is.data.frame(x),
                is.flag(rm_orig))

    # apply correct method
    UseMethod("outcome_fixer", x)

}



#' Default method for outcome_fixer()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar
#' @seealso \code{\link{tbcleanr}}
#' @export

outcome_fixer.default <- function(x, ...) {

  # check whether adm object class can be applied
  y <- adm_classr(x)

  # recycle if class sucessfully applied
  if (length(class(y)) > length(class(x))) {

    outcome_fixer(y)

  } else {

    message("No adm object class detected: outcome_fixer() not applied.")
    x
  }

}
