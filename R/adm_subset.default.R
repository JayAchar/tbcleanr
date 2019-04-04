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
