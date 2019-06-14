#' Convert drug doses to binary output
#'
#' Take data frame with drug dosing variables and output
#' binary factorised variables
#' @param x data frame containing drug variables
#' @param ... further arguments passed to or from other methods.
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export

drug_fixer <- function(x, ...) {
# check input
    assert_that(is.data.frame(x))

# apply useMethod approach
    UseMethod("drug_fixer", x)
}


#' Default method for drug_fixer()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar
#' @seealso \code{\link{tbcleanr}}
#' @export

drug_fixer.default <- function(x, ...) {

  # check whether adm object class can be applied
  y <- adm_classr(x)

  # recycle if class sucessfully applied
  if (length(class(y)) > length(class(x))) {

    adm_subset(y)

  } else {

     message("No adm object class detected: drug_fixer() not applied.")
    x
  }

}
