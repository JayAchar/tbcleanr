#' Lab sample fixer
#'
#' Consolidate mycobacterial sample types in Chechen lab data
#' @param x data frame containing sample date variables
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @importFrom assertthat assert_that

lab_sample_fixer <- function(x, rm_orig = TRUE, ...) {

  # check input
  assert_that(is.data.frame(x))

  UseMethod("lab_sample_fixer", x)

}


#' Default method for lab)sample_fixer()
#'
#' Allow data frames with unspecified object class to pass through
#' @param x data frame containing variables
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export

lab_sample_fixer.default <- function(x, ...) {

  if ("epiinfo" %in% class(x)) {

    message("lab_sample_fixer() not applied to EpiInfo lab data")
    return(x)

    } else {

    message("No lab object class detected: lab_sample_fixer() not applied.")
    return(x)

    }

}
