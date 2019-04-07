#' Subset Koch 6 adm variables
#'
#' Subset pre-specified Koch 6 TB admission variables
#' @param x data frame containing variables
#' @param add string of any additional variables to keep
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export 

adm_subset.koch6 <- function(x, add = NULL, ...) {

object_class <- class(x)        

# define variables to keep
keep <-
  c(
    "registrationnb",
    "dstnumber",
    "dateofbirth",
    "datedeat",
    "gender",
    "weight",
    "height",
    "cdhistory",
    "cdstrainprofil",
    "cdstrainconf",
    "ecgqt",
    "ecghr",
    "ecgrr",
    "ecgqtcf",
    "diabetes",
    "cardiodi",
    "renalfail",
    "cav",
    "cavD",
    "XRay_Finding",
    "cavsimple",
    "labClinDate",
    "Hemoglobin",
    "Creatinine",
    "cdhivenrol",
    "HIV",
    "CD4count",
    "Starttre",
    "E",
    "H",
    "R",
    "Z",
    "Am",
    "Cm",
    "Km",
    "Lfx",
    "Mfx",
    "Ofx",
    "Cs",
    "Eto",
    "PAS",
    "PAS Na",
    "Pto",
    "Amx-Clv",
    "Bdq",
    "Cfz",
    "Clr",
    "Dld",
    "hdH",
    "ImpCln",
    "Lzd",
    "Mpm",
    "dateend",
    "dateout",
    "outfirst",
    "outfirst2013",
    "2013outcome"
  )

## Additional specified variables
k <- c(keep, add)		# add additional requested variables
x <- subset(x, select = k)

class(x) <- object_class
x
}
