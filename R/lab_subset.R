#' Subset lab variables
#'
#' Subset pre-specified TB laboratory variables
#' @param x data frame containing variables
#' @param add string of any additional variables to keep
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom assertthat assert_that
#' @export


lab_subset <- function(x, add = NULL, ...) {
  
# check input
  assert_that(is.character(add) | is.null(add))
	assert_that(is.data.frame(x))

# apply useMethod
  UseMethod("lab_subset", x)

}

# # =====================================================
# # Koch 6
# 	if (software == "koch_6") {
# 
# 		# All projects clinical lab
# 			if (file == "clinical_lab") {
# 				# convert all variables to lower case
# 				names(x) <- tolower(names(x))
# 				k <- c("registrationnb", "labclindate", "hemoglobin", "thrombocyt", "ast", "alt",
# 						"creatinine", "glucose", "potassium", "magnesium", "serumalbumin")
# 			}
# 	} 
# 		
# # Excel or Epiinfo
# 	if (software %in% c("epiinfo", "excel")) {
# 
# 
# 	# KK clinical laboratory
# 		if (project == "kk" & file == "clinical_lab") {
# 			k <- c("APID", "Test date (dd/mm/yy)", "Test name", "Result", "Comment")
# 			nms <- c("APID", "date", "test", "result", "comm")
# 		}
# 	}
# 
# 
# 
# x


