#' Non-Standard Evaluation variable renamer
#'
#' Rename variables to standard sets to allow use of NSE packages
#' @param x data frame
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @param fun string to define which function requires NSE renaming
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @export
#' @seealso \code{\link{tbcleanr}}
#' @examples
#' \dontrun{
#' nse_renamer(p, software = "koch_6", project = "chechnya", file = "adm", fun = "drug_duration")
#' }


nse_renamer <- function(x, software = c("excel", "koch_6", "epiinfo"),
						project = c("kk", "chechnya"),
						file = c("adm", "lab", "clinical_lab"), 
						fun = c("baseliner", "converter", "drug_duration",
								"dst_baseliner", "ecg_collectr"), ...) {

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check all args
	software <- match.arg(software)
	project <- match.arg(project)
	file <- match.arg(file)
	fun <- match.arg(fun)


# ======================================================
# generate variables
#	if (fun == "drug_duration") {
#		newnames <- c("id", "change", "start", "end", "drug", "drug_change",
#									"change_dose")
		
#		if (project == "chechnya") {
#					dc <- paste0("C", arguments$drug)
#					cd <- paste0("N", arguments$drug)
#			oldnames <- c("RegistrationNb", "changedate", "Starttre",
#									 "dateend", arguments$drug, dc, cd)
#			}
#	}

	if (fun == "baseliner") {
		newnames <- c("id", "samp_date", "start", "end", "culture", "smear")
		
		if (project == "chechnya") {
			oldnames <- c("id", "samp_date", "Starttre", "dateend", "culture", "smear")
		}

		if (project == "kk") {
			if (software %in% c("epiinfo", "excel")) {
							oldnames <- c("id", "samp_date", "STARTTRE",
											"DATEN", "culture", "smear")
			}
		}
	}

	if (fun == "converter") {
		newnames <- c("id", "starttre", "dateend", "culture", "samp_date", "smear")
		
		if (project == "chechnya") {
			oldnames <- c("idno", "Starttre", "dateend", "culture", 
						"samp_date", "smear")
		}

		if (project == "kk") {
			oldnames <- c("id", "STARTTRE", "DATEN", "culture",
						"samp_date", "smear")
		}
	}

	if (fun == "dst_baseliner") {
		newnames <- c("idno", "starttre", "dateend", "labno", "samp_date")

		if (project == "kk") {
			oldnames <- c("id", "STARTTRE", "DATEN", "MICRLABN", "samp_date")
		}
	}

	if (fun == "ecg_collectr") {
		newnames <- c("id", "starttre", "dateend", "date", "test", "result")

		if (project == "kk") {
			oldnames <- c("id", "STARTTRE", "DATEN", "date", "test", "result")
		}
	}
	
# ======================================================
# check old and new args are same length
		if (! identical(length(oldnames), length(newnames))) {
			stop("New var names and old var names should be the same length")
			}
# ======================================================
# rename variables
		# locate the oldnames in the dataframe
	place <- match(oldnames, names(x))
		# change from oldnames to newnames
	names(x)[place] <- newnames

x
}
