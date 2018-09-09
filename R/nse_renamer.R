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
								"dst_baseliner", "ecg_collectr", "change_cleanr",
								"all_drugs_on_dater"), ...) {

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
# =============================== #

	if (fun == "all_drugs_on_dater") {
		newnames <- c("id", "start", "base_inh", "base_lfx", "base_mfx", "base_bdq", 
										"base_dlm", "base_cfz")
		
		if (software == "epiinfo") {
			oldnames <- c("id", "STARTTRE", "HDH", "LXDLX", "MXDMX", "BDQDBDQ",
                    "DLMDDLM", "CLODCLO")
		}

		if (software == "koch_6") {
									oldnames <- c("id", "Starttre", "H", "Lfx", "Mfx", "Bdq",
											"Dld", "Cfz")
			}
		}


# =============================== #
	if (fun == "baseliner") {
		newnames <- c("id", "samp_date", "start", "end", "culture", "smear")
		
		if (software == "koch_6") {
			oldnames <- c("id", "samp_date", "Starttre", "dateend", "culture", "smear")
		}

		if (software == "epiinfo") {
									oldnames <- c("id", "samp_date", "STARTTRE",
											"DATEN", "culture", "smear")
			}
		}
# =============================== #
		if (fun == "change_cleanr") {
			newnames <- c("id", "date", "bdq_change", "dlm_change", "mfx_change",
										"cfz_change", "lfx_change")

			if (software == "epiinfo") {
				oldnames <- c("APID", "DACHAN", "CXG", "CXI", "CXC", "CCLO", "CXE")
			}
			if (software == "koch_6") {
				oldnames <- c("RegistrationNb", "changedate", "CBdq", "CDld", "CMfx",
											"CCfz", "CLfx")
			}
		}

# =============================== #
	if (fun == "converter") {
		newnames <- c("id", "starttre", "dateend", "culture", "samp_date", "smear")
		
		if (software == "koch_6") {
			oldnames <- c("id", "Starttre", "dateend", "culture", 
						"samp_date", "smear")
		}
		if (software == "epiinfo") {
			oldnames <- c("id", "STARTTRE", "DATEN", "culture",
						"samp_date", "smear")
		}
	}

# =============================== #
	if (fun == "dst_baseliner") {
		newnames <- c("id", "starttre", "dateend", "labno", "samp_date")

		if (software == "epiinfo" & project == "kk") {
			oldnames <- c("id", "STARTTRE", "DATEN", "MICRLABN", "samp_date")
		}
		if (software == "koch_6" & project == "kk") {
			oldnames <- c("id", "Starttre", "dateend", "MICRLABN", "samp_date")
	}
			if (software == "koch_6" & project == "chechnya") {
			oldnames <- c("id", "Starttre", "dateend", "dbno", "samp_date")
	}
	}

# =============================== #
	if (fun == "ecg_collectr") {
		newnames <- c("id", "starttre", "date", "result")

		if (software %in% c("excel", "epiinfo")) {
			oldnames <- c("id", "STARTTRE", "ecg_date", "qtcf")
		}
		if (software == "koch_6") {
			oldnames <- c("id", "Starttre", "ecg_date", "qtcf")
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
