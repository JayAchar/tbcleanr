#' Subset variables
#'
#' Subset variables along pre-defined variable  
#' sets   
#' @param x data frame containing variables
#' @param software define software used for data collection.
#' Values can be "excel", "koch_6", "epiinfo"
#' @param project define project location to apply.
#' Values can be "kk", "chechnya".
#' @param file define database file argument to apply.
#' Values can be "adm", "lab", "clinical_lab",
#' @param add string of any additional variables to keep
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @examples
#' \dontrun{
#' subset_vars(p, set = "msc500")
#' }


subset_vars <- function(x, software = c("excel", "koch_6", "epiinfo"),
								project = c("kk", "chechnya"),
								file = c("adm", "lab", "clinical_lab"), 
								add = NULL, ...) {
# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check all args
	software <- match.arg(software)
	project <- match.arg(project)
	file <- match.arg(file)

# =====================================================
# Koch 6
	if (software == "koch_6") {
		# KK SCR & chechnya adm
			if (file == "adm") {
				k <- c("registrationnb", "dstnumber", "dateofbirth", "datedeat", "gender",
					"weight", "height", "ecgqt", "ecghr", "ecgrr", "ecgqtcf",
					"diabetes", "cardiodi", "renalfail", "cav", "cavD", "labClinDate",
					"Hemoglobin", "Creatinine", "cdhivenrol", "HIV", "CD4count", "Starttre",
					"E", "H", "R", "Z", "Am", "Cm", "Km", "Lfx", "Mfx", 
					"Ofx", "Cs", "Eto", "PAS", "PAS Na", "Pto", "Amx-Clv",
					"Bdq", "Cfz", "Clr", "Dld", "hdH", "ImpCln", "Lzd", "Mpm",
					"dateend", "dateout", "outfirst", "outfirst2013", "2013outcome")
			}

		# All projects clinical lab
			if (file == "clinical_lab") {
				# convert all variables to lower case
				names(x) <- tolower(names(x))
				k <- c("registrationnb", "labclindate", "hemoglobin", "thrombocyt", "ast", "alt",
						"creatinine", "glucose", "potassium", "magnesium", "serumalbumin")
			}
	} 
		

# =====================================================
# EpiInfo
if (software == "epiinfo") {
	# KK EpiInfo admission
		if(project == "kk" & file == "adm") {
		k <- c("APID", "BIRTDATE", "SEX", "EMPL", "EVER", "INJECT", "ALCO", "HOMELESS",
					"HEALTHWO", "PRIWO", "TOBACCO", "REGRP", "PRETRAPID", "WEIGHT", 
					"HEIGHT", "HD", "EE", "RR", "ZP","CSC", "SMS", "AMA", "KMK", "CPX", "OFX",
					"TT", "ETHE","PASP", "AMXC","CFZ", "CLRC","CMC", "OTH", "SPE1", "SITEDIS",
					"ABNORM", "XRAYRES", "HIV", "STARTTRE", "CATTRE", "DIABETES","CARDIODI",
					"RENALFAI","PSYCHI", "SEIZURE", "HEPADIS", "HDH","RDR", "EDE", "ZDZ", "SDS",
					"KADKA", "OFLDOFL", "CAPDCAP", "ETHDETH", "CYCLDCYCL", "AMXDAMX", "PASDPAS",
					"CLADCLA", "CLODCLO", "LXDLX", "MXDMX", "PTDPT", "LZDDLZD", "IMPDIMP",
					"BDQDBDQ", "PHA", "DATEN", "RES", "NEWAPID", "DATEDEAT",
					"TRANDT", "OUTD")
		}
}

# =====================================================
# Excel or Epiinfo
	if (software %in% c("epiinfo", "excel")) {
	# KK Laboratory 	
			if (project == "kk" & file == "lab") {
			k <- c("APID", "MICRLABN", "FIRST", "SECOND", "THIRD", "BK1", "BK2", "BK3",
				"RES", "RES02", "RES03", "RES04", "RESULT", "RESULT02", "RESULT03",
				"RESULT04", "MGITH", "MGITE", "MGITR", "MGITZ", "HAINH",
				"HAINR", "H", "E", "R", "Z", "KM", "OF", "CAP",
				"H1", "E1", "R1", 
				"Z1", "KM1", "OF1", "CAP1",
				"MFX1", "GX_res1", "GX_res2", "GX_res3", "GX_res4")				
			}
	}


		
# =====================================================
# Excel
	if (software == "excel") {
		# Chechnya laboratory
		if (project == "chechnya" & file == "lab") {
		k <- c("dbno", "dstno", "dob", "sputum", "dcol1", "dcol2", "dcol3",
					"micro1", "micro2", "micro3", "xpert1err", "xpert2err", "xpert1res",
					"xpert2res", "xpert1rif", "xpert2rif", "mgitres",
					"ms", "mr", "mh", "mz", "me", "mcm", "mam", "mlfx", "ljres", 
					"ljs", "ljr", "ljh", "ljz", "lje",
					"ctmicres", "ctmgitres",
					"cts", "ctr", "cth", "ctz", "cte", "ctcm", "ctam", "ctlfx", "ctmfx",
					"ctmfx2", "ctlzd", "cthres", "cthrifres", "cthinhres")
		}

		# KK clinical laboratory
		if (project == "kk" & file == "clinical_lab") {
			k <- c("APID", "Test date (dd/mm/yy)", "Test name", "Result", "Comment")
			nms <- c("APID", "date", "test", "result", "comm")
		}


	}  

# =====================================================
## Additional specified variables
		k <- c(k, add)		# add additional requested variables
		x <- subset(x, select = k)

# rename subsetted variables if required
	if (exists("nms")) {
		names(x) <- nms
	}

x
}

