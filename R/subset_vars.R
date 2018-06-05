#' Subset variables
#'
#' Subset variables along pre-defined variable  
#' sets   
#' @param x data frame containing variables
#' @param set define variable set to apply. Values can be "msc500", "chechnya_myco_lab",
#' "k6_adm_standard", "nukus_epi_info"
#' @param add string of any additional variables to keep
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @export
#' @examples
#' \dontrun{
#' subset_vars(p, set = "msc500")
#' }


subset_vars <- function(x, set, add = NULL,  ...) {
# acceptable values for "set" arg
	s <- c("msc500", "chechnya_myco_lab", "k6_adm_standard", "nukus_epi_info",
			"nukus_clin_lab", "k6_clin_lab", "nukus_myco_lab")

# check input
	if (!(is.data.frame(x))) {
			stop("input paramter, x, must be a data frame")
	}

# check set is within acceptable values
	if (! set %in% s) {
		stop("Specify set argument within specified values")
	}
# =====================================================
# set = msc500
	if (set == "msc500") {
		k <- c("registrationnb", "dstnumber", "dateofbirth", "datedeat", "gender",
					"weight", "height", "ecgqt", "ecghr", "ecgrr", "ecgqtcf",
					"diabetes", "cardiodi", "renalfail", "cav", "cavD", "labClinDate",
					"Hemoglobin", "Creatinine", "cdhivenrol", "HIV", "CD4count", "Starttre",
					"E", "H", "R", "Z", "Am", "Cm", "Km", "Lfx", "Mfx", 
					"Ofx", "Cs", "Eto", "PAS", "PAS Na", "Pto", "Amx-Clv",
					"Bdq", "Cfz", "Clr", "Dld", "hdH", "ImpCln", "Lzd", "Mpm",
					"dateend", "dateout", "outfirst", "outfirst2013", "2013outcome")
	}
# =====================================================
	if (set == "k6_adm_standard") {
		k <- c("registrationnb", "dstnumber", "dateofbirth", "datedeat", "gender",
					"weight", "height",
					"diabetes", "cardiodi", "renalfail", "cav", "cavD", "labClinDate",
					"Hemoglobin", "Creatinine", "cdhivenrol", "HIV", "CD4count", "Starttre",
					"E", "H", "R", "Z", "Am", "Cm", "Km", "Lfx", "Mfx", 
					"Ofx", "Cs", "Eto", "PAS", "PAS Na", "Pto", "Amx-Clv",
					"Bdq", "Cfz", "Clr", "Dld", "hdH", "ImpCln", "Lzd", "Mpm",
					"dateend", "dateout", "outfirst", "outfirst2013", "2013outcome")
	}
# =====================================================
# set = nukus_epi_info
if (set == "nukus_epi_info") {
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
# =====================================================
# set = nukus_myco_lab
	if (set == "nukus_myco_lab") {
		k <- c("APID", "REAS", "MICRLABN", "FIRST", "SECOND", "THIRD", "BK1", "BK2", "BK3",
			"RES", "RES02", "RES03", "RES04", "RESULT", "RESULT02", "RESULT03",
			"RESULT04", "MGITH", "MGITE", "MGITR", "MGITS", "MGITZ", "HAINH",
			"HAINR", "HSMPL", "H", "E", "R", "S", "Z", "KM", "OF", "CAP", "ETH",
			"CS", "PAS", "AMI", "OTHER1", "SPEC", "OTHER2", "SPECI", "H1", "E1", "R1", 
			"S1", "Z1", "KM1", "OF1", "CAP1", "ETH1", "CS1", "PAS1", "AMI1",
			"MFX1", "LZD1", "GX_res1", "GX_res2", "GX_res3")
	}  

# =====================================================
# set = chechnya_myco_lab
	if (set == "chechnya_myco_lab") {
		k <- c("dbno", "dstno", "dob", "sputum", "dcol1", "dcol2", "dcol3",
					"micro1", "micro2", "micro3", "xpert1err", "xpert2err", "xpert1res",
					"xpert2res", "xpert1rif", "xpert2rif", "mgitres",
					"ms", "mr", "mh", "mz", "me", "mcm", "mam", "mlfx", "ljres", 
					"ljs", "ljr", "ljh", "ljz", "lje",
					"ctmicres", "ctmgitres",
					"cts", "ctr", "cth", "ctz", "cte", "ctcm", "ctam", "ctlfx", "ctmfx",
					"ctmfx2", "ctlzd", "cthres", "cthrifres", "cthinhres")
	}
# =====================================================
# set = nukus_clin_lab
	if (set == "nukus_clin_lab") {
		k <- c("APID", "Test date (dd/mm/yy)", "Test name", "Result", "Comment")
		nms <- c("APID", "date", "test", "result", "comm")
	}
# =====================================================
# set = k6_clin_lab
	if (set == "k6_clin_lab") {
		# convert all variables to lower case
		names(x) <- tolower(names(x))
		k <- c("registrationnb", "labclindate", "hemoglobin", "thrombocyt", "ast", "alt",
				"creatinine", "glucose", "potassium", "magnesium", "serumalbumin")
	}
# =====================================================


		k <- c(k, add)		# add additional requested variables
		x <- subset(x, select = k)

# rename subsetted variables if required
	if (exists("nms")) {
		names(x) <- nms
	}

x
}

