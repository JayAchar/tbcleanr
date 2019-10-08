#' Subset EpiInfo adm variables
#'
#' Subset pre-specified EpiInfo TB admission variables
#' @inheritParams adm_subset
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @export 

adm_subset.epiinfo <- function(x, add = NULL, ...) {

object_class <- class(x)

# define variables to keep
keep <-
  c(
    "APID",
    "BIRTDATE",
    "SEX",
    "EMPL",
    "EVER",
    "INJECT",
    "ALCO",
    "HOMELESS",
    "HEALTHWO",
    "PRIWO",
    "TOBACCO",
    "REGRP",
    "PRETRAPID",
    "WEIGHT",
    "HEIGHT",
    "NB1",
    "NB2",
    "HD",
    "EE",
    "RR",
    "ZP",
    "CSC",
    "SMS",
    "AMA",
    "KMK",
    "CPX",
    "OFX",
    "DLMDDLM",
    "TT",
    "ETHE",
    "PASP",
    "AMXC",
    "CFZ",
    "CLRC",
    "CMC",
    "OTH",
    "SPE1",
    "SITEDIS",
    "ABNORM",
    "XRAYRES",
    "HIV",
    "STARTTRE",
    "DIPRO",
    "CATTRE",
    "DIABETES",
    "CARDIODI",
    "RENALFAI",
    "PSYCHI",
    "SEIZURE",
    "HEPADIS",
    "HDH",
    "RDR",
    "EDE",
    "ZDZ",
    "SDS",
    "KADKA",
    "OFLDOFL",
    "CAPDCAP",
    "ETHDETH",
    "CYCLDCYCL",
    "AMXDAMX",
    "PASDPAS",
    "CLADCLA",
    "CLODCLO",
    "LXDLX",
    "MXDMX",
    "PTDPT",
    "LZDDLZD",
    "IMPDIMP",
    "BDQDBDQ",
    "PHA",
    "DATEN",
    "RES",
    "NEWAPID",
    "DATEDEAT",
    "TRANDT",
    "OUTD"
  )

## Additional specified variables
k <- c(keep, add)		# add additional requested variables
x <- subset(x, select = k)

class(x) <- object_class

x
}
