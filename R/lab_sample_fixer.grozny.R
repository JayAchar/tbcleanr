#' Lab sample fixer
#'
#' Consolidate mycobacterial sample types in Chechen lab data
#' @param x data frame containing sample date variables
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom stringr str_detect
#' @export 

lab_sample_fixer.grozny <- function(x, rm_orig = TRUE, ...) {
  
  # define variable name
  v <- c("sputum")
  
  # check variable present
  if (! v %in% names(x)) {
    stop("Sample type variable not available")
  }
  
  # check variable is.character
  if (! is.character(x[[v]])) {
    stop("Sample type variable is not a string variable")
  }
  
  # new variable for EPTB and sputum specimens
  x$sample <- NA_integer_
  
  # find sputum specimens
  s <- which(str_detect(x[[v]], "Sputum"))
  # find EPTB specimens
  e <- which(str_detect(x[[v]], "EPTB"))
  
  
  # recode sample variable
  x$sample[s] <- 1
  x$sample[e] <- 2
  
  # factorise sample variable
  x$sample <- factor(x$sample, levels = 1:2, 
                     labels = c("Sputum", "EP sample"))
  
  # how many NA's introduced
  sp <- sum(is.na(x[[v]]))
  sa <- sum(is.na(x$sample))
  p <- sa - sp
  if (! identical(p, 0)) message(paste0("lab_sample_fixer: NAs introduced = ", p))
  
  # remove original variable
  if (rm_orig) {
    x[[v]] <- NULL
  }
  
  x
}
