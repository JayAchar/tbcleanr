#' Consolidate Xpert results
#'
#' Take laboratory data set and consolidate Xpert results
#' @inheritParams xpert_result_fixer
#' @author Jay Achar 
#' @seealso \code{\link{tbcleanr}}
#' @export 


xpert_result_fixer.grozny <- function(x, rm_orig = TRUE, ...) {
  
  # define xpert variable names
  x_vars <- c("xpert1err", "xpert2err",
              "xpert1res", "xpert2res",
              "xpert1rif", "xpert2rif")
  
  #check vars are within data frame
  if (! all(x_vars %in% names(x))) {
    stop("Xpert variables are not all present in data frame")
  }
  
  # define sub variables
  err <- x_vars[1:2]
  res <- x_vars[3:4]
  rif <- x_vars[5:6]
  
  # recode errors
  error_adjust <- function(e) {
    e[!is.na(e)] <- 1
    e[is.na(e)] <- 0
    e <- as.numeric(e)
    return(e)
  }
  x[ , err] <- lapply(x[, err], error_adjust)
  
  # recode results applying error corrections
  x$xpert1res[x$xpert1res == "MTB NOT DETECTED"] <- 0
  x$xpert1res[! is.na(x$xpert1res) & ! x$xpert1res == 0] <- 1
  x$xpert1res[x$xpert1err == 1] <- NA_character_
  x$xpert1res <- as.numeric(x$xpert1res)
  
  x$xpert2res[x$xpert2res == "MTB NOT DETECTED"] <- 0
  x$xpert2res[! is.na(x$xpert2res) & ! x$xpert2res == 0] <- 1
  x$xpert2res[x$xpert2err == 1] <- NA
  x$xpert2res <- as.numeric(x$xpert2res)
  
  # consolidate rif results
  rif_adjust <- function(r) {
    r[r == "NOT DETECTED"] <- 0
    r[r == "INDETERMINATE"] <- NA
    r[r == "DETECTED"] <- 1
    r <- as.numeric(r)
    return(r)
  }
  x[ ,rif] <- lapply(x[ ,rif], rif_adjust)
  
  
  # consolidate xpert results
  x$xpert_res <- do.call(pmax, c(x[ , res], na.rm = T))
  
  # consolidate xpert rif result
  x$xpert_rif <- do.call(pmax, c(x[ , rif], na.rm = T))
  
  
  # remove original variables
  if (rm_orig %in% c("TRUE", "T")) {
    x[, x_vars] <- NULL
  }
  
  # factorise final variables
  x$xpert_res <- factor(x$xpert_res, levels = 0:1,
                        labels = c("Negative", "Positive"))
  
  x$xpert_rif <- factor(x$xpert_rif, levels = 0:1,
                        labels = c("Not detected", "Detected"))
  
  # warning if xpert rif result present, but xpert detection negative
  error_detection_neg <- sum(x$xpert_res == "Negative" & ! is.na(x$xpert_rif), na.rm = T)
  if (error_detection_neg > 0) warning("Xpert rif result available when Xpert MTB not detected")
  
  # warning if xpert rif result present, but xpert detection == NA
  error_detection_na <- sum(is.na(x$xpert_res) & ! is.na(x$xpert_rif), na.rm = T)
  if (error_detection_na > 0) warning("Xpert rif result available when Xpert MTB not available")
  
  
  x
  
}
