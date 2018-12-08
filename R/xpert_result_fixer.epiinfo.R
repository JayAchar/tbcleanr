#' Consolidate Xpert results
#'
#' Take laboratory data set and consolidate Xpert results
#' @param x data frame containing variables
#' @param rm_orig remove original variables - TRUE or FALSE
#' @param ... further arguments passed to or from other methods
#' @author Jay Achar \email{jay.achar@@doctors.org.uk}
#' @seealso \code{\link{tbcleanr}}
#' @importFrom purrr map_at map_df
#' @importFrom stringr str_detect

xpert_result_fixer.epiinfo <- function(x, rm_orig = TRUE, ...) {
 # save original class
  class_start <- class(x)
  
  x_vars <- c("GX_res1", "GX_res2", "GX_res3", "GX_res4")
  
  #check vars are within data frame
  if (! all(x_vars %in% names(x))) {
    stop("Xpert variables are not all present in data frame")
  }
  
  # reclassify errors across all variables
  fix_errors <- function(e) {
    # convert invalid results to NA
    e[e %in% c("ERROR", "INVALID", "NO RESULT")] <- NA
    e
  }
  
  # generate MTB result
  x_mtb <- function(m) {
    # not detected = 0
    m[m == "MTB NOT DETECTED"] <- 0
    
    # MTB detected 				
    detected <- str_detect(m, pattern = "MTB DETECTED")
    m[detected] <- 1	
    
    m
  }
  
  # generate rif result
  x_rif <- function(r) {
    # MTB not detected = NA
    not_detect <- str_detect(r, pattern = "MTB NOT DETECTED")
    r[not_detect] <- NA
    
    # not detected
    string <- str_detect(r, pattern = paste(c("Rif Resistance NOT DETECTED", "Rif Resistance INDETERMINATE"), collapse = "|"))
    r[string] <- 0
    
    # detected
    string_detected <- str_detect(r, pattern = "Resistance DETECTED")
    r[string_detected] <- 1
    
    r
  }
  
  # apply error fix to all original variables
  x <- map_at(.x = x, .at = x_vars, .f = fix_errors)
  
  # apply MTB result generator to all original variables
  mtb <- map_df(.x = x[x_vars], .f = x_mtb)
  y_names <- c("mtb1", "mtb2", "mtb3", "mtb4")
  names(mtb) <- y_names
  
  # apply rif resistance generator to all original variables
  resist <- map_df(.x = x[x_vars], .f = x_rif)
  r_names <- c("rif1", "rif2", "rif3", "rif4")
  names(resist) <- r_names
  
  # consolidate xpert results
  x$xpert_res <- do.call(pmax, c(mtb[, y_names], na.rm = T))
  
  # consolidate xpert rif result
  x$xpert_rif <- do.call(pmax, c(resist[, r_names], na.rm = T))
  
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  
  # factorise final variables
  x$xpert_res <- factor(x$xpert_res, levels = 0:1,
                        labels = c("Negative", "Positive"))
  
  x$xpert_rif <- factor(x$xpert_rif, levels = 0:1,
                        labels = c("Not detected", "Detected"))
  
  # remove original variables
  if (rm_orig) {
    x[, x_vars] <- NULL
  }
  
  # warning if xpert rif result present, but xpert detection negative
  error_detection_neg <- sum(x$xpert_res == "Negative" & ! is.na(x$xpert_rif), na.rm = T)
  if (error_detection_neg > 0) warning("Xpert rif result available when Xpert MTB not detected")
  
  # warning if xpert rif result present, but xpert detection == NA
  error_detection_na <- sum(is.na(x$xpert_res) & ! is.na(x$xpert_rif), na.rm = T)
  if (error_detection_na > 0) warning("Xpert rif result available when Xpert MTB not available")

  # reapply starting class  
  class(x) <- class_start
  
  x
}
