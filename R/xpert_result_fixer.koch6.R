#' Consolidate Xpert results
#'
#' Take laboratory data set and consolidate Xpert results
#' @inheritParams xpert_result_fixer
#' @author Jay Achar 
#' @importFrom dplyr filter select mutate left_join
#' @importFrom tidyr gather
#' @importFrom rlang .data
#' @seealso \code{\link{tbcleanr}}
#' @export 


xpert_result_fixer.koch6 <- function(x, rm_orig = TRUE, ...) {
  
  . <- NULL
  
  #check vars are within data frame
  if (! all("GeneXpertResult" %in% names(x))) {
    stop("Xpert variables are not all present in data frame")
  }
  
  # generate xpert result variable
  
  x$xpert_res <- ifelse(x$GeneXpertResult == 0 | x$GeneXpertResult == 3, NA_integer_,
                        ifelse(x$GeneXpertResult == 2, 0, 1))
  
  x$xpert_res <- factor(x$xpert_res, levels = 0:1,
                        labels = c("Negative", "Positive"))
  
  # extract xpert rif result 
  extract_xpert_rif <- function(x) {
    # check that id number is unique
    length(unique(x$Id_Bacterio)) == nrow(x)
    
    xpert_dst_id <- x %>% 
      # keep only Xpert positive samples
      dplyr::filter(.data$GeneXpertResult == 1) %>% 
      dplyr::select(.data$RegistrationNb, .data$Id_Bacterio, grep("^DSTMethod", names(.))) %>% 
      tidyr::gather(key = "dst_number", value = "dst_type", -.data$RegistrationNb, -.data$Id_Bacterio) %>% 
      dplyr::filter(.data$dst_type == "GeneXpert") %>% 
      # generate variable to identify which Rif result to extract for Xpert
      dplyr::mutate(rif_var = paste0("R", stringr::str_extract(.$dst_number, pattern = "[0-9]$"))) %>% 
      dplyr::select(.data$Id_Bacterio, .data$rif_var)
    
    xpert_results <- x %>% 
      dplyr::inner_join(xpert_dst_id, by = "Id_Bacterio") %>% 
      select(.data$Id_Bacterio, .data$rif_var, grep("^R[0-9]$", names(.))) %>% 
      tidyr::gather(key = "rif_dst", value = "rif_result", -.data$Id_Bacterio, -.data$rif_var) %>% 
      filter(.data$rif_dst == .data$rif_var) %>% 
      select(.data$Id_Bacterio, 
             xpert_rif = .data$rif_result) %>% 
      mutate(xpert_rif = ifelse(.data$xpert_rif == 2, 0, .data$xpert_rif),
             xpert_rif = factor(.data$xpert_rif, levels = 0:1,
                                labels = c("Not detected", "Detected")))  
    
    x %>% 
      dplyr::left_join(xpert_results, by = "Id_Bacterio")
  }
  
  x <- extract_xpert_rif(x)
  
  
  # warning if xpert rif result present, but xpert detection negative
  error_detection_neg <- sum(x$xpert_res == "Negative" & ! is.na(x$xpert_rif), na.rm = T)
  if (error_detection_neg > 0) warning("Xpert rif result available when Xpert MTB not detected")
  
  # warning if xpert rif result present, but xpert detection == NA
  error_detection_na <- sum(is.na(x$xpert_res) & ! is.na(x$xpert_rif), na.rm = T)
  if (error_detection_na > 0) warning("Xpert rif result available when Xpert MTB not available")
  
  if (rm_orig) x$GeneXpertResult <- NULL
  
  x
}
