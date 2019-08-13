#' Retrieve look-up value
#'
#' @param key original name(s) representing keys in look-up table. These are used
#' to identify the correct value to be outputted by the function
#' @param table_name single string identifying internal package look-up tables. Current
#' options include "oca_missions" and "oca_projects"
#'
#' @importFrom assertthat assert_that
#'
#' @return character vector of values from look-up table with equal length as input "key"
#' arguement
#'

get_lookup_value <- function(key,
                             table_name) {
  # check args
  assert_that(is.character(key),
              length(table_name) > 1 & is.character(table_name))
  
  
  # available lookup tables in tbcleanr
  lookup_tables <- c("oca_missions", "oca_projects")
  # convert lookup table object name to string
  table_name_string <- deparse(substitute(table_name))
  
  if (!(table_name_string %in% lookup_tables)) {
    stop("Look-up table not found - respecify in table_name arg")
  }
  
  # remove vector name from output
  unname(table_name[key])
  
}




#' Extract TB data from imported Standard Indicators object
#'
#' @param x data frame of imported Standard Indicators data
#'
#' @importFrom assertthat assert_that
#' @return data frame
#'

extract_tb_data <- function(x) {
  assert_that(is.data.frame(x))
  
  # first row to extracct
  first_tb_row <- which(x[, 1] == "Tuberculosis (Quarterly data)")
  if (length(first_tb_row) != 1) {
    stop("TB data not detected - review raw data")
  }
  
  # last row to extract
  
  if (any(x[, 1] == "Total known outcomes", na.rm = T)) {
    last_tb_row <- which(x[, 1] == "Total known outcomes")
  } else {
    last_tb_row <- which(x[, 1] == "Failure rate (TB)")
  }
  
  if (length(last_tb_row) != 1) {
    stop("TB data not detected - review raw data")
  }
  
  # check that data is formatted as expected
  rows <- last_tb_row - (first_tb_row + 2)
  print(paste(rows, "rows of data have been extracted", sep = " "))
  if (rows != 23) {
    warning(
      paste(
        "extract_tb_data: Only",
        rows,
        "rows of data were extracted. This may not be sufficient. Please check the raw data",
        sep = " "
      )
    )
  }
  
  # remove first two rows of TB data - not required
  first_tb_row <- first_tb_row + 2
  
  # subset data
  tb <- x[c(1, first_tb_row:last_tb_row),]
  
  assert_that(is.data.frame(tb))
  return(tb)
}


#' Clean and transpose Standard Indicators object
#'
#' @param df data frame of imported and extracted TB Standard Indicators
#' @param year character of length 1 representing the year of data collected
#'
#' @importFrom assertthat assert_that
#' @importFrom purrr map_at
#' @return data frame of tidy TB Standard Indicators data
#'

transpose_tb_data <- function(df,
                              year) {
  # check arg
  assert_that(is.data.frame(df))
  
  # how many rows in df?
  row_count <- nrow(df)
  
  
  # add year as row
  df[2,] <- year
  df[2, 1] <- "year"
  
  # remove rows not required
  df <- df[-3, ]
  df <- df[-c(12:14), ]
  df <- df[-5, ]
  
  # remove columns not required
  if (colnames(df)[3] != "projects") {
    stop(
      "Extracted TB data appears to have the wrong number of columns - columns 2-7 are destined to be dropped."
    )
  }
  df <- df[,-c(2:7)]
  
  # rename first column (variables)
  if (row_count == 25) {
    var_names <- c(
      "country",
      "year",
      "new_adm",
      "new_retx",
      "mdr_tx",
      "dr_tx",
      "new_hiv_status",
      "prop_new_hiv_status",
      "new_tb_hiv_pos",
      "prop_new_tb_hiv_pos",
      "out_cohort",
      "success",
      "prop_success",
      "ltfu",
      "prop_ltfu",
      "death",
      "prop_death",
      "fail",
      "prop_fail",
      "total_outcomes"
    )
  }
  if (row_count == 24) {
    var_names <- c(
      "country",
      "year",
      "new_adm",
      "new_retx",
      "mdr_tx",
      "dr_tx",
      "new_hiv_status",
      "prop_new_hiv_status",
      "new_tb_hiv_pos",
      "prop_new_tb_hiv_pos",
      "out_cohort",
      "success",
      "prop_success",
      "ltfu",
      "prop_ltfu",
      "death",
      "prop_death",
      "fail",
      "prop_fail"
    )
  }
  
  df[, 1] <- var_names
  
  # transpose
  df <- data.frame(t(df), stringsAsFactors = F)
  
  # fix transposed version of df
  colnames(df) <- df[1,]
  
  # remove first row
  df <- df[-1,]
  
  # change rownames to varialbe = project
  project_names <- rownames(df)
  df <- cbind(project_names, df, stringsAsFactors = FALSE)
  rownames(df) <- NULL
  colnames(df)[1] <- "project"
  
  # format columns
  df <- purrr::map_at(df, c(3:21), as.numeric)
  
  df <- data.frame(df, stringsAsFactors = FALSE)
  
  df
}


#' Xpert variable detector
#'
#' Find Xpert variables within data frame by using Regex. Adjust
#' when lab changes variable names
#' @param x data frame with xpert variables
#' @importFrom stringr str_detect
#' @return string of variable names matching regex for Xpert results

xpert_variable_detector <- function(x) {
  . <- NULL
  # new xpert variable names added in late 2018
  xpert_variable_regex <- c("^g[en]*x_res\\d$")
  
  xpert_additions <- names(x) %>%
    tolower() %>%
    str_detect(pattern = xpert_variable_regex) %>%
    subset(names(x), subset = .)
  
  xpert_additions
}


#' Drug adherence
#'
#' Calculate monthly drug adherence percentage
#' @param df data frame with EpiInfo "Follow" data including all monthly adherence data
#' @param drug string representing drug to be analysed
#' @importFrom stringr str_which
#' @importFrom assertthat assert_that
#' @importFrom dplyr %>%  group_by summarise_at ungroup
#' @importFrom rlang .data

drug_adhere <- function(df,
                        drug) {
  
  assertthat::assert_that(is.data.frame(df),
                          length(drug) == 1,
                          is.character(drug))
  
  # generate string to match drug variables
  start_var_string <- c("ND", "P", "N", "D", "NO", "O", "NM", "M", "R")
  
  if (any(drug %in% drug_adhere_vars$drug_abb)) {
    drug <- drug_adhere_vars$var_name[drug_adhere_vars$drug_abb == drug]
  }
  
  drug_string <- paste0("^", start_var_string, drug, "$")
  
  if (drug == "R") {
    drug_string[6] <- "ORI"
  } else if (drug == "CLA") {
    drug_string[9] <- "WCLA"
  }
  
  # match numerical position of drug variables in data frame
  drug_vars <- stringr::str_which(names(df), pattern = paste0(drug_string, collapse = "|"))
  
  assertthat::assert_that(length(drug_vars) == 9L)
  
  # add all monthly figures by APID 
  df <- df %>% 
    dplyr::group_by(.data$APID, .data$FOLAFT) %>% 
    dplyr::summarise_at(.funs = sum,
                 # na.rm = TRUE,
                 .vars = drug_vars - 2) %>% 
    ungroup()
  
  # match numerical position of drug variables in data frame
  drug_vars <- stringr::str_which(names(df), pattern = paste0(drug_string, collapse = "|"))
  
  # check that missed cumulative dosing is correctly calculated
  incorrect_missing_dose <- sum(!df[, drug_vars[8]] == df[, drug_vars[4]] - df[, drug_vars[6]], na.rm = TRUE)
  
  if (incorrect_missing_dose > 0) {
    message(paste0(incorrect_missing_dose, " rows have incorrectly calculated missing dosage results"))
  }
  
  # check that ingested dose < prescribed drug dose
  inconsistent_dosing <- sum(!df[, drug_vars[6]] <= df[, drug_vars[4]], na.rm = TRUE)
  
  if (inconsistent_dosing > 0) {
    warning(paste0(inconsistent_dosing, " rows have incorrectly recorded dosing data"))
  }
  
  # calculate monthly percentage adherence using cumulative dosing variables
  df$dose_adhere <- df[[drug_vars[6]]] / df[[drug_vars[4]]] * 100
  
  # convert all NaN to NA_integer where both dose prescribed and dose taken == 0
  df$dose_adhere[df[[6]] == 0 & df[[4]] == 0] <- NA_integer_ 
  
  # format and rename output
  out <- df[, c("APID", "FOLAFT", "dose_adhere")]
  
  if (any(drug %in% drug_adhere_vars$var_name)) {
    drug <- drug_adhere_vars$drug_abb[drug_adhere_vars$var_name == drug]
  }
  
  # rename output variables
  names(out) <- c("APID", "tx_month", paste0("adhere_pct_", drug))
  
  out
}
