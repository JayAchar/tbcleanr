library(magrittr)
new_patient <- function(id_number, 
                        month, 
                        change_dose,
                        prescribed_dose,
                        prescribed_days,
                        total_dose,
                        no_obs_days,
                        total_obs_dose,
                        missed_days,
                        missed_dose,
                        reason) {
  
  id <- paste0("XYZ", id_number)
  FOLAFT <- month
  NDH <- change_dose
  PH <- prescribed_dose
  NH <- prescribed_days
  DH <- total_dose
  NOH <- no_obs_days
  OH <- total_obs_dose
  NMH <- missed_days
  MH <- missed_dose
  RH <- reason
  
  out <- data.frame(APID = id,
                    FOLAFT = FOLAFT,
                    var1 = rnorm(1),
                    var2 = rnorm(1),
                    NDH = NDH,
                    PH = PH,
                    NH = NH,
                    DH = DH,
                    NOH = NOH,
                    OH = OH,
                    NMH = NMH,
                    MH = MH,
                    RH = RH,
                    NDETH = rbinom(1, 10, 0.5),
                    var4 = rbinom(1, 10, 0.3),
                    stringsAsFactors = FALSE)
  out
}

# define each patient
records <- list(
  # one month of dosing data
  list(1, 1, NA_integer_, 0, NA_integer_, 12000, NA_integer_, 8000, NA_integer_, 4000, 5),
  # no Z drug data
  list(2, 1, NA_integer_, 0, NA_integer_, 0, NA_integer_, 0, NA_integer_, 0, NA_integer_),
  # missed all dosage
  list(3, 1, NA_integer_, 0, NA_integer_, 12000, NA_integer_, 0, NA_integer_, 12000, 3),
  # duplicate months
  list(4, c(1, 2, 2), NA_integer_, 0, NA_integer_, c(12000, 8000, 4000), NA_integer_, c(8000, 6000, 4000),
       NA_integer_, c(4000, 2000, 0), 3),
  # prescribed dosage missing
  list(5, 1, NA_integer_, 0, NA_integer_, NA_integer_, NA_integer_, 8000, NA_integer_, 12000, 3)

)

# construct formula to generate data frame
  num <- length(records[[1]])
  args <- paste0(".x[[", 1:num, "]]", collapse = ", ")
  form <- as.formula(paste0("~ new_patient(", args, ")"))

epi <- purrr::map(records, .f = form) %>% 
  dplyr::bind_rows()

saveRDS(epi, "inst/testdata/drug_adhere_epi.rds", version = 2)
