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
  NDZ <- change_dose
  PZ <- prescribed_dose
  NZ <- prescribed_days
  DZ <- total_dose
  NOZ <- no_obs_days
  OZ <- total_obs_dose
  NMZ <- missed_days
  MZ <- missed_dose
  RZ <- reason
  
  out <- data.frame(APID = id,
                    FOLAFT = FOLAFT,
                    NDZ = NDZ,
                    PZ = PZ,
                    NZ = NZ,
                    DZ = DZ,
                    NOZ = NOZ,
                    OZ = OZ,
                    NMZ = NMZ,
                    MZ = MZ,
                    RZ = RZ,
                    stringsAsFactors = FALSE)
  out
}

# define each patient
records <- list(
  list(1, 1, NA_integer_, 0, NA_integer_, 12000, NA_integer_, 8000, NA_integer_, 4000, 5)
)

# construct formula to generate data frame
  num <- length(records[[1]])
  args <- paste0(".x[[", 1:num, "]]", collapse = ", ")
  form <- as.formula(paste0("~ new_patient(", args, ")"))

epi <- purrr::map(records, .f = form) %>% 
  dplyr::bind_rows()

saveRDS(epi, "inst/testdata/drug_adhere_epi.rds", version = 2)
