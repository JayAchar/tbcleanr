# Test data for lab_date_fixer()
## EpiInfo

data <- data.frame(FIRST = "12/1/14",
                   SECOND = "8/7/17",
                   THIRD = "01/4/16",
                   stringsAsFactors = F)

data <- data %>% purrr::map_df(.f = lubridate::dmy)
class(data) <- c(class(data), "epiinfo")
saveRDS(data, "inst/testdata/lab_date_fixer_epi.rds")

## Grozny

data <- data %>% 
  dplyr::rename(dcol1 = FIRST,
                dcol2 = SECOND,
                dcol3 = THIRD)

class(data) <- class(data)[1:3] %>% c("grozny")
saveRDS(data, "inst/testdata/lab_date_fixer_groz.rds")

### Koch6
data <- data.frame(Samplecollectiondate = c("1/1/12", "5/2/14"),
                   stringsAsFactors = FALSE)

data$Samplecollectiondate <- lubridate::dmy(data$Samplecollectiondate)
class(data) <- c(class(data), "koch6")
saveRDS(data, "inst/testdata/lab_date_fixer_k6.rds")
