# Test data for lab_date_fixer()
## EpiInfo

data <- data.frame(FIRST = "12/1/14",
                   SECOND = "8/7/17",
                   THIRD = "01/4/16",
                   stringsAsFactors = F)

data <- data %>% purrr::map_df(.f = lubridate::dmy)
class(data) <- c("epiinfo", class(data))
saveRDS(data, "inst/testdata/lab_date_fixer_epi.rds", version = 2)

## Grozny

data <- data %>% 
  dplyr::rename(dcol1 = FIRST,
                dcol2 = SECOND,
                dcol3 = THIRD)

class(data) <- append("grozny", class(data)[2:4])
saveRDS(data, "inst/testdata/lab_date_fixer_groz.rds", version = 2)

### Koch6
data <- data.frame(Samplecollectiondate = c("1/1/12", "5/2/14"),
                   stringsAsFactors = FALSE)

data$Samplecollectiondate <- lubridate::dmy(data$Samplecollectiondate)
class(data) <- c("koch6", class(data))
saveRDS(data, "inst/testdata/lab_date_fixer_k6.rds", version = 2)
