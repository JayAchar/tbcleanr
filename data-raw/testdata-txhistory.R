# testdata for txhistory
# EpiInfo
set.seed(1000)
data <- data.frame(APID = paste0("XYZ", 1:20),
                   REGRP = sample(1:8, 20, replace = T), 
                   stringsAsFactors = F)
class(data) <- c(class(data), "epiinfo")
saveRDS(data, "inst/testdata/txhistory_epiinfo.rds")


# ===
## Koch 6
set.seed(999)
data <- data.frame(registrationnb = as.character(100023:100042),
                   cdhistory = sample(0:5, 20, replace = T),
                   stringsAsFactors = F)
class(data) <- c(class(data), "koch6")
saveRDS(data, "inst/testdata/txhistory_koch6.rds")

