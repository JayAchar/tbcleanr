# testdata for txhistory
# EpiInfo
set.seed(1000)
data <- data.frame(APID = paste0("XYZ", 1:20),
                   REGRP = sample(1:8, 20, replace = T), 
                   stringsAsFactors = F)
class(data) <- c("epiinfo", class(data))
saveRDS(data, "inst/testdata/txhistory_epiinfo.rds", version = 2)


# ===
## Koch 6
set.seed(999)
data <- data.frame(registrationnb = as.character(100023:100042),
                   cdhistory = sample(0:5, 20, replace = T),
                   stringsAsFactors = F)
class(data) <- c("koch6", class(data))
saveRDS(data, "inst/testdata/txhistory_koch6.rds", version = 2)

