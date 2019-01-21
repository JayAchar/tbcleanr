# testdata for recorded_dst
# EpiInfo
set.seed(1000)
data <- data.frame(APID = paste0("XYZ", 1:20),
                   DIPRO = sample(c(1:7, 99), 20, replace = T), 
                   stringsAsFactors = F)
class(data) <- c(class(data), "epiinfo")
saveRDS(data, "inst/testdata/recorded_dst_epiinfo.rds")


# ===
## Koch 6
set.seed(998)
data <- data.frame(registrationnb = as.character(100013:100042),
                   cdstrainprofil = c(rep(0, 2),
                                      rep(1, 5),
                                      rep(2, 20),
                                      rep(3, 1),
                                      rep(4, 2)),
                   cdstrainconf   = c(rep(0, 2),
                                      rep(0, 5),
                                      sample(c(0:7), 20, replace = T),
                                      rep(0, 1),
                                      rep(0, 2)),
                   stringsAsFactors = F)
class(data) <- c(class(data), "koch6")
saveRDS(data, "inst/testdata/recorded_dst_koch6.rds")

