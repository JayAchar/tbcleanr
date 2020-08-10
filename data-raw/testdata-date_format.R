# date_format test data

# Epiinfo - lab - dates all character strings
chr_str <- structure(list(APID = "XYZ1",
                          FIRST = "28/08/2003",
                          SECOND = NA_character_,
                          CULRES = "30/09/2003"), 
                     row.names = c(NA, -1L), 
                     class = c("tbl_df", "tbl", "data.frame", "epiinfo"))
saveRDS(chr_str, "inst/testdata/date_format_chr_str.rds", version = 2)

# Epiinfo - lab - date formats mixed POSITct and character strings
chr_posix <- structure(list(APID = "XYZ1", 
                            FIRST = structure(1496440800, class = c("POSIXct","POSIXt"), tzone = ""), 
                            CULRES = "30/09/2003"), 
                       row.names = c(NA,-1L), 
                       class = c("tbl_df", "tbl", "data.frame", "epiinfo"))
saveRDS(chr_posix, "inst/testdata/date_format_chr_posix.rds", version = 2)

# EpiInfo - lab - date formats all POSIXct
posix <- structure(list(APID = "XYZ1", 
                        FIRST = structure(1496440800, class = c("POSIXct","POSIXt"), tzone = ""), 
                        CULRES = structure(1496500800, class = c("POSIXct","POSIXt"), tzone = "")), 
                   row.names = c(NA,-1L), 
                   class = c("tbl_df", "tbl", "data.frame", "epiinfo"))
saveRDS(posix, "inst/testdata/date_format_posix.rds", version = 2)
