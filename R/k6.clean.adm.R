#' Clean Koch6 admission data frame
#'
#' This function takes a data frame derived from a Koch 6 admission export and formats multiple variables to manage dates and factors, while renaming. 
#' @param x data frame or data.table
#' @param format specify the lubridate output format
#' @keywords TB
#' @export


k6.clean.adm <- function(x, format = dmy) {
		require(dplyr)
		require(purrr)
		require(lubridate)

	var_names <- names(x)
# -----------------------------------------------------------------------------

		# format date variables
			x <- date.format(x, format = format)

		# rename to differentiate hx of HBV/HCV
			if ("hepB" %in% var_names) {
					x %>%
					rename("hx_hbv" = "hepB") -> x
			} 

			if ("hepC" %in% var_names) {
					x %>%
					rename("hx_hcv" = "hepC") -> x
			}
# -----------------------------------------------------------------------------

		# lower case all variable names
				colnames(x) <- tolower(colnames(x))

		# recapture all variable names for lower case
				var_names <- names(x)

		# split registrationnb into rayon and number
				x %>%
					mutate(rayon = substr(registrationnb, 1, 3), 
									idno = substr(registrationnb, 4, 10)) %>%
					select(-registrationnb) -> x

				# change idno to numerical
					as.numeric(x$idno) -> x$idno

# -----------------------------------------------------------------------------

		# rename variables
			if ("dateofbirth" %in% var_names) {
				x %>%
					rename("dob" = "dateofbirth") -> x
			}
			if ("datedeat" %in% var_names) {
				x %>%
					rename("deathdate" = "datedeat") -> x
			}

# -----------------------------------------------------------------------------

		# label all factors
			ny <- c("diabetes", "cardiodi", "renalfail")
			cavity <- c("cav", "cavd")
			outcome <- c("outfirst", "outfirst2013")
			gender <- c("gender")
			hiv <- c("hiv")
			drugs <- c("e", "h", "r", "z", "am", "cm", "km", "lfx", "mfx", 
					"ofx", "cs", "eto", "pas", "pas na", "pto", "amx-clv",
					"bdq", "cfz", "clr", "dld", "hdh", "impcln", "lzd", "mpm")

			factor_ny <- ny[ny %in% var_names]
			factor_cavity <- cavity[cavity %in% var_names]
			factor_outcome <- outcome[outcome %in% var_names]
			factor_gender <- gender[gender %in% var_names]
			factor_hiv <- hiv[hiv %in% var_names]
			factor_drugs <- drugs[drugs %in% var_names]

					# gender factor
				x %>%
				map_at(factor_gender, factor,
								levels = c("0", "1", "2"),
								labels = c("NA", "Male", "Female"))	%>%
				as_tibble() -> x

					# hiv factor
				x %>%
				map_at(factor_hiv, factor,
								levels = c("0", "1", "2", "3", "4"),
								labels = c("Unknown", "Positive", "Negative", "Not Done", "Pending"))	%>%
				as_tibble() -> x

					# Yes/No factors
				x %>%
				map_at(factor_ny, factor,
								levels = c("0", "1"),
								labels = c("No", "Yes")) %>%
				as_tibble() -> x

					# cavities on x-ray factors
				x %>%
				map_at(factor_cavity, factor,
								levels = c("0", "1", "2"),
								labels = c("Not detected", "1 lobe", "> 1 lobe")) %>%
				as_tibble() -> x

				# outcome variables as factors
				x %>%
				map_at(factor_outcome, factor,
								levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8"),
								labels = c("On Tx", "Cure", "Complete", "Died", "Fail", "LTFU", 
										"Transfer Out", "Other", "Tranfer In")) %>%
				as_tibble() -> x

				# factorise drugs in baseline regimen
				for (i in factor_drugs) {
				x[[i]][is.na(x[[i]])] <- 0

				x[[i]][x[i] > 0] <- 1
				
				x[[i]] <- factor(x[[i]], 
							levels = c("0", "1"),
							labels = c("No", "Yes"))
				}


		return(x)		
}
