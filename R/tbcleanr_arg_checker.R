

tbcleanr_arg_checker <- function(...) {
	# acceptable values for args
		# software
		s <- c("excel", "koch_6", "epiinfo")

		# project
		p <- c("kk", "chechnya")

		# file
		f <- c("adm", "lab", "clinical_lab")

# check software arg
	if (! software %in% s) {
			set_options <- paste(s, collapse = ", ")
			error_message <- paste("\'software\' arg should be ", set_options, sep = "")
		stop(error_message)
	}		

# check project arg
	if (! project %in% p) {
			set_options <- paste(p, collapse = ", ")
			error_message <- paste("\'project\' arg should be ", set_options, sep = "")
		stop(error_message)
	}		

# check file arg
	if (! file %in% f) {
			set_options <- paste(f, collapse = ", ")
			error_message <- paste("\'file\' arg should be ", set_options, sep = "")
		stop(error_message)
	}		

}