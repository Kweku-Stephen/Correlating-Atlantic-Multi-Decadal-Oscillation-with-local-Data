# Importing standardized anomalies of 11 temperature indices ####

library(magrittr)

# Sequential import 
# indices <- (dir(pattern = "INDICES") |>
# 		  	readxl::excel_sheets())[1:11] |> 
# 	lapply(
# 		# Anonymous function to import sheets
# 		\(sheetname = "", workbook = "", row_range = ""){
# 			
# 			(readxl::read_excel(
# 				workbook,
# 				sheetname,
# 				row_range
# 			)[c(1, 15:26)]) |>
# 				setNames(c("Year", month.abb))
# 			
# 		},
# 		# Extra arguments to the anonymous function above
# 		workbook = dir(pattern = "INDICES"),
# 		row_range = readxl::cell_rows(c(13:70))
# 	) |> 
# 	# naming all elemetns of the returned list
# 	setNames((dir(pattern = "INDICES") |>
# 				readxl::excel_sheets())[1:11])
# 
# 
# # Extracting years from 1960 to 2016
# indices %<>% lapply(
# 	\(tibble = "", years = "") subset(tibble, Year >= 1960 & Year <= 2016)
# )
	


# Parallel import
# Creating a cluster of 3 nodes to import data in parallel
cl <- parallel::makeCluster(parallel::detectCores() - 5, type = "PSOCK")

# importing data in parallel
indices <- parallel::parLapply(
	# cluster of 3 nodes
	cl,
	
	# chunking on workers side
	split(
		(dir(pattern = "INDICES") |>
		 	readxl::excel_sheets())[1:11],
		as.factor(rep(1:3, c(4,4, 3)))
	),
	
	# Anonymous function to import multiple excel sheets
	function(sheetname = "", workbook = "", range = ""){
		
		lapply(
			sheetname, 
			\(sheetname) readxl::read_excel(workbook, sheetname, range, na = "-99.99")[c(1, 15:26)]
		)
	},
	
	# Extra arguments to the anonymous function above
	workbook = dir(pattern = "INDICES"),
	range = readxl::cell_rows(c(13, 70))
)

# stopping cluster
parallel::stopCluster(cl)
# Computing annual means of indices ####


# Data Reshaping - combining all lists with indices into a single list ####
indices <- indices |> . =>
	do.call(c, .) |> 
	setNames((dir(pattern = "INDICES") |> readxl::excel_sheets())[1:11])


# Renaming all columns of each tibble of the list "indices" ####
indices %<>% lapply(
	\(tibble = "") setNames(tibble, c("Year", month.abb))
)


# Mean annual anomalies for each index ####
indices %<>% lapply(
	\(tibble = "") {
		return(
			tibble::tibble(
				Year = tibble[, "Year"],
				Mean_anomaly = apply(tibble[ ,-1], 1, mean, na.rm = TRUE)
			)
		)
	}
)


