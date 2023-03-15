# Loading magrittr ####
library(magrittr)
Sys.setenv("_R_USE_PIPEBIND_" = "true")

# Data Import ####
# Listing files to be imported
dir(
	"indices/",
	pattern = "^[^Ii][^Oo][^Dd]", 
	full.names = TRUE
) |> 
	
	# importing listd files sequentially as data tables into a list
	lapply(
		\(data = "") data.table::fread(data, header = FALSE, na.strings = c("-99.99", "-99.9"))
	) |> 
	
	# naming elements of the returned list from above using filenames
	setNames(dir("indices/", pattern = "^[^Ii][^Oo][^Dd]")) |> 
	
	# naming indivisual columns of each element of the list returned from above
	lapply(\(data) setNames(data, c("Year", month.abb))) |> 
	
	# Averaging anomalies for all years for each data frmae of the list returned from above
	lapply(
		\(data = "") {
			data.frame(
				Year = data[,1],
				mean = apply(data[ ,-1], 1, mean, na.rm = TRUE)
			)
		}
	) -> indices_reshape_1



# Appending IOD to indices_reshape_1 ####
indices_reshape_1[["IOD.txt"]] <- data.frame(
	
	
	#reading in timestamp of IOD.txt
	Date = scan(
		dir("Indices/", pattern = "^[Ii][Oo][Dd]\\_", full.names = TRUE), 
		what = character(), 
		quiet = TRUE
	) |> . =>
		# Extracting only entries beginning with numbers
		grep("^[0-9]", ., value = TRUE) |> . =>
		# Removing "T" from all entries
		gsub("[A-z]", " ", ., ignore.case = TRUE),
	
	
	# Reading in anomalies of IOD.txt
	mean = scan(
		dir("Indices/", pattern = "^[Ii][Oo][Dd]\\.", full.names = TRUE),
		what = character(),
		quiet = TRUE
	) |> . =>
		# Extractin only numbers from the vector returned
		grep("^\\-|^[0-9]", ., value = TRUE) |> . =>
		# Removing all "," from entries
		gsub(",", "", .)
	
	
)



# Averaging all anomalies of IOD by year ####
indices_reshape_1[["IOD.txt"]] <- data.frame(
	
	# Transforming "Date" into an R date object
	Date = as.Date(indices_reshape_1[["IOD.txt"]][ ,"Date"], format = "%Y-%m-%d %H:%M:%S"),
	
	# Converting "mean" column from character to numeric
	mean = as.numeric(indices_reshape_1[["IOD.txt"]][ ,"mean"])
	
) |> . =>
	
	# Averaging anaomalies by year
	aggregate(
		mean ~ format(.[ ,"Date"], format = "%Y"),
		data = .,
		FUN = mean
	) |>
	
	# renaming columns
	setNames(c("Year", "mean")) |> 
	
	# Transforming Year column to integer
	transform(Year = as.integer(Year))



# replacing all -99.9s in each dataframe of indices_reshape_1with NA ####
indices_reshape_1 %<>% 
	lapply(
		\(data = "") {
			data[ ,"mean"][data[ ,"mean"] %in% c(-99.9, -99.99, -9988, -9999, 9999, 9988)] <- NA
			return(data)
		}
	)

# Categorizng Stations into 3 groups "Coastal, Forest Belt and Northern" ####
Categories_Stations <- tibble::tibble(
	
	# Station Categories
	Stations = c("Coastal_Stations", "Forest Belt", "Northern_Stations"),
	
	# Creating list columns using Station data based on category
	Data = list(
		# Coastal Stations
		Coastal = standard_anomalies[grep("Ada|Accra|Axim|[Tt]akoradi|[Ss]altpond|[Tt]ema", names(standard_anomalies), value = TRUE)] |> 
			# Converting all Year Columns to integers
			lapply(\(data = "") transform(data, Year = as.integer(Year))),
		
		# Forest Belt 
		Forest = standard_anomalies[grep("Abe|[Ww]en|[Ss]un|[Kk]um|[Hh]|Kete|Aka|Kof|[Aa][Kk][Ii]|Sefw|Akus", names(standard_anomalies), value = TRUE)] |> 
			# Converting all Year Columns to integers
			lapply(\(data = "") transform(data, Year = as.integer(Year))),
		
		# Northern Stations
		Northern = standard_anomalies[grep("[Tt][Aa][Mm]|[Yy][Ee]|[Nn][Aa][Vv]|[Bb][Oo][Ll]", names(standard_anomalies), value = TRUE)] |> 
			# Converting all Year Columns to integers
			lapply(\(data = "") transform(data, Year = as.integer(Year)))
	)
	
) 



# Averaging all Tmax and Tmin based on categories created ####

# For Coastal Stations
# Extracting longest time period out of the Coastal stations list
vec_coastal <- do.call(
	c,
	lapply(Categories_Stations$Data$Coastal, \(data) range(data[ ,"Year"]))
) |> 
	as.numeric()

Coastal_Station_wrang <- lapply(
	
	# Coastal Stations
	Categories_Stations$Data$Coastal,
	
	# Function loop over it each element of Coastal Station
	\(data = "") {
		dplyr::full_join(
			data.frame(Year = min(vec_coastal):max(vec_coastal)),
			data
		)
	}
) |> . =>
	
	# Piping output to lopping construct "lapply" to column-bind TMaxs and TMins of Northern 
	# Stations into a single dataframe
	lapply(
		# List of Coastal Stations
		.,
		
		# Anonymous function to column-bind all TMaxs and TMins of each dataframe into a single dataframe
		\(data = "") {
			if (!identical(data, .[[1]])) {
				dplyr::select(data, TMax, TMin)
			} else {
				data
			}
		}
	) |> . =>
	# Column binding TMaxs and TMins
	do.call(cbind, .)

# Computing mean annual average for Tmax and Tmin for ForestBelt_Stations
Coastal_Station_avg <- data.frame(
	
	# Years
	Year = Coastal_Station_wrang[ ,grep("Year", names(Coastal_Station_wrang))],
	
	# Average of TMax
	TMax = apply(
		# Extracting only TMaxs
		Coastal_Station_wrang[ ,grep("[Tt][Mm][Aa][Xx]", names(Coastal_Station_wrang), value = TRUE)],
		
		# Choosing rowise averaging
		1,
		
		# Function for averaging
		mean,
		
		# Other arguments/calls to the mean function
		na.rm = TRUE
	),
	
	#Average of TMin
	TMin = apply(
		# Extracting only TMaxs
		Coastal_Station_wrang[ ,grep("[Tt][Mm][Ii][Nn]", names(Coastal_Station_wrang), value = TRUE)],
		
		# Choosing rowise averaging
		1,
		
		# Function for averaging
		mean,
		
		# Other arguments/calls to the mean function
		na.rm = TRUE
	)
	
	
)





# For Forest/Middle Belt Stations
# Extracting longest time period out of the Forest Belt stations list
vec_Forest <- do.call(
	c,
	lapply(Categories_Stations$Data$Forest, \(data) range(data[ ,"Year"]))
) |> 
	as.numeric()

ForestBelt_Station_wrang <- lapply(
	
	# Coastal Stations
	Categories_Stations$Data$Forest,
	
	# Function loop over it each element of Coastal Station
	\(data = "") {
		dplyr::full_join(
			data.frame(Year = min(vec_Forest):max(vec_Forest)),
			data
		)
	}
) |> . =>
	
	# Piping output to lopping construct "lapply" to column-bind TMaxs and TMins of Northern 
	# Stations into a single dataframe
	lapply(
		# List of Forest Belt Stations
		.,
		
		# Anonymous function to column-bind all TMaxs and TMins of each dataframe into a single dataframe
		\(data = "") {
			if (!identical(data, .[[1]])) {
				dplyr::select(data, TMax, TMin)
			} else {
				data
			}
		}
	) |> . =>
	# Column binding TMaxs and TMins
	do.call(cbind, .)

# Computing mean annual average for Tmax and Tmin for ForestBelt_Stations
ForestBelt_Station_avg <- data.frame(
	
	# Years
	Year = ForestBelt_Station_wrang[ ,grep("Year", names(ForestBelt_Station_wrang))],
	
	# Average of TMax
	TMax = apply(
		# Extracting only TMaxs
		ForestBelt_Station_wrang[ ,grep("[Tt][Mm][Aa][Xx]", names(ForestBelt_Station_wrang), value = TRUE)],
		
		# Choosing rowise averaging
		1,
		
		# Function for averaging
		mean,
		
		# Other arguments/calls to the mean function
		na.rm = TRUE
	),
	
	#Average of TMin
	TMin = apply(
		# Extracting only TMaxs
		ForestBelt_Station_wrang[ ,grep("[Tt][Mm][Ii][Nn]", names(ForestBelt_Station_wrang), value = TRUE)],
		
		# Choosing rowise averaging
		1,
		
		# Function for averaging
		mean,
		
		# Other arguments/calls to the mean function
		na.rm = TRUE
	)
	
	
)





# For Northern Stations ####

# Extracting longest time period out of the Northern stations list
vec_North <- do.call(
	c,
	lapply(Categories_Stations$Data$Northern, \(data) range(data[ ,"Year"]))
) |> 
	as.numeric()

# Data wrangling into desired structure to compute average
Northern_Station_wrang <- lapply(
	
	# Coastal Stations
	Categories_Stations$Data$Northern,
	
	# Function loop over it each element of Coastal Station to fill missing 
	# years to be up to station with longest period
	\(data = "") {
		dplyr::full_join(
			data.frame(Year = min(vec_North):max(vec_North)),
			data
		)
	}
) |> . =>
	
	# Piping output to lopping construct "lapply" to column-bind TMaxs and TMins of Northern 
	# Stations into a single dataframe
	lapply(
		# List of Northern Stations
		.,
		
		# Anonymous function to column bind all TMaxs and TMins into a single dataframe
		\(data = "") {
			if (!identical(data, .[[1]])) {
				dplyr::select(data, TMax, TMin)
			} else {
				data
			}
		}
	) |> . =>
	# Column binding TMaxs and TMins
	do.call(cbind, .)

# Computing mean annual average for Tmax and Tmin for Northern Stations
Northern_Station_avg <- data.frame(
	
	# Years
	Year = Northern_Station_wrang[ ,grep("Year", names(Northern_Station_wrang))],
	
	# Average of TMax
	TMax = apply(
		# Extracting only TMaxs
		Northern_Station_wrang[ ,grep("[Tt][Mm][Aa][Xx]", names(Northern_Station_wrang), value = TRUE)],
		
		# Choosing rowise averaging
		1,
		
		# Function for averaging
		mean,
		
		# Other arguments/calls to the mean function
		na.rm = TRUE
	),
	
	#Average of TMin
	TMin = apply(
		# Extracting only TMaxs
		Northern_Station_wrang[ ,grep("[Tt][Mm][Ii][Nn]", names(Northern_Station_wrang), value = TRUE)],
		
		# Choosing rowise averaging
		1,
		
		# Function for averaging
		mean,
		
		# Other arguments/calls to the mean function
		na.rm = TRUE
	)
	
	
)



# Appending all averages of Categorised Stations as a list to Categories_Stations tibble ####
Categories_Stations %<>% within(
	{
		Averages = list(
			
			# Averages of Coastal Stations
			Coastal_Stations = Coastal_Station_avg,
			
			# Averages of Forest Belt 
			ForestBelt = ForestBelt_Station_avg,
			
			# Averages of Northern Stations
			Northern_Stations = Northern_Station_avg
		)
	}
)




