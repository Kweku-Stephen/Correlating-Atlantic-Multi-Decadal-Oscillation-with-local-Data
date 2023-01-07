# Loading the magrittr package ####
library(magrittr)

# Invoking the pipebind operator ####
Sys.setenv("_R_USE_PIPEBIND_" = "true")

# Creating a cluster of four workers to import multiple datasets ####
cl <- parallel::makeCluster(
	parallelly::availableCores() - 4,
	type = "PSOCK"
)


# Data Import of 22 synoptic stations across Ghana ####
Stations <- dir(
	path = "Gauge_Raw/",
	pattern = ".txt",
	full.names = TRUE
 ) |> . =>
	parallel::clusterApplyLB(
		cl,
		.,
		data.table::fread,
		na.strings = c("-99.9", "99.9", "9999", "9988", "-9999", "-9988")
	) |> 
	setNames(
		dir(
			path = "Gauge_Raw/",
			pattern = "txt"
		)
	)
	

# Data Transformations ####
# Initializing data on all nodes/workers
parallel::clusterExport(cl, "Stations")

# checking data is initialized in each node's environment
parallel::clusterCall(cl, function(data) length(data), Stations)


# Converting variables year, month, day of each dataframe of the list "Stations" to a date class
parallel::clusterApply(
	# cluster of 4 nodes
	cl,
	
	# chunking on data side
	split(
		names(Stations),
		as.factor(unlist(mapply(rep, 1:4, c(6, 5, 5, 6))))
	),
	
	# anonymous function to convert columns year, month, day to a date object
	function(vector = ""){
		Stations[vector] |> 
			lapply(
				\(data = "") {
					subset(data, year >= 1960 & year <= 2016) |> . =>
						data.table::data.table(
							Date = as.Date(paste(.[ ,year], .[ ,month], .[ ,day], sep = "-"), "%Y-%m-%e"),
							rr = .[ ,rr],
							TMax = .[ ,TMax],
							TMin = .[ ,TMin]
						)
				}
			) 
	}
) -> Stations_1

# checking classes of each column or variable in each data.table of the list Stations_1
lapply(
	Stations_1,
	\(data) lapply(data, \(data) sapply(data, class))
)

# Conveting Accra rr, TMax and TMin variable to numeric
Stations_1[[1]][["Accra_rr_temp.txt"]] <- transform(
	Stations_1[[1]][[2]],
	rr = as.numeric(Stations_1[[1]][["Accra_rr_temp.txt"]][ ,rr]),
	TMax = as.numeric(Stations_1[[1]][["Accra_rr_temp.txt"]][ ,TMax]),
	TMin = as.numeric(Stations_1[[1]][["Accra_rr_temp.txt"]][ ,TMin])
)



# Extracting all nested lists into a single list of stations
# Each element of the returned list from the above transformation is a list containing containng data.tables
# Stations_1 <- lapply(
# 	1:length(Stations_1),
# 	\(vec = "")  {a = Stations_1[[vec]]; return(a)}
# ) 



# Computing mean annual temperature for max and min for all 22 stations ####

# Initializing Stations_1 on all nodes
parallel::clusterExport(cl, "Stations_1")

parallel::clusterApply(
	# cluster of 4 nodes
	cl,
	
	#chunking on data side
	1:4,
	
	# Anonymous function to compute anomalies
	function(vec = "") {
		lapply(
			Stations_1[[vec]],
			\(data = "") {
				aggregate(cbind(TMax, TMin) ~ format(data[ ,Date], "%Y"), data = data, FUN = mean, na.rm = TRUE) |> 
					setNames(c("Year", "TMax", "TMin"))
			}
		)
	}
) |> . =>
	do.call(c, .) -> Stations_mean_temp


# Computing the Annual anomalies of Tmax and Tmin ####
parallel::clusterExport(cl, "Stations_mean_temp")

parallel::clusterApply(
	# cluster of 4 nodes
	cl,
	
	#chunking on worker side
	1:4,
	
	# Anonymous function to compute anomalies
	function(vec = "") {
		lapply(
			Stations_mean_temp[[vec]],
			\(data = "") {
				data.frame(
					Year = data[ ,"Year"],
					TMax = with(data, (TMax - mean(TMax, na.rm = TRUE))/ sd(TMax, na.rm = TRUE)),
					TMin = with(data, (TMin - mean(TMin, na.rm = TRUE))/ sd(TMin, na.rm = TRUE))
				)
			}
		)
	}
) |> . =>
	do.call(c, .) -> standard_anomalies


# Stoping Cluater
parallel::stopCluster(cl)

# convering Stations_mean_temp to a single dataframe with a new column "Stations" #
listed <- within(
	(do.call(rbind, Stations_mean_temp)),
	
	{
		# Columns to add
		Stations = rep(
			names(Stations_mean_temp), 
			lapply(Stations_mean_temp, nrow) |> . => 
				do.call(c, .)
		)
	}
) |> 
	# Transforming Year variable to integer
	transform(Year = as.integer(Year))



# Visualizations ####
library(trelliscopejs)
library(ggplot2)



ggplot(data = listed, aes(x = Year, y = TMax)) +
	geom_line(col = "firebrick") +
	# Conditioning by Stations
	facet_trelliscope(
		~ Stations,
		scales = "sliced",
		auto_cog = TRUE,
		nrow = 2, 
		ncol = 2
	)





