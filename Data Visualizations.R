##################################################################################################
# Function to wrangling data into desired format to generate Taylor Diagram ####

Taylor_Data <- function(indices = "", Category = "", variable = "TMin") {
	
	# Populating missing years  from 1960 to 2016 in each index data
	lapply(
		indices,
		
		# Anonymous function to fill missing years 
		\(data) {
			# (Bottleneck) re-write in C++
			dplyr::full_join(
				data.frame(Year = 1960:2016),
				data
			)
		}
	) |> . =>
		# rowbinding ouput from pipe into a matrix/dataframe
		do.call(rbind, .) |>  
		
		# Subsetting  years from 1960 to 2016 from indices_reshape_1 (indices)
		subset(Year >= 1960 & Year <= 2016) |>  
		
		# Appending ground data and index names to output from pipe
		within(
			{
				# replicating mean of Coastal Stations the number of times as the number of indices available
				obs = rep(Categories_Stations[["Averages"]][[Category]][ ,variable], times = length(names(indices)))
				
				# replicating each individual index just as the length of years 
				indices = rep(
					#formating names of indices first
					gsub(".txt", "", names(indices)),
					# number of repetitions for each index
					each = length(1960:2016)
				)
			}
		)
	
	
}


####################################################################################################

# Taylor Diagram for TMax of Coastal Stations ####
# Generating data for Taylor plot
Coastal_Stations_tayData_TMax <- Taylor_Data(
	#
	indices = indices_reshape_1,
	
	#
	Category = "Coastal_Stations",
	
	#
	variable = "TMax"
)

# Visualization
openair::TaylorDiagram(
	# Data
	Coastal_Sations_tayData_TMax,
	
	# observations/ground data
	obs = "obs",
	
	# mod/index's anomalies
	mod = "mean",
	
	# groups/indices
	group = "indices"
	
)

# ssaving to disk
dev.copy(png, filename = "Coastal Stations TMax.png", height = 1500, 1000)
dev.off()




# Taylor Diagram for TMax of ForestBelt ####
# Generating data for Taylor plot
ForestBelt_tayData_TMax <- Taylor_Data(
	#
	indices = indices_reshape_1,
	
	#
	Category = "ForestBelt",
	
	#
	variable = "TMax"
)

# Visualization
openair::TaylorDiagram(
	# Data
	ForestBelt_tayData_TMax,
	
	# observations/ground data
	obs = "obs",
	
	# mod/index's anomalies
	mod = "mean",
	
	# groups/indices
	group = "indices"
	
)


# ssaving to disk
dev.copy(png, filename = "ForestBelt TMax.png", height = 1500, 1000)
dev.off()



# Taylor Diagram for TMax of Northern Stations ####
# Generating data for Taylor plot
Northern_Stations_tayData_TMax <- Taylor_Data(
	#
	indices = indices_reshape_1,
	
	#
	Category = "Northern_Stations",
	
	#
	variable = "TMax"
)

# Visualization
openair::TaylorDiagram(
	# Data
	Northern_Stations_tayData_TMax,
	
	# observations/ground data
	obs = "obs",
	
	# mod/index's anomalies
	mod = "mean",
	
	# groups/indices
	group = "indices"
	
)


# ssaving to disk
dev.copy(png, filename = "Northern Stations TMax.png", height = 1500, 1000)
dev.off()







# Taylor Diagram for TMin of Coastal Stations ####
# Generating data for Taylor plot
Coastal_Sations_tayData_TMin <- Taylor_Data(
	indices = indices_reshape_1, 
	
	Category = "Coastal_Stations", 
	
	variable = "TMin"
)

# Visualization
openair::TaylorDiagram(
	# Data
	Coastal_Sations_tayData_TMin,
	
	# observations/ground data
	obs = "obs",
	
	# mod/indices' anomalies
	mod = "mean",
	
	# groups/indices
	group = "indices"
	
)

dev.copy(png, filename = "Coastal Stations TMin.png", height = 1500, 1000)
dev.off()


# Taylor Diagram for TMin of Coastal Stations ####
# Generating data for Taylor plot
ForestBelt_tayData_TMin <- Taylor_Data(
	indices = indices_reshape_1, 
	
	Category = "Coastal_Stations", 
	
	variable = "TMin"
)

# Visualization
openair::TaylorDiagram(
	# Data
	ForestBelt_tayData_TMin,
	
	# observations/ground data
	obs = "obs",
	
	# mod/indices' anomalies
	mod = "mean",
	
	# groups/indices
	group = "indices"
	
)

dev.copy(png, filename = "ForestBelt TMin.png", height = 1500, 1000)
dev.off()


# Taylor Diagram for TMin of Coastal Stations ####
# Generating data for Taylor plot
Northern_Sations_tayData_TMin <- Taylor_Data(
	indices = indices_reshape_1, 
	
	Category = "Northern_Stations", 
	
	variable = "TMin"
)

# Visualization
openair::TaylorDiagram(
	# Data
	Northern_Sations_tayData_TMin,
	
	# observations/ground data
	obs = "obs",
	
	# mod/indices' anomalies
	mod = "mean",
	
	# groups/indices
	group = "indices"
	
)

dev.copy(png, filename = "Northern Stations TMin.png", height = 1500, 1000)
dev.off()



# Spatial Plots of mean temperature for all 22 synoptic Stations in Ghana ####

# importing country shapefile of Ghana
Ghana <- sf::st_read(
	"C:/Users/pc/Downloads/Compressed/ghana_administrative/ghana_administrative.shp"
)

# Importing cordinates for stations
co_ords <- read.csv(
	"C:/Users/pc/Downloads/synoptic.csv", 
	sep = ",",
	header = TRUE
) |> 
	#converting to spatial object
	sf::st_as_sf(
		coords = c("LONGITUDE", "LATITUDE"),
		crs = sf::st_crs(Ghana)
	)


# Computing Mean tmax and tmin of all 22 Stations
mean_temp <- lapply(
	do.call("c", Stations_mean_temp), 
	\(data = "") apply(data[ ,grep("[^Year]", names(data))], 2, mean, na.rm = T)
) |> . => 
	do.call(rbind, .) |> 
	as.data.frame()

# inner joing co_ords and mean_temp by a common variable
co_ords <- dplyr::inner_join(
	co_ords,
	mean_temp,
	c("STATION_NA" = "station")
)

# Plotting
tmap::tm_shape(Ghana) +
	tmap::tm_lines() +
	tmap::tm_shape(co_ords) +
	tmap::tm_bubbles(col = "Average_tmp", size = 3) +
	tmap::tm_text(text = "STATION_NA", size = 0.8, interval.closure = "righ", just = "top") +
	tmap::tm_legend(outside = TRUE, outside.position = c("right"),
				 legend.stack = "horizontal")
