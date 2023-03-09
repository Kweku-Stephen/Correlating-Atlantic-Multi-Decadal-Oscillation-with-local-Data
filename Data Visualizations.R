##################################################################################################
# Function to wrangling data into desired format to generate Taylor Diagram ####

Taylor_Data <- function(indices = "", Category = "", variable = "") {
	
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
	Coastal_Stations_tayData_TMax,
	
	# observations/ground data
	obs = "obs",
	
	# mod/index's anomalies
	mod = "mean",
	
	# groups/indices
	group = "indices"
	
)

# ssaving to disk
dev.copy(png, filename = "Coastal.png", height = 462, width = 942)
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
dev.copy(png, filename = "ForestBelt TMax.png", height = 462, width = 942)
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
dev.copy(png, filename = "Northern Stations TMax.png", height = 462, width = 942)
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

dev.copy(png, filename = "Coastal Stations TMin.png", width = 942, height = 462)
dev.off()


# Taylor Diagram for TMin of ForestBelt ####
# Generating data for Taylor plot
ForestBelt_tayData_TMin <- Taylor_Data(
	indices = indices_reshape_1, 
	
	Category = "ForestBelt", 
	
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

dev.copy(png, filename = "ForestBelt TMin.png", height = 462, width = 942)
dev.off()


# Taylor Diagram for TMin of Northern Stations ####
# Generating data for Taylor plot
Northern_Stations_tayData_TMin <- Taylor_Data(
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

dev.copy(png, filename = "Northern Stations TMin.png", height = 462, width = 942)
dev.off()




# Time Series plot and trend ####
Annual_timeSeries <- tibble::tibble(
	
	Stations = c("Coastal Stations", "ForestBelt", "Northern Stations"),
	
	Graphs_TMax = list(
		
		Coastal = Categories_Stations$Averages$Coastal_Stations |> 
			ggplot(aes(x = Year, y = TMax)) +
			geom_line(col = "firebrick", lwd = 1.5) +
			geom_smooth(method = "lm", col = "darkblue", se = FALSE) +
			scale_x_continuous(breaks = seq(1960, 2016, by = 6)) +
			theme(axis.text = element_text(size = 18),
				 axis.title = element_text(size = 20, face = "bold")),
		
		ForestBelt = Categories_Stations$Averages$ForestBelt |> 
			ggplot(aes(x = Year, y = TMax)) +
			geom_line(col = "firebrick", lwd = 1.5) +
			geom_smooth(method = "lm", col = "darkblue", se = FALSE) +
			scale_x_continuous(breaks = seq(1960, 2016, by = 6)) +
			theme(axis.text = element_text(size = 18),
				 axis.title = element_text(size = 20, face = "bold")),
		
		Northern = Categories_Stations$Averages$Northern_Stations |> 
			ggplot(aes(x = Year, y = TMax)) +
			geom_line(col = "firebrick", lwd = 1.5) +
			geom_smooth(method = "lm", col = "darkblue", se = FALSE) +
			scale_x_continuous(breaks = seq(1960, 2016, by = 6)) +
			theme(axis.text = element_text(size = 18),
				 axis.title = element_text(size = 20, face = "bold"))
	),
	
	Graphs_TMin = list(
		
		Coastal = Categories_Stations$Averages$Coastal_Stations |>
			ggplot(aes(x = Year, y = TMin)) +
			geom_line(col = "firebrick", lwd = 1.5) +
			geom_smooth(method = "lm", col = "darkblue", se = FALSE) +
			scale_x_continuous(breaks = seq(1960, 2016, by = 6)) +
			theme(axis.text = element_text(size = 18),
				 axis.title = element_text(size = 20, face = "bold")),
		
		ForestBelt = Categories_Stations$Averages$ForestBelt |> 
			ggplot(aes(x = Year, y = TMin)) +
			geom_line(col = "firebrick", lwd = 1.5) +
			geom_smooth(method = "lm", col = "darkblue", se = FALSE) +
			scale_x_continuous(breaks = seq(1960, 2016, by = 6)) +
			theme(axis.text = element_text(size = 18),
				 axis.title = element_text(size = 20, face = "bold")),
		
		Northern = Categories_Stations$Averages$Northern_Stations |> 
			ggplot(aes(x = Year, y = TMin)) +
			geom_line(col = "firebrick", lwd = 1.5) +
			geom_smooth(method = "lm", col = "darkblue", se = FALSE) +
			scale_x_continuous(breaks = seq(1960, 2016, by = 6)) +
			theme(axis.text = element_text(size = 18),
				 axis.title = element_text(size = 20, face = "bold"))
	)
	
)






# Spatial Plots of mean temperature for all 22 synoptic Stations in Ghana ####

# importing country shapefile of Ghana
Ghana <- sf::st_read(
	"C:/Users/pc/Downloads/Compressed/ghana/ghana.shp"
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

# Adding co_ords STATION_NA variable to mean_temp
co_ords <- co_ords[order(co_ords$STATION_NA), ]
	

# Computing Mean tmax and tmin of all 22 Stations
mean_temp <- lapply(
	do.call("c", Stations_mean_temp), 
	\(data = "") apply(data[ ,grep("[^Year]", names(data))], 2, mean, na.rm = T)
) |> . => 
	do.call(rbind, .) |> 
	as.data.frame() |> 
	within({
		station = co_ords$STATION_NA
	})


# inner joing co_ords and mean_temp by a common variable
co_ords <- dplyr::inner_join(
	co_ords,
	mean_temp,
	c("STATION_NA" = "station")
)


# Adding average temp to co_ords 
co_ords <- within(co_ords, { Average_Temp = apply(mean_temp[ ,1:2], 1, mean, na.rm = TRUE)})

# Re-naming "STATIONS_NA" in the shapefile "co_ords" to simpler names for easy plot visualization
co_ords$STATION_NA <- {
	
	# Renaming Stations
	strsplit(co_ords$STATION_NA, " ") |> 
		lapply(\(name) name[1]) |> . =>
		do.call(rbind, .) |> 
		as.character() -> a
	
	# replacing names meeting the conditions stated in the loop below
	for(name in a){
		if(name == "KETE") {
			a[name] <- "KETE KRACHI"
		} else if (name == "SEFWI"){
			a[name] <- "SEFWI BEKWAI"
		} else {
			a[name] <- name
		}
	}
	
	STATION_NA
	
}
	



# Plotting
# Colour palette
org <- RColorBrewer::brewer.pal(5, "Oranges")

# Tmap ####
# Average Temp
tmap::tm_shape(Ghana) +
	tmap::tm_lines(lwd = 1) +
	tmap::tm_shape(co_ords) +
	tmap::tm_bubbles(col = "Average_Temp", size = 1, palette = org) +
	tmap::tm_text(text = "STATION_NA", size = 0.4, just = "top", ymod = 0.8, xmod = -.5) +
	tmap::tm_legend(outside = TRUE, outside.position = c("right"), legend.stack = "horizontal") +
	tmap::tm_layout(fontface = "bold", fontfamily = "") +
	tmap::tm_compass(type = "arrow", size = 2, position = c("right", "top"))

tmap::tmap_save(filename = "Average_Temp_of_Ghana.png", width = 7, height = 8)


# Max Temp
tmap::tm_shape(Ghana) +
	tmap::tm_lines(lwd = 1) +
	tmap::tm_shape(co_ords) +
	tmap::tm_bubbles(col = "TMax", size = 1, palette = org) +
	tmap::tm_text(text = "STATION_NA", size = 0.4, just = "top", ymod = 0.8, xmod = -.5) +
	tmap::tm_legend(outside = TRUE, outside.position = c("right"), legend.stack = "horizontal") +
	tmap::tm_layout(fontface = "bold", fontfamily = "") +
	tmap::tm_compass(type = "arrow", size = 2, position = c("right", "top"))

tmap::tmap_save(filename = "Maximum Temp of Ghana.png", width = 7, height = 8)


# Min Temp
tmap::tm_shape(Ghana) +
	tmap::tm_lines(lwd = 1) +
	tmap::tm_shape(co_ords) +
	tmap::tm_bubbles(col = "TMin", size = 1, palette = org) +
	tmap::tm_text(text = "STATION_NA", size = 0.4, just = "top", ymod = 0.8, xmod = -.5) +
	tmap::tm_legend(outside = TRUE, outside.position = c("right"), legend.stack = "horizontal") +
	tmap::tm_layout(fontface = "bold", fontfamily = "") +
	tmap::tm_compass(type = "arrow", size = 2, position = c("right", "top"))

tmap::tmap_save(filename = "Minimum Temp of Ghana.png", width = 7, height = 8)

