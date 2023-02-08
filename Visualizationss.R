# Visualizations ####
install.packages("trelliscopjs")
library(trelliscopejs)
library(ggplot2)


# Data Import
listed = read.csv("path to listed.csv", header = TRUE, stringsAsFactors = TRUE)

# plotting
ggplot(data = listed, aes(x = Year, y = TMax)) +
	geom_line(col = "firebrick") +
	# Conditioning by Stations
	facet_trelliscope(~ Stations)


