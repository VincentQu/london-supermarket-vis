# Visualising London supermarket locations
# 18/11/2014

library(dplyr)
library(ggplot2)
library(gpclib)
library(maptools)

whichBorough <- function(lon, lat) {
  ans = NULL
  for(i in 1:length(lon)) {
    for(b in levels(boroughs$BOROUGH)) {
      currentPolygon <- filter(boroughs, BOROUGH == b)
      if(as.logical(point.in.polygon(lon[i], lat[i], currentPolygon$long, currentPolygon$lat))) {
        ans[i] = b
        break
      }
    }
  }
  return(ans)
}

# Reading in the shape files for background layers of plots 
gpclibPermit()

london <- readShapePoly("../shp/London_Boundary.shp")
london <- fortify(london)

boroughs <- readShapePoly("../shp/London_Boroughs.shp")
boroughs_geom <- fortify(boroughs, region = "NUMBER")
boroughs <- merge(boroughs_geom, boroughs@data, by.x = "id", by.y = "NUMBER")
rm(boroughs_geom)

thames <- readShapePoly("../shp/River_Thames_Longer.shp")
thames  <- fortify(thames)
thames <- filter(thames, long < 0.28)

# Reading in supermarket data
supermarkets <- tbl_df(read.csv(file = "../Data/Supermarket_Locations.txt", header = TRUE, sep = "\t"))

# Processing supermarket data frame
supermarkets <- supermarkets %>%
  # Selecting relevant columns
  select(ID:Fascia, Locality:LatWGS84) %>%
  # Renaming columns
  rename(RetailerDetail = Fascia, Lon = LongWGS84, Lat = LatWGS84) %>%
  # Filtering supermarkets within London's boundaries
  filter(as.logical(point.in.polygon(Lon, Lat, london$long, london$lat))) %>%
  # Converting ID into a factor
  mutate(ID = factor(ID),
  # Assigning the corresponding borough to each supermarket (takes a few minutes to run!)       
         Borough = whichBorough(Lon, Lat))

# Saving data frames (including boroughs and Thames)
save(supermarkets, boroughs, thames, file = "supermarket_data.rda")
