# Visualising London supermarket locations
# 18/11/2014

library(dplyr)
library(ggplot2)
library(gpclib)
library(maptools)
library(sp)

# Function that assigns a London borough to a pair of coordinates
whichBorough <- function(lon, lat) {
  # Requires the boroughs shape file to be loaded into R
  # Very slow!
  # Based on point.in.polygon() from the sp package
  # TODO:
  # - Add input validation
  #   - length(lon) == length(lat)
  #   - is.numeric(lon) & is.numeric(lat)
  # - Modify output for coordinates that are not within London (currently NA)
  
  # Declaring variable that will be returned
  ans = NULL
  # Looping over coordinate pairs (input variable)
  for(i in 1:length(lon)) {
    # Looping over boroughs
    for(b in levels(boroughs$BOROUGH)) {
      # Temporarily saving the current borough
      currentPolygon <- filter(boroughs, BOROUGH == b)
      # Check if coordinate pair is in borough
      if(as.logical(point.in.polygon(lon[i], lat[i], currentPolygon$long, currentPolygon$lat))) {
        # If so, then save the borough in the ans vector, break out of the (borough) loop,
        # and jump to next coordinate pair
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
thames <- fortify(thames)
thames <- filter(thames, long < 0.28)  # "Cutting off" parts of the river in the far east (not needed for visualisation)

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
  mutate(
    # Converting ID into a factor
    ID = factor(ID),
    # Assigning the corresponding borough to each supermarket (takes a few minutes to run!)       
    Borough = whichBorough(Lon, Lat))

# Saving data frames (including boroughs and Thames)
save(supermarkets, boroughs, thames, file = "supermarket_data.rda")
