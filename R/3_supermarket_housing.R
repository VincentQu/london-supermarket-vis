# Comparing patterns in disseminations of supermarkets and housing prices from 2013
# 20/11/2014

rm(list=ls())
library(dplyr)
library(ggplot2)
library(grid)


load("supermarket_data.rda")

housing13.raw <- tbl_df(read.csv("../Data/Housing/Housing_Prices_2013.csv", header = TRUE))

housing13 <- housing13.raw %>%
  select(date, region, Lon, Lat, type, price) %>%
  mutate(date = as.POSIXct(strptime(date, "%Y-%m-%d %H:%M"))) %>%
  filter(Lon > -1,
         price > 50000)

ggplot(housing13, aes(x = price)) +
  geom_histogram() +
  scale_x_continuous(trans = "log10")

### Mapping all houses coloured and sized based on their price
### plus the location of London Waitrose branches

ggplot() +
  # Boroughs
  geom_polygon(data = boroughs, aes(x = long, y = lat, group = group), 
               fill = "#FEFCFA", color = "#EEEAE6", size = 0.8) +
  # Thames
  geom_polygon(data = thames, aes(x = long, y = lat),
               fill = "#77A2B8", color = NA) +
  # Housing prices
  geom_point(data = housing13, aes(x = Lon, y = Lat, colour = price, size = price), alpha = 0.7) +
  scale_colour_gradient(low = "#FCFCBE", high = "#E08E79", trans = "log10",
                        breaks = c(1e5, 1e6, 1e7), labels = c("£100,000", "£1,000,000", "£10,000,000"),
                        guide = guide_colorbar(title = "Housing Price in £", barwidth = 2, barheight = 10,
                                               ticks = FALSE, nbin = 50, order = 1)) +
  scale_size_continuous(range = c(0.3, 1.5), trans = "log10", guide = FALSE) +  # guide = FALSE removes legend
  # Waitrose branches
  geom_point(data = filter(supermarkets, Retailer == "Waitrose"),
             aes(x = Lon, y = Lat, shape = "Waitrose"), colour = "#6AA628", size = 4) +
  scale_shape_manual(values = 17,
                     guide = guide_legend(title = element_blank())) +
  # Annotations
  annotate("text", x = -0.5, y = 51.29, fontface = 2, hjust = 0, color = "#696561", label = "Sources:") +
  annotate("text", x = -0.5, y = 51.282, hjust = 0, color = "#696561", label = "landregistry.gov.uk") +
  annotate("text", x = -0.5, y = 51.274, hjust = 0, color = "#696561", label = "geolytix.co.uk/blog/?p=238") +
  # Map title
  ggtitle(toupper("Housing prices 2013 and Waitrose branches in London")) +
  # Coordinate system
  coord_map() +
  # Theme
  theme(
    text = element_text(family = "Oswald", face = "plain"),
    panel.background = element_rect(fill="#EEEAE6"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.background = element_rect(fill = NA),
    legend.title = element_text(size=20, colour = "#898581", face = "plain"),
    legend.text = element_text(size=16, colour = "#898581"),
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    legend.key = element_rect(fill = NA, colour = NA),
    panel.border = element_rect(color = "#898581", size = 4, fill = NA),
    plot.title = element_text(size=40, colour = "#696561", vjust = 1.2)
  )
