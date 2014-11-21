# Supermarket visualisations
# 18/11/2014

rm(list=ls())
library(dplyr)
library(ggplot2)
library(grid)


load("supermarket_data.rda")

# Plotting location of all London supermarkets
ggplot() + 
  geom_polygon(data = boroughs, aes(x = long, y = lat, group = group), 
               fill = "#333333", color = "#444444") +
  geom_polygon(data = thames, aes(x = long, y = lat),
               fill = "#77A2B8", color = NA) +
  geom_point(data = supermarkets, aes(x = Lon, y = Lat, color = Retailer)) +
  coord_map() + 
  theme(
    panel.background = element_rect(fill="#444444"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

# Analysing the frequency of each retailer
supermarkets %>%
  group_by(Retailer) %>%
  tally(sort = TRUE)

topSupermarkets <- filter(supermarkets, Retailer %in% c("Tesco", "Sainsburys", "Marks And Spencer", "Waitrose"))
otherSupermarkets <- filter(supermarkets, !(Retailer %in% c("Tesco", "Sainsburys", "Marks And Spencer", "Waitrose")))

# Map top supermarkets -- all on one map
ggplot() + 
  geom_polygon(data = boroughs, aes(x = long, y = lat, group = group), 
               fill = "#F9F5F1", color = "#E9E5E1", size = 0.8) +
  geom_polygon(data = thames, aes(x = long, y = lat),
               fill = "#77A2B8", color = NA) +
  geom_point(data = topSupermarkets, aes(x = Lon, y = Lat, color = Retailer, shape = Retailer),
             size = 4) +
  coord_map() + 
  scale_color_manual(values = c("#095530", "#E87907", "#2E4496", "#6AA628")) +
  scale_shape_manual(values = c("M", "S", "T", "W")) +
  guides(shape = guide_legend(override.aes = list(size=6))) +
  theme(
    panel.background = element_rect(fill="#E9E5E1"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(family = "Oswald", size=16, colour = "#898581", face = "plain"),
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    legend.key.size = unit(30, "points"),
    panel.border = element_rect(color = "#898581", size = 4, fill = NA)
  )

# Map top supermarkets -- each one into on an individual map
# Change order ov levels for visualisation purposes
topSupermarkets  <- mutate(topSupermarkets,
                           Retailer = factor(Retailer, levels =
                                               c("Sainsburys", "Tesco", "Waitrose", "Marks And Spencer")))

ggplot() + 
  # Boroughs
  geom_polygon(data = boroughs, aes(x = long, y = lat, group = group), 
               fill = "#F9F5F1", color = "#E9E5E1", size = 0.8) +
  # River Thames
  geom_polygon(data = thames, aes(x = long, y = lat),
               fill = "#77A2B8", color = NA) +
  # Supermarkets
  geom_point(data = topSupermarkets, aes(x = Lon, y = Lat, color = Retailer, shape = Retailer),
             size = 4.5) +
  # Facet by Retailer
  facet_wrap(~ Retailer) +
  coord_map() + 
  scale_color_manual(values = c("#E87907", "#2E4496", "#6AA628", "#095530")) +
  scale_shape_manual(values = c("S", "T", "W", "M")) +
  theme(
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill="#E9E5E1"),
    panel.border = element_rect(color = "#898581", size = 4, fill = NA),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "#898581"),
    strip.text = element_text(color = "#F9F5F1", family = "Oswald", face = "plain", size = 18)
  )


# Calculate the Tesco/Sainsbury's ratio for each borough
TSRatio <- supermarkets %>%
  filter(Retailer %in% c("Tesco", "Sainsburys")) %>%
  group_by(Borough, Retailer) %>%
  tally() %>%
  summarise(TSRatio = n[2]/n[1]) %>%
  arrange(desc(TSRatio))

boroughs <- merge(boroughs, TSRatio, by.x = "BOROUGH", by.y = "Borough")

ggplot(boroughs, aes(x = long, y = lat, group = group, fill = TSRatio)) +
  geom_polygon(size = 0.7, colour = "#E9E5E1") + 
  scale_fill_gradient2(low = "#FA6900", mid = "white", high = "#00B5D1", midpoint = 1,
                       guide = guide_colorbar(barwidth = 2, barheight = 10, nbin = 100)) +
  coord_map() +
  ggtitle(label = "Tesco/Sainsbury's Ratio") +
  annotate("text", x = 0.08, y = 51.69, hjust = 0, color = "#696561", label = "The darker the shade of blue") +
  annotate("text", x = 0.08, y = 51.67, hjust = 0, color = "#696561", label = "the higher the relative frequency") +
  annotate("text", x = 0.08, y = 51.65, hjust = 0, color = "#696561", label = "of Tesco branches") +
  annotate("text", x = -0.55, y = 51.33, hjust = 0, color = "#696561", label = "Only three boroughs") +
  annotate("text", x = -0.55, y = 51.31, hjust = 0, color = "#696561", label = "(Sutton, Camden, and Wandsworth)") +
  annotate("text", x = -0.55, y = 51.29, hjust = 0, color = "#696561", label = "have more Sainsbury's than Tesco branches") +
  theme(
    text = element_text(family = "Oswald"),
    panel.background = element_rect(fill="#E9E5E1"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.background = element_rect(fill = NA),
    legend.title = element_blank(),
    legend.text = element_text(family = "Oswald", size=16, colour = "#898581", face = "plain"),
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    legend.key.size = unit(30, "points"),
    panel.border = element_rect(color = "#898581", size = 4, fill = NA),
    plot.title = element_text(family = "Roboto Slab", size=40, colour = "#696561", face = "bold", vjust = 1.2)
  )

# Map other supermarkets
ggplot() + 
  geom_polygon(data = boroughs, aes(x = long, y = lat, group = group), 
               fill = "#F9F5F1", color = "#E9E5E1", size = 0.8) +
  geom_polygon(data = thames, aes(x = long, y = lat),
               fill = "#77A2B8", color = NA) +
  geom_point(data = otherSupermarkets, aes(x = Lon, y = Lat, color = Retailer, shape = Retailer),
             size = 3.5) +
  coord_map() + 
  #   scale_color_manual(values = c("#093D30", "#E87907", "#2E4496", "#6AA628")) +
  theme(
    panel.background = element_rect(fill="#E9E5E1"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )