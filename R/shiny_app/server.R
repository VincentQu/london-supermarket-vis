# server.R
load("../supermarket_data.rda")
library(dplyr)
library(ggplot2)
library(grid)

topSupermarkets <- filter(supermarkets, Retailer %in% c("Tesco", "Sainsburys", "Marks And Spencer", "Waitrose"))

shinyServer(function(input, output) {

  sprmrkt <- reactive(filter(supermarkets, Retailer == input$retailer))
  
  p1 <- renderPlot(ggplot() + 
  # Boroughs
  geom_polygon(data = boroughs, aes(x = long, y = lat, group = group), 
               fill = "#F9F5F1", color = "#E9E5E1", size = 0.8) +
  # Thames
  geom_polygon(data = thames, aes(x = long, y = lat),
               fill = "#77A2B8", color = NA) +
  # Supermarkets
  geom_point(data = filter(supermarkets, Retailer == input$retailer),
             aes(x = Lon, y = Lat), size = 2, shape = 17) +
#   scale_color_manual(values = c("#095530", "#E87907", "#2E4496", "#6AA628")) +
  scale_shape_manual(values = c("M", "S", "T", "W"),
                     guide = FALSE) +
  # Coordinate system
  coord_map() + 
  # Theme
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
  ))
  
  output$text01 <- renderText(c("Location of", input$retailer, "branches in London"))
  
  output$text02 <- renderPrint(head(subset(supermarkets, Retailer == input$retailer)))
  
  output$plot01 <- p1
  
})
