# ui.R

shinyUI(fluidPage(

  # Title
  titlePanel("Location of London Supermarkets"),
  
  sidebarLayout(
    
    # Sidebar
    sidebarPanel(
      
#       helpText("Choose the retailers that you want to map."),
      
      # Checkboxes (Retailer) 
      radioButtons("retailer", label = h3("Retailer"),
                         c("Tesco" = "Tesco",
                           "Sainsbury's" = "Sainsburys",
                           "Marks & Spencer" = "Marks And Spencer",
                           "Waitrose" = "Waitrose",
                           "Aldi" = "Aldi",
                           "Lidl" = "Lidl"),
                         selected = "Tesco")
      ),
    
    # Main Panel
    mainPanel(
      
      textOutput("text01"),
      
      plotOutput("plot01"),
      
      verbatimTextOutput("text02")
      
      )
    
    )
  
  ))