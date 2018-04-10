#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(tidyverse)

##Data
private_parcel <- st_read(dsn = ".",
                          layer = "Private_Parcels")

addresses <- private_parcel %>% 
  select(Planning, Master__35, Master__36, Improvem_1, Land.Value, Area_, Acres, Stand_Id)


  
colnames(addresses) <- c("ParcelID", "Addr1", "Addr2", "Improvement", "LandValue", "Area", "Acres", "PublicStand", "geometry")

addresses1 <- na.omit(addresses)

addresses1 <- addresses1[!(addresses1$ParcelID ==1159),]

addresses2 <- addresses1 %>% 
  st_set_geometry(NULL)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Property Fire Risk Calculator Test"),
   
   # Sidebar with a Address Sections
   sidebarLayout(
      sidebarPanel(
         selectInput('Addr1',
                     'Type your address:',
                     choices = unique1,
                     multiple = TRUE,
                     selectize = TRUE),
         submitButton("Look up City")
      ),
      
      # Show the address searched for
      mainPanel(
         tableOutput("Output1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  Output1 <- reactive({addresses2[addresses2$Addr1 %in% input$"Addr1", ]})
    
  output$Output1 <- renderTable(Output1())
      # just print the city related to the addr
      
}

# Run the application 
shinyApp(ui = ui, server = server)

