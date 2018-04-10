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

colnames(addresses) <- c("ParcelID", "Addr1", "Addr2", "Improvement", "LandValue", "Area", "Acres", "PublicStand")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Property Fire Risk Calculator Test"),
   
   # Sidebar with a Address Sections
   sidebarLayout(
      sidebarPanel(
         selectizeInput("Address",
                     "Type your address:",
                     addresses$Addr1,
                     multiple = FALSE,
                     selectize = TRUE)
      ),
      
      # Show the address searched for
      mainPanel(
         verbatimTextOutput("Output1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$Output1 <- renderPrint(
      # just print the called address
      input$Address)
}

# Run the application 
shinyApp(ui = ui, server = server)

