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
library(DT)

##Data
private_parcel <- st_read(dsn = ".",
                          layer = "Private_Parcels")

addresses <- private_parcel %>% 
  select(Planning, Master__35, Master__36, Improvem_1, Land.Value, Area_, Acres, Stand_Id)


  
colnames(addresses) <- c("ParcelID", "Addr1", "Addr2", "Improvement", "LandValue", "Area", "Acres", "PublicStand", "geometry")

addresses1 <- na.omit(addresses)

addresses1 <- addresses1[!(addresses1$ParcelID ==1159),]

addresses2 <- addresses1 %>% 
  st_set_geometry(NULL) %>% 
  select(Addr1, Addr2, Improvement, LandValue)

# Define UI for application that draws a histogram
ui <- fluidPage(
   br(),
   fluidRow(
   # Application title
   column(4,
     h4("Search Property"),
     p("Look up expected Values for your Property")
   ),
   
   # Sidebar with a Address Sections
   column(4,
          h4("Expected Values"),
          p("These are predicted values for your property")
   ),
   
   column(4,
          h4("Enter Custom Values"),
          p("Enter Custom Values if you feel predicted values are inaccurate")
          )
   ),
   #Address Confirmation
   fluidRow(
     column(4,
            hr(),
            selectInput('Addr1',
                        'Type your address:',
                        choices = addresses2$Addr1,
                        multiple = TRUE,
                        selectize = TRUE),
            submitButton("Look up Info")
     ),
     column(4,
            hr(),
            h4("City, State, Zip Code"),
            tableOutput("Address2")
     ),
     column(4,
            hr(),
            h4(),
            h4("Confirm Address"),
            p("Check to make sure your address looks right!")
     )
   ),
   fluidRow(
     column(4,
            hr(),
            h4("Land Value"),
            p("This is the value of your land, not including improvements, structures, etc")
            
     ),
     column(4,
            hr(),
            h4("Predicted Land Value"),
            tableOutput("LandVal1")
            ),
     column(4,
            hr(),
            numericInput("landvalue", "Custom LandValue", value = 0)
     )
   ),

   fluidRow(
     column(4,
            hr(),
            h4("Improvements Value"),
            p("This is the predicted Improvements Value for your Parcel")
            ),
     column(4,
            hr(),
            h4("Predicted Improvements"),
            tableOutput("Improve1")
     ),
     column(4,
            hr(),
            numericInput("LandValue", "InputLV", value = 0, step = NA)
     )
   ),
   fluidRow(
     column(4,
            hr(),
            h4("Calculated"),
            p("Total Property Value (Sum of Land Value + Improvements")
            
     ),
     column(4,
            hr(),
            h4("Predicted Total Value"),
            tableOutput("Total1")
     ),
     column(4,
            hr(),
            numericInput("TotVal2", "Custom Total Value", value = 0)
     )
   )
) 


# Define server logic required to draw a histogram
server <- function(input, output) {

    addresses2 <- addresses1 %>% 
      st_set_geometry(NULL) %>% 
      select(Addr1, Addr2, Improvement, LandValue)
    addresses2$LandValue <- as.numeric(levels(addresses2$LandValue))[addresses2$LandValue]
    addresses2$Improvement <- as.numeric(levels(addresses2$Improvement))[addresses2$Improvement]
    
      
    addrInput <- reactive({
      a <- addresses2 %>% 
        subset(Addr1 == input$Addr1)%>% 
        select(LandValue)
      return(a)
    })
    
    output$Table1 <- renderTable(addrInput(),
                                 colnames = FALSE)
    
    addrLandValue <- reactive({
      a <- addresses2 %>% 
        subset(Addr1 == input$Addr1)%>% 
        select(LandValue)
      return(a)
    })
    
    output$LandVal1 <- renderTable(addrLandValue(),
                                 colnames = FALSE)
    
    addrImprove <- reactive({
      b <- addresses2 %>% 
        subset(Addr1 == input$Addr1)%>% 
        select(Improvement)
      return(b)
    })
    
    output$Improve1 <- renderTable(addrImprove(),
                                 colnames = FALSE)

  
    #output$LandVal1 <- renderText({
    #W <- addresses2$LandValue[addresses2$Addr1 %in% input$"Addr1"]
    #as.vector(W)
  
    #})
  
  #output$Improve1 <- renderText({
    #Imp1 <- addresses2$Improvement[addresses2$Addr1 %in% input$"Addr1"]
    #as.vector(Imp1)
    
  #})
  
  output$Address2 <- renderTable({
    Address2 <- addresses2$Addr2[addresses2$Addr1 %in% input$"Addr1"]
    
  }, colnames = FALSE)

  
  addrTotal <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Improvement, LandValue) %>% 
      mutate(Total = Improvement + LandValue) %>% 
      select(Total)
    return(a)
  })
  
  output$Total1 <- renderTable(addrTotal(),
                                 colnames = FALSE)
    
  #output$TotVal1 <- renderText({
   # land <- addresses2$LandValue[addresses2$Addr1 %in% input$"Addr1"]
  #  land <- as.vector(land)
   # Impr <- addresses2$Improvement[addresses2$Addr1 %in% input$"Addr1"]
    #Impr <- as.vector(Impr)
    #paste("This is the result =", land+Impr)
  #})
    
  
  #Addr2 <- reactive({addresses2[addresses2$Addr1 %in% input$"Addr1", ]}) 
  
  #Output2 <- reactive({addresses2[addresses2$Addr1 %in% input$"Addr1", ]})
  
  #output$Table1 <- renderDataTable({
    #reactive({addresses2[addresses2$Addr1 %in% input$Addr1]})
   # })
      
  #output$Table2 <- renderTable(Output2())    
}

# Run the application 
shinyApp(ui = ui, server = server)

