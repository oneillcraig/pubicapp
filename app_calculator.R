
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
#library(DT)

##Data

addresses2 <- read_csv("addresses2.csv")
addresses2$LandValue <- as.numeric(addresses2$LandValue)
addresses2$Improvement <- as.numeric(addresses2$Improvement)
addresses2$NT_HVRA <- as.numeric(addresses2$NT_HVRA)
addresses2$Min_HVRA <- as.numeric(addresses2$Min_HVRA)
addresses2$Med_HVRA <- as.numeric(addresses2$Med_HVRA)
addresses2$Opt_HVRA <- as.numeric(addresses2$Opt_HVRA)

addresses3 <- read_csv("addresses3.csv")
addresses3$LandValue <- as.numeric(addresses3$LandValue)
addresses3$Improvement <- as.numeric(addresses3$Improvement)
addresses3$HVRA <- as.numeric(addresses3$HVRA)
addresses3$TreatmentCost <- as.numeric(addresses3$TreatmentCost)
addresses3$Ntbase <- as.numeric(addresses3$Ntbase)

#addresses2$LandValue <- as.numeric(levels(addresses2$LandValue))[addresses2$LandValue]
#addresses2$Improvement <- as.numeric(levels(addresses2$Improvement))[addresses2$Improvement]

#private_parcel <- st_read(dsn = ".",
                          #layer = "Private_Parcels")

#addresses <- private_parcel %>% 
  #select(Planning, Master__35, Master__36, Improvem_1, Land.Value, Area_, Acres, Stand_Id)



#colnames(addresses) <- c("ParcelID", "Addr1", "Addr2", "Improvement", "LandValue", "Area", "Acres", "PublicStand", "geometry")

#fireignit <- read_csv("fireignitionrisk.csv")

#colnames(fireignit) <- c("ParcelID", "GA25", "PA25")

#addresses <- merge(addresses, fireignit, by = "ParcelID")

#fireloss <- read_csv("HVRAseverityloss.csv")

#colnames(fireloss) <- c("ParcelID", "NT_Loss", "Opt21_Loss")

#addresses <- merge(addresses, fireloss, by = "ParcelID")

#addresses1 <- na.omit(addresses)

#addresses1 <- addresses1[!(addresses1$ParcelID ==1159),]

#addresses2 <- addresses1 %>% 
  #st_set_geometry(NULL) %>% 
  #select(ParcelID, Addr1, Addr2, Improvement, LandValue, GA25, PA25, NT_Loss, Opt21_Loss)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    br(),
    h1("Cost Calculator demo")
  ),
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
           selectizeInput('Addr1',
                          'Type your address:',
                          choices = addresses2$Addr1,
                          multiple = FALSE,
                          #selectize = TRUE,
                          options = list(
                            maxOptions = 1,
                            placeholder = 'Please type address here',
                            onInitialize = I('function() { this.setValue(""); }')
                          )
           )#,
           #submitButton("Look up Info")
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
    span(),
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
  ),
  fluidRow(
    column(4,
           hr(),
           h4("Predicted Fire Ignition"),
           p("Probability of Fire Occuring on Land through 2050")
           
    ),
    column(4,
           hr(),
           h4("Fire Ignition Probability"),
           tableOutput("FireIgnit1")
    ),
    column(4,
           hr(),
           sliderInput("upSlider", "Custom FireIgnit Value", min = 0.01, max = 0.75, value = 0.15, step = 0.01)
    )
  ),
  fluidRow(
    column(4,
           hr(),
           h4("Predicted Fire Severity"),
           p("Predicted Damage Potential of Fire should Fire Occur without Treatment!")
           
    ),
    column(4,
           hr(),
           h4("Fire Severity Probability"),
           tableOutput("FireSev1")
    ),
    column(4,
           hr(),
           sliderInput("FireSev2", "Custom FireSev Value", min = 0.1, max = 1, value = 0.4, step = 0.1)
    )
  ),
  fluidRow(
    column(4,
           hr(),
           h4("Calculated"),
           p("Total Property Value Loss (Total Value * Fire Ignition Prob * Fire Severity)")
           
    ),
    column(4,
           hr(),
           h4("Predicted Loss by 2050 if No Treatment is done!"),
           tableOutput("Loss1")
    ),
    column(4,
           hr(),
           numericInput("Loss2", "Custom Loss Value", value = 0)
    )
  ),
  fluidRow(
    column(4,
           hr(),
           h4("Choose Treatment Intensity"),
           p("Treatments are a community action!  If the community bands together to treat more land, then there is a higher likelihood of your property's fire risk decreases")
           
    ),
    column(4,
           hr(),
           selectInput("TreatExt",
                       "Choose Treatment Extent:",
                        choices = c("NT",
                                    "Min",
                                    "Mid",
                                    "Opt"))
    ),
    column(4,
           hr(),
           p("After Selecting treatment, this is your probability for complete loss should fire occur on your parcel"),
           tableOutput("NewSev"),
           p("After selecting treatment, this is your expected losses through 2050"),
           tableOutput("NewDamages"),
           p("This is amount of money saved (negative = savings) from avoided fire damages through treatment"),
           tableOutput("DiffNT"),
           p("This is the 'evenly shared' cost of treatment based on desired treatment extent"),
           tableOutput("TreatChoice"),
           p("This is the difference between your treatment costs and savings (i dont know whats good maybe negative?)"),
           tableOutput("netgain")
    )
  ),
  fluidRow(
    column(4,
           hr(),
           h4("Treatment Costs"),
           p("Your Cost of Treating your land!")
           
    ),
    column(4,
           hr(),
           p("Estimated Treatment Cost based on Mechanical Thinning Parcel Area"),
           tableOutput("TreatmentCost1"),
           p("Estimated Treatment Cost based on Handthinning"),
           tableOutput("TreatmentcostsHT")
    ),
    column(4,
           hr(),
           numericInput("TreatmentCost2", "Custom Treatment Cost (Total, or should this be per acre?)", value = 0)
    )
  ),
  fluidRow(
    column(4,
           hr(),
           h4("Potential Avoided Fire Damage Savings"),
           p("The money you potentially save!  Not adjusted for NPV")
           
    ),
    column(4,
           hr(),
           p("Estimated based on Mechanical:"),
           tableOutput("SavingMech"),
           p("Estimated based on Handthinning:"),
           tableOutput("SavingHT")
    ),
    column(4,
           hr(),
           numericInput("TreatmentCost2", "Custom Treatment Cost (Total, or should this be per acre?)", value = 0)
    )
  )
) 


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #addresses2 <- addresses1 %>% 
    #st_set_geometry(NULL) %>% 
    #select(Addr1, Addr2, Improvement, LandValue, GA25, PA25, NT_Loss, Opt21_Loss, Acres)
  
  #addresses2 <- read_csv("addresses2.csv")
  
  #addresses2$LandValue <- as.numeric(levels(addresses2$LandValue))[addresses2$LandValue]
  #addresses2$Improvement <- as.numeric(levels(addresses2$Improvement))[addresses2$Improvement]
  
  
  #control <- input$Addr1
  
  #GA2 <- addresses2 %>% 
  #subset(Addr1 == control) %>% 
  #select(GA2)
  
  #addrInput <- reactive({
  #a <- addresses2 %>% 
  #subset(Addr1 == input$Addr1)%>% 
  #select(LandValue)
  #return(a)
  #})
  
  #output$Table1 <- renderTable(addrInput(),
  #colnames = FALSE)
  
  addrLandValue <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(LandValue)
    a[,1] <- sapply(a[,1], function(x) paste0("$",x))
    return(a)
  })
  
  output$LandVal1 <- renderTable(addrLandValue(),
                                 colnames = FALSE)
  
  addrImprove <- reactive({
    b <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Improvement)
    b[,1] <- sapply(b[,1], function(x) paste0("$",x))
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
    a[,1] <- sapply(a[,1], function(x) paste0("$",x))
    return(a)
  })
  
  output$Total1 <- renderTable(addrTotal(),
                               colnames = FALSE)
  
  addrIgnit <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(GA25) 
    return(a)
  })
  
  output$FireIgnit1 <- renderTable(addrIgnit(),
                                   colnames = FALSE)
  
  DefaultIgnit <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1) %>% 
      select(GA25)
    return(a)
  })
  
  output$IgnitSlider <- renderUI({
    sliderInput("FireIgnit2", 
                "Custom FireIgnit Value", 
                min = 0.01, 
                max = 0.75, 
                value = YYY, 
                step = 0.01)
  })
  
  #updateSliderInput(session, "upSlider",
  #label = "FireIgnit2",
  #value = GA25,
  #min = 0.01,
  #max = 0.75,
  #step = 0.001)
  
  addrHVRA <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(NT_HVRA) 
    return(a)
  })
  
  output$FireSev1 <- renderTable(addrHVRA(),
                                 colnames = FALSE)
  
  addrLoss <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Improvement, LandValue, GA25, NT_HVRA) %>% 
      mutate(Tot_NT_Loss = (Improvement + LandValue) * GA25 * NT_HVRA) %>% 
      select(Tot_NT_Loss)
    a[,1] <- sapply(a[,1], function(x) paste0("$",x))
    return(a)
  })
  
  output$Loss1 <- renderTable(addrLoss(),
                              colnames = FALSE)
  
  
  addrTreatmentCost1 <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Acres) %>% 
      mutate(TreatmentCost = ((Acres * 2750)*3)) %>% 
      select(TreatmentCost)
    a[,1] <- sapply(a[,1], function(x) paste0("$",x))
    return(a)
  })
  
  output$TreatmentCost1 <- renderTable(addrTreatmentCost1(),
                              colnames = FALSE)
  
  
  addrTreatmentsCostHT <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Acres) %>% 
      mutate(TreatmentCost = ((Acres / 0.2)*2025*3)) %>% 
      select(TreatmentCost)
    a[,1] <- sapply(a[,1], function(x) paste0("$",x))
    return(a)
  })
  
  output$TreatmentcostsHT <- renderTable(addrTreatmentsCostHT(),
                               colnames = FALSE)
  
  addrSavingMech <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Improvement, LandValue, GA25, NT_Loss, Acres) %>% 
      mutate(MechCost = (Acres * 2750)*3) %>% 
      mutate(Tot_Loss = (Improvement + LandValue) * GA25 * NT_Loss) %>% 
      mutate(SavingsMech = Tot_Loss - MechCost) %>% 
      select(SavingsMech)
    a[,1] <- sapply(a[,1], function(x) paste0("$",x))
    return(a)
  })
  
  output$SavingMech <- renderTable(addrSavingMech(),
                                         colnames = FALSE)
  
  addrSavingHT <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Improvement, LandValue, GA25, NT_Loss, Acres) %>% 
      mutate(HTCost = (Acres / 0.2)*2025*3) %>% 
      mutate(Tot_Loss = (Improvement + LandValue) * GA25 * NT_Loss) %>% 
      mutate(SavingsHT = Tot_Loss - HTCost) %>% 
      select(SavingsHT)
    a[,1] <- sapply(a[,1], function(x) paste0("$",x))
    return(a)
  })
  
  output$SavingHT <- renderTable(addrSavingHT(),
                                   colnames = FALSE)

  #####Treatment Calcs
  
  addrNewSev <- reactive({
    a <- addresses3 %>% 
      subset(Addr1 == input$Addr1)%>%
      subset(Treat == input$TreatExt) %>% 
      select(HVRA) #%>% 
    #a[,1] <- sapply(a[,1], function(x) paste0("$",x))
    return(a)
  })
  
  output$NewSev <- renderTable(addrNewSev(),
                                 colnames = FALSE)

  
  addrNewDamages <- reactive({
    b <- input$TreatExt
    a <- addresses3 %>% 
      subset(Addr1 == input$Addr1)%>% 
      subset(Treat == input$TreatExt) %>%  #Would need to stack the data by treatment ext
      select(Improvement, LandValue, GA25, HVRA) %>% 
      mutate(Tot_NT_Loss = (Improvement + LandValue) * GA25 * HVRA) %>% 
      select(Tot_NT_Loss)
    a[,1] <- sapply(a[,1], function(x) paste0("$",x))
    return(a)
  })
  
  output$NewDamages <- renderTable(addrNewDamages(),
                              colnames = FALSE)
  
  addrDiff <- reactive({
    a <- addresses3 %>% 
      subset(Addr1 == input$Addr1)%>% 
      subset(Treat == input$TreatExt) %>%  #Would need to stack the data by treatment ext
      select(Improvement, LandValue, GA25, HVRA, Ntbase) %>% 
      mutate(Tot_NT_Loss = ((Improvement + LandValue) * GA25 * HVRA) - Ntbase) %>% 
      select(Tot_NT_Loss)
    a[,1] <- sapply(a[,1], function(x) paste0("$",x))
    return(a)
  })
  
  output$DiffNT <- renderTable(addrDiff(),
                                   colnames = FALSE)
  
  addrTcost <- reactive({
    a <- addresses3 %>% 
      subset(Addr1 == input$Addr1)%>% 
      subset(Treat == input$TreatExt) %>%  #Would need to stack the data by treatment ext
      select(TreatmentCost)
    a[,1] <- sapply(a[,1], function(x) paste0("$",x))
    return(a)
  })
  
  output$TreatChoice <- renderTable(addrTcost(),
                               colnames = FALSE)
  
  addrnet <- reactive({
    a <- addresses3 %>% 
      subset(Addr1 == input$Addr1)%>% 
      subset(Treat == input$TreatExt) %>%  #Would need to stack the data by treatment ext
      select(Improvement, LandValue, GA25, HVRA, Ntbase, TreatmentCost) %>% 
      mutate(Tot_NT_Loss = (Ntbase - (Improvement + LandValue) * GA25 * HVRA) - TreatmentCost) %>% 
      select(Tot_NT_Loss)
      
    a[,1] <- sapply(a[,1], function(x) paste0("$",x))
    return(a)
  })
  
  output$netgain <- renderTable(addrnet(),
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

