#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Final App, dashboard


library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(sp)
library(raster)
library(tidyverse)
library(shinycssloaders)

private_parcel <- st_read(dsn = ".",
                          layer = "Private_Parcels")

addresses <- private_parcel %>% 
  select(Planning, Master__35, Master__36, Improvem_1, Land.Value, Area_, Acres, Stand_Id)



colnames(addresses) <- c("ParcelID", "Addr1", "Addr2", "Improvement", "LandValue", "Area", "Acres", "PublicStand", "geometry")

fireignit <- read_csv("fireignitionrisk.csv")

colnames(fireignit) <- c("ParcelID", "GA25", "PA25")

addresses <- merge(addresses, fireignit, by = "ParcelID")

fireloss <- read_csv("HVRAseverityloss.csv")

colnames(fireloss) <- c("ParcelID", "NT_Loss", "Opt21_Loss")

addresses <- merge(addresses, fireloss, by = "ParcelID")

addresses1 <- na.omit(addresses)

addresses1 <- addresses1[!(addresses1$ParcelID ==1159),]

addresses2 <- addresses1 %>% 
  st_set_geometry(NULL) %>% 
  select(Addr1, Addr2, Improvement, LandValue, GA25, PA25, NT_Loss, Opt21_Loss)


private <- st_read(dsn = ".", layer = "Private_Parcels")

private_t <- st_transform(private, "+init=epsg:4326")

private_tclass <- private_t %>% 
  select(Stand_Id)

carbon <- read.csv("carbon.csv")
carbon$Discount_Rate <- as.factor(carbon$Discount_Rate)

cba <- read.csv("cba.csv")
cba$Discount_Rate <- as.factor(cba$Discount_Rate)


condclass <- st_read(dsn = ".", layer = "conclassdiss_mod")
cond_df <- st_transform(condclass, "+init=epsg:4326")
cond_class <- cond_df %>%
  select(Departure)

condNames <- unique(cond_class$Departure)
palcond <- colorFactor(palette = c("yellow","orange","red"), domain = condNames)

fireregime <- st_read(dsn = ".", layer = "Fire_Regimes_Mod")
regime_df <- st_transform(fireregime, "+init=epsg:4326")
regime_class <- regime_df %>%
  select(FireRegime)

Low_Severity <- regime_class %>%
  filter(FireRegime == "I - Low Severity Fire")
Mid_Severity <- regime_class %>%
  filter(FireRegime == "III - Mixed Severity FIre")
High_Severity <- regime_class %>%
  filter(FireRegime == "IV = High Severity Fire")

SAF_Cover <- st_read(dsn = ".", layer = "ForestCover")
SAF_Cover_df <- st_transform(SAF_Cover, "+init=epsg:4326")
SAF_class <- SAF_Cover_df %>%
  select(SAF_Name)

dinkey_boundary <- st_read(dsn = ".", layer = "DinkeyBoundary")
dinkey_df <- st_transform(dinkey_boundary, "+init=epsg:4326")

SAFNames <- unique(SAF_class$SAF_Name)
palrainbow <- colorFactor(palette = rainbow(18), domain = SAFNames)

RegimeNames <- unique(fireregime$FireRegime)
palfireregime <- colorFactor(palette = c("burlywood4", "yellow","orange","red", "darkgrey", "deepskyblue"), domain = RegimeNames)


Subset_2000 <- read.csv("Subset_2000.csv")

SDI260_CFL_Change <- Subset_2000 %>% 
  mutate(CFL_Change = Average_260 - Average_nt) %>% 
  select(CFL_Change)


choice <- c("Aspect", "Slope", "Elevation")


tiff_stack <- raster::stack("Aspect.tif", "Slope.tif", "Elevation.tif")
# could convert these to shape files, but tiffs appear smaller so it wouldn't make it faster





private <- st_read(dsn = ".", layer = "Private_Parcels")

private_t <- st_transform(private, "+init=epsg:4326")

private_tclass <- private_t %>% 
  select(Stand_Id)



private_cfl <- st_read(dsn = '.', layer = "private_cfls")

private_tcfl <- st_transform(private_cfl, "+init=epsg:4326") %>% 
  select(CFL_Cat_30, NoTreatSev, IgnitGA25, geometry)

Treatment <- private_tcfl%>% 
  select(CFL_Cat_30) %>% 
  mutate("Treatment")
colnames(Treatment) <- c("Severity", "Treatment", "geometry")

NoTreatment <- private_tcfl%>% 
  select(NoTreatSev) %>% 
  mutate("No Treatment")
colnames(NoTreatment) <- c("Severity", "Treatment", "geometry")

DataT <- rbind(NoTreatment, Treatment)

color <- colorFactor(palette = "Reds", 
                     domain = c(0,1,2,3,4,5,6), 
                     na.color = "transparent")


dinkey_boundary <- st_read(dsn = '.', layer = "DinkeyBoundary")
dinkey_df <- st_transform(dinkey_boundary, "+init=epsg:4326")




# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  
  
  
  dashboardHeader(title = "Saving Sierras App"),
  
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("About Page", tabName = "tab_7"),
      menuItem("Topographical Information", tabName = "tab_2"),
      menuItem("Fire History", tabName = "tab_3"),
      menuItem("Forest Cover", tabName = "tab_4"),
      menuItem("Fire Severity on Public Lands", tabName = "tab_1"),
      menuItem("Fire Severity on Private Lands", tabName = "tab_5"),
      menuItem("Cost Benefit Analysis", tabName = "tab_6"),
      menuItem("Cost Calculator", tabName = "tab_8")
  
      )
      
    ),
  
  
  dashboardBody(
    tabItems(
      
      
      tabItem(tabName = "tab_7",
              
              fluidRow(
                
                box(includeText("about.txt"))
              )
              
              ),
      
      tabItem(tabName = "tab_1",
              fluidRow(
                box(withSpinner(plotOutput("my_graph1")), width = 12), 
               # box(title = "Title",
                #    checkboxInput(inputId = "addmean",
                #                  label = "Add Mean Line?",
                #                  value = FALSE)),
                tabBox(tabPanel("Summary", verbatimTextOutput("summary")), width = 12),
               box(includeText("test.txt"), width = 12)
              )),
      
      tabItem(tabName = "tab_2",
              fluidRow(
                box(withSpinner(leafletOutput("my_graph2", height = 432))),
                box(title = "Private Lands Maps",
                    selectInput("class", 
                                "Choose Map:", 
                                choices = choice)),
                
                box(includeText("topographical.txt"), width = 12)
              )),

      tabItem(tabName = "tab_3",
              fluidRow(
                box(withSpinner(leafletOutput("my_graph3", height = 432))),
                box(title = "Historical Fire Regime",
                    selectInput("regime_class", 
                                "Choose Regime Level:", 
                                choices = unique(regime_class$FireRegime))),
                box(includeText("firehistory.txt"), width = 6)
              ),
              
              fluidRow(
                box(withSpinner(leafletOutput("my_graph7", height = 432))),
                box(title = "Change in Fire Regime",
                    selectInput("cond_class", 
                                "Choose Level of Change:", 
                                choices = unique(cond_class$Departure))),
                
                box(includeText("departure.txt"), width = 6)
              )
      ),
      
      tabItem(tabName = "tab_4",
              fluidRow(
                box(withSpinner(leafletOutput("my_graph4", height = 700, width = 700))),
                
                
                
                box(includeText("forestcover.txt"), width = 12)
                
                
                
            
              )
      
              ),
      
      tabItem(tabName = "tab_5",
              fluidRow(
                box(withSpinner(leafletOutput("my_graph5", height = 432))),
                box(title = "Private Lands Fire Severity",
                    selectInput("treatment", 
                                "Choose Treatment Type:", 
                                choices = unique(DataT$Treatment))),
                
                box(includeText("fireprivate.txt"), width = 12)
              )
      ),
      
      tabItem(tabName = "tab_6",
      fluidRow(
        
        box(withSpinner(plotOutput(outputId = "distPlot2"
                       
        )), width = 6),
        
        box(selectInput("Stakeholder", "Choose Stakeholder:", choices = unique(cba$Stakeholder)),
            selectInput("Treatment_Type", "Choose Treatment Type:", choices = unique(cba$Treatment_Type)),
            selectInput("Climate_Model2", "Choose Climate Model:", choices = unique(cba$Climate_Model2)), width = 6)),
      
      fluidRow(
        
        box(withSpinner(plotOutput(outputId = "distPlot"
                       
        )), width = 6), 
        
        box(
          selectInput("Climate_Model", "Choose Climate Model:", choices = unique(carbon$Climate_Model)), 
          sliderInput(inputId = "Year",
                      label = "Year",
                      min = 2018,
                      max = 2050,
                      value = 2050,
                      sep = "")),
        
        box(includeText("cba.txt"), width = 12)
        )
      
      ),
      tabItem(tabName = "tab_8",
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
                       ),
                       actionButton("update", "Look up Info", icon("refresh"), class = "btn btn-primary")
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
      )
    )
  )



    
  
server <- function(input, output){
  
  
  output$my_graph1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- SDI260_CFL_Change$CFL_Change 
  
    
    output$summary <- renderPrint({
      x <- SDI260_CFL_Change$CFL_Change             # Define x again
      summary(x, digits = 3)
    })
    
    ggplot(SDI260_CFL_Change, aes(SDI260_CFL_Change$CFL_Change)) +
      geom_histogram(binwidth = 0.08, alpha = 1, color = "black", size = 0.5, aes(fill = cut(CFL_Change, c(-Inf, 0.04, Inf)))) +
      scale_fill_manual(name = "CFL_Change", values = c("(-Inf,0.04]" = "forestgreen",
                                                        "(0.04, Inf]" = "firebrick"),
                        labels = c("Less Than 0", "Greater Than 0")) +
      theme_bw() +
      theme(legend.position = "none") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(title = "Change in Fire Severity Across the Landscape", x = "Conditional Flame Length Change (ft)", y = "Count") 
    
    # draw the histogram with the specified number of bins
#    if(input$addmean) {
#       
#       abline(v = mean(x),
#              lwd = 2,
#              lty = 2)
#       
#    }
    
  })
 
  
  
  
  
  
  output$my_graph2 <- renderLeaflet({
    
    tiffmap <- subset(tiff_stack, input$class, drop=TRUE)
    
    
    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(tiffmap),
                        na.color = "transparent")
    #leaflet(private_tclass) %>% 
    #addTiles() %>% 
    
    
    leaflet(private_tclass) %>% 
      addTiles() %>% 
      addRasterImage(tiffmap, colors = pal, opacity = 0.8) %>% 
      addLegend (pal = pal, values = values(tiffmap),
                 title = input$class) %>% 
      addPolygons(color = "black",
                  weight = 0.5, fill = NA) %>%
      addPolygons(data = dinkey_df,
                  weight = 1,
                  color = "black",
                  fillColor = "transparent")
    
    
    
    
    
  })
  
  output$my_graph3 <- renderLeaflet({
    regime_sub <- regime_class %>%
      filter(FireRegime == input$regime_class)

    leaflet(regime_sub) %>% 
      addTiles() %>% 
      addPolygons(weight = 0.5,
                  color = "black",
                  fillColor = ~palfireregime(RegimeNames),
                  fillOpacity = 0.5) %>%
      addPolygons(data = dinkey_df,
                  weight = 1,
                  color = "black",
                  fillColor = "transparent") %>%
      addPolygons(data = private_tclass,
                  weight = 0.5,
                  color = "black",
                  fillColor = "yellow",
                  fillOpacity = 0.3)})
    
  output$my_graph7 <- renderLeaflet({
    cond_sub <- cond_class %>%
      filter(Departure == input$cond_class)
    
    leaflet(cond_sub) %>% 
      addTiles() %>% 
      addPolygons(weight = 0.5,
                  color = "black",
                  fillColor = ~palcond(condNames),
                  fillOpacity = 0.5) %>%
      addPolygons(data = dinkey_df,
                  weight = 1,
                  color = "black",
                  fillColor = "transparent") %>%
      addPolygons(data = private_tclass,
                  weight = 0.5,
                  color = "black",
                  fillColor = "yellow",
                  fillOpacity = 0.3)})
  
  output$my_graph4 <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data = dinkey_df,
                  weight = 1,
                  color = "black",
                  fillColor = "grey",
                  fillOpacity = 0.5,
                  group = "Dinkey Boundary") %>%
      addPolygons(data = SAF_class,
                  weight = 0.5,
                  color = "black",
                  fillColor = ~palrainbow(SAFNames),
                  fillOpacity = 0.5,
                  group = "Vegetation") %>%
      addPolygons(data = private_tclass,
                  weight = 0.5,
                  color = "black",
                  fillColor = "yellow",
                  fillOpacity = 0.5,
                  group = "Private Parcels") %>%
      addLegend(pal = palrainbow, 
                values = SAFNames,
                title = "Forest Cover Types") %>%
      addLayersControl(
        baseGroups = c("Vegetation"),
        overlayGroups = c("Dinkey Boundary", "Private Parcels"),
        options = layersControlOptions(collapsed = FALSE)
      )
      
      
    })
  
  output$my_graph5 <- renderLeaflet({
    private_map <- DataT %>%
      filter(Treatment == input$treatment)
    
    leaflet(private_map) %>% 
      addTiles() %>% 
      addPolygons(weight = 0.5,
                  color = "Black",
                  fillColor = ~color(private_map$Severity),
                  fillOpacity = .9) %>%
      addPolygons(data = dinkey_df,
                  weight = 2.0,
                  color = "Grey",
                  fillColor = "Transparent",
                  opacity = 1.0) %>% 
      addLegend (pal = color, values = DataT$Severity,
                 title = "Fire Severity Level",
                 opacity = 1.0)
  })
  
  output$distPlot <- renderPlot({
    x <- carbon$DifferenceToDate
    Years <- input$Year 
    pick_model <- input$Climate_Model
    ggplot(subset(carbon, Year == Years & Climate_Model == pick_model), aes(x = Discount_Rate, y = DifferenceToDate)) +
      geom_col(aes(fill = Discount_Rate)) +
      theme_bw(base_size = 13) +
      ylab("Cumulative Value ($US Millions)") +
      xlab("Discount Rate (%)") + 
      ggtitle("Value of Carbon Sequestration (in $US)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(expand = c(0,0), limits = (c(0, 15))) +
      coord_flip() + 
      guides(fill = FALSE)
  })
  
  output$distPlot2 <- renderPlot({
    x <- cba$NPV
    Stakeholders <- input$Stakeholder 
    pick_treatment <- input$Treatment_Type
    pick_model2 <- input$Climate_Model2
    ggplot(subset(cba, Stakeholder == Stakeholders & Treatment_Type == pick_treatment & Climate_Model2 == pick_model2), aes(x = Discount_Rate, y = NPV)) +
      geom_col(aes(fill = Discount_Rate)) +
      theme_bw(base_size = 13) +
      ylab("Net Value (US$ Millions)") +
      xlab("Discount Rate (%)") + 
      ggtitle("Value of Fuel Treatments (in $US)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(expand = c(0,0), limits = (c(0, 350))) +
      coord_flip() + 
      guides(fill = FALSE)
  })

  addresses2 <- addresses1 %>% 
    st_set_geometry(NULL) %>% 
    select(Addr1, Addr2, Improvement, LandValue, GA25, PA25, NT_Loss, Opt21_Loss, Acres)
  addresses2$LandValue <- as.numeric(levels(addresses2$LandValue))[addresses2$LandValue]
  addresses2$Improvement <- as.numeric(levels(addresses2$Improvement))[addresses2$Improvement]
  
  
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
      select(NT_Loss) 
    return(a)
  })
  
  output$FireSev1 <- renderTable(addrHVRA(),
                                 colnames = FALSE)
  
  addrLoss <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Improvement, LandValue, GA25, NT_Loss) %>% 
      mutate(Tot_NT_Loss = (Improvement + LandValue) * GA25 * NT_Loss) %>% 
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

}
  
  
  
  
shinyApp(ui = ui, server = server)

