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
library(rgdal)
library(shinycssloaders)
library(png)


### Cost Calculator Code

# Private Parcel Data

private <- st_read(dsn = ".", 
                   layer = "Private_Parcels")

addresses <- private %>% 
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

#addresses2 <- addresses1 %>% 
#st_set_geometry(NULL) %>% 
#select(Addr1, Addr2, Improvement, LandValue, GA25, PA25, NT_Loss, Opt21_Loss)





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
palcond <- colorFactor(palette = "red", domain = condNames)

fireregime <- st_read(dsn = ".", layer = "Fire_Regimes_Mod")
regime_df <- st_transform(fireregime, "+init=epsg:4326")
regime_class <- regime_df %>%
  select(FireRegime)

Low_Severity <- regime_class %>%
  filter(FireRegime == "I - Low Severity Fire")
Mid_Severity <- regime_class %>%
  filter(FireRegime == "III - Mixed Severity FIre")
High_Severity <- regime_class %>%
  filter(FireRegime == "IV - High Severity Fire")

SAF_Cover <- st_read(dsn = ".", layer = "ForestCover")
SAF_Cover_df <- st_transform(SAF_Cover, "+init=epsg:4326")
SAF_class <- SAF_Cover_df %>%
  select(SAF_Name)

#ponderosa <- SAF_class %>% 
#select(SAF_class, SAF_Name == "Pacific Pondersa Pine")

dinkey_boundary <- st_read(dsn = ".", layer = "DinkeyBoundary")
dinkey_df <- st_transform(dinkey_boundary, "+init=epsg:4326")

SAFNames <- unique(SAF_class$SAF_Name)
palrainbow <- colorFactor(palette = rainbow(18), domain = SAFNames)

RegimeNames <- unique(fireregime$FireRegime)
palfireregime <- colorFactor(palette = "yellow", domain = RegimeNames)

Subset_2000 <- read.csv("Subset_2000.csv")

SDI260_CFL_Change <- Subset_2000 %>% 
  mutate(CFL_Change = Average_260 - Average_nt) %>% 
  select(CFL_Change)


choice <- c("Aspect", "Slope", "Elevation")


#tiff_stack <- raster::stack("Aspect.tif", "Slope.tif", "Elevation.tif")
# could convert these to shape files, but tiffs appear smaller so it wouldn't make it faster



private_cfl <- st_read(dsn = '.', layer = "private_cfls")

private_tcfl <- st_transform(private_cfl, "+init=epsg:4326") %>% 
  select(CFL_Cat_30, NoTreatSev, IgnitGA25, geometry)

Treatment <- private_tcfl%>% 
  select(CFL_Cat_30) %>% 
  mutate("Treatment") %>% 
  mutate(CFL_Cat_30 = CFL_Cat_30 + 1)

colnames(Treatment) <- c("Severity", "Treatment", "geometry")

NoTreatment <- private_tcfl%>% 
  select(NoTreatSev) %>% 
  mutate("No Treatment") %>% 
  mutate(NoTreatSev = NoTreatSev + 1)

colnames(NoTreatment) <- c("Severity", "Treatment", "geometry")

DataT <- rbind(NoTreatment, Treatment)


##########
#Cost Calculator Data Inputs
#########

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

addressesCust <- read_csv("addressesCust.csv")
addressesCust$LandValue <- as.numeric(addressesCust$LandValue)
addressesCust$Improvement <- as.numeric(addressesCust$Improvement)
addressesCust$HVRA <- as.numeric(addressesCust$HVRA)
addressesCust$TreatmentCost <- as.numeric(addressesCust$TreatmentCost)
addressesCust$Ntbase <- as.numeric(addressesCust$Ntbase)

##########
#End Cost Calculator INputs
##########


# switched from reds color scheme to divergent RdYlGn, couldn't figure out how to reverse palette, so manually inputed the values

color <- colorFactor(palette = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027"), 
                     domain = c(0, 1, 2, 3, 4 , 5, 6), 
                     na.color = "transparent")



dbHeader <- dashboardHeader(title = "SRCD Fire App",
                            
                            tags$li(a(href = 'https://www.savingsierras.com/',
                                      img(src = 'Bren-logo-horizontal.png',
                                          title = "Company Home", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"
                            ))



# Define UI for application that draws a histogram
ui <- dashboardPage(skin = ("green"),
                    
                    
                    
                    
                    
                    dbHeader,
                    
                    
                    
                    dashboardSidebar(
                      
                      
                      sidebarMenu(
                        
                        menuItem("About Page", tabName = "tab_7"),
                        #menuItem("Topographical Information", tabName = "tab_2"),
                        menuItem("Fire History", tabName = "tab_3"),
                        menuItem("Forest Cover", tabName = "tab_4"),
                        menuItem("Fire Severity on Public Lands", tabName = "tab_1"),
                        menuItem("Fire Severity on Private Lands", tabName = "tab_5"),
                        menuItem("Cost Benefit Analysis", tabName = "tab_6"),
                        #menuItem("Cost Calculator", tabName = "tab_8"),
                        menuItem("Cost Calculator", tabName = "tab_9"),
                        
                        
                        
                        
                        img(src='Bren-logo-circular.png', align = "center", height = 150)
                        
                      )
                      
                    ),
                    
                    
                    
                    dashboardBody(
                      tabItems(
                        
                        
                        tabItem(tabName = "tab_7",
                                
                                fluidRow(
                                  
                                  
                                  box(tags$h4("Forests, Fuel, and Fire"), p("Decades of fire 
                                                                            suppression in the southern Sierra Nevada Mountains 
                                                                            have led to unnaturally dense forest stands and high levels 
                                                                            of combustible fuels on the landscape. This, along 
                                                                            with severe drought, and bark beetle infestations 
                                                                            has left the southern Sierra Nevada Mountains 
                                                                            vulnerable to high severity fires that pose health, 
                                                                            safety, and economic risks to communities in the Wildland-Urban Interface. 
                                                                            Because severe fire poses an immediate threat to 
                                                                            individuals and infrastructure adjacent to national 
                                                                            forest lands, treatments to reduce 
                                                                            fuel loads on the landscape, such as mechanical thinning, 
                                                                            should be implemented on private lands. 
                                                                            While there are significant upfront costs 
                                                                            that restrict the ability of private 
                                                                            landowners to implement these treatments, there are opportunities 
                                                                            through federal and state agencies,
                                                                            such as grants and cost share programs, 
                                                                            to reduce these barriers.
                                                                            "), width = 6),
                                  box(tags$h4("Web Application Development"), p("This web application was developed to motivate fuel treatments on private lands in the Southern Sierra Nevada, in order to reduce the risk of catastrophic wildfires. It was developed by masters' students at the Bren School of Environmental Science & Management, at the University of California - Santa Barbara."), width = 6),
                                  box(tags$h4("Contact Information"), p("Justin Heyerdahl - jheyerdahl@bren.ucsb.edu"), p("Chris Hughes - chughes@bren.ucsb.edu"), p("Tess Morgridge - tmorgridge@bren.ucsb.edu"), p("Craig O'Neill - coneill@bren.ucsb.edu"), p("Jason White - jwhite@bren.ucsb.edu"), width = 6),
                                  
                                  
                                  
                                  
                                  # These have to be in the www folder in the project 
                                  
                                  
                                  
                                  
                                  box(img(src= 'Site_Visit.png', align = "left", height = 310), img(src= 'DinkeyActivities.png', height = 320), width = 12),
                                  box(p("Historical fire suppression, drought, and bark beetle attacks have resulted 
                                        in significant tree mortality across the Dinkey Landscape, presenting a significant 
                                        risk of catastrophic fire (photo from May 2017). While there has been action to reduce 
                                        this risk on public lands by decreasing tree densities, little has been done on private lands."), width = 12)
                                  
                                  
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
                                  box(p("This figure shows the changes in conditional flame length (in feet) across the entire landscape when the entire landscape is treated. Red bars indicate regions where fire severity increased after fuel treatments, in part due to the presence of unburnt slash piles on the landscape. Green bars indicate regions where fire severity decreased after fuel treatments. The table shows the summary statistics of the changes in fire severity."), width = 12)
                                )),
                        
                        #tabItem(tabName = "tab_2",
                        #fluidRow(
                        #box(withSpinner(leafletOutput("my_graph2", height = 432))),
                        #box(title = "Private Lands Maps",
                        #    selectInput("class", 
                        #                "Choose Map:", 
                        #                choices = choice)),
                        
                        #box(p(("This figure shows topographical information for private lands in the Dinkey Landscape. Users can select between Aspect (in degrees), Slope (in degrees), and elevation (in meters). These are factors that can significantly impact vegetation characteristics as well as fire behavior.")), width = 12)
                        #)),
                        
                        tabItem(tabName = "tab_3",
                                fluidRow(
                                  box(withSpinner(leafletOutput("my_graph3", height = 432))),
                                  box(title = "Historical Fire Regime",
                                      selectInput("regime_class", 
                                                  "Choose Regime Level:", 
                                                  choices = unique(regime_class$FireRegime)),
                                      submitButton("Submit")),
                                  box(p("This figure shows the historical fire regimes across the Dinkey Landscape. 
                                        Users can select which fire regime the map will show. 
                                        Much of the Southern Sierra Nevada was historically 
                                        dominated by low severity, high frequency fires."), width = 6)
                                ),
                                
                                fluidRow(
                                  box(withSpinner(leafletOutput("my_graph7", height = 432))),
                                  box(title = "Change in Fire Regime",
                                      selectInput("cond_class", 
                                                  "Choose Level of Change:", 
                                                  choices = unique(cond_class$Departure)),
                                      submitButton("Submit")),
                                  
                                  box(p("This figure shows the change in fire regime compared to historical data. Users can select the level of change that they wish the map to show. Much of the Southern Sierra Nevada has had a significant increase in forest density, increasing the risk of high severity, catastrophic fire."), width = 6)
                                  )
                                  ),
                        
                        tabItem(tabName = "tab_4",
                                fluidRow(
                                  box(withSpinner(leafletOutput("my_graph4", height = 700, width = 700))),
                                  
                                  
                                  
                                  box(p("This figure shows the dominant vegetation classes across the Dinkey Landscape, with private lands overlaying the map. Vegetation type can significantly impact fire behavior. Trees that grow to have larger trunk areas grow thicker bark, which allows them to endure naturally occuring, low severity fires. Shorter trees and shrubs, which have thinner bark and low-hanging foliage, are more susceptible to fire damage and can result in more severe fires.", width = 12)
                                      
                                      
                                      
                                      
                                  )
                                  
                                )),
                        
                        tabItem(tabName = "tab_5",
                                fluidRow(
                                  box(withSpinner(leafletOutput("my_graph5", height = 432))),
                                  # box(withSpinner(leafletOutput("my_graph8", height = 432))),
                                  box(title = "Private Lands Fire Severity",
                                      selectInput("treatment", 
                                                  "Choose Treatment Type:", 
                                                  choices = unique(DataT$Treatment)),
                                      submitButton("Submit")),
                                  
                                  box(p("This figure shows fire severities under a scenario when no fuel treatments occur, and a scenario when 21% of the landscape is treated. Fire severities of 4, 5, and 6 indicate a high severity fire."), width = 12)
                                )
                        ),
                        
                        
                        ###  COST BENEFIT ANALYSES
                        
                        tabItem(tabName = "tab_6",
                                fluidRow(
                                  
                                  box(withSpinner(plotOutput(outputId = "distPlot2"
                                                             
                                  )), width = 6),
                                  
                                  box(selectInput("Stakeholder", "Choose Stakeholder:", choices = unique(cba$Stakeholder)),
                                      selectInput("Treatment_Type", "Choose Treatment Type:", choices = unique(cba$Treatment_Type)),
                                      selectInput("Climate_Model2", "Choose Climate Model:", choices = unique(cba$Climate_Model2)), 
                                      submitButton("Submit"),
                                      width = 6),
                                  
                                  box(p("This figure shows the total value of treating 21% of the landscape for each stakeholder under 
                                        two different treatment types and two different climate projections. Users can select 
                                        which stakeholder they wish the see results for, which treatment type they wish to use, 
                                        and which climate projection they wish to use."))),
                                
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
                                                sep = ""),
                                    submitButton("Submit")),
                                  
                                  box(p("This figure shows the difference in value of forest carbon sequestration, based on the 2018 market value for carbon credits in California, after treating 21% of the landscape. Users can select which climate projection they wish to use, and which year they wish to see values for. Note that the value for carbon sequestration is negative for the first year of treatment, which coincides with a large thinning event. Over time, properly thinned forests amass a greater amount of biomass (e.g. carbon) than unthinned forests, which is condensed into fewer, but larger growing trees. Properly thinned forests are also less susceptible to high-intensity wildfire, which would otherwise reduce the amount of carbon a forest can sequester over time."), width = 6)
                                )
                                
                                  ),
                        ####Cost Calculator###########
                        tabItem(tabName = "tab_9",
                                fluidRow(
                                  br(),
                                  h1("Property Owner Fire Risk and Cost Calculator")
                                ),
                                fluidRow(
                                  column(4,
                                         #style = "background-color:#dfdedc",
                                         hr(),
                                         h4("1.  Select Your Address"),
                                         selectizeInput('Addr1',
                                                        'Enter address:',
                                                        choices = addresses2$Addr1,
                                                        multiple = FALSE,
                                                        #selectize = TRUE,
                                                        options = list(
                                                          maxOptions = 1,
                                                          placeholder = 'Please type address here',
                                                          onInitialize = I('function() { this.setValue(""); }')
                                                        )
                                         ),
                                         submitButton("Look up Info")
                                  ),
                                  column(4,
                                         #style = "background-color:#dfdedc",
                                         hr(),
                                         h4("City, State, Zip Code"),
                                         h4(" "),
                                         tableOutput("Address2")
                                  ),
                                  column(4,
                                         #style = "background-color:#dfdedc",
                                         hr(),
                                         h4(),
                                         h4(""),
                                         p("")
                                  )
                                ),
                                
                                #Address Confirmation
                                
                                fluidRow(
                                  #column(4,
                                  #       hr(),
                                  #       h4("Land Value"),
                                  #       p("This is the value of your land, not including improvements, structures, etc")
                                  
                                  #),
                                  column(4,
                                         #style = "background-color:#dfdedc",
                                         hr(),
                                         h4("Predicted Land Value"),
                                         tableOutput("LandVal1")#,
                                         #p("According to the Fresno County Board of Assesors, this is the land value of your property")
                                  ),
                                  column(4,
                                         #style = "background-color:#dfdedc",
                                         hr(),
                                         h4("Predicted Improvements"),
                                         tableOutput("Improve1")#,
                                         #p("According to the Fresno County Board of Assesors, this is the value of any improvements or structures on your property")
                                  ),
                                  column(4,
                                         #style = "background-color:#dfdedc",
                                         hr(),
                                         h4("Predicted Total Value"),
                                         tableOutput("Total1")
                                  )
                                  
                                ),
                                fluidRow(
                                  #column(4,
                                  #      hr(),
                                  #       h4("Predicted Fire Ignition"),
                                  #       p("Probability of Fire Occuring on Land through 2050")
                                  
                                  #),
                                  column(4,
                                         hr(),
                                         h4("Predicted Fire Ignition Probability"),
                                         tableOutput("FireIgnit1"),
                                         p("This is the probability that fire will occur on your land between 2018 and 2050")
                                  ),
                                  column(4,
                                         hr(),
                                         h4("Predicted Fire Severity Probability"),
                                         tableOutput("FireSev1"),
                                         p("Modeled fire severity predicts to destructive potential of a fire should it occur on your land")
                                  ),
                                  column(4,
                                         hr(),
                                         h4("Predicted Loss in Value"),
                                         tableOutput("Loss1"),
                                         p("If no treatment is done, this is your predicted loss in total property value due to fire through the year 2050")
                                  )
                                  #column(4,
                                  #       hr(),
                                  #       sliderInput("upSlider", "Custom FireIgnit Value", min = 0.01, max = 0.75, value = 0.15, step = 0.01)
                                  #)
                                  #),
                                ),
                                fluidRow(
                                  column(4,
                                         style = "background-color:#fe9e00",
                                         hr(),
                                         h5()
                                  ),
                                  column(4,
                                         style = "background-color:#fe9e00",
                                         hr(),
                                         h5()
                                  ),
                                  column(4,
                                         style = "background-color:#fe9e00",
                                         hr(),
                                         h5()
                                  )
                                  #column(4,
                                  #       hr(),
                                  #       sliderInput("upSlider", "Custom FireIgnit Value", min = 0.01, max = 0.75, value = 0.15, step = 0.01)
                                  #)
                                  #),
                                ),
                                #fluidRow(
                                #  span(),
                                #column(4,
                                #       hr(),
                                #       h4("Improvements Value"),
                                #       p("This is the predicted Improvements Value for your Parcel")
                                #),
                                #,
                                #column(4,
                                #       hr(),
                                #       numericInput("LandValue", "InputLV", value = 0, step = NA)
                                #  )
                                #),
                                #fluidRow(
                                #  column(4,
                                #         hr(),
                                #         h4("Calculated"),
                                #         p("Total Property Value (Sum of Land Value + Improvements")
                                #         
                                #  ),
                                #  
                                #  column(4,
                                #         hr(),
                                #         numericInput("TotVal2", "Custom Total Value", value = 0)
                                #  )
                                #),
                                fluidRow(
                                  column(12,
                                         hr(),
                                         h4("Now See How Treatment Affects You!"))
                                  
                                  
                                  
                                  
                                  
                                ),
                                fluidRow(
                                  column(12,
                                         #hr(),
                                         h5("First, choose your Property Value and Fire Risk Profile"),
                                         p("You can either use our predicted values listed above, or choose your own values!")
                                  )
                                ),
                                fluidRow(
                                  column(4,
                                         hr(),
                                         #h4("Input Total Property Value"),
                                         numericInput("landvalue", "Input Custom Total Property Value", value = 0)),
                                  column(4,
                                         hr(),
                                         selectInput("TreatExt",
                                                     "Choose Treatment Extent:",
                                                     choices = c("NT",
                                                                 "Min",
                                                                 "Mid",
                                                                 "Opt"))),       
                                  column(4,
                                         hr(),
                                         sliderInput("FireProb2", "Choose your custom Fire Ignition Probability", min = 0.1, max = 1, value = 0.5, step = 0.1)#,
                                         #sliderInput("FireSev2", "Custom FireSev Value", min = 0.1, max = 1, value = 0.4, step = 0.1)
                                  )),
                                fluidRow(  
                                  column(4,
                                         hr(),
                                         h4("Property Fire Severity"),
                                         p("This is value refers to the damage potential of a fire on your property, utilizing different treatment intensities on a community level can affect this value"),
                                         tableOutput("NewSev"),
                                         h4("Treatment Costs"),
                                         p("This is the 'evenly shared' cost of treatment based on desired community level of treatment "),
                                         tableOutput("TreatChoice"),
                                         h4("Post-treatment Expected Financial Losses"),
                                         p("After selecting treatment, this is your expected losses through 2050"),
                                         tableOutput("DiffNTa")
                                         
                                  ),
                                  column(4,
                                         hr(),
                                         h4("Avoided Fire Damages"),
                                         p("This is amount of money saved from avoided fire damages due to treatment"),
                                         #tableOutput("DiffNTa"),
                                         h4("Net Benefits of Treatment"),
                                         p("This is the difference between your treatment costs and savings due to avoided fire damage"),
                                         tableOutput("netgain")
                                  )
                                )#,
                                #fluidRow(
                                #  column(4,
                                #         hr(),
                                #         h4("Calculated"),
                                #         p("Total Property Value Loss (Total Value * Fire Ignition Prob * Fire Severity)")
                                #         
                                #  ),
                                #  
                                #  column(4,
                                #         hr(),
                                #         numericInput("Loss2", "Custom Loss Value", value = 0)
                                #  )
                                #),
                                #fluidRow(
                                #  column(4,
                                #         hr(),
                                #         h4("Choose Treatment Intensity"),
                                #         p("Treatments are a community action!  If the community bands together to treat more land, then there is a higher likelihood of your property's fire risk decreases")
                                #         
                                #  ),
                                #  column(4,
                                #         hr(),
                                #         selectInput("TreatExt",
                                #                     "Choose Treatment Extent:",
                                #                     choices = c("NT",
                                #                                 "Min",
                                #                                 "Mid",
                                #                                 "Opt"))
                                #  ),
                                #  column(4,
                                #         hr()#,
                                #         #p("After Selecting treatment, this is your probability for complete loss should fire occur on your parcel"),
                                #tableOutput("NewSev"),
                                #p("After selecting treatment, this is your expected losses through 2050"),
                                #tableOutput("NewDamages"),
                                #p("This is amount of money saved (negative = savings) from avoided fire damages through treatment"),
                                #tableOutput("DiffNT"),
                                #p("This is the 'evenly shared' cost of treatment based on desired treatment extent"),
                                #tableOutput("TreatChoice"),
                                #p("This is the difference between your treatment costs and savings (i dont know whats good maybe negative?)"),
                                #tableOutput("netgain")
                                #   )
                                #  ),
                                # fluidRow(
                                #    column(4,
                                #           hr(),
                                #           h4("Treatment Costs"),
                                #           p("Your Cost of Treating your land!")
                                #           
                                #    ),
                                ##    column(4,
                                #           hr(),
                                #           p("Estimated Treatment Cost based on Mechanical Thinning Parcel Area"),
                                #           tableOutput("TreatmentCost1"),
                                #           p("Estimated Treatment Cost based on Handthinning"),
                                #           tableOutput("TreatmentcostsHT")
                                #    ),
                                #    column(4,
                                #           hr(),
                                #           numericInput("TreatmentCost2", "Custom Treatment Cost (Total, or should this be per acre?)", value = 0)
                                #       )
                                #      ),
                                #     fluidRow(
                                #        column(4,
                                #               hr(),
                                #               h4("Potential Avoided Fire Damage Savings"),
                                #               p("The money you potentially save!  Not adjusted for NPV")
                                #               
                                #        ),
                                #        column(4,
                                #               hr(),
                                #               p("Estimated based on Mechanical:"),
                                #               tableOutput("SavingMech"),
                                #               p("Estimated based on Handthinning:"),
                                #               tableOutput("SavingHT")
                                #        ),
                                #        column(4,
                                #               hr(),
                                #               numericInput("TreatmentCost2", "Custom Treatment Cost (Total, or should this be per acre?)", value = 0)
                                #        )
                                #      )
                                #)
                                ##########CostCalculator End################
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
      y <- summary(x, digits = 3)
      z <- round(y, digits = 2)
      z
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
  
  
  
  
  
  #####Topo Info Begin   #######
  #  output$my_graph2 <- renderLeaflet({
  
  
  # TOPOGRAPHICAL INFORMATION
  
  #    tiffmap <- subset(tiff_stack, input$class, drop=TRUE)
  
  
  #    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(tiffmap),
  #                        na.color = "transparent")
  #leaflet(private_tclass) %>% 
  #addTiles() %>% 
  
  
  #    leaflet(private_tclass) %>% 
  #      addTiles() %>% 
  #      addRasterImage(tiffmap, colors = pal, opacity = 0.8) %>% 
  #      addLegend (pal = pal, values = values(tiffmap),
  #                 title = input$class) %>% 
  #      addPolygons(color = "black",
  #                  weight = 0.5, fill = NA) %>%
  #      addPolygons(data = dinkey_df,
  #                  weight = 1,
  #                  color = "black",
  #                  fillColor = "transparent")
  
  
  
  
  
  #  })
  
  ###### FIRE HISTORY#####  
  
  output$my_graph3 <- renderLeaflet({
    
    
    
    
    
    
    regime_sub <- regime_class %>%
      filter(FireRegime == input$regime_class)
    
    leaflet(regime_sub) %>% 
      addTiles() %>% 
      addPolygons(weight = 0.5,
                  color = "black",
                  fillColor = ~palfireregime(RegimeNames),
                  fillOpacity = 0.5,
                  group = "Historical Fire Regime") %>%
      addPolygons(data = dinkey_df,
                  weight = 1,
                  color = "black",
                  fillColor = "transparent",
                  group = "Dinkey Boundary") %>%
      addPolygons(data = private_tclass,
                  weight = 1,
                  color = "black",
                  fillColor = "darkblue",
                  fillOpacity = 0.5,
                  group = "Private Parcels") %>%
      
      
      addLayersControl(
        baseGroups = c("Historical Fire Regime"),
        overlayGroups = c("Dinkey Boundary", "Private Parcels"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
  })
  
  ##### ALSO FIRE HISTORY #####    
  output$my_graph7 <- renderLeaflet({
    
    
    
    
    
    
    
    
    
    cond_sub <- cond_class %>%
      filter(Departure == input$cond_class)
    
    leaflet(cond_sub) %>% 
      addTiles() %>% 
      addPolygons(weight = 0.5,
                  color = "black",
                  fillColor = ~palcond(condNames),
                  fillOpacity = 0.5,
                  group = "Change in Fire Regime") %>%
      addPolygons(data = dinkey_df,
                  weight = 1,
                  color = "black",
                  fillColor = "transparent",
                  group = "Dinkey Boundary") %>%
      addPolygons(data = private_tclass,
                  weight = 0.5,
                  color = "black",
                  fillColor = "darkblue",
                  fillOpacity = 0.5,
                  group = "Private Parcels") %>%
      
      addLayersControl(
        baseGroups = c("Change in Fire Regime"),
        overlayGroups = c("Dinkey Boundary", "Private Parcels"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
  })
  
  
  #### FOREST COVER ######  
  output$my_graph4 <- renderLeaflet({
    
    
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data = dinkey_df,
                  weight = 3,
                  color = "black",
                  fillColor = "grey",
                  fillOpacity = 0,
                  group = "Dinkey Boundary") %>%
      addPolygons(data = SAF_class,
                  weight = 0.5,
                  color = "black",
                  fillColor = ~palrainbow(SAFNames),
                  fillOpacity = 0.5,
                  group = "Vegetation") %>%
      # addPolygons(data = ponderosa,
      # weight = 0.5,
      #color = "black",
      #fillColor = "green",
      #fillOpacity = 0.5,
      #group = "Ponderosa") %>%
      addPolygons(data = private_tclass,
                  weight = 2,
                  color = "white",
                  fillColor = "yellow",
                  fillOpacity = 0,
                  group = "Private Parcels") %>%
      addLegend(pal = palrainbow, 
                values = SAFNames,
                title = "Forest Cover Types") %>%
      
      
      addLayersControl(
        baseGroups = c("Fire Resistant", "Not Fire Resistant", "All Vegetation"),
        overlayGroups = c("Dinkey Boundary", "Private Parcels"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
  })
  
  ##### FIRE SEVERITY OF PRIVATE LANDS#####  
  
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
  
  
  #### FIRE SEVERITY OF PRIVATE LANDS######    
  output$my_graph8 <- renderLeaflet({
    
    
    
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
  
  ####Carbon CBA ######  
  
  output$distPlot <- renderPlot({
    x <- carbon$DifferenceToDate
    Years <- input$Year 
    pick_model <- input$Climate_Model
    ggplot(subset(carbon, Year == Years & Climate_Model == pick_model), aes(x = Discount_Rate, y = DifferenceToDate)) +
      geom_col(fill = c("darkblue", "lightblue")) +
      theme_bw(base_size = 13) +
      ylab("Cumulative Value ($US Millions)") +
      xlab("Discount Rate (%)") + 
      ggtitle("Value of Carbon Sequestration (in $US)") +
      #scale_fill_manual(name="Treatment", values = c("SDI 300" = "cyan3", "SDI 260" = "purple")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(expand = c(0,0), limits = (c(-0.5, 15))) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      coord_flip() + 
      geom_hline(yintercept = 0, size = 0.5, colour = "black",
                 linetype = "dotted") +
      guides(fill = FALSE)
  })
  
  output$distPlot2 <- renderPlot({
    x <- cba$NPV
    Stakeholders <- input$Stakeholder 
    pick_treatment <- input$Treatment_Type
    pick_model2 <- input$Climate_Model2
    ggplot(subset(cba, Stakeholder == Stakeholders & Treatment_Type == pick_treatment & Climate_Model2 == pick_model2), aes(x = Discount_Rate, y = NPV)) +
      geom_col(fill = c("darkgreen", "lightgreen")) +
      theme_bw(base_size = 13) +
      ylab("Net Value (US$ Millions)") +
      #scale_fill_manual(name="Treatment", values = c("SDI 300" = "cyan3", "SDI 260" = "purple")) +
      xlab("Discount Rate (%)") + 
      ggtitle("Value of Fuel Treatments (in $US)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(expand = c(0,0), limits = (c(0, 350))) +
      coord_flip() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      guides(fill = FALSE)
  })
  
  #####Cost Calculator ######  
  
  #addresses2 <- addresses1 %>% 
  #st_set_geometry(NULL) %>% 
  #select(Addr1, Addr2, Improvement, LandValue, GA25, PA25, NT_Loss, Opt21_Loss, Acres)
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
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(a)
    
    
    
  })
  
  output$LandVal1 <- renderTable(addrLandValue(),
                                 colnames = FALSE)
  
  
  addrImprove <- reactive({
    b <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Improvement)
    b[,1] <- sapply(b[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(b)
  })
  
  output$Improve1 <- renderTable(addrImprove(),
                                 colnames = FALSE,
                                 digits = 2)
  
  
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
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
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
      select(Improvement, LandValue, GA25, NT_Loss) %>% 
      mutate(Tot_NT_Loss = (Improvement + LandValue) * GA25 * NT_Loss) %>% 
      select(Tot_NT_Loss)
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(a)
  })
  
  output$Loss1 <- renderTable(addrLoss(),
                              colnames = FALSE,
                              digits = 2)
  
  
  
  
  addrTreatmentCost1 <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Acres) %>% 
      mutate(TreatmentCost = ((Acres * 2750)*3)) %>% 
      select(TreatmentCost)
    a[,1] <- sapply(a[,1], function(x) paste0("$",round(x,2)))
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
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
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
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
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
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(a)
  })
  
  output$SavingHT <- renderTable(addrSavingHT(),
                                 colnames = FALSE)
  
  addrNewSev <- reactive({
    a <- addresses3 %>% 
      subset(Addr1 == input$Addr1)%>%
      subset(Treat == input$TreatExt) %>% 
      select(HVRA) #%>% 
    #a[,1] <- sapply(a[,1], function(x) paste0("$",round(x,2)))
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
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
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
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(a)
  })
  
  output$DiffNT <- renderTable(addrDiff(),
                               colnames = FALSE)
  
  addrDiffa <- reactive({
    a <- input$landvalue * input$FireProb2 #* input$FireSev2
    #a <- as.data.frame(a)
    b <- addressesCust %>% 
      subset(Addr1 == input$Addr1) %>% 
      subset(Treat == input$TreatExt) %>% 
      select(HVRA)
    b <- b[1,]
    b <- as.numeric(b)
    ab <- as.data.frame(a*b)
    ab <- sapply(ab, function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(ab)
  })
  
  output$DiffNTa <- renderTable(addrDiffa(),
                                colnames = FALSE)
  
  addrTcost <- reactive({
    a <- addresses3 %>% 
      subset(Addr1 == input$Addr1)%>% 
      subset(Treat == input$TreatExt) %>%  #Would need to stack the data by treatment ext
      select(TreatmentCost)
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(a)
  })
  
  output$TreatChoice <- renderTable(addrTcost(),
                                    colnames = FALSE)
  
  #  addrnet <- reactive({
  #    a <- addresses3 %>% 
  #      subset(Addr1 == input$Addr1)%>% 
  #      subset(Treat == input$TreatExt) %>%  
  #      select(Improvement, LandValue, GA25, HVRA, Ntbase, TreatmentCost) %>% 
  #      mutate(Tot_NT_Loss = (Ntbase - (Improvement + LandValue) * GA25 * HVRA) - TreatmentCost) %>% 
  #      select(Tot_NT_Loss)
  #    
  #    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
  #    return(a)
  #  })
  
  #output$netgain <- renderTable(addrnet(),
  #                               colnames = FALSE)  
  
  
  ##Update this part for the Avoided Costs Section  
  addrAvoideda <- reactive({
    a <- input$landvalue * input$FireProb2 #* input$FireSev2
    #a <- as.data.frame(a)
    b <- addressesCust %>% 
      subset(Addr1 == input$Addr1) %>% 
      subset(Treat == input$TreatExt) %>% 
      select(HVRA)
    b <- b[1,]
    b <- as.numeric(b)
    c <- addresses3 %>% #No Treatment Loss (Pulls Severity from Address Lookup)
      subset(Addr1 == input$Addr1) %>% 
      subset(Treat == "NT") %>% 
      select(HVRA) #Pulls up the address based on the lookup, filters databse to that address and the NT HVRA value, pulls the HVRA.  Convert that to a numeric  
    c <- c[1,]
    c <- as.numeric(c) #No Treatment Alternative
    ab <- as.data.frame(a*b) #After Treatment Fire Loss
    abc <- as.data.frame(c - ab) #Savings
    t <- addressesCust %>% 
      subset(Addr1 == input$Addr1)%>% 
      subset(Treat == input$TreatExt) %>%  
      select(TreatmentCost)
    abct <- as.data.frame(abc - t)
    abct <- sapply(abct, function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(abct)
  })  
  
  output$netgain <- renderTable(addrAvoideda(),
                                colnames = FALSE)  
  
  
}




shinyApp(ui = ui, server = server)

