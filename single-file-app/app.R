# load packages ----
library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(markdown)
library(rsconnect)


# data ----
southern_fish <- read_csv("data/totalDDX_fish_southernCA.csv")

#data wrangling 
# Read in sediment data 
sediment_summary = readRDS(here::here("single-file-app", "data","totalDDX_sediment_zone_summary.rds")) %>%
  dplyr::select(Name, Est.2003, Est.2008, Est.2013, Est.2018) %>% 
  gather(key="Year",value="TotalDDT",Est.2003:Est.2018) %>% 
  dplyr::mutate(Year = case_when(Year == "Est.2003" ~ "2003",
                                 Year == "Est.2008" ~ "2008",
                                 Year == "Est.2013" ~ "2013",
                                 Year == "Est.2018" ~ "2018")) %>% 
  dplyr::group_by(Name, Year) %>%
  dplyr::summarize(TotalDDT.sed = (mean((TotalDDT)))) %>%
  dplyr::ungroup() 

# Read in fish life history 
fish_lh = read.csv(here::here("single-file-app", "data", "fish_life_history.csv")) %>% 
  dplyr::mutate(species = tolower(species))

# select station area
fish_location = read.csv(here::here("single-file-app", "data", "totalDDX_fish_southernCA.csv")) %>% 
  dplyr::select(CompositeCompositeID, CompositeStationArea)

# Read in fish data, and join with sediments 
fish_reg = read.csv(here::here("single-file-app", "data","totalDDT_fish_southernCA.csv")) %>% # Read in fish DDT values 
  # We have sediment data blocked off by 2003, 2008, 2013, and 2018. Figure out what (continous) fish years go with which sediment years. 
  dplyr::mutate(NewYear = case_when(Year %in% c(1995:2005) ~ "2003", 
                                    Year %in% c(2006:2010) ~ "2008", 
                                    Year %in% c(2011:2015) ~ "2013", 
                                    Year %in% c(2016:2022) ~ "2018")) %>% 
  left_join(., fish_location) %>% 
  left_join(., sediment_summary, by=c("CompositeStationArea"="Name", 
                                      "NewYear"="Year")) %>% 
  dplyr::left_join(., fish_lh, by=c("CompositeCommonName"="species")) %>% 
  dplyr::mutate(feeding_position = case_when(feeding_position == "pelagic" ~ "Pelagic",
                                             feeding_position == "midwater" ~ "Midwater",
                                             feeding_position == "benthopelagic " ~ "Benthopelagic",
                                             feeding_position == "benthic" ~ "Benthic", 
                                             TRUE ~ feeding_position)) %>% 
  dplyr::mutate(feeding_position = factor(feeding_position, levels=c("Pelagic","Midwater","Benthopelagic","Benthic"))) %>% 
  dplyr::mutate(trophic_category = case_when(trophic_category == "herbivore" ~ "Herbivore",
                                             trophic_category == "primary_carnivore" ~ "Primary Carnivore",
                                             trophic_category == "secondary_carnivore" ~ "Secondary Carnivore",
                                             trophic_category == "tertiary_carnivore" ~ "Tertiary Carnivore"))



# Add transformed columns (TotalDDT which is non-lipid normalized for fish)
fish_reg$TotalDDT.trans.non = log(fish_reg$TotalDDT + 1) # add + 1 to account for 0 values
fish_reg$TotalDDT.sed.trans = log(fish_reg$TotalDDT.sed + 1)

# Add censoring for values equal to zero (so value is constrained to fall between zero and MDL)
fish_reg = fish_reg %>% 
  dplyr::mutate(Censored = ifelse(TotalDDT.trans.non == 0, "interval","none"), 
                
                # ask about this limit since its divided by Lipid
                Detection.Limit = ifelse(is.na(MDL.min), 
                                         0.5, # if MDL.min is an NA value fill with this value
                                         log1p(MDL.min))) %>% 
  
  dplyr::mutate(Year = Year - 1998) # We want to use years since 1998 

fish_reg = fish_reg %>%
dplyr::mutate(feeding_position = factor(feeding_position, 
                                        levels=c("Benthic","Benthopelagic","Midwater","Pelagic")))



#---------------------------------------------
ui <- navbarPage(
  
  title = "SaferSeafood",
  
  # (Page 1) intro tabPanel ----
  tabPanel(title = "About this app",
           
           
           "Dichlorodiphenyltrichloroethane (DDT) is an insecticide that is resistant to degradation and can cause increased risks of cancer, premature births, developmental abnormalities, and neurological diseases in humans and animals. A recent rediscovery of a vast barrel field of DDT-laced sludge off the coast of southern California has captured the attention of the public and raised concerns regarding consumption of contaminated seafood. Alongside direct public health impacts, a decrease in seafood consumers poses a threat to the regional economy and recreational fishing communities. The California Environmental Protection Agency Office of Environmental Health Hazard Assessment (OEHHA) currently issues statewide consumption advisories for coastal communities. However, these advisories are severely limited as they are site and species-specific, covering only two chemicals: Mercury and PCBs.  These limitations make it nearly impossible for consumers to receive proper guidance on all safe and healthy seafood products. Scientists at the Scripps Institute of Oceanography and CalCOFI have collected and compiled fish and sediment monitoring data in order to understand the human and ecological impact as a result of the DDT dumping. Their current model has shown that they can predict localized risk in sport fish, and our goal is to expand on this to develop a spatiotemporal statistical model to produce more accurately predicted DDT concentrations for unmeasured species and locations. We will also develop an interactive online application that will allow users to input species, location, and demographic information and receive predicted DDT concentrations and advisories. This project will help to inform the public and give users the autonomy to understand the risk and make informed decisions on their seafood consumption." 
           
  ), # END (Page 1) intro tabPanel
  
  # (Page 2) data viz tabPanel ----
  tabPanel(title =  "Explore the Data",
           
           
           #tabsetPanel to contain tabs for data viz 
           tabsetPanel(
             
             # weight tab
             tabPanel(title = "Weight Distribution",
                      
                      # weight sidebar layout ----
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          pickerInput(inputId = "feed_position_input", label = "Select feeding position type(s):",
                                      choices = unique(fish_reg$feeding_position), # alternatively: choices = c("rapid", "cascade" ...)
                                      selected = c("Pelagic", "Midwater", "Benthopelagic", "Benthic"), 
                                      options = pickerOptions(actionsBox = TRUE), # creates "Select All / Deselect All" buttons
                                      multiple = TRUE) # END channel type pickerInput
                          
                        ), 
                        
                        mainPanel(
                          
                          plotOutput(outputId = "feeding_scatterplot_output") 
                        ) #end weight main Panel 
                        
                        
                      ) #end weigth sidebar layout 
                      
                      
             ), #end weight tab panel
             
             
             #data tab ----
             tabPanel(title = "The Data",
                      
                      
                      # Data sidebar layout ----
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          "Data inputs go here"
                          
                        ), 
                        
                        mainPanel(
                          
                          "Data outputs go here" 
                          
                        ) #end Data main Panel 
                        
                        
                      ) #end data sidebar layout 
                      
                      
             ) #end data tab panel
             
           ) # end tabsetPanel 
           
  ) #end (page 2) data viz tab panel 
  
) # END navbarPage




# user interface ----
ui <- fluidPage(
  
  # app title ----
  tags$h1("SaferSeafood"),
  
  # app subtitle ----
  h4(strong("Landing page for DDT concentration analysis")),
  
  # total DDT slider input ----
  sliderInput(inputId = "DDT_conc_input", label = "Select a DDT range :",
              min = 0, max = 7240, value = c(0, 7200)),
  
  # body mass plot output ----
  plotOutput(outputId = "DDT_concentration_scatter_output"),
  
  
  # year input ----
  checkboxGroupInput(inputId = "species_input", label = "Select species:",
                     choices = unique(southern_fish$CompositeCommonName), # or `unique(penguins$year)` | NOTE: update checkbox display name by using "New name" = "observation name" (e.g "The year 2007" = 2007)
                     selected = c("white croaker", "jacksmelt")),
  
  # DT output ----
  DT::dataTableOutput(outputId = "DDT_DT_output")
  
  )



# server instructions ----
server <- function(input, output) {
  
  #filter DDT concentrations 
  DDT_df <- reactive({
    southern_fish %>%
    filter(TotalDDT %in% c(input$DDT_conc_input[1]:input$DDT_conc_input[2]))
  })
  
  #render ltered DDT scatter plot reactive ----
  output$DDT_concentration_scatter_output <- renderPlot({
    ggplot(na.omit(DDT_df()), 
           aes(x = WeightAvg.g, y = TotalDDT)) +
      geom_point(color = "red") +
      theme_minimal() +
      theme(legend.position = c(0.85, 0.2),
            legend.background = element_rect(color = "white"))
    
  })
  
  # filter for species ---- 
  species_df <- reactive({
    southern_fish %>%
      filter(CompositeCommonName %in% c(input$species_input))
  })
  
  
  # render DT:: datatable ----
  output$DDT_DT_output <- DT::renderDataTable({
    
    DT::datatable(species_df(),
                  options = list(pagelength = 10),
                  rownames = FALSE)
    
  })
  
  # filter for feeding position ----
  
  fish_reg_filter <- reactive({
    fish_reg %>%
      filter(feeding_position %in% c(input$feeing_position_input))
    
  })
  


} #end server

# combine UI & server into an app ----
shinyApp(ui = ui, server = server)