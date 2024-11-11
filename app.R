library(shiny);library(leaflet);library(sf);library(tidyverse);library(shinydashboard);library(patchwork)
source("ggplot_themes.R")
library(mailtoR)

#### Data ####
##### Locations #####
read_csv("Data/SDU_locations_2024.csv") %>% 
  tibble() %>% 
  rename(long = N_Coordina, 
         lat = E_Coordina) %>% 
  mutate(long = as.numeric(long),
         lat = as.numeric(lat)) %>% 
  select(Station:lat) %>% 
  rename(type = ID,
         station = Station) %>% 
  mutate(type = case_when(type == "Piezometerrør" ~ "Piezometer",
                   T ~ type),
         id = as.character(paste0(station,"_",Natur_type)))-> equipment

readxl::read_excel("Data/Røgbølle_stationer.xlsx") %>% 
  rename(long = N,
         lat = E) %>% 
  mutate(long = as.numeric(long),
         lat = as.numeric(lat),
         station = parse_number(station),
         station = as.character(station),
         type = "Greenhouse gas",
         id = as.character(station),
         id = paste0(month,"_",station))-> ghg_station

read_csv("Data/garn_location.csv") %>% 
  rename(long = y,
         lat = x) %>% 
  mutate(type = "Fish",
         garn = parse_number(name),
         id = as.character(garn)) -> fiskegarn

kemi_point<- tibble(x = 666003, y = 6065651, type = "Lake", id = "Lake") %>% 
  st_as_sf(coords = c("x","y"), crs = 25832) %>% 
  st_transform("+proj=longlat +datum=WGS84") %>% 
  as.data.frame() %>% 
  mutate(lat = 11.57686, long = 54.71074)
  
bind_rows(equipment,
          ghg_station,
          fiskegarn,
          kemi_point) %>% 
  mutate(col = case_when(type == "Piezometer" ~ "darkorange",
                         type == "Isco" ~ "red",
                         type == "Fish" ~ "purple",
                         type == "Greenhouse gas" ~ "forestgreen",
                         type == "Lake" ~ "royalblue")) -> combined_markers

streams <- st_read("Data/vandløb.shp") %>% 
  st_set_crs("EPSG:25832") %>% 
  st_transform("+proj=longlat +datum=WGS84")

wet <- st_read("Data/vådområder 2024-26.shp") %>% 
  st_set_crs("EPSG:25832") %>% 
  st_transform("+proj=longlat +datum=WGS84")


mean(c(equipment$long, ghg_station$long, fiskegarn$long)) -> mid_long
mean(c(equipment$lat, ghg_station$lat, fiskegarn$lat)) -> mid_lat

wms_url <- "https://api.dataforsyningen.dk/orto_foraar_DAF?token=3d6237caab0287976f475f68c890a359"

##### Observations #####

read_csv("Data/GHG/oktober_ghg.csv") %>% 
  pivot_longer(co2_flux:ebul_ch4) %>% 
  mutate(name = as_factor(name),
         name = factor(name, levels = c("ebul_ch4","diff_ch4","co2_flux"))) -> ghg_results

readxl::read_excel("Data/Røgbølle sø fiskeundersøgelse sep 2024.xlsx") %>% 
  rename(date = 1, garn = 3, art = 4, length = 5, weight = 6) %>% 
  select(-Sø) %>% 
  mutate(type = "Fish",
         id = as.character(garn),
         date = ymd(date)) -> fangst_data

kemi_select <- tibble(analysis = c("Phosphor, total-P","Nitrogen,total N","Ammoniak+ammonium-N","Ortho-phosphat-P","Chlorophyl A","pH","Alkalinitet,total TA","Nitrit+nitrat-N"," Ammonium-N"))

read_delim("Data/Unders_gelsestype_20241021_063609/Vandkemi - Sø.csv", 
              delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                  grouping_mark = "."), trim_ws = TRUE) %>% 
  select(location = Stedtekst, date = Dato, depth = "Dybde (m)",analysis = Stofparameter, value = Resultat, unit = Enhed) %>% 
  filter(analysis %in% kemi_select$analysis) %>% 
  mutate(date = dmy(date),
         doy = yday(date),
         year = year(date),
         type = "Kemi",
         analysis = case_when(analysis=="Nitrogen,total N" ~ "Nitrogen, total-N",
                              analysis== "Alkalinitet,total TA" ~ "Alkalinitet",
                              T ~ analysis),
         from = "Miljøportalen",
         label = paste0(analysis," (",unit,")"),
         label = as.factor(label),
         label = factor(label, levels = c("Phosphor, total-P (mg/l)","Ortho-phosphat-P (mg/l)","Nitrogen, total-N (mg/l)","Ammoniak+ammonium-N (mg/l)","Nitrit+nitrat-N (mg/l)","Chlorophyl A (µg/l)","pH (pH)","Alkalinitet (mmol/l)"))) -> lake_chemistry_miljo

readxl::read_excel("Data/Røgbølle sø vandkemi SDU.xlsx") %>% 
  rename("Phosphor, total-P" = 2, "Ortho-phosphat-P" = 3, "Chlorophyl A" = 4, "Nitrogen, total-N" = 5, "Nitrit+nitrat-N" = 6) %>% 
  mutate(date = as.Date(`Sampling dato`),
         doy = yday(date),
         type = "Kemi",
         year = year(date)) %>% 
  pivot_longer("Phosphor, total-P":"Nitrit+nitrat-N") %>% 
  select(date,analysis = name,value,doy,year,type) %>% 
  mutate(unit = case_when(analysis %in% c("Phosphor, total-P","Nitrogen, total-N","Ammoniak+ammonium-N","Ortho-phosphat-P","Nitrit+nitrat-N"," Ammonium-N") ~ "mg/l",
                          analysis == "Chlorophyl A" ~ "µg/l",
                          analysis == "pH" ~ "pH",
                          analysis == "Alkalinitet" ~ "mmol/l"),
         from = "SDU",
         label = paste0(analysis," (",unit,")"),
         label = as.factor(label),
         label = factor(label, levels = c("Phosphor, total-P (mg/l)","Ortho-phosphat-P (mg/l)","Nitrogen, total-N (mg/l)","Ammoniak+ammonium-N (mg/l)","Nitrit+nitrat-N (mg/l)","Chlorophyl A (µg/l)","pH (pH)","Alkalinitet (mmol/l)"))) -> lake_chemistry_sdu

bind_rows(lake_chemistry_miljo,
          lake_chemistry_sdu)  -> lake_chemistry

#### UI ####

ui <- dashboardPage(
    dashboardHeader(title = "Søholt"),
    dashboardSidebar(
        sidebarMenu(
          id = "tabs",
          menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
          menuItem("Map", tabName = "map", icon = icon("map")),
          menuItem("Data", tabName = "data", icon = icon("database"),
            menuSubItem("Lake chemistry", tabName = "water"),
            menuSubItem("Fish", tabName = "fish"),
            menuSubItem("Greenhouse gas", tabName = "ghg"))
        )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "welcome",
          fluidRow(
            h1("Welcome to this page dedicated to the project 'ENHANCE' at Søholt Storskov"),
            tags$br(),
            h4("Here you will find information regarding locations and data from the project: 'Søholt Storskov - værkstedsområde for studier i positive effekter af genopretning af naturlig hydrologi'"),
            #Søholt Storskov - værkstedsområde for studier i positive effekter af genopretning af naturlig hydrologi
            h4("Start by pressing one of the menus on the left side"),
            tags$br(),
            tags$img(src = "Søholt storskov fig.png", width = "40%", height = "40%"),
            tags$br(),
            h6("This project is supported by Aage V. Jensens Naturfond"),
            tags$img(src = "Aage_logo.jpeg", width = 200, height = 200),
            h6("Any inquiries can be addressed to ",mailtoR(email = "Jonassoe@biology.sdu.dk",
                    text = "Jonas Stage Sø")),
          )),
        tabItem(tabName = "map",
          fluidRow(
            h2("Select a location to explore data from this site"),
            br(),
            leafletOutput("site", width = "100%", height = "80vh")
        )),
        tabItem(tabName = "data",
          fluidRow(
          )),
        tabItem(tabName = "water",
          fluidRow(
            h2("Here you will find data on lake water chemistry 🧪"),
            h5("All of the data collected before 2024 is downloaded from Miljødata")),
            box(checkboxGroupInput("analysis_plot_select", "", 
                                    choices = c("Phosphor, total-P","Ortho-phosphat-P","Nitrogen, total-N","Ammoniak+ammonium-N","Nitrit+nitrat-N","Chlorophyl A","pH","Alkalinitet")),
                sliderInput("chemistry_year_select", "Select years to display", min = 1973, max = 2025,value = c(1973,2025), sep = "")),
          fluidRow(
            plotOutput("chem_plot_output", height = 800))),
        tabItem(tabName = "fish",
          fluidRow(
            h2("Data on fish-surveys 🐠"),
            box(
                selectInput("fish_net_selection", "Select net", choices = c("All",seq(1,12,1))),
                sliderInput("fish_weight_range", "Select weight range (g)", min = 0, max = 10000,value = c(0,10000)),
                sliderInput("fish_length_range", "Select length range (cm)", min = 0, max = 10000,value = c(0,10000))),
            box(title = "Select species",
                  checkboxGroupInput("species_select", "", 
                                     choices = c("Aborre","Brasen","Hork","Rudskalle","Suder","Pigsmerling","Skalle"),
                                     selected =c("Aborre","Brasen","Hork","Rudskalle","Suder","Pigsmerling","Skalle")))),
          fluidRow(
            plotOutput("fish_plot_weight")),
          fluidRow(
            plotOutput("fish_plot_length"))
            #box(title = "Survey year",sliderInput("fish_year", "Select which year you would like to see observations from:", 2024, 2024,2024, sep = "")
            ),
        tabItem(tabName = "ghg",
          fluidRow(
            h2("Here you will find data on greenhouse gas emissions"),
            box(checkboxGroupInput("ghg_plot_select", "Select which type of flux you want to see", 
                                   choices = c("Ebullitive Methane" = "ebul_ch4", "Diffusive Methane" = "diff_ch4","Diffusive Carbon Dioxide" = "co2_flux")),
            sliderInput("ghg_range_select", "Zoom in on range ", min = -10000, max = 10000,value = c(-10000,10000)))),
            plotOutput("ghg_plot", height = 600, width = 400))
    )))
#### Server ####
server <- function(input, output, session) {

##### Map #####  
  output$site <- renderLeaflet({
    leaflet() %>%
      addWMSTiles(
        wms_url,
        layers = "geodanmark_2023_12_5cm", 
        options = c(WMSTileOptions(format = "image/png", transparent = T),
                    providerTileOptions(minZoom = 1, maxZoom = 100))) %>% 
      addPolylines(data = streams, color = "blue",fillOpacity = 1,opacity =1, group = "Water") %>% 
      addPolygons(data = wet, opacity = 1, color = "blue",fillOpacity = 1, group = "Water", stroke = F) %>%  
      addCircleMarkers(data = combined_markers, 
                          ~lat, 
                          ~long, 
                          fillColor = ~col, 
                          col = "black", 
                          stroke = T, 
                          fillOpacity = 1, 
                          opacity = 1,
                          layerId = ~id,
                          group = ~type) %>% 
      addLegend(data = combined_markers,
                position = "bottomleft",
                labels = c("Water","Lake","Piezometer","Isco","Greenhouse gas","Fish"),
                colors = c("blue","royalblue","darkorange","red","forestgreen","purple"),
                opacity = 1) %>% 
      addLayersControl(overlayGroups = c("Water","Lake","Piezometer","Isco","Greenhouse gas","Fish")) 
  })

  observeEvent(input$site_marker_click,{
    id_select <- input$site_marker_click$id
    type_select <- input$site_marker_click$group
    if (type_select %in% c("Lake")) {updateTabItems(session, "tabs", selected = "water")}
    else if (type_select == "Greenhouse gas") updateTabItems(session, "tabs", selected = "ghg")
    else if (type_select == "Fish") updateTabItems(session, "tabs", selected = "fish")
    print(id_select);print(type_select)
    updateSelectInput(session, "fish_net_selection", selected = id_select)
  })

##### Data ####
###### Water Chemistry ######
  chem_data <-  reactive({
    lake_chemistry %>% 
      filter(analysis %in% input$analysis_plot_select &
             between(year, input$chemistry_year_select[1],input$chemistry_year_select[2])) -> chemical_select 
    return(chemical_select)
  })  

  output$chem_plot_output<-renderPlot({
    req(input$analysis_plot_select)
  chem_data_plot1 <- chem_data() %>% 
      ggplot(aes(date, value, col = year, group = year)) + 
      geom_point() + 
      theme(legend.key.width= unit(4, 'cm'),
            strip.background = element_blank(), 
            strip.text.x = element_blank(),
            strip.text.y = element_blank()) +
      scale_color_viridis_c() + 
      tema + 
      labs(x = "",
           y = "",
           col = "Year")  +
      facet_grid(label~1, scales = "free")
    
  chem_data_plot2 <- chem_data() %>% 
      ggplot(aes(doy, value, col = year, group = year)) + 
      geom_point() + 
      theme(legend.key.width= unit(4, 'cm'),
            strip.background = element_blank(), 
            strip.text.x = element_blank()) +
      scale_color_viridis_c() + 
      tema +
      labs(x = "",
           y = "",
           col = "Year") +
      scale_x_continuous(breaks = c(1,32,60,91,121,152,182,213,244,274,305,335),
                         labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) + 
      facet_grid(label~1, scales = "free")
  
  chem_data_plot1 + chem_data_plot2 +plot_layout(guides = "collect", axes = "collect",axis_titles = "collect") & theme(legend.position = "bottom") })

###### GHG ######
  ghg_data <-  reactive({
    ghg_results %>% 
      filter(name %in% input$ghg_plot_select) -> ghg_select 
    
    updateSliderInput(session, "ghg_range_select", 
                      value = c(floor(min(ghg_select$value,na.rm=T)), ceiling(max(ghg_select$value,na.rm=T))),
                      min = floor(min(ghg_select$value,na.rm=T)), max = ceiling(max(ghg_select$value,na.rm=T)), step = 1)
    
    return(ghg_select)
  })  
  
  output$ghg_plot<- renderPlot({
  req(input$ghg_plot_select)
  my_labeller <- as_labeller(c(co2_flux="CO[2]", diff_ch4="Diffusive~CH[4]", ebul_ch4="Ebullitive~CH[4]"),
                               default = label_parsed)  
  ghg_data() %>% 
    ggplot(aes(as.factor(station), value, fill = as.factor(name))) + 
    geom_boxplot(show.legend = F) + 
    facet_grid(name~1, scales = "free",labeller = my_labeller) + 
    coord_cartesian(ylim = c(input$ghg_range_select[1],input$ghg_range_select[2])) +
    tema + 
    theme(strip.text.x = element_blank()) + 
    labs(x = "Station",
         y = bquote("Flux (µmol m"^-2*" h"^-1*")"))
  })
  
###### Fish ######
  
  fish_data <-  reactive({
                        if (input$fish_net_selection == "All") 
                        {fish_data <- fangst_data %>% 
                            filter(art %in% input$species_select)} 
                            else {
                              fish_data <- fangst_data %>% 
                                              filter(id == input$fish_net_selection &
                                                     art %in% input$species_select)}

                  updateSliderInput(session, "fish_weight_range", 
                                    value = c(floor(min(fish_data$weight)), ceiling(max(fish_data$weight))),
                                    min = floor(min(fish_data$weight)), max = ceiling(max(fish_data$weight)), step = 1)
                  
                  updateSliderInput(session, "fish_length_range", 
                                    value = c(floor(min(fish_data$length)), ceiling(max(fish_data$length))),
                                    min = floor(min(fish_data$length)), max = ceiling(max(fish_data$length)), step = 1)
                  
                  return(fish_data)
                              })
  

  
  output$fish_plot_weight <- renderPlot({
    req(input$species_select)
    fish_data() %>% 
      filter(between(weight,input$fish_weight_range[1], input$fish_weight_range[2]),
             between(length, input$fish_length_range[1],input$fish_length_range[2])) %>% 
      ggplot(aes(weight, fill = art)) + 
      geom_histogram(position = position_dodge2()) +
      tema + 
      labs(x = "Weight (g)",
           y = "Count", 
           fill = "Species") + 
      theme(legend.position = "none")
  })
  
  output$fish_plot_length <- renderPlot({
    req(input$species_select)
    fish_data() %>% 
      filter(between(weight,input$fish_weight_range[1], input$fish_weight_range[2]),
             between(length, input$fish_length_range[1],input$fish_length_range[2])) %>% 
      ggplot(aes(length, fill = art)) + 
      geom_histogram(position = position_dodge2()) +
      tema + 
      labs(x = "Length (cm)",
           y = "Count", 
           fill = "Species")
  })
  
}
shinyApp(ui, server)