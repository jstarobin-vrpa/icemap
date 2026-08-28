
# setup

library(bslib)
library(DT)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(openxlsx)
library(sf)
library(shiny)
library(shinyBS)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

# import data

load("~/Documents/2026 | ACLU-PA Voting Rights Litigation Associate/projects/ice/data/ice_data.RData")
carto_api <- "cb1_2dqa_1_ffc22f31de11903b2ce40e4f"

# define UI     

ui <- navbarPage(
  title = paste("ICE in Pennsylvania, 2026"),
  # set theme
  theme = shinytheme("cosmo"),
  # set HTML tags style
  tags$style(type = "text/css",
             # for moving "No Data" box to bottom of legend
             "div.info.legend.leaflet-control br {clear: both;}",
             "#leafletMap {height: calc(87vh) !important;}"),
  tabPanel("Dashboard",
           # app sidebar
           sidebarLayout(
             sidebarPanel(
               bsCollapse(id = "sidebar",
                          bsCollapsePanel(title = "Map Controls",
                                          style = "primary",
                                          pickerInput(
                                            inputId = "ice",
                                            label = "ICE Activity",
                                            choices = c("Field Offices",
                                                        "Detention Facilities",
                                                        "287(g) Agreements"),
                                            selected = c("Field Offices",
                                                         "Detention Facilities",
                                                         "287(g) Agreements"),
                                            multiple = TRUE,
                                            options = pickerOptions(actionsBox = TRUE,
                                                                    selectAllText = NULL,
                                                                    selectedTextFormat = "count > 2")
                                          ),
                                          pickerInput(
                                            inputId = "elections",
                                            label = "Ballot Return Outlets",
                                            choices = c("County Election Offices",
                                                        "Satellite Election Offices",
                                                        "Drop Boxes"),
                                            selected = c("County Election Offices",
                                                         "Satellite Election Offices",
                                                         "Drop Boxes"),
                                            multiple = TRUE,
                                            options = pickerOptions(actionsBox = TRUE,
                                                                    selectAllText = NULL,
                                                                    selectedTextFormat = "count > 2")
                                          ),
                                          pickerInput(
                                            inputId = "geography",
                                            label = "Geography",
                                            choices = c("Counties",
                                                        "Municipalities",
                                                        "US House Districts",
                                                        "PA Senate Districts",
                                                        "PA General Assembly Districts",
                                                        "None"),
                                            selected = "Counties",
                                            multiple = FALSE
                                          ),
                                          actionButton(
                                            inputId = "popup_clear",
                                            label = "Clear All Popups",
                                            width = "100%"
                                          )
                          ),
                          bsCollapsePanel(title = "Summary Readout",
                                          style = "info",
                                          htmlOutput("summary")),
                          multiple = TRUE,
                          open = c("Map Controls", "Summary Readout")
               ),
               width = 3
             ),
             mainPanel = card(
               tabsetPanel(
                 tabPanel("ICE Map",
                          leafletOutput(outputId = "leafletMap",
                                        width = "98.5%"),
                 ),
                 tabPanel("ICE Data Table",
                          br(),
                          downloadBttn(outputId = "iceCSV",
                                       label = "Export as .CSV",
                                       style = "bordered",
                                       color = "primary",
                                       size = "sm"),
                          br(),
                          br(),
                          DTOutput("mappedVoterContacts",
                                   width = "98.5%"),
                          br()
                 ),
                 tabPanel("County-Level Readout",
                          br(),
                          downloadBttn(outputId = "iceAggCSV",
                                       label = "Export as .CSV",
                                       style = "bordered",
                                       color = "primary",
                                       size = "sm"),
                          br(),
                          br(),
                          DTOutput("countyReadout",
                                   width = "98.5%"),
                          br()
                 ),
                 selected = "ICE Map",
                 type = "pills"
               ),
               full_screen = TRUE
             ),
             fluid = TRUE
           )
  ),
  tabPanel("About",
           ## About page
           htmlOutput("about")
  )
)

server <- function(input, output) {
  
  # user selects forms of ICE activity
  data_ice_filtered <- reactive({
    data_ice %>%                   ##### REVISIT AFTER NEW ICE DATA
      filter(icetype %in% input$ice) %>%
      mutate(icetype_color = case_when(icetype == "Field Offices" ~ "#eb301e",
                                       icetype == "Detention Facilities" ~ "#9c4deb",
                                       icetype == "287(g) Agreements" ~ "orange",
                                       .default = "black")) %>%
      arrange(icetype) %>%
      return()
  }) %>%
    bindEvent(input$ice, 
              ignoreNULL = FALSE)
  
  # create HTML tags for ICE labels and popups
  tags_ice <- reactive({
    case_when(data_ice_filtered()$icetype == "Field Offices" ~                 
                paste0(ifelse(is.na(data_ice_filtered()$supervising_office),
                              paste0("<b>Field Office:</b> ", data_ice_filtered()$name),
                              paste0("<b>Field Office:</b> ", data_ice_filtered()$name, " ", data_ice_filtered()$type,
                                     "<br>
                                     Supervising Office: ", data_ice_filtered()$supervising_office)),
                       "<br>
                       <b>Agency:</b> ", data_ice_filtered()$agency,
                       "<br>
                       <b>County:</b> ", data_ice_filtered()$county),
              data_ice_filtered()$icetype == "Detention Facilities" ~ 
                paste0("<b>Detention Facility:</b> ", data_ice_filtered()$name,
                       "<br>
                       <b>Facility Code:</b> ", data_ice_filtered()$detention_facility_code,
                       "<br>
                       <b>County:</b> ", data_ice_filtered()$county,
                       "<br>
                       <b>Detention Stats, Past 365 Days:</b>
                       <p style = 'margin-left: 3px;'>
                       - Days with At Lease One Detention: ", data_ice_filtered()$days_with_detentions_daily_last_year,
                       "<br>
                       - Average Daily Detention Population: ", ifelse(round(data_ice_filtered()$average_daily_population_last_year) >= 1,
                                                                       format(round(data_ice_filtered()$average_daily_population_last_year), big.mark = ","),
                                                                       " < 1 "),
                       "<br>
                       - Max Daily Detention Population: ", format(data_ice_filtered()$max_daily_population_last_year, big.mark = ","),
                       "</p>"),
              data_ice_filtered()$icetype == "287(g) Agreements" ~ 
                paste0("<b>287(g) Agreement:</b> ", data_ice_filtered()$name,
                       "<br>
                       <b>County:</b> ", data_ice_filtered()$county,
                       "<br>
                       <b>Partnership Model:</b> ", data_ice_filtered()$type,
                       "<br>
                       <b>Date Signed:</b> ", data_ice_filtered()$signed_formatted,
                       "<br>
                       <b>MOA:</b> ", case_when(data_ice_filtered()$moa == "link" ~ paste0("<a href = ", data_ice_filtered()$target_g, ">Linked</a>"),
                                         data_ice_filtered()$moa == "link pending" ~ "Link Pending",
                                         is.na(data_ice_filtered()$moa) ~ "None Listed",
                                         .default = data_ice_filtered()$moa),
                       "<br>
                       <b>Addendum:</b> ", case_when(data_ice_filtered()$addendum == "link" ~ paste0("<a href = ", data_ice_filtered()$target_h, ">Linked</a>"),
                                              data_ice_filtered()$addendum == "link pending" ~ "Link Pending",
                                              is.na(data_ice_filtered()$addendum) ~ "None Listed",
                                              .default = data_ice_filtered()$addendum)),
              .default = "") %>%
      # ensure output always has length > 0 even if no ice data selected for display 
      { if(length(.) == 0) paste0("No Data") else . } %>%
      # render text as HTML
      lapply(HTML)
  }) %>%
    bindEvent(data_ice_filtered())
  
  # user selects geography
  data_census_filtered <- reactive({
    data_census %>%
      filter(geotype %in% input$geography) %>%
      mutate(contest_color = case_when(contest == "Competitive" ~ "salmon",
                                       contest == "Standard" ~ "lightgray",
                                       contest == "No Data" ~ "gray",
                                       .default = "orange")) %>%
      select(name, contest, contest_color, geometry) %>%
      arrange(contest) %>%
      return()
  }) %>%
    bindEvent(input$geography)
  
  # create HTML tags for geography labels and popups
  tags_geo <- reactive({
    paste0("Geography: ", data_census_filtered()$name,
           "<br>
           Contest Type: ", data_census_filtered()$contest) %>%
      # render text as HTML
      lapply(HTML)
  }) %>%
    bindEvent(data_census_filtered())
  
  # generate leaflet map
  output$leafletMap <- renderLeaflet({
    # base map includes only aspects that don't change dynamically
    leaflet(data = basemap) %>%
      setView(lng = basemap$pa_center_lng,
              lat = basemap$pa_center_lat, 
              zoom = 7) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addTiles(urlTemplate = paste0("https://basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png?key=", carto_api),
               attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>, 
                              &copy; <a href="https://carto.com/attributions">CARTO</a>',
                       group = "backgroundMap") %>%
      addMapPane("polygons", zIndex = 410) %>%
      addMapPane("polylines", zIndex = 415) %>%
      addMapPane("markers", zIndex = 420) %>%
      addPolylines(color = "black",
                   weight = 2,
                   options = pathOptions(pane = "polylines"),
                   group = "state_border")               ### Add functionality to hide this? And to hide background map?
  })
  
  proxy <- leafletProxy("leafletMap")
  
  observe({
    proxy %>%
      clearGroup("geoshapes") %>%
      removeControl("geolegend") %>%
      addPolygons(data = data_census_filtered(),     ### integrate municipality lines
                  stroke = TRUE,
                  color = "black",
                  weight = 1,
                  fill = TRUE,             #### REVISIT FOR RACIAL DEMOGRAPHICS AND POPUPS
                  fillColor = data_census_filtered()$contest_color,
                  fillOpacity = 0.7,
                  label = tags_geo(),
                  labelOptions = labelOptions(direction = "top"),
                  popup = tags_geo(),
                  popupOptions = popupOptions(autoClose = FALSE,
                                              closeOnClick = FALSE),
                  group = "geoshapes",
                  options = pathOptions(pane = "polygons")) %>%
      { if(input$geography != "None") 
        addLegend(map = .,
                  data = data_census_filtered(),
                  position = "bottomright",
                  title = "Contest Type",
                  pal = colorFactor(palette = unique(data_census_filtered()$contest_color),
                                    domain = data_census_filtered()$contest),
                  values = ~ contest,
                  layerId = "geolegend") }
  }) %>%
    bindEvent(input$geography)
  
  observe({
    proxy %>%
      clearGroup("icemarkers") %>%
      removeControl("icelegend") %>%
      addCircleMarkers(data = data_ice_filtered() %>%
                         pull(geometry),
                       radius = 5,
                       stroke = TRUE,
                       color = "black",
                       weight = 3,
                       opacity = 0.8,
                       fill = TRUE,             
                       fillColor = data_ice_filtered()$icetype_color,
                       fillOpacity = 0.8,
                       label = tags_ice(),
                       labelOptions = labelOptions(direction = "left"),
                       popup = tags_ice(),
                       popupOptions = popupOptions(autoClose = FALSE,
                                                   direction = "left",   ###### FIX - NOT DOING ANYTHING ATM
                                                   closeOnClick = FALSE),
                       group = "icemarkers",
                       options = pathOptions(pane = "markers")) %>%
      { if(length(input$ice) > 0)
        addLegend(map = .,
                  data = data_ice_filtered(),
                  position = "bottomright",
                  title = "ICE Type",
                  pal = colorFactor(palette = unique(data_ice_filtered()$icetype_color),
                                    domain = data_ice_filtered()$icetype),
                  values = ~ icetype,
                  layerId = "icelegend") }
    
    print(data_ice_filtered()[, c(1, 26)])
      
  }) %>%
    bindEvent(input$ice, input$popup_clear)              ## debug first time clicking clear all popups, then integrate other data
  
  observe({
    proxy %>%
      clearMarkers()
  }) %>%
    bindEvent(input$popup_clear)
  
  output$summary <- renderText({
    paste0("<b>Field Offices Displayed:</b> ", nrow(filter(data_ice_filtered(), icetype == "Field Offices")),
           "<br>
            <b>Detention Facilities Displayed:</b> ", nrow(filter(data_ice_filtered(), icetype == "Detention Facilities")),
           "<br>
            <b>287(g) Agreements Displayed:</b> ", nrow(filter(data_ice_filtered(), icetype == "287(g) Agreements")))
  })
  
  output$about <- renderText({
    paste0("<style>
           .title {
              font-size: 20px;
              margin-bottom: 0;
           }
           </style>
           
          <p class = title><b>CONCEPT</b></p>
          This app visualizes . ATTRIBUTION. This product uses the Census Bureau Data API but is not endorsed or certified by the Census Bureau.
           <br>
           ICE Field Offices, Sub-Offices, Detention Centers: 'government data published by ICE, collated by the Deportation Data Project, and analyzed by [your organization].'")
  })
  
}

shinyApp(ui = ui, server = server)