
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

load("ice_data.RData")

# define UI     

ui <- navbarPage(
  title = paste("ICE in Pennsylvania, 2026 - IN PROGRESS - DO NOT SHARE"),
  # set theme
  theme = shinytheme("cosmo"),
  # set HTML tags style
  tags$style(type = "text/css",
             # for moving "No Data" box to bottom of legend
             "div.info.legend.leaflet-control br {clear: both;}",
             "#leafletMap {height: calc(87vh) !important;}"),
  tabPanel("Dashboard",
           # map sidebar
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
                                          div(style = "margin: auto; width: 90%;",
                                            sliderInput(
                                              inputId = "time_287g",
                                              label = "287(g) Dates Signed",
                                              min = min(data_ice_point$signed, na.rm = TRUE),
                                              max = max(data_ice_point$signed, na.rm = TRUE),
                                              value = c(min(data_ice_point$signed, na.rm = TRUE),
                                                        max(data_ice_point$signed, na.rm = TRUE)),
                                              ticks = FALSE,
                                              timeFormat = "%D"
                                            )
                                          ),
                                          pickerInput(
                                            inputId = "demographic",
                                            label = "Background Demographic",
                                            choices = c(# "Total ICE (??) Arrests",       ##### EDIT
                                                        "Electoral Races",
                                                        "Total Population",
                                                        "Total Population Non-White",
                                                        "Total Population Hispanic or Latino",
                                            #            "Proportion Non-White (%)",
                                            #            "Proportion Hispanic or Latino (%)",
                                                        "None"),
                                            selected = "Total Population",
                                            multiple = FALSE
                                          ),
                                          #pickerInput(
                                          #  inputId = "elections",
                                          #  label = "Ballot Return Outlets",
                                          #  choices = c("County Election Offices",
                                          #              "Satellite Election Offices",
                                          #              "Drop Boxes"),
                                          #  selected = c("County Election Offices",
                                          #               "Satellite Election Offices",
                                          #               "Drop Boxes"),
                                          #  multiple = TRUE,
                                          #  options = pickerOptions(actionsBox = TRUE,
                                          #                          selectAllText = NULL,
                                          #                          selectedTextFormat = "count > 2")
                                          #),
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
                 tabPanel("Map",
                          leafletOutput(outputId = "leafletMap",
                                        width = "98.5%"),
                 ),
                 tabPanel("ICE Activity Table",
                          br(),
                          downloadBttn(outputId = "iceActivityCSV",
                                       label = "Export as .CSV",
                                       style = "bordered",
                                       color = "primary",
                                       size = "sm"),
                          br(),
                          br(),
                          DTOutput("iceActivityTable",
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
                 selected = "Map",
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
  data_ice_point_filtered <- reactive({
    data_ice_point %>%                      ##### REVISIT AFTER NEW ICE DATA
      filter(icetype %in% input$ice) %>%
      { if(any("287(g) Agreements" %in% .$icetype)) 
        filter(., (.$signed >= input$time_287g[1] & .$signed <= input$time_287g[2]) | 
                 is.na(.$signed))
        else 
          . } %>%
      mutate(icetype_color = case_when(icetype == "Field Offices" ~ "#eb301e",
                                       icetype == "Detention Facilities" ~ "#9c4deb",
                                       icetype == "287(g) Agreements" ~ "orange",
                                       .default = "black")) %>%
      arrange(icetype) %>%
      return()
  }) %>%
    bindEvent(input$ice, 
              input$time_287g,
              ignoreNULL = FALSE)
  
  # create HTML tags for ICE labels and popups
  tags_ice <- reactive({
    case_when(data_ice_point_filtered()$icetype == "Field Offices" ~                 
                paste0(ifelse(is.na(data_ice_point_filtered()$supervising_office),
                              paste0("<b>Field Office:</b> ", data_ice_point_filtered()$name),
                              paste0("<b>Field Office:</b> ", data_ice_point_filtered()$name, " ", data_ice_point_filtered()$type,
                                     "<br>
                                     <b>Supervising Office:</b> ", data_ice_point_filtered()$supervising_office)),
                       "<br>
                       <b>Agency:</b> ", data_ice_point_filtered()$agency,
                       "<br>
                       <b>County:</b> ", data_ice_point_filtered()$county),
              data_ice_point_filtered()$icetype == "Detention Facilities" ~ 
                paste0("<b>Detention Facility:</b> ", data_ice_point_filtered()$name,
                       "<br>
                       <b>Facility Code:</b> ", data_ice_point_filtered()$detention_facility_code,
                       "<br>
                       <b>County:</b> ", data_ice_point_filtered()$county,
                       "<br>
                       <b>Detention Stats, Past 365 Days:</b>
                       <p style = 'margin-left: 3px;'>
                       - Days with At Least One Detention: ", data_ice_point_filtered()$days_with_detentions_daily_last_year,
                       "<br>
                       - Average Daily Detention Population: ", ifelse(round(data_ice_point_filtered()$average_daily_population_last_year) >= 1,
                                                                       format(round(data_ice_point_filtered()$average_daily_population_last_year), big.mark = ","),
                                                                       " < 1 "),
                       "<br>
                       - Max Daily Detention Population: ", format(data_ice_point_filtered()$max_daily_population_last_year, big.mark = ","),
                       "</p>"),
              data_ice_point_filtered()$icetype == "287(g) Agreements" ~ 
                paste0("<b>287(g) Agreement:</b> ", data_ice_point_filtered()$name,
                       "<br>
                       <b>County:</b> ", data_ice_point_filtered()$county,
                       "<br>
                       <b>Partnership Model:</b> ", data_ice_point_filtered()$type,
                       "<br>
                       <b>Date Signed:</b> ", data_ice_point_filtered()$signed_formatted,
                       "<br>
                       <b>MOA:</b> ", case_when(data_ice_point_filtered()$moa == "link" ~ paste0("<a href = ", data_ice_point_filtered()$moa_link, ">Linked</a>"),
                                         data_ice_point_filtered()$moa == "link pending" ~ "Link Pending",
                                         is.na(data_ice_point_filtered()$moa) ~ "None Listed",
                                         .default = data_ice_point_filtered()$moa),
                       "<br>
                       <b>Addendum:</b> ", case_when(data_ice_point_filtered()$addendum == "link" ~ paste0("<a href = ", data_ice_point_filtered()$addendum_link, ">Linked</a>"),
                                              data_ice_point_filtered()$addendum == "link pending" ~ "Link Pending",
                                              is.na(data_ice_point_filtered()$addendum) ~ "None Listed",
                                              .default = data_ice_point_filtered()$addendum)),
              .default = "") %>%
      # ensure output always has length > 0 even if no ice data selected for display 
      { if(length(.) == 0) paste0("No Data") else . } %>%
      # render text as HTML
      lapply(HTML)
  }) %>%
    bindEvent(data_ice_point_filtered())
  
  # user selects geography
  data_census_filtered <- reactive({
    data_census %>%
      filter(geotype %in% input$geography) %>%
      { if(input$demographic == "None")
        select(., name, geometry)
        else 
          select(., name, input$demographic, geometry) %>%
          arrange(input$demographic) } %>%
      return()
  }) %>%
    bindEvent(input$geography,
              input$demographic)
  
  # update geography color scheme with demographic selection
  census_pal <- reactive({
    if(input$demographic == "Electoral Races") {
      colorFactor(palette = c("salmon", "lightgray", "lightgray"),
                  levels = sort(unique(data_census$`Electoral Races`)))
    } else if(input$demographic == "ACLU-PA Priority/Risk") {
      colorFactor(palette = c("purple", "orange", "beige"),
                  levels = unique(pull(data_census_filtered()), 2))
    } else if(grepl("Population|Proportion", input$demographic)) {
      colorNumeric(palette = "Blues",
                   domain = pull(data_census_filtered(), 2)) 
    } else if(input$demographic == "None") {
      "lightgray"
    }
  }) %>%
    bindEvent(data_census_filtered())
  
  # create HTML tags for geography labels and popups
  tags_geo <- reactive({
    paste0("<b>Geography:</b> ", data_census_filtered()$name,
           "<br>
           <b>", case_when(input$demographic == "Electoral Races" ~ "Contest Type: ",
                           input$demographic == "None" ~ "",
                           .default = paste0(input$demographic, ": ")), "</b>",
           if(input$demographic == "None") { "" }
           else if(is.numeric(data_census_filtered()[[2]])) { format(data_census_filtered()[[2]], big.mark = ",") }
           else if(is.character(data_census_filtered()[[2]])) { data_census_filtered()[[2]] }) %>%      ##### FIX DEFUNCT COLUMNS
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
              zoom = 8) %>%
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
  
  # generate ice markers
  observe({
    proxy %>%
      clearGroup("icemarkers") %>%
      removeControl("icelegend") %>%
      addCircleMarkers(data = data_ice_point_filtered() %>%
                         pull(geometry),
                       radius = 5,
                       stroke = TRUE,
                       color = "black",
                       weight = 3,
                       opacity = 0.8,
                       fill = TRUE,             
                       fillColor = data_ice_point_filtered()$icetype_color,
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
                  data = data_ice_point_filtered(),
                  position = "bottomright",
                  title = "ICE Type",
                  pal = colorFactor(palette = unique(data_ice_point_filtered()$icetype_color),
                                    domain = data_ice_point_filtered()$icetype),
                  values = ~ icetype,
                  layerId = "icelegend") }
  }) %>%
    bindEvent(data_ice_point_filtered(), input$popup_clear)  ## debug first time clicking clear all popups, then integrate other data
  
  # generate background geography
  observe({
    # define palette
    pal <- census_pal()
    # include exception for background demographic "None"
    pal_full <- if(class(pal) == "function") {
      # if census_pal() returns a function,
      # input census data to generate palette for fillColor
      pal(pull(data_census_filtered(), 2))
      # if census_pal() returns single-value character vector,
      # return single value for fillColor
    } else {
      pal
    }
    # build background map
    proxy %>%
      clearGroup(., "geoshapes") %>%
      removeControl("geolegend") %>%
      addPolygons(data = data_census_filtered(),     ### integrate municipality lines
                  stroke = TRUE,
                  color = "black",
                  weight = 1,
                  fill = TRUE,          
                  fillColor = pal_full,
                  fillOpacity = 0.7,
                  label = tags_geo(),
                  labelOptions = labelOptions(direction = "top"),
                  popup = tags_geo(),
                  popupOptions = popupOptions(autoClose = FALSE,
                                              closeOnClick = FALSE),
                  group = "geoshapes",
                  options = pathOptions(pane = "polygons")) # %>%
     # { if(input$geography != "None") 
    #    addLegend(map = .,
     #             data = data_census_filtered(),
    #              position = "bottomright",
     #             title = "Contest Type",
    #              pal = colorFactor(palette = unique(data_census_filtered()$contest_color),
     #                               domain = data_census_filtered()$contest),
    #              values = ~ contest,
    #              layerId = "geolegend") }
  }) %>%
    bindEvent(data_census_filtered(), input$popup_clear)
  
  # clear popups on click
  observe({
    proxy %>%
      clearPopups()
  }) %>%
    bindEvent(input$popup_clear)
  
  # create ice activity table widget
  
  # COMMENT
  data_ice_point_filtered_export <- reactive({
    data_ice_point_filtered() %>%
      mutate(MOA = ifelse(!is.na(moa_link),
                          str_to_title(paste0("<a href=", moa_link, ">", moa, "</a>")),
                          str_to_title(moa))) %>%
      mutate(Addendum = ifelse(!is.na(addendum_link),
                               str_to_title(paste0("<a href=", addendum_link, ">", addendum, "</a>")),
                               str_to_title(addendum))) %>%
      select(-c("county_fips_code",
                "moa",
                "moa_link",
                "addendum",
                "addendum_link",
                "icetype_color",
                "signed_formatted")) %>%
      rename("Type" = icetype,
             "County" = county,
             "Unit Name" = name,
             "ICE Agency" = agency,
             "Model or Subtype" = type,
             "Supervising Office" = supervising_office,
             "Address" = address,
             "City" = city,
             "State" = state,
             "ZIP" = zip,
             "Detention Facility Code" = detention_facility_code,
             "Days with Detentions, Past 365 Days" = days_with_detentions_daily_last_year,
             "Average Daily Detention Population, Past 365 Days" = average_daily_population_last_year,
             "Max Daily Detention Population, Past 365 Days" = max_daily_population_last_year) %>%
      st_drop_geometry()
  })
  
  output$iceActivityTable <- renderDT({
    datatable(data_ice_point_filtered_export(),
              rownames = FALSE,
              options = list(pageLength = 50,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': 'white'});",
                               "}")
              ),
              # read HTML instead of escaping to normal string
              escape = FALSE
    ) %>%
      formatStyle(columns = 1:6,
                  backgroundColor = "white")
  })
  
  ## Create voter contacts download behavior
  output$iceActivityCSV <- downloadHandler(
    filename = function() {
      paste0("ice-activity", "-test", ".csv")         #### CHANGE DOWNLOAD NAME
    },
    content = function(file) {
      write.csv(data_ice_point_filtered_export(),
                file,
                row.names = FALSE)
    }
  )
  
  # generate summary readout tab
  output$summary <- renderText({
    paste0("<b>Field Offices Displayed:</b> ", nrow(filter(data_ice_point_filtered(), icetype == "Field Offices")),
           "<br>
            <b>Detention Facilities Displayed:</b> ", nrow(filter(data_ice_point_filtered(), icetype == "Detention Facilities")),
           "<br>
            <b>287(g) Agreements Displayed:</b> ", nrow(filter(data_ice_point_filtered(), icetype == "287(g) Agreements")))
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