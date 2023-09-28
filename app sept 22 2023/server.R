library(shiny)
library(rsconnect)
library(tidyr)
library(ggplot2)
library(sf)
library(dplyr)
library(leaflet)
library(stringr)
library(kableExtra)
library(jsonlite)
library(magrittr)
library(HatchedPolygons)
library(tidyverse)


#### Data processing ####  
dat <- st_read("homeownership_17_21.geojson") %>%
  st_as_sf() %>%
  st_transform("EPSG:4326") %>%
  st_make_valid() %>%
  filter(is.na(owner_occ_hh_pct_21) == FALSE) %>%
  dplyr::mutate(NAME = word(NAME, 1))

dat <- dat %>%
  dplyr::select(NAME) %>%
  st_centroid() %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  left_join(dat, by = "NAME") %>%
  st_as_sf()


rural <- hatched.SpatialPolygons(dat %>% filter(rural == 1), density = 13, angle = c(45, 135)) %>%
  st_union() %>%
  st_as_sf()

#### Leaflet formatting ####  
##### palette #####
# Compute quintiles
#quintiles <- quantile(dat$owner_occ_hh_pct)
quintiles <- c(52, 71, 74, 78, 85)
names(quintiles) = c("52%", "71%", "74%", "78%", "85%")
# Create color palette based on data range
color_palette <- colorBin("YlGnBu", bins = quintiles, domain = dat$owner_occ_hh_pct_21)

##### labels #####
labs_dat <- sprintf(
  "<strong>%s</strong><br/>
  Home Ownership Rate: %.0f%%<sup></sup><br/>
  Statewide Median: 74%%",
  dat$NAME, dat$owner_occ_hh_pct_21
) %>% lapply(htmltools::HTML)

##### title #####
title_dat <- tags$div(
  HTML("<strong>Homeownership rates by County (%)</strong><br/>
        Hover to see individual counties"))

#### server function ####
server <- function(input, output, session) {
  

output$leaflet <- renderLeaflet({
  leaflet(dat) %>%
    addPolygons(fillColor = ~color_palette(owner_occ_hh_pct_21),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                # label = labs_dat,
                # labelOptions = labelOptions(
                #   style = list("font-weight" = "normal", padding = "3px 8px"),
                #   textsize = "15px",
                #   direction = "auto")
                ) %>%
    addLabelOnlyMarkers(~lon, ~lat, label =  ~as.character(NAME),
                        labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T, style = list(
                          "color" = "DarkSlateBlue",
                          "font-family" = "sans-serif",
                         # "font-weight" = "bold",
                          "font-size" = "12px")),
                        group = "txt_labels") %>%
    addControl(title_dat, position = "topright") %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addLegend(pal = color_palette, title = "Percent", opacity = 1, values = ~quintiles,
              position = "bottomright") %>%
    groupOptions("txt_labels", zoomLevels = 8:100)  
})

x = reactiveVal(1)
observeEvent(input$rural,{
  x(x()+1) # increment x by 1
  x <- as.numeric(x())
})

observeEvent(input$rural, {
  if((x() %% 2) == 0) {
    leafletProxy("leaflet") %>%
      addPolylines(
        data = rural,
        weight = 1.5,
        layerId  = "rural") 
  } else {
    leafletProxy("leaflet") %>%
      removeShape(layerId  = "rural")
  }
    })


##### plot #####
output$plot <- renderPlot({
  dat %>%
    mutate(reord = as.numeric(owner_occ_hh_pct_21) + as.numeric(rural),
           ID = fct_reorder(NAME, reord, .desc = F)) %>% 
  ggplot(aes(x = reorder(ID, rural), y = owner_occ_hh_pct_21, fill = owner_occ_hh_pct_21)) +
    geom_bar(color = NA, stat = "identity") +
  # geom_text(aes(label=NAME), colour = "navy") +
    geom_text(aes(label=paste(owner_occ_hh_pct_21, "%", sep = '')), hjust=0, colour = "navy", alpha = 0.6,  position = "dodge") +
   #scale_color_manual(values = c("white", "navy")) +
    scale_fill_distiller(palette = "YlGnBu", direction = 1) +
    labs(x = "Urban Counties                                                                                                        Rural Counties", y = "", fill = "%", color = "Rural County") +
    theme_minimal() +
    theme(legend.position = "none",
          text = element_text(size = 16),
          axis.title.y = element_text(face = "bold")) +
    coord_flip()
})

##### summary #####
output$tab <- renderTable({
    data.frame(quartile_1 = quantile(dat$owner_occ_hh_pct_21, probs = 0.25, na.rm = TRUE),
              mean = mean(dat$owner_occ_hh_pct_21, na.rm = TRUE),
              median = median(dat$owner_occ_hh_pct_21, na.rm = TRUE),
              quartile_3 = quantile(dat$owner_occ_hh_pct_21, probs = 0.75, na.rm = TRUE),
              max = max(dat$owner_occ_hh_pct_21, na.rm = TRUE)) 
  
  
})

}