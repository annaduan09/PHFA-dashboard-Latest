library(shiny)
library(rsconnect)
library(tidyr)
library(ggplot2)
library(sf)
library(dplyr)
library(leaflet)
library(stringr)
library(jsonlite)
library(magrittr)
library(shinythemes)

ui <- navbarPage(theme = shinytheme("yeti"),
                 title = "Pennsylvania Counties Housing Explorer",
                   sidebarLayout(
                     sidebarPanel(width = 3,
                      selectInput("variable", "Select a variable", choices = c("Homeownership rate (%)" = "owner_occ_hh_pct_21",
                                                                               "Residents" = "population_2010",
                                                                               "Median Age" = "median_age_2020",
                                                                               "Median Family Income" = "medhhinc_2020"), selected = "owner_occ_hh_pct_21"),
                      # checkboxGroupInput("dataLayers", "Rural counties",
                      #                    choices = c("Show" = "rural"),
                      #                    selected = character(0)),
                      actionButton("rural", "Show Rural Counties"),
                      shiny::p("Use this web app to explore housing trends across Pennsylvania counties."),
                     )
                  ,
                    mainPanel(
                      tabsetPanel(type = "pills",
                                 tabPanel(width = 8, title = h4("Data Mapper"), h3("Interactive Map"), leafletOutput("leaflet", height = "800px",
                                                        width = "1000px")),
                                 tabPanel(width = 8, title = h4("Statewide Comparisons"), h3("Homeownership Across Counties"), plotOutput("plot", height = "1000px",
                                                   width = "800px"),
                                          h6(textOutput("caption", container = span))),
                                 tabPanel(width = 8, title = h4("Data Summary and Download"), h3("Data Viewer"), tableOutput("tab")))
                      )
                    )
                 )



