###########################################
#Arquivo: ui.R
#Autor: Lucas Bicalho
###########################################

## Libraries
library(shiny)
library(shinydashboard)
require(ggplot2)
library(ggplot2)
library(plyr)
library(plotly)
library(DT)
library(ggthemes)
library(ECharts2Shiny)
library(devtools)
library(gapminder)
library(forcats)

## Preparing sidebar items
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashbd", icon = icon("dashboard")),
    menuItem("Data", tabName = "datafile", icon = icon("th")),
    menuItem("Graphics Visualization", icon = icon("navicon"), tabName = "graphs", 
             menuSubItem("Dot Charts", tabName = "dotcharts", icon = icon("ellipsis-v")), 
             menuSubItem("Pie Charts", tabName = "piecharts", icon = icon("pie-chart")),
             menuSubItem("Bar Charts", tabName = "barcharts", icon = icon("bar-chart")),
             menuSubItem("Animated Plots", tabName = "animatedplots", icon = icon("angle-double-right"))
    ),
    menuItem("HeatMap", tabName = "hm", icon = icon("map-marker")),
    
    br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),hr(),
    
    # email sharing link
    menuItem("Feedback & suggestion", icon = icon("envelope-o"),
             href = "mailto:?lucas.bicalho@unifesp.br?subject=Feedback on AcSP app"),
    # source code link
    menuItem("Source code", icon = icon("file-code-o"), 
             href = "https://github.com/oraza/sectarianviolencePK"),
    # github fork link
    menuItem("Fork me @ github", icon = icon("code-fork"), 
             href = "https://github.com/oraza/sectarianviolencePK") 
  )
)

## Preparing body items
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashbd",
            fluidRow(
              valueBoxOutput("vbox1", width = 6),
              valueBoxOutput("vbox2", width = 6)),
            h2("Introduction",  align = "center", style = "font-family: 'times'; color:red"),
            p("Traffic accidents in São Paulo are becoming more frequent, mainly due to the recklessness of drivers,
              the constant increase in the number of vehicles and the lack of adequate infrastructure. 
              As a way of monitoring and projecting data, statistics are collected every year by the", a("Traffic 
              Engineering Company of São Paulo (CET-SP)", href = "http://www.cetsp.com.br/"),". Through this information
              it is possible to get an idea of the main factors influencing such events, which are often fatal.", 
              style = "font-family: 'times'"),
            fluidPage(
              fluidRow(
                column(
                  h2("About this app ...", align = "center", style = "font-family: 'times'; color:red"),
                  p("This app helps you to explore and visualize the main information about Traffic Accidents in 
                    São Paulo in the year 2016. 
                    I have used the database available", a("here.",
                    href="http://www.cetsp.com.br/media/562061/relatorioanualacidentestransito-2016.pdf"), 
                    style = "font-family: 'times'"),
                  width = 4,
                  align = "left"
                  
                ),
                column(
                  h2("How to use!", style = "font-family: 'times'; color:red"),
                  p("This app contains two major sections; database and graphs.", 
                    style = "font-family: 'times'"), 
                  p("Section for", strong("data"), "presents the database, which is provided with copying, printing and 
                    downloading options. Section for", strong("visualization"), "has two sub-sections.
                    Within each sub-section, navigation panel contains four 
                    tabs that plot data points according to the selected tab. Graphs are plotted on an interactive platform,
                    such that when the mouse-pointer hovers over data-point it shows further information. Users can choose 
                    to turn off the data-points by clicking on labels given in the legend.", 
                    style = "font-family: 'times'"),
                  width = 8,
                  align = "left"
                ),
                br(),br()
            )
          ),
          br(),br(),br(),br(),br(),br(),br(),
          p(strong("Suggested citation:"), "App for exploring and visualizing Traffic Accidents in São Paulo 
              in the year 2016, Version 1.0,", style = "font-family: 'times'", 
              tags$img(src = "C_thin.png", width = "10px", height = "10px"), "Lucas Bicalho")
    ),
    
    tabItem(tabName = "datafile",
            box(title = "Accident Data",
                width = 12, 
                DT::dataTableOutput('da.tab'))
    ),
    
    tabItem(tabName = "dotcharts",
            mainPanel(
              tabsetPanel(
                tabPanel("Mortos",
                         plotlyOutput("plotDotMortos"), width = "auto"),
                tabPanel("Feridos", 
                         plotlyOutput("plotDotFeridos"), width = "auto"))
            )
    ),
    
    tabItem(tabName = "piecharts",
            mainPanel(
              tabsetPanel(
                tabPanel("Graphic 1",
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Selector for variable to plot against AcSP ----
                           selectInput("variable1", "Vehicle:",
                                       c("Car" = "automovel",
                                         "Motorcycle" = "moto",
                                         "Bus" = "onibus",
                                         "Truck" = "caminhao",
                                         "Bicycle" = "bicicleta")),
                           # Input: Checkbox for whether outliers should be included ----
                           checkboxInput("outliers", "Show outliers", TRUE)
                         ),
                           plotlyOutput("plotPie1"), width = "auto"),
                tabPanel("Graphic 2",
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Selector for variable to plot against AcSP ----
                           selectInput("variable2", "Vehicle:",
                                       c("Car" = "automovel",
                                         "Motorcycle" = "moto",
                                         "Bus" = "onibus",
                                         "Truck" = "caminhao",
                                         "Bicycle" = "bicicleta")),
                           # Input: Checkbox for whether outliers should be included ----
                           checkboxInput("outliers", "Show outliers", TRUE)
                         ),
                         plotlyOutput("plotPie2"), width = "auto")
              )
            )
    ),
    
    tabItem(tabName = "barcharts",
            mainPanel(
              tabsetPanel(
                tabPanel("Graphic 1",
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Selector for variable to plot against AcSP ----
                           selectInput("variable3", "Vehicle:",
                                       c("Car" = "automovel",
                                         "Motorcycle" = "moto",
                                         "Bus" = "onibus",
                                         "Truck" = "caminhao",
                                         "Bicycle" = "bicicleta")),
                          
                           # Input: Checkbox for whether outliers should be included ----
                           checkboxInput("outliers", "Show outliers", TRUE)
                         ),
                         # Sidebar panel for inputs ----
                         sidebarPanel(
                           # Input: Selector for variable to plot against AcSP ----
                           selectInput("variable4", "Period:",
                                       c("Dawn" = "Dawn",
                                         "Morning" = "Morning",
                                         "Afternoon" = "Afternoon",
                                         "Night" = "Night")),
                           
                           # Input: Checkbox for whether outliers should be included ----
                           checkboxInput("outliers", "Show outliers", TRUE)
                         ),
                         # Main panel for displaying outputs ----
                         mainPanel(
                           # Output: Formatted text for caption ----
                           h3(textOutput("caption")),
                           # Output: Plot of the requested variable against AcSP ----
                           plotlyOutput("plotBar1")
                         ), width = "auto"),
                tabPanel("Graphic 2",
                         plotlyOutput("plotBar2"), width = "auto")
              )
            )
    ),
    
    tabItem(tabName = "animatedplots",
            mainPanel(
              tabsetPanel(
                tabPanel("Mortos X Distrito x Período",
                         plotlyOutput("plotanim1"), width = "auto")
              ),
              tabsetPanel(
                tabPanel("Dendrograma",
                         plotlyOutput("plotanim2"), width = "auto")
              )
            )
    ),
    
    tabItem(tabName = "hm",
            box(title = "HeatMap",
                width = 12),
            mainPanel(
                      htmlOutput("inc"),
                      # Sidebar panel for inputs ----
                      sidebarPanel(
                        # Input: Selector for variable to plot against AcSP ----
                        selectInput("variable3", "Vehicle:",
                                    c("Car" = "automovel",
                                      "Motorcycle" = "moto",
                                      "Bus" = "onibus",
                                      "Truck" = "caminhao",
                                      "Bicycle" = "bicicleta")),
                        
                        # Input: Checkbox for whether outliers should be included ----
                        checkboxInput("outliers", "Show outliers", TRUE)
                      )
            )
    )
  )
)

## Putting them together into a dashboardPage
ui <- dashboardPage( 
  skin="blue",
  # add this -> navbarMenu()
  dashboardHeader(
    title="Accidents in Sao Paulo",
    titleWidth = 250,
    #Facebook Sharing
    tags$li(class = "dropdown",
            tags$a(href = "http://www.facebook.com/sharer.php?u=https://ow-raza.shinyapps.io/sectarianviolencePK/", 
                   target = "_blank", 
                   tags$img(height = "20px", 
                            src = "fb2.png")
            )
    ),
    # Linkedin link
    tags$li(class = "dropdown",
            tags$a(href = "http://www.linkedin.com/shareArticle?mini=true&url=https:
                   //ow-raza.shinyapps.io/sectarianviolencePK/", 
                   target = "_blank", 
                   tags$img(height = "20px", 
                            src = "linkedin.png")
            )
    ),
    # Twitter link
    tags$li(class = "dropdown",
            tags$a(href = "http://twitter.com/share?url=https://ow-raza.shinyapps.io/sectarianviolencePK/  
                   &text= my first shiny app @reye27",
                   target = "_blank",
                   tags$img(height = "20px", 
                            src = "twitter.png")
            )
    ),
    # Github link
    tags$li(class = "dropdown",
            tags$a(href = "https://github.com/lucasbic",
                   target = "_blank",
                   tags$img(height = "20px", 
                            src = "github.png")
            )
    )
  ),
  sidebar,
  body
)