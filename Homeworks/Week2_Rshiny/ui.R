#setwd("C:/Users/jieun/Desktop/answer")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(dplyr)
library(nnet)

dashboardPage( skin="black",
  dashboardHeader(title = "Minju Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("widget",tabName="widget",icon=icon("box")),
      menuItem("practice",tabName = "practice", icon = icon("pencil-alt")),
      menuItem("Database", tabName = "Database", icon = icon("database")),
      menuItem("EDA", tabName = "EDA", icon = icon("chart-pie")),
      menuItem("Modeling", tabName = "Modeling", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    
    tabItems(
      #first tab(widget)-----------------------
      tabItem(tabName = "widget",
              fluidRow(
                column(3,
                       h3("Buttons"),
                       actionButton(inputId= "action",
                                    label = "Action")),
                column(3, 
                       checkboxGroupInput(inputId = "checkGroup", 
                                          label = h3("Checkbox group"), 
                                          choices = list("Choice 1" = 1, 
                                                         "Choice 2" = 2, 
                                                         "Choice 3" = 3),
                                          selected = 1)),
                column(3, 
                       dateInput(inputId = "date", 
                                 label = h3("Date input"), 
                                 value = "2019-02-23"))),
              
              fluidRow(
                column(3,
                       dateRangeInput("dates", h3("Date range"))),
                column(3,
                       fileInput("file", h3("File input"))),
                column(3, 
                       numericInput("num", 
                                    h3("Numeric input"), 
                                    value = 1))),
              
              fluidRow(
                column(3,
                       selectInput(inputId= "select", 
                                   label = h3("Select box"), 
                                   choices = list("Choice 1" = 1, "Choice 2" = 2,
                                                  "Choice 3" = 3), selected = 1)),
                column(3, 
                       sliderInput(inputId = "slider1", 
                                   label = h3("Sliders"),
                                   min = 0, max = 100, value = 50),
                       
                       sliderInput("slider2", "",
                                   min = 0, max = 100, value = c(25, 75))),
                
                column(3, 
                       textInput("text", h3("Text input"), 
                                 value = "Enter text..."))   
              )),
      
      #second tab(practice)----------------------------
      tabItem(tabName = "practice",
              fluidRow(box(width = 6,
                           selectInput("species",
                                       label = "Choose Species type in Data iris",
                                       choices = c("None",as.character(unique(iris$Species))),
                                       selected = "None")),
                       box(width = 6,
                           textOutput("selected_species")))
      ),
      
      #third tab(database)--------------------------
      tabItem(tabName = "Database",
              fluidRow(column(6,
                              fileInput("data","Load data(Only csv!!)",
                                        multiple = F,
                                        accept = c('.csv')))),
              fluidRow(box(width = 12,DT::dataTableOutput("iris_df"),
                           style="height:400px; overflow-y: scroll; overflow-x: scroll;"))),
      
      #fourth tab(EDA)--------------------------------------
      tabItem(tabName = "EDA",
              fluidRow(box(width = 4,
                           uiOutput("xvar"),
                           uiOutput("yvar"),
                           uiOutput("species_chk"),
                           
                           actionButton(inputId="draw",
                                        label = "draw plot")
              ),
              box(width = 8,
                  plotOutput("plot_iris")))),
      
      #fifth tab(modeling)---------------------------------
      
      tabItem(tabName="Modeling",
              fluidRow(box(width = 12,solidHeader = T, status ="warning", title="회귀 모델식",
                           h3("회귀식"))),
              fluidRow(box(width=6,
                           title="입력값",
                           status="success",
                           numericInput("Petal.Length",
                                        "Insert the Petal.Length of iris",
                                        value = 3.5),
                           numericInput("Petal.Width"))),
              fluidRow(box(width=6,
                           title="예측된 Species 값",
                           status="success",
                           solidHeader = T,
                           h3("It"),
                           h4(paste()))))
      
      
      
    )
  )
)



