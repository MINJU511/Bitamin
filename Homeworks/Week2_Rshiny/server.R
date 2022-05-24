library(ggplot2); library(dplyr)

server <- function(input, output) {
  
  #second tab(practice)----------------
  output$selected_species <- renderText({ 
    paste("You have selected", input$species, "species from iris Data")
  })
  
  #third tab(database)----------------
  df <- reactive({
    if(is.null(input$data)){
      read.csv("iris.csv")
    } else
    read.csv(input$data$datapath)
    })
  
  output$iris_df <- renderDataTable({
    df()
  })
  
  #fourth tab(EDA)-------------------
  output$xvar <- renderUI({
    selectInput('xvar','X axis',
                choices = colnames(df()),
                selected = colnames(df())[1])
  })
  output$yvar <- renderUI({
    selectInput('yvar','Y axis',choices = colnames(df()),
                selected = colnames(df())[2])
  })
  
  output$species_chk <- renderUI({
    checkboxGroupInput(inputId = "species_chk",
                       label = "Choose Species you want to see..",
                       choices = as.character(unique(df()$Species)))
  })
  
  plot <- eventReactive(input$draw,{
    
      ggplot(df() %>% filter(Species == input$species_chk),
             aes(!!sym(input$xvar),!!sym(input$yvar),
                      group=Species, col=Species))+
        geom_point()+
        theme_bw()
  })
  
  output$plot_iris <- renderPlot({
    plot()
  })
  
  #fifth tab(Modeling)----------------------------
  
  # traning_set, test_set
  log_model <- reactive({
    
    set.seed(0921)
    train_idx <- sample(1:nrow(df()), nrow(df()) * 0.7 )
    test_idx <- setdiff(1:nrow(df()),train_idx)
    train <- df()[train_idx,]
    
    multi_logit_m <- multinom(Species ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width, data = train)
    multi_logit_m
    
    })
  output$pred_species <- renderText(({
    multi_logit_p <- predict(log_model(),
                             newdata=data.frame(Petal.Length = input$Petal.Length
                                                Petal.),
                             type = "prob")
    paste(round(max(multi_logit_p))*100,"%",as.character(names(which.max(multi_logit_p))))
  }))
  
  
  
}





