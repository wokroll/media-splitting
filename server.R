library(shiny)
library(readxl)
library(ggplot2)
library(shinyjs)
library(pracma)
library(plotly)
source("helpers.R", encoding = "utf-8")
source("grad_desc.R", encoding = "utf-8")

shinyServer(function(input, output) {

    #зчитування login i password з бази даних
    user_base <- readRDS("user_base.rds")
    credentials <- callModule(shinyauthr::login, 
                              id = "login", 
                              data = user_base,
                              user_col = user,
                              pwd_col = password,
                              sodium_hashed = TRUE,
                              log_out = reactive(NULL)
    )
    
    path <- "Audiences Data New.xlsx"
    
    observe({
        req(credentials()$user_auth)

        output$myControls <- renderUI({
            tagList(
                numericInput("budget",label = "Budget", value = 2000000,
                             min = 10000,max = 3000000, step = 1000),
                radioButtons("type_audience", label = h3("Select"),
                             choices = list("Download your own audience" = 1, "Select prepared audience" = 2),selected = 2))
            })
        
        output$runCalculations <- renderUI({
            tagList(
                actionButton("run_long", "Run bust"),
                actionButton("run_desc","Run gradient descent")  
            )
        })
        
        output$prepared_audience <- renderUI({
            selectInput("audience", label = h3("Select audience"),
                        choices = c(excel_sheets(path)), selected = 1)
        })
        
        output$download_audience <- renderUI({
            fileInput("my_data", label = h3("Завантажте ввідний файл"))
        })
        
        output$all_output <- renderUI({
            tagList(
                textOutput("reach"),
                textOutput("start_text"),
                textOutput("help_text"),
                tableOutput("results_percents"),
                textOutput("help_text2"),
                tableOutput("results_abs"),
                textOutput("time")
            )
        })
    })

    
    my_df <- reactive({
        req(credentials()$user_auth)
        req(input$type_audience)
        
        if(input$type_audience == 2){
            read_excel(path, sheet = input$audience)
        }else{
            req(input$my_data)
            read_excel(input$my_data$datapath, sheet = 1)
        }
    })
    
    cpp = reactive({
        as.numeric(c(unlist(my_df()[1,c(7:11)]))) 
    })
        

        #if (max(df[,2])>=1){
        #    df[,2:6] <- df[,2:6]/100
        #}


    #  events for Busting
    observeEvent(
        input$run_long, {
                        showModal(modalDialog("Running calculations with busting method", footer=NULL))
                        output$reach <- renderText(paste("Maximal reach is: ",as.character(round(all_calculations(my_df(), as.numeric(results), input$budget, cpp())*100, 2)), "%"))
                        start <- Sys.time()
                        results <- optimal_split(plot_data(df = my_df(), budget = input$budget, cpp = cpp()))
                        end <- Sys.time()
                        output$help_text <- renderText("Share of each media budget in percents:")
                        output$results_percents <- renderTable(results*100)
                        output$help_text2 <- renderText("Amount of budget for each media:")
                        output$results_abs <- renderTable(round(input$budget*results, 0 ))
                        output$time <- renderText(paste("Calculations took: ", as.character(round(end-start, 2)), " seconds"))
                        removeModal()
                        }
    )
    
    # events for gradient descent
    observeEvent(
        input$run_desc, {
                        showModal(modalDialog("Running calculations with gradient descent method", footer=NULL))
                        output$reach <- renderText(paste("Maximal reach is: ",as.character(round(all_calculations(my_df(), as.numeric(results), input$budget, cpp())*100, 2)), "%"))
                        start <- Sys.time()
                        results <- optimize(df = my_df(), budget = input$budget, cpp = cpp())
                        end <- Sys.time()
                        output$help_text <- renderText("Share of each media budget in percents:")
                        output$results_percents <- renderTable(results*100)
                        output$help_text2 <- renderText("Amount of budget for each media:")
                        output$results_abs <- renderTable(round(input$budget*results , 0))
                        output$time <- renderText(paste("Calculations took: ", as.character(round(end-start, 2)), " seconds"))
                        removeModal()
                        }
    )
    

})
