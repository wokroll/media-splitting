library(shiny)
library(shinyauthr)

shinyUI(fluidPage(

    titlePanel("Split optimizer"),
    sidebarLayout(
        sidebarPanel(
            
            shinyjs::useShinyjs(), 

            uiOutput("myControls"),
            conditionalPanel("input.type_audience == 1", uiOutput("download_audience")),
            conditionalPanel("input.type_audience == 2", uiOutput("prepared_audience")),
            uiOutput("runCalculations")
        ),

        mainPanel(
        
        # add login panel UI function
        loginUI(id = "login",title = "Введіть логін і пароль, будь ласка",user_title = "Логін",pass_title ="Пароль", 
                    login_title = "Увійти",error_message = "Невірний логін або пароль"),
        uiOutput("all_output")
            
        )
    )
    )
)
