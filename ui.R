
header <- dashboardHeader(title =  span(img(src="scilinker_logo.svg", width = 110)),
                          titleWidth = 188,
                          tags$li(a(href = 'https://bsc.es/',
                                    img(src = 'bsc_logo.png',
                                        title = "BSC logo", height = "30px"),
                                    style = "padding-top:10px; padding-bottom:10px;"),
                                  class = "dropdown")
                          )


  
                        
sidebar <- dashboardSidebar(
    sidebarMenu(id = "tabs",
                column(12,
                    uiOutput("hello_user"),
                    fluidRow(
                    # Botón para abrir el modal de inicio de sesión,  # Botón para abrir el modal de inicio de sesión
                    actionButton("openLoginModal", "Login", icon("log-in", lib="glyphicon"), width="88%")),
                    menuItemModuleUI("menu"),
                    verbatimTextOutput("count")
                    )
                )
        )

body <- dashboardBody(
              shinyjs::useShinyjs(),
              tags$head(
                  # tags$script(inactivity),
                  tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css"),
                  tags$link(rel = "scilinker icon", href = "small_logo.ico"),
                  tags$title(span("Scilinker app"))
              ),
              includeCSS("www/mystyle.css"),
              fluidRow(
                column(
                    width = 12,
                    h3(textOutput("welcomeMessage")),
                    
                )
              ),
              fluidRow(
                  htmlOutput("output_test"),
              )
)


ui <- dashboardPage(header, sidebar, body,skin = "black")


# tags$script(inactivity),
 



