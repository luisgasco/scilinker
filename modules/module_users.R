

createUserUI <- function(id)
{
    ns <- NS(id)
    tagList(
        title = "Create new user",
        h1("New user data:"),
        # Input para el nombre de usuario
        fluidRow(
            column(4, "User name"),
            column(8, textInput(ns("new_user_name"), label = NULL, value = ""))
        ),
        fluidRow(
            column(4, "Password"),
            column(8, passwordInput(ns("new_password"), label = NULL, value = ""))
        ),
        # Input para el email
        fluidRow(
            column(4, "Email:"),
            column(8, textInput(ns("new_email"), label = NULL, value = ""))
        ),
        # Input para el rol
        fluidRow(
            column(4, "Role:"),
            column(8, selectInput(ns("new_role"), label = NULL, choices = c("annotator", "admin"), selected = "annotator"))
        ),
        actionButton(ns("add_user"), "Add new user")
    )
}

createUser <- function(input, output, session,con){
    ns <- session$ns
    observeEvent(input$add_user, {
        # Generate fields to save user
        query <- toJSON(list(
            user_name = input[["new_user_name"]],
            password = input[["new_password"]],
            email = input[["new_email"]],
            role = input[["new_role"]],
            projects = list()
        ), auto_unbox  = TRUE)
        # Creamos entrada en la user collection
        con$insert(query)
        # print("GUARDA USER???")
        showNotification("User successfully created", type = "default")
        removeModal()
        shinyjs::reset("add_user")
        
    },ignoreInit=TRUE, ignoreNULL = TRUE, once = TRUE )
}


userConfigUI <- function(id)
{
    ns <- NS(id)
    
    box(
    fluidRow(
        column(
            12, 
                height = 500, shinycssloaders::withSpinner(DT::DTOutput(ns('table_users'),height = "400px"))
        )
    ),
    fluidRow(
        column(
            12, 
            offset = 0, 
            align = "center",
            actionButton(ns("create_user"), "Create a new user",icon = icon("user"),width = "100%")
        )
    )
    )
}



userConfig<- function(input, output, session, con)
{
    ns <- session$ns
    
    output$project_stats <- renderUI({
        column(12,
               fluidRow(
                   h4("General"),
                   # valueBoxOutput(ns("documents")),
                   # valueBoxOutput(ns("current_annotations")),
                   # downloadButton(ns("download_annotations"), "Download annotations tsv")
               ),
               hr(),
               fluidRow(
                   h4("User stats"),
                   uiOutput(ns("user_stats"))
               )
        )
    })

    users_of_project <- reactive({
        users <- con$find()
        # Verifica que los elementos de la columna "projects" sean dataframes
        users$projects <- lapply(users$projects, as.data.frame)
        
        # Utiliza la función mutate para crear la nueva columna "projects_list"
        users <- users %>%
            mutate(projects_list = lapply(projects, function(df) df$project))
        
        users
    })
    
    output$table_users = DT::renderDataTable({  
        # print(users_of_project())
        DT::datatable(users_of_project(),
                      rownames=FALSE,
                      extensions = c('Responsive',"Scroller"),
                      options = list(
                          deferRender = TRUE,
                          scrollY = 300,
                          scrollX=FALSE,
                          scroller = TRUE,
                          autoWidth = TRUE,
                          columnDefs = list(list(visible=FALSE, targets=c( 4)))
                          )
                    )
        },server = TRUE
    )

    
    observeEvent(input$create_user, {
        showModal(modalDialog(id = "create_user_modal", createUserUI(ns("createUser"))))
        callModule(createUser, "createUser",con=con)
        
        shinyjs::reset("create_user")
    },ignoreInit = TRUE, ignoreNULL = TRUE, once=TRUE )
}
