
# createUser module  ----
# Module to manage the creation of new users
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
        actionButton(ns("add_user"), "Add new user"),
        uiOutput(ns("status_existing_user_name"))
    )
}
createUser <- function(input, output, session,con)
{
    ns <- session$ns
    # Observe Event to listen when the add_user button is created
    observeEvent(input$add_user, {
        # Check if user_name does not exist in database to save it
        if (!(input[["new_user_name"]] %in%  con$find()$user_name)){
            # Generate the json structure to include in the MongoDB
            query <- toJSON(list(
                user_name = input[["new_user_name"]],
                password = input[["new_password"]],
                email = input[["new_email"]],
                role = input[["new_role"]],
                projects = list()
            ), auto_unbox  = TRUE)
            # Insert new user data to user collection
            con$insert(query)
            showNotification("User successfully created", type = "default")
            # Close modal
            removeModal()
            shinyjs::reset("add_user")
            
            
        } else {
            output$status_existing_user_name <- renderUI({
                HTML(paste("<p style='color:red;'>",
                           "The selected user_name already exists in the database.",
                           "</p>"))
            })
            showNotification("User don't created", type = "error")
        }
        
        
        
    },ignoreInit=TRUE, ignoreNULL = TRUE)
}

# userConfig module  ----
# Module to show the UI page of Users tab
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
            offset=0,
            align = "center", 
            uiOutput(ns("delete_user_ui"))
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
    user2delete <- reactiveVal("")
    
    # Reactive values representing users of the platform
    users_of_project <- reactive({
        users <- con$find()
        # Verifica que los elementos de la columna "projects" sean dataframes
        users$projects <- lapply(users$projects, as.data.frame)
        
        # Utiliza la función mutate para crear la nueva columna "projects_list"
        users <- users %>%
            mutate(projects_list = lapply(projects, function(df) df$project))
        
        users
    })
    
    # Show users in data table
    output$table_users = DT::renderDataTable({  
        # print(users_of_project())
        DT::datatable(users_of_project(),
                      rownames=FALSE,
                      extensions = c('Responsive',"Scroller"),
                      selection = "single",
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
    
    # ADD USER observe event
    # Input USER
    
    observeEvent(input$create_user, {
        showModal(modalDialog(id = "create_user_modal", createUserUI(ns("createUser"))))
        callModule(createUser, "createUser",con=con)
        shinyjs::reset("create_user")
    },ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # Render selected user delete button
    output$delete_user_ui <- renderUI({
        selected_row <- input$table_users_rows_selected
        if (length(selected_row) == 0) {
            return(NULL)
        } else {
            user2delete(users_of_project()[selected_row, ]$user_name)
            actionButton(ns("delete_user"), paste0("Delete ",user2delete() ), icon = icon("x"), width="100%") 
        }
        
    })
    # If delete_user button is clicked, show modal
    observeEvent(input$delete_user, {
        showModal(
            modalDialog(
                id = "confirmation_modal",
                title = "Confirm",
                tagList(
                    "This is a destructive action and could break some of the active annotation projects.",
                    "If you want to continue, type the name of the user in the text field and press 'Delete' ",
                    textInput(ns("confirm_user_name"),label = "",value = ""),
                    uiOutput(ns("status_confirm_user_name"))
                ),
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton(ns("confirm_delete"), "Delete", class = "btn-danger",style = "color:white")
                )
                
            )
        )
    },ignoreInit=TRUE, ignoreNULL = TRUE)
    # IF confirm_delete is clicked, verify the input text field and delete gazzeteer from db.
    observeEvent(input$confirm_delete, {
        # If written user name is the same name than user selected
        if (input$confirm_user_name == user2delete()){
            # Delete user entry from con
            con$remove(paste0('{"user_name": "',input$confirm_user_name, '"}'))
            removeModal()
            # Show info. modal
            showModal(modalDialog(id = "user_deleted", 
                                  HTML("User successfully deleted\n Reload the page to update the menu.") , 
                                  size="s")
            )
        } else {
            output$status_confirm_user_name <- renderUI({
                HTML(paste("<p style='color:red;'>",
                           "The written text does not match the name of the user",
                           "</p>"))
            })
        }
    },ignoreInit=TRUE, ignoreNULL = TRUE)
    
    
    
    
}
