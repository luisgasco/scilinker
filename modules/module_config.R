library(shinyjs)
library(DT)
library(mongolite)
library(jsonlite)
library(dplyr)
library(shinyWidgets)
library(fontawesome)
shinyjs::useShinyjs()


# Aux. function to validate old password and new password
validate_password <- function(old_password, new_password, db_password){
    if (old_password == db_password) {
        return(TRUE)
    } else {
        return(FALSE)
    }
    
}

# changePass module  ----
# Module to manage the change of user password
changePassUI  <- function(id)
{
    ns <- NS(id)
    tagList(
            title = "Update password",
            passwordInput(ns("old_password"), "Current password:"),
            passwordInput(ns("new_password"), "New password:"),
            actionButton(ns("btn_save_password"), "Save")
    )    
}
changePass <- function(input, output, session,con)
{
    ns <- session$ns
    # If btn_save_password is clicked
    observeEvent(input$btn_save_password, {
        # Show UI data and validate current passowrd
        valor_entrada = validate_password(input$old_password, input$new_password, session$userData$data$password)
        if (valor_entrada) {
            # If the old password are correct
            # Query the user data in the database
            query_find <- list(user_name = session$userData$data$user)
            json_query_find <- toJSON(query_find, auto_unbox = TRUE)
            # Upload the password field
            query_set <- list("$set" = list("password" = input$new_password))
            json_query_set <- toJSON(query_set, auto_unbox = TRUE)
            con$update(json_query_find, json_query_set)
            # Update the session data
            session$userData$data$password = input$new_password
            showNotification("Password successfully updated.", type = "default")
            # Logs for tracing errors
            # print("Contraseña guardada")
            # print(input$btn_save_password)
            shinyjs::reset("btn_save_password")
            
        }
        else {
            showNotification(
                "Incorrect current password",
                type = "warning"
            )
        }
    },ignoreInit=TRUE, ignoreNULL = TRUE, once = TRUE )
}

# logoutModule module  ----
# Module to manage the logout process.
generalConfigInterfaceUI <- function(id)
{
    ns <- NS(id)
    fluidRow(
        column(12,
        tabBox( 
            title="",
            id = "config_tab",
            height = "620",
            tabPanel("User config", 
                     uiOutput(ns("config_tab"))
            ),
            tabPanel("General user stats", 
                    div(
                         tags$p("Here you have some statistical information about the user's projects"),
                         column(6,
                                div(
                                    tags$p("Total user projects:"),
                                    valueBoxOutput(ns("projects"), width = 12)
                                )
                         ),
                         column(6,
                                div(
                                    tags$p("Total user documents:"),
                                    valueBoxOutput(ns("documents"), width = 12)
                                )
                         )
                     )
            )
        )
    )
    )

}
generalConfigInterface <- function(input, output, session, con)
{
    ns <- session$ns
    # Generate content for ValueBox with number of projects associated to the user
    output$projects <- renderValueBox({
        valueBox(
            length(session$userData$data$projects[[1]]$project), "Projects", icon = icon("folder"),
            color = "purple"
        )
    })
    # Generate content for ValueBox with number of documents associated to the user
    output$documents <- renderValueBox({
        valueBox(
            sum(lengths(session$userData$data$projects[[1]]$documents)), "Documents", icon = icon("file"),
            color = "olive"
        )
    })
    # Generate content for ValueBox with number of gazetteers associated to the user
    output$gazetteers <- renderValueBox({
        valueBox(
            sum(lengths(session$userData$terminologies)), "Gazetteers", icon = icon("list"),
            color = "blue"
        )
    })
    # Generate UI to show user data and password change button
    output$config_tab <- renderUI({
        div(
            # Encabezado
            h1("User data:"),
            # Input para el nombre de usuario
            fluidRow(
                column(4, "User name"),
                column(8, disabled(textInput("user_name", label = NULL, value = session$userData$data$user_name)))
            ),
            # Input para la contraseña
            fluidRow(
                column(4, "Password:"),
                column(8, actionButton(ns("btn_update_password"), "Update password"))
            ),
            # Input para el email
            fluidRow(
                column(4, "Email:"),
                column(8, disabled(textInput("email", label = NULL, value = session$userData$data$email)))
            ),
            # Input para el rol
            fluidRow(
                column(4, "Role:"),
                column(8, disabled(selectInput("role", label = NULL, choices = c("annotator", "admin"), selected = session$userData$data$role)))
            )
        )
    })

    
    # Observe Event to generate the modal with fields to change the user password
    observeEvent(input$btn_update_password, {
        showModal(modalDialog(id = "updateModal", changePassUI(ns("changePass"))))
        callModule(changePass, "changePass", con = con)
        shinyjs::reset("btn_update_password")
    },ignoreInit = TRUE, ignoreNULL = TRUE, once=TRUE )
}
  




