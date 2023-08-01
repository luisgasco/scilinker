library(shinyjs)
library(DT)
library(mongolite)
library(jsonlite)
library(dplyr)
library(shinyWidgets)
library(fontawesome)

shinyjs::useShinyjs()

validate_password <- function(old_password, new_password, db_password){
    if (old_password == db_password) {
        return(TRUE)
    } else {
        return(FALSE)
    }
    
}

changePassUI  <- function(id){
    ns <- NS(id)
    tagList(
            title = "Update password",
            passwordInput(ns("old_password"), "Current password:"),
            passwordInput(ns("new_password"), "New password:"),
            actionButton(ns("btn_save_password"), "Save")
    )    
}

changePass <- function(input, output, session,con) {
    ns <- session$ns
    
    observeEvent(input$btn_save_password, {
        valor_entrada = validate_password(input$old_password, input$new_password, session$userData$data$password)
        if (valor_entrada) {
            # Si el usuario y la contraseña son correctos, cerrar el modal y mostrar un mensaje de bienvenida
            # removeModal()  # Cerrar el modal
            query_find <- list(user_name = session$userData$data$user)
            json_query_find <- toJSON(query_find, auto_unbox = TRUE)
            # Construye la consulta como un diccionario en R
            query_set <- list("$set" = list("password" = input$new_password))
            json_query_set <- toJSON(query_set, auto_unbox = TRUE)
            # Actualizamos database y session data
            con$update(json_query_find, json_query_set)
            session$userData$data$password = input$new_password
            print("GUARDA???")
            showNotification("Password successfully updated.", type = "default")
            print(input$btn_save_password)
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

generalConfigInterfaceUI <- function(id)
{
    ns <- NS(id)
    fluidRow(
        column(12,
        tabBox( 
            title="",
            id = "config_tab",
            height = "620",
            tabPanel("User stats", 
                    div(
                     valueBoxOutput(ns("projects")),
                     valueBoxOutput(ns("documents"))
                    )
                    ),
            tabPanel("User config", 
                     uiOutput(ns("config_tab"))
                     )
        )
    )
    )

}
generalConfigInterface <- function(input, output, session, con)
{
    ns <- session$ns

    output$projects <- renderValueBox({
        valueBox(
            length(session$userData$data$projects[[1]]$project), "Projects", icon = icon("folder"),
            color = "purple"
        )
    })
    output$documents <- renderValueBox({
        valueBox(
            sum(length(session$userData$data$projects[[1]]$documents)), "documents", icon = icon("file"),
            color = "olive"
        )
    })
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
    
    observeEvent(input$btn_update_password, {
        showModal(modalDialog(id = "updateModal", changePassUI(ns("changePass"))))
        callModule(changePass, "changePass", con = con)
        shinyjs::reset("btn_update_password")
    },ignoreInit = TRUE, ignoreNULL = TRUE, once=TRUE )
    
}
  




