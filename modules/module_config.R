library(shinyjs)
library(DT)
library(mongolite)
library(jsonlite)
library(dplyr)
library(shinyWidgets)
library(fontawesome)

shinyjs::useShinyjs()


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
generalConfigInterface <- function(input, output, session,datos_reactive)
{
    ns <- session$ns
    
    output$projects <- renderValueBox({
        valueBox(
            length(session$userData$data$projects[[1]]$project), "Projects", icon = icon("list"),
            color = "purple"
        )
    })
    output$documents <- renderValueBox({
        valueBox(
            length( sum(lengths(session$userData$data$projects[[1]]$documents))), "documents", icon = icon("list"),
            color = "purple"
        )
    })
    output$config_tab <- renderUI({
        div(
            # Encabezado
            h1("Configuración de Usuario"),
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
        showModal(
            modalDialog(
                title = "Actualizar contraseña",
                passwordInput(ns("old_password"), "Contraseña antigua:"),
                passwordInput(ns("new_password"), "Nueva contraseña:", value = ""),
                actionButton(ns("btn_save_password"), "Guardar"),
                easyClose = TRUE
            )
        )
    })
    # Logic to update password
    observeEvent(input$btn_save_password, {
        old_password <- isolate(input$old_password)
        new_password <- isolate(input$new_password)
        # user <- user_data()
        
        # Verificar si la contraseña antigua ingresada coincide con la almacenada
        if (old_password == session$userData$data$password) {
            # Actualizar la contraseña en la base de datos (esto es un ejemplo, reemplaza con tu lógica real)
            # Suponemos que 'user_id' es el identificador del usuario en tu colección MongoDB
            # user_id <- "your_user_id"
            # mongo_collection$update(user_id, list("$set" = list(password = new_password)))
            
            # Actualizar el valor de la contraseña en la sesión
            # user$password <- new_password
            # user_data(user)
            
            # Cerrar el modal después de actualizar
            # removeModal()
            print(new_password)
            # Mostrar mensaje de éxito
            showNotification("Contraseña actualizada con éxito.", type = "default")
        } else {
            # Mostrar mensaje de error si la contraseña antigua no coincide
            showNotification("Contraseña antigua incorrecta. Inténtalo nuevamente.", type = "error")
        }
    })

}
  