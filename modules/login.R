# LOGIN_LOGOUT_MODuLES
# Módulo para el formulario de inicio de sesión
loginModuleUI <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = "Inicio de sesión",
            textInput(ns("username"), "User"),
            passwordInput(ns("password"), "Password"),
            actionButton(ns("login"), "Login")
        )
    )
}

logoutModuleUI <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = "You are going to log out",
            actionButton(ns("login"), "Logout")
        )
    )
}

loginModule <- function(input, output, session, con, userState) {
    ns <- session$ns
    
    observeEvent(input$login, {
        # MIRO EL ESTADO DE userSTATE# SI ES FALSE ENTRO A AUTENTICAR. SI NO CAMBIO.
        if (authenticate_user(input$username, input$password, con)) {
            # Si el usuario y la contraseña son correctos, cerrar el modal y mostrar un mensaje de bienvenida
            removeModal()  # Cerrar el modal
            showModal(
                modalDialog(
                    title = "Bienvenido",
                    paste0("Bienvenido, ", input$username),  # Mostrar el nombre de usuario
                    easyClose = TRUE
                )
            )
            
            query_mongo_user <- con$find(query = paste('{"user_name": "', input$username, '"}', sep = ''))
            userState$user <- input$username  # Guardar el nombre de usuario en el estado del usuario
            userState$loggedIn <- TRUE
            userState$data <- query_mongo_user
            userState$role <- query_mongo_user$role
            userState$projects <- query_mongo_user$projects[[1]]$project
            # Save to session user_data
            session$userData$user <- input$username
            session$userData$loggedIn <- TRUE  
            session$userData$data <- userState$data
            session$userData$role <- userState$role
            session$userData$projects <- userState$projects
            
        } else {
            # Si el usuario y la contraseña son incorrectos, mostrar un mensaje de error
            showNotification(
                "Usuario o contraseña incorrectos. Por favor, inténtalo de nuevo.",
                type = "warning"
            )
            
        }
        
    }, ignoreInit = TRUE)  # Ignorar la activación del evento durante la inicialización del módulo
}

logoutModule <- function(input, output, session, con, userState) {
    ns <- session$ns
    
    observeEvent(input$login, {
        showModal(
            modalDialog(
                title = "Goodbye",
                paste0("Goodbye, ", input$username),  # Mostrar el nombre de usuario
                easyClose = TRUE
            )
        )
        
        userState$user <- NULL
        userState$loggedIn <- FALSE
        userState$data <- NULL
        userState$role <- NULL
        userState$projects <- NULL
        # Save to session user_data
        session$userData$user <- input$username
        session$userData$loggedIn <- FALSE  
        session$userData$data <- userState$data
        session$userData$role <- userState$role
        session$userData$projects <- userState$projects
        
    }, ignoreInit = TRUE)  # Ignorar la activación del evento durante la inicialización del módulo
}