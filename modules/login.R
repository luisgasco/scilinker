# LOGIN_LOGOUT_MODuLES
# loginModule module  ----
# Module to display the Login form to the application when the login button is pressed.
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
loginModule <- function(input, output, session, con, con_terminologies,con_projects, userState) {
    ns <- session$ns
    # Observe event to access app 
    observeEvent(input$login, {
        # If user data is correct
        if (authenticate_user(input$username, input$password, con)) {
            # Close modal and show a welcome modal
            removeModal()  # Cerrar el modal
            showModal(
                modalDialog(
                    title = "Welcome to Scilinker",
                    paste0("Welcome, ", input$username), 
                    easyClose = TRUE
                )
            )
            # Update userState reactive values and session data
            query_mongo_user <- con$find(query = paste('{"user_name": "', input$username, '"}', sep = ''))
            userState$user <- input$username  # Guardar el nombre de usuario en el estado del usuario
            userState$loggedIn <- TRUE
            userState$data <- query_mongo_user
            userState$role <- query_mongo_user$role
            # print(query_mongo_user$role)
            # print(query_mongo_user$projects)
            # asdasd<<- con_projects
            # print(ifelse( query_mongo_user$role=="admin",con_projects$find()$name, query_mongo_user$projects[[1]]$project))
            userState$projects <- query_mongo_user$projects[[1]]$project
            userState$terminologies  <- con_terminologies$find()$name
            # Save to session user_data
            session$userData$user <- input$username
            session$userData$loggedIn <- TRUE  
            session$userData$data <- userState$data
            session$userData$role <- userState$role
            session$userData$projects <- userState$projects
            session$userData$terminologies <-  userState$terminologies
        } else {
            # If user data is incorrect, show a notification
            showNotification(
                "Incorrect username or password. Please try again.",
                type = "warning"
            )
            
        }
        
    }, ignoreInit = TRUE)  # Ignore event activation during module initialization
}

# logoutModule module  ----
# Module to manage the logout process.
logoutModuleUI <- function(id) {
    ns <- NS(id)
    tagList(
        box(
            title = "You are going to log out",
            actionButton(ns("login"), "Logout")
        )
    )
}
logoutModule <- function(input, output, session, con, userState) {
    ns <- session$ns
    # If logout (login) button is clicked. Erase the userState data.
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
        userState$terminologies <- NULL
        # Save to session user_data
        session$userData$user <- input$username
        session$userData$loggedIn <- FALSE  
        session$userData$data <- userState$data
        session$userData$role <- userState$role
        session$userData$projects <- userState$projects
        session$userData$terminologies <-  userState$terminologies
        
    }, ignoreInit = TRUE)  # Ignore event activation during module initialization
}