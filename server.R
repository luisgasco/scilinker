

# Definir el servidor de la aplicación Shiny
vals <- reactiveValues(count=0)
server <- function(input, output, session) {
    # Reactive variables
    userState <- reactiveValues(user = NULL, loggedIn = FALSE, 
                                role = NULL, data = NULL, 
                                projects = NULL)
    
    # Variable reactiva para controlar la visibilidad del módulo
    showModule <- reactiveVal(TRUE)
    
    # Login observers
    ## Update login button
    observeEvent(userState$loggedIn, {
        if (userState$loggedIn)
        {
            # If the user is logged in, update the label of the login button to "Logout"
            updateActionButton(session, "openLoginModal", label = "Logout")
            # Call the 'menuItemModule' module to update the menu items accordingly
            callModule(menuItemModule, "menu")
            
        } else if (!userState$loggedIn)
        {
            # If the user is not logged in, update the label of the login button to "Login"
            updateActionButton(session, "openLoginModal", label = "Login")
            # Call the 'menuItemModule' module to update the menu items accordingly
            callModule(menuItemModule, "menu")
        }
    })
    
    ## Login modeal dialog opening when the user clicks on the login button
    observeEvent(input$openLoginModal, {
        if (!userState$loggedIn)
        {
            # If the user is not logged in, show the login modal dialog
            showModal(modalDialog(id = "loginModal", loginModuleUI("loginModule")))
            # Call the 'loginModule' module to handle the login process
            callModule(loginModule, "loginModule", con = con, userState = userState)
        } else
        {
            # If the user is logged in, show the logout modal dialog
            showModal(modalDialog(id = "loginModal", logoutModuleUI("logoutModule")))
            # Call the 'logoutModule' module to handle the logout process
            callModule(logoutModule, "logoutModule", con = con, userState = userState)
        }
    })
    
    # Simple renders
    ## Render a UI element displaying a welcome message to the user
    output$hello_user <- renderUI({
        if (!userState$loggedIn) {
            # Return an empty string if the user is not logged in
            return("")  
        } else {
            # Display a welcome message with the user's name and role
            return(div(HTML(paste0("Welcome <b>", userState$user, " </b> <br>", "Your role is ", userState$role))))
        }
    })
    ## Render a text message for the welcome message in the dashboard
    output$welcomeMessage <- renderText({
        if (!userState$loggedIn) {
            return("Please log in to access the application.")
        } else if (userState$loggedIn && input$tabs == "") {
            return(paste("Bienvenido,", userState$user, "\n","Tu rol es de ", userState$role))  # Mostrar el nombre de usuario en el dashboard
        } else {
            return()  # Mostrar el nombre de usuario en el dashboard
        }
    })
    
    # Create connections with MongoDB
    con <- mongo(collection = mongo_user_collection,
                 db = mongo_database,
                 url = paste0("mongodb://",mongo_host,":",mongo_port),
                 options = ssl_options(weak_cert_validation = TRUE)) 
    con_documents <- mongo(collection = mongo_documents_collection,
                           db = mongo_database,
                           url = paste0("mongodb://",mongo_host,":",mongo_port),
                           options = ssl_options(weak_cert_validation = TRUE)) 
    con_mentions <- mongo(collection = mongo_mentions_collection,
                          db = mongo_database,
                          url = paste0("mongodb://",mongo_host,":",mongo_port),
                          options = ssl_options(weak_cert_validation = TRUE)) 
    con_annotations <- mongo(collection = mongo_annotations_collection,
                             db = mongo_database,
                             url = paste0("mongodb://",mongo_host,":",mongo_port),
                             options = ssl_options(weak_cert_validation = TRUE)) 
    
    # Count number of sessions
    isolate(vals$count <- vals$count + 1)
    
    # When a session ends, decrement the counter.
    session$onSessionEnded(function(){
        isolate(vals$count <- vals$count - 1)
    })
    output$count <- renderText({
        paste0("Number of sessions: ", vals$count)
    })
    
    # Observer to update thes pages when clicking options on sidebar
    observeEvent(input$tabs, {
        if (input$tabs %in% session$userData$projects) {
            # If user is annotator enter annotator interface
            if (userState$role == "annotator"){
                # Render annotator interface module
                output$output_test <- renderUI({
                    generalAnnotatorInterfaceUI("x")
                })
                callModule(generalAnnotatorInterface, "x") 
                
                # mongo user
                query_mongo_user <- con$find(query = paste("{\"user_name\": \"",
                                                           userState$user, "\"}", 
                                                           sep = ""))
                # Update session data with user information
                session$userData$user <- userState$user
                session$userData$loggedIn <- TRUE
                session$userData$role <- userState$role
                session$userData$projects <- session$userData$projects
                session$userData$current_project <- input$tabs
                session$userData$annotation_db_endpoint <- con_annotations
                session$userData$mentions_db_endpoint <- con_mentions
                session$userData$documents_db_endpoint <- con_documents
                session$userData$data <- query_mongo_user
                
                # ObserveEvent for full_text panel in annoator interface
                observeEvent(input[["x-pannel_output-full_text"]], {
                    if (input[["x-pannel_output-full_text"]] == TRUE)
                    {
                        shinyjs::show("x-texto_output-texto_output")
                    } else
                    {
                        shinyjs::hide("x-texto_output-texto_output")
                    }
                })
            } else if (userState$role == "admin"){
                # Render validation interface module
                output$output_test <- renderUI({
                    generalValidationInterfaceUI("y")
                })
                callModule(generalValidationInterface, "y",con)
                
                # mongo user
                query_mongo_user <- con$find(query = paste("{\"user_name\": \"",
                                                           userState$user, "\"}", 
                                                           sep = ""))
                # Update session data with user information
                session$userData$user <- userState$user
                session$userData$loggedIn <- TRUE
                session$userData$role <- userState$role
                session$userData$projects <- session$userData$projects
                session$userData$current_project <- input$tabs
                session$userData$annotation_db_endpoint <- con_annotations
                session$userData$mentions_db_endpoint <- con_mentions
                session$userData$documents_db_endpoint <- con_documents
                session$userData$data <- query_mongo_user
            }
            
            
        } else if (input$tabs == "config")
        {
            output$output_test <- renderUI({
                generalConfigInterfaceUI("y")
            })
            callModule(generalConfigInterface, "y", con=con) #
            
        } else if (input$tabs == "about")
        {   
            
            output$output_test <- renderUI({
                print("otrooos")
            })
            print("about")
        } else if (input$tabs == "users")
        {
            output$output_test <- renderUI({
                userConfigUI("w")
            })
            callModule(userConfig, "w", con)
        } else
        {
            output$output_test <- renderUI({
                print("otrooos")
            })
        }
    }, ignoreInit = TRUE)

    
}


