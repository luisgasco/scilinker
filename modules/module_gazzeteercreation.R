
# generalValidationInterface module  ----
# Module to show stats and validate mentions of users of an specific project
generalGazzeteerCreationInterfaceUI <- function(id, con)
{
    ns <- NS(id)
    users_of_app <- con$find()$user_name
    users_of_app <- users_of_app[users_of_app!="admin"]
    # TODO: READ GAZZETEERS FROM DATABASE
    fluidRow(
        box(width=6,
            title = h2("Upload new terminology"),
            tagList(
                title = h4("Terminology info"),
                # Input para el nombre de usuario
                fluidRow(
                    class = "align-items-center",
                    column(4, "Terminolgy name/id"),
                    column(8, textInput(ns("new_gaz_name"), label = NULL, value = ""))
                ),
                fluidRow(
                    class = "align-items-center",
                    column(4, ""),
                    column(8, uiOutput(ns("status_name_gaz_output")))
                ),
                fluidRow(
                    class = "align-items-center",
                    column(4, "Description of the terminolgy"),
                    column(8, textInput(ns("gaz_description"), label = NULL, value = ""))
                )
            ),
            tags$hr(style = 'width:100%; border-top: 1px solid #e0e0e0;'),
            tagList(
                title = h4("Terminology data"),
                # Input para el rol
                fluidRow(
                    class = "align-items-center",
                    column(4, "Upload data:"),
                    column(8, fileInput(ns("file_gaz"), "Upload tsv file", accept = ".tsv"))
                ),
                fluidRow(
                    class = "align-items-center",
                    column(4, "Hyperlink pattern to online terminology viewer:"),
                    column(8, textAreaInput(ns("pattern_hyperlink"), "", value = "", placeholder = "Paste the hyperlink with the string '{CODE}' where the code in the link would be placed. For example: https://browser.ihtsdotools.org/?perspective=full&conceptId1={CODE}&edition=MAIN/SNOMEDCT-ES/2022-10-31&release=&languages=es,en&latestRedirect=false"))
                ),
                fluidRow(
                    class = "align-items-center",
                    column(4, ""),
                    column(8, uiOutput(ns("status_file_gaz_output")))
                ),
                fluidRow(
                    class = "align-items-center",
                    column(4, ""),
                    column(8,actionButton(ns("upload_gaz"), "Upload",icon = icon("cloud-upload"),width = "100%"))
                )
            ),
            tags$hr(style = 'width:100%; border-top: 1px solid #e0e0e0;'),
            column(12,actionButton(ns("create_terminology"), "Create new terminology", icon = icon("list"), width = "100%"))
        ),
        box(width=6,collapsible = TRUE, 
            help_gazcreation()
        )
    )
}


generalGazzeteerCreationInterface<- function(input, output, session, con, con_terminologies, con_projects)
{
    ns <- session$ns
    
    # Observe event upload data
    # This observe event read the data, check the format, and calculate number
    # of rows (mentions). 
    # Variable reactiva para almacenar el contenido del archivo cargado
    uploaded_data <- reactiveVal(NULL)
    status_upload <- reactiveVal(NULL)
    # Variable reactiva para almacenar el mensaje de estado
    status_message <- reactiveVal(NULL)
    # Variable reactiva para almacenar la availability del nombre
    gaz_name_accepted <- reactiveVal(NULL)
    status_gaz_name <- reactiveVal(NULL)
    
    observeEvent(input$upload_gaz, {
        file <- input$file_gaz
        if (!is.null(file)) {
            # Leer el archivo y almacenarlo en uploaded_data
            # AQUÏ NO LEE TODO SOLO UNA PARTE. HACER PRINT DE DATA PARA VER QUE SALE
            data <- read.delim(file$datapath, header = TRUE, sep = "\t",
                               colClasses = c("code" = "character"))

            # Verificar las columnas requeridas
            required_columns <- c("code", "language", "term", "semantic_tag", "mainterm")
            if (all(required_columns %in% colnames(data))) {
                # Actualizar el mensaje de estado en verde
                status_message(paste("<p style='color:green;'>",
                                     paste("File uploaded. Number of lexical entries:", nrow(data)),
                                     "</p>"))
                # Almacenar el archivo en uploaded_data
                uploaded_data(data)
                status_upload(TRUE)
            } else {
                # Actualizar el mensaje de estado en rojo
                status_message(paste("<p style='color:red;'>Error: Incorrect file format.</p>"))
                # Almacenar NULL en uploaded_data para indicar que el archivo no es válido
                uploaded_data(NULL)
                status_upload(FALSE)
            }
        }
    })
    # Actualizar el texto de salida de estado
    output$status_file_gaz_output <- renderUI({
        HTML(status_message())
    })
    
    # Actualizar el texto de salida de estado de nombre
    output$status_name_gaz_output <- renderUI({
        HTML(status_gaz_name())
    })
    
    # Observe event create gaz
    # This observe event upload the data into database
    observeEvent(input$create_terminology, {
        # First, validate the name of the terminology
        existing_projects <- con_projects$find()$name
        existing_names <- con_terminologies$find()$name
        # If name already exist in database, ask user to change ir
        if (input$new_gaz_name %in% existing_names){
            status_gaz_name(paste("<p style='color:red;'>",
                                  "The selected name already exists in the gazetteer database, please use another identification name.",
                                  "</p>"))
            runjs('$("#y-new_gaz_name").css("color", "red");')
            gaz_name_accepted(FALSE)
        } else if (input$new_gaz_name %in% existing_projects){
            status_gaz_name(paste("<p style='color:red;'>",
                                  "The selected name already exists in the project database, use a unique identifier other than a project name.",
                                  "</p>"))
            runjs('$("#y-new_gaz_name").css("color", "red");')
            gaz_name_accepted(FALSE)
        }else {
            status_gaz_name(paste("<p style='color:green;'>",
                                  "Valid name",
                                  "</p>"))
            runjs('$("#y-new_gaz_name").css("color", "green");')
            gaz_name_accepted(TRUE)
        }
        # Check if data is loaded
        if (is.null(uploaded_data())){
            status_message(paste("<p style='color:red;'>There was an error uploading the file. Please select it again and click 'Upload tsv file'.</p>"))
            status_upload(FALSE)
        } else {
            status_upload(TRUE)
        }
        # Then, save data into the database
        if (gaz_name_accepted() & status_upload()){
            # Show modal
            showModal(modalDialog(
                tags$div(
                    style = "display: flex; flex-direction: column; align-items: center;",
                    tags$div(
                        style = "display: flex; align-items: center; margin-bottom: 10px;",
                        tags$img(src = "small_logo.png", title = "small logo", height = "40px"),
                        tags$p(
                            style = "font-weight: bold; text-align: center; margin-left: 0px;",
                            "Creating the gazetteer. This may take a while, please do not close the tab."
                        )
                    ),
                    tags$p(HTML("<img src=\"loading.gif\">"))
                ),
                footer = NULL, size = "s", fade = TRUE
            ))
            
            # Create the entry in terminologies list collection
            create_gaz_info(input$new_gaz_name,
                            input$gaz_description,
                            input$pattern_hyperlink,
                            paste0("mongodb://",mongo_host,":",mongo_port),
                            mongo_database,
                            mongo_terminologies_collection)
            # Create the entries in term collections
            create_gaz_entries(uploaded_data(),
                               input$new_gaz_name,
                               paste0("mongodb://",mongo_host,":",mongo_port),
                               mongo_database,
                               mongo_terms_collection)
            # Once data is created, open modal indicating that was created and reset
            # inputs.
            removeModal()
            # Reset values
            updateTextInput(session = session , "new_gaz_name", value="")
            updateTextInput(session = session ,"gaz_description", value="")
            updateTextAreaInput(session=session, "pattern_hyperlink", value="")
            status_message("")
            status_upload(NULL)
            uploaded_data(NULL)
            status_gaz_name("")
            gaz_name_accepted(NULL)
            
            showModal(modalDialog(id = "terminology_created", 
                                  HTML("Gazzeteer successfully created.\n Reload the page to display it in the menu.") , 
                                  size="s")
            )
            
            
        } else {
            showModal(modalDialog(HTML("Gazzeteer don't created.\n Verify all values in the form.") , 
                                  size="s")
            )
        }
        
    },ignoreInit=TRUE, ignoreNULL = TRUE)
    
    
    
}