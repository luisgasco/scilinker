
source(paste0(base_path,"/scilinker/modules/help_menus.R"))


# generalValidationInterface module  ----
# Module to show stats and validate mentions of users of an specific project
generalProjectCreationInterfaceUI <- function(id, con, con_terminologies)
{
    ns <- NS(id)
    # Get users of the app
    users_of_app <- con$find()$user_name
    users_of_app <- users_of_app[users_of_app!="admin"]
    # Get gazzeteers of the app
    gazz_of_app <- con_terminologies$find()$name
    
    # UI
    fluidRow(
            box(width=6,
                title = h2("Create new project"),
                tagList(
                    title = h4("Project info"),
                    # Input para el nombre de usuario
                    fluidRow(
                        class = "align-items-center",
                        column(4, "Project name"),
                        column(8, textInput(ns("new_project_name"), label = NULL, value = ""))
                    ),
                    fluidRow(
                        class = "align-items-center",
                        column(4, ""),
                        column(8, uiOutput(ns("status_name_project_output")))
                    ),
                    fluidRow(
                        class = "align-items-center",
                        column(4, "Description of the project"),
                        column(8, textInput(ns("project_description"), label = NULL, value = ""))
                    )
                ),
                tags$hr(style = 'width:100%; border-top: 1px solid #e0e0e0;'),
                tagList(
                    title = h4("Project data"),
                    # Input para el rol
                    fluidRow(
                        class = "align-items-center",
                        column(4, "Upload data:"),
                        column(8, fileInput(ns("file_data"), "Upload Zip file", accept = ".zip"))
                    ),
                    # Indicates whether the data have predictions or not
                    fluidRow(
                        class = "align-items-center",
                        column(4,""),
                        column(8, checkboxInput(ns("prenormalised_data"), label = "The data I have upload have pre-calculated candidate codes", value = FALSE))
                    ),
                    fluidRow(
                        class = "align-items-center",
                        column(4,""),
                        column(8, 
                               shinyjs::hidden(
                                   pickerInput(
                                       inputId = ns("candidates_model"),
                                       label = "Select your model to generate candidates:", 
                                       choices = available_models
                                   )
                               ),
                               shinyjs::hidden(
                                   sliderInput(ns("number_candidates"), "Number of candidates to generate:",
                                               min = 1, max = max_candidates,
                                               value = 1)
                               )
                            )
                    ),
                    fluidRow(
                        class = "align-items-center",
                        column(4,""),
                        column(8,
                               pickerInput(
                                    inputId = ns("assign_norm_gazz"),
                                    label = "Search and select gazzeteer you want to use to normalize the data", 
                                    choices = gazz_of_app,
                                    multiple = FALSE,
                                    options = pickerOptions(
                                        `live-search` = TRUE,
                                        title = "Nothing selected")
                                ))
                    ),
                    fluidRow(
                        class = "align-items-center",
                        column(4, ""),
                        column(8, uiOutput(ns("status_file_data_output")))
                    ),
                    fluidRow(
                        class = "align-items-center",
                        column(4,""),
                        column(8,actionButton(ns("add_data"), "Upload data",width ="100%"))
                    )
                ),
                tags$hr(style = 'width:100%; border-top: 1px solid #e0e0e0;'),
                tagList(
                    title = h4("Annotators"),
                    # Input para el rol
                    fluidRow(
                        class = "align-items-center",
                        column(4, "Users to be part of the project:"),
                        column(8, 
                               pickerInput(
                                   inputId = ns("assign_users"),
                                   label = "Search and select users", 
                                   choices = users_of_app,
                                   multiple = TRUE,
                                   options = list(
                                       `live-search` = TRUE)
                               ),
                               label = NULL, value = "")
                    ), 
                    fluidRow(
                        class = "align-items-center",
                        column(4, ""),
                        column(8, uiOutput(ns("status_user_selection_output")))
                    ),
                    # Percentage of documents for agreement.
                    fluidRow(
                        class = "align-items-center",
                        column(4, "Do you want to calculate agreement on a percentage of documents?"),
                        column(1, prettySwitch(
                                    inputId = ns("bool_perc_agreement"),
                                    label = "", 
                                    status = "success",
                                    fill = TRUE,
                                    bigger = FALSE
                                )
                        ),
                        column(7, uiOutput(ns("show_perc_agreement"))),
                        style = 'margin-bottom: 20px;'
                    ),
                    fluidRow(
                        class = "align-items-center",
                        column(4, "Do you want to assign a specific percentage of documents to each project user?"),
                        column(1, prettySwitch(
                            inputId = ns("bool_perc_users"),
                            label = "", 
                            status = "success",
                            fill = TRUE,
                            bigger = FALSE
                        )
                        ),
                        column(7,uiOutput(ns("sliders_perc_users")))
                    ),
                    fluidRow(
                        class = "align-items-center",
                        column(5, ""),
                        column(7, uiOutput(ns("status_project_creation_output")))
                    ),
        
                    # See python code generated in a notebook (scilinker_analysis)
                ),
                fluidRow(
                    column(12,actionButton(ns("add_project"), "Create new project",width ="100%"))
                )
            ),
            box(width=6,collapsible = TRUE, 
                help_projectcreation()
            )

    )
}


generalProjectCreationInterface<- function(input, output, session, con, con_terminologies, con_projects)
{
    ns <- session$ns
    # Ractive values for data upload
    status_upload_file <- reactiveVal(NULL)
    status_upload_accepted <- reactiveVal(NULL)
    file_info <- reactiveValues(
        file = NULL,
        temp_dir = NULL,
        txt_folder = NULL,
        txt_files = NULL,
        tsv_file = NULL,
        tsv_df = NULL, 
        gazzeteer_id = NULL
    )
    # Reactive values for project creation
    # Variable reactiva para almacenar la availability del nombre
    status_project_name <- reactiveVal(NULL)
    status_project_creation <- reactiveVal(NULL)
    status_user_selection<- reactiveVal(NULL)
    project_name_accepted <- reactiveVal(NULL)
    project_user_selection_accepted <- reactiveVal(NULL)
    project_user_percentage_accepted <- reactiveVal(NULL)
    
    # Show candidates_model_config output depending on prenormalised_data input
    observeEvent(input$prenormalised_data,{
        # Show/hide inputs 
        toggle("candidates_model", anim=TRUE, time=0.5, animType="slide")
        toggle("number_candidates", anim=TRUE, time=0.5, animType="slide")
    },ignoreInit=TRUE)
    # Show slider for select percentage of documents for agreement when input$bool_perc_agreement is TRUE
    observeEvent(input$bool_perc_agreement, {
        if (input$bool_perc_agreement) {
            # Si bool_perc_agreement es TRUE, mostrar el sliderInput
            output$show_perc_agreement <- renderUI({
                
                sliderInput(ns("perc_agreement_slider"), NULL, min = 1, max = 100, value = 5, post="%")
            })
        } else {
            # Si bool_perc_agreement es FALSE, mostrar un mensaje o cualquier otro contenido que desees
            output$show_perc_agreement <- renderText("Percentage agreement is not selected.")
        }
    })
    
    # Show slider for each user to select percentage of documents 
    observeEvent(input$bool_perc_users, {
        if (input$bool_perc_users) {
            if(length(input$assign_users)==0){
                output$sliders_perc_users <- renderText("Please, select users from the selector")
            }else{
                observe({
                    # Si bool_perc_agreement es TRUE, mostrar el sliderInput
                    sliders <- lapply(input$assign_users, function(user) {
                        sliderInput(
                            ns(paste0("slider_perc_", user)),
                            label = NULL,
                            min = 1, max = 100, value = 20, , post=paste0("% docs ",user)
                        )
                    })
                    output$sliders_perc_users <- renderUI({
                        tags$div(style = "overflow-y: scroll;  max-height:150px; border: 1px solid #e0e0e0; padding: 10px",
                                 do.call(tagList, sliders))
                    })
                })
            }
        } else {
            # Si bool_perc_agreement es FALSE, mostrar un mensaje o cualquier otro contenido que desees
            output$sliders_perc_users <- renderText("The documents will be distributed equally among users.")
        }
    })
    
    # Load and Unzip file.
    observeEvent(input$add_data,{
        # Get file
        file_info$file <- input$file_data
        # Create temporal folder to unzip
        file_info$temp_dir <- file.path(tempdir(),input$file_data$name)
        
        # If file is not null
        if (!is.null(file_info$file)  &  input$assign_norm_gazz != "") {
            # Unzip zip file in that folder
            unzip(file_info$file$datapath, exdir = file_info$temp_dir)
            # Check file structure(carpeta txt y archivo tsv con columnas X.).
            # A. existing txt folder
            file_info$txt_folder <- file.path(file_info$temp_dir, "txt")
            # B. Existing tsv file
            file_info$tsv_file <- list.files(file_info$temp_dir, pattern = "\\.tsv$", full.names = TRUE)
            # Logic
            if (!file.exists(file_info$txt_folder) || !file.info(file_info$txt_folder)$isdir) {
                status_upload_file(paste("<p style='color:red;'>Error: Incorrect zip file data structure. 
                                         The zip should contain a folder named txt with the text files inside.</p>"))
                file_info$file <- NULL
                file_info$temp_dir <- NULL
                file_info$txt_folder <- NULL
                file_info$txt_files <- NULL
                file_info$tsv_file <- NULL
                file_info$tsv_df <- NULL
                file_info$gazzeteer_id <- NULL
                return()
            } else if (length(file_info$tsv_file) == 0) {
                status_upload_file(paste("<p style='color:red;'>Error: Incorrect zip file data structure. 
                                         The zip file must contain in the root a tsv file with the mentions to normalize.</p>"))
                file_info$file <- NULL
                file_info$temp_dir <- NULL
                file_info$txt_folder <- NULL
                file_info$txt_files <- NULL
                file_info$tsv_file <- NULL
                file_info$tsv_df <- NULL
                file_info$gazzeteer_id <- NULL
                return()
            } 
            # C. List of txt files
            file_info$txt_files <- list.files(file_info$txt_folder, pattern = "\\.txt$", full.names = TRUE)
            # D. Check Columns of tsv file
            # Read the data
            # Read tsv file:
            data <- read.delim(file_info$tsv_file, header = TRUE )
            # If data is prenormalised
            if (input$prenormalised_data){
                # Check column names with data
                columns <- c("filenameid","mention_class","span","codes")
                if (all(columns %in% colnames(data))) {
                    # Transform code column if it exists
                    data$codes <- lapply(strsplit(gsub("\\[|\\]", "", data$codes), ", "), as.character)
                    # Clean comillas added when reading file
                    data$codes <- lapply(data$codes, function(x) gsub("^'|'$", "", x))
                    # Actualizar el mensaje de estado en verde
                    status_upload_file(paste("<p style='color:green;'>",
                                         paste("File uploaded. Number of documents:", length(file_info$txt_files),". Number of mentions:",nrow(data) ),
                                         "</p>"))
                    # DELETE
                    # textos_cargados <<- file_info$txt_files
                    # textos_nombre <<- lapply(textos_cargados, function(file_path) tools::file_path_sans_ext(basename(file_path)))
                    # tsv_cargado <<- data
                    # Almacenar el archivo en uploaded_data
                    file_info$tsv_df <- data
                    file_info$gazzeteer_id <- input$assign_norm_gazz
                } else {
                    # Actualizar el mensaje de estado en rojo
                    status_upload_file(paste("<p style='color:red;'>Error: Incorrect column names in tsv file..</p>"))
                    # Almacenar NULL en uploaded_data para indicar que el archivo no es válido
                    file_info$file <- NULL
                    file_info$temp_dir <- NULL
                    file_info$txt_folder <- NULL
                    file_info$txt_files <- NULL
                    file_info$tsv_file <- NULL
                    file_info$tsv_df <- NULL
                    file_info$gazzeteer_id <- NULL
                    return()
                }
            } else {
                columns <- c("filenameid","mention_class","span")
                if (all(columns %in% colnames(data))) {
                    # Actualizar el mensaje de estado en verde
                    status_upload_file(paste("<p style='color:green;'>",
                                             paste("File uploaded. Number of documents:", length(file_info$txt_files),". Number of mentions:",nrow(data) ),
                                             "</p>"))
                    # Almacenar el archivo en uploaded_data
                    file_info$tsv_df <- data
                    file_info$gazzeteer_id <- input$assign_norm_gazz
                } else {
                    # Actualizar el mensaje de estado en rojo
                    status_upload_file(paste("<p style='color:red;'>Error: Incorrect column names in tsv file.</p>"))
                    # Almacenar NULL en uploaded_data para indicar que el archivo no es válido
                    file_info$file <- NULL
                    file_info$temp_dir <- NULL
                    file_info$txt_folder <- NULL
                    file_info$txt_files <- NULL
                    file_info$tsv_file <- NULL
                    file_info$tsv_df <- NULL
                    file_info$gazzeteer_id <- NULL
                    return()
                }
                
            }
        } else {
            status_upload_file(paste("<p style='color:red;'>Error: File not uploaded or gazeteer not selected from the drop-down menu.</p>"))
            return()
        }
        # unlink(file_info$temp_dir,recursive=TRUE)
    },ignoreInit=TRUE, ignoreNULL = TRUE)
    
    output$status_file_data_output <- renderUI({
        HTML(status_upload_file())
    })
    
    
    # 
    observeEvent(input$add_project,{
        # Verify project name
        existing_gazs <- con_terminologies$find()$name
        existing_names <- con_projects$find()$name
        # If name already exist in database, ask user to change ir
        if (input$new_project_name %in% existing_names){
            status_project_name(paste("<p style='color:red;'>",
                                  "The selected name already exists in the projects database, please use another identification name.",
                                  "</p>"))
            runjs('$("#y-new_project_name").css("color", "red");')
            project_name_accepted(FALSE)
        } else if (input$new_project_name %in% existing_gazs){
            status_project_name(paste("<p style='color:red;'>",
                                      "The selected name already exists in the gazetteers database, use a unique identifier other than a gazetteer name.",
                                      "</p>"))
            runjs('$("#y-new_project_name").css("color", "red");')
            project_name_accepted(FALSE)
        }else {
            status_project_name(paste("<p style='color:green;'>",
                                  "Valid name",
                                  "</p>"))
            runjs('$("#y-new_project_name").css("color", "green");')
            project_name_accepted(TRUE)
        }
        # Verify status upload_data. Check if data is loaded
        if (is.null(file_info$tsv_df)){
            status_upload_file(paste("<p style='color:red;'>There was an error uploading the file. Please select it again and click 'Upload tsv file'.</p>"))
            status_upload_accepted(FALSE)
        } else {
            status_upload_accepted(TRUE)
        }
        # Verify that at leas one user has been selected
        if (length(input$assign_users) > 0){
            user_list <- input$assign_users
            status_user_selection(paste("<p style='color:green;'>Successful user selection.You have selected ",length(user_list),"users for this project</p>"))
            project_user_selection_accepted(TRUE)
        } else {
            # Status message
            status_user_selection(paste("<p style='color:red;'>You have not selected any user. Please select at least one.</p>"))
            project_user_selection_accepted(FALSE)
            return()
        }
        
        # Verify that percetage assigned to users is equal to 100%
        # If bool_perc_users is FALSE, distribute the percentage equally among users
        if (input$bool_perc_users){
            #Read number from sliders
            user_perc_docs_list <- unlist(sapply(paste0("slider_perc_", user_list), function(name) input[[name]]))
            # If sum is not 100, error.
            if(sum(user_perc_docs_list)!=100){
                status_project_creation(paste("<p style='color:red;'>Error in the selection of percentages assigned to each user. Adjust them so that their sum is 100.</p>"))
                project_user_percentage_accepted(FALSE)
                return()
            } else{
                status_project_creation(paste("<p style='color:green;'> Correct selection of percentages assigned to each user.</p>"))
                project_user_percentage_accepted(TRUE)
            }
            
        } else {
            # Divide 100 evenly by n and adjust the excess
            n <- length(user_list)
            user_perc_docs_list <- rep(100 %/% n, n)
            user_perc_docs_list[1:(100 %% n)] <- user_perc_docs_list[1:(100 %% n)] + 1
            
            # Verify and correct when sum is not 100
            diff_sum <- sum(user_perc_docs_list) - 100
            if (diff_sum != 0) {
                # Adjust the excess or defect to the first element of the vector
                user_perc_docs_list[1] <- user_perc_docs_list[1] - diff_sum
            }
            project_user_percentage_accepted(TRUE)
        }
        
        # Proceed with the project creation if all status are correct
        if (project_name_accepted() & status_upload_accepted()& 
            project_user_selection_accepted() & project_user_percentage_accepted()){
            # Show modal
            showModal(modalDialog(
                tags$div(
                    style = "display: flex; flex-direction: column; align-items: center;",
                    tags$div(
                        style = "display: flex; align-items: center; margin-bottom: 10px;",
                        tags$img(src = "small_logo.png", title = "small logo", height = "40px"),
                        tags$p(
                            style = "font-weight: bold; text-align: center; margin-left: 0px;",
                            "Creating the project. This may take a while, please do not close the tab."
                        )
                    ),
                    tags$p(HTML("<img src=\"loading.gif\">"))
                ),
                footer = NULL, size = "s", fade = TRUE
            ))
            
            # Generate structure to user and documents
            users_list <- mapply(function(user, perc_elements) {
                list(user = user, perc_elements = perc_elements/100)
            }, user = user_list, perc_elements = user_perc_docs_list, SIMPLIFY = FALSE)
            
            # print(users_list)
            # Create annotation project
            # TEST IF IT WORKS AND ADD NEW FIELD CALLED "gazzeteer"
            
            
            create_annotation_project(mongo_url = paste0("mongodb://",mongo_host,":",mongo_port),
                                      db_name = mongo_database, 
                                      collection_name = mongo_projects_collection,
                                      project_name = input$new_project_name,
                                      project_description = input$project_description, 
                                      users_list = users_list,
                                      gazetteer_id = input$assign_norm_gazz,
                                      model_name = ifelse(input$prenormalised_data,"candidates precomputed",  input$candidates_model) ,
                                      number_candidates = ifelse(input$prenormalised_data,"candidates precomputed",  as.character(input$number_candidates))
                                      )  
            # Insert documents into documents_collection
            create_project_documents(mongo_url = paste0("mongodb://",mongo_host,":",mongo_port),
                                     db_name = mongo_database, 
                                     collection_name = mongo_documents_collection,
                                     text_files_path = file_info$txt_files,
                                     project_name = input$new_project_name)
            # Insert mentions into mentions_collection
            user_info_out = import_annotations_to_mongodb(mongo_url = paste0("mongodb://",mongo_host,":",mongo_port),
                                                          db_name = mongo_database,
                                                          collection_name = mongo_mentions_collection, 
                                                          project_id = input$new_project_name, 
                                                          admin_user_name = session$userData$user,
                                                          users_list = users_list,
                                                          agreement_perc = ifelse(input$bool_perc_agreement, input$perc_agreement_slider/100, 0), # 0 or input
                                                          candidate_df = file_info$tsv_df # tsv_df
                                                           )
            
            # Update user collection 
            # asdasdasd<<- user_info_out
            for (user in user_info_out){
                project_info = list(
                    project = input$new_project_name,#,
                    documents = user$docs
                )
                create_update_user(mongo_url = paste0("mongodb://",mongo_host,":",mongo_port),
                                    db_name = mongo_database,
                                    collection_name = mongo_user_collection ,
                                    user_name = user$user,
                                    project_info = project_info)

            }
            
            
            
            
            # If data have not candidates, ask the API to get predictions
            if (!input$prenormalised_data){
                # URL for the POST request
                url <- "http://localhost:8080/generate_candidates_by_project"
                # Data to be sent in the POST request
                post_data <- list(
                    db_name = mongo_database,
                    project_id = input$new_project_name,
                    gazetteer = input$assign_norm_gazz,
                    model_name = input$candidates_model,
                    k_candidates = as.character(input$number_candidates)
                )
                print(post_data)
                result_future <- future_promise({httr::POST(url, body = post_data, encode = "json")})  %>% 
                    then(
                        onFulfilled = function(value) {
                            # Getting here means promise1 succeeded
                            print("POST sent to API")
                            return(content(value, "text"))
                        },
                        onRejected = function(err) {
                            # Getting here means promise1 failed
                            print("ERROR WHEN SENDING POST TO API")
                        }
                    )
                
            }
            
            removeModal()
            
            
            # Reset reactive values and inputs
            file_info$file <- NULL
            file_info$temp_dir <- NULL
            file_info$txt_folder <- NULL
            file_info$txt_files <- NULL
            file_info$tsv_file <- NULL
            file_info$tsv_df <- NULL
            file_info$gazzeteer_id <- NULL
            status_project_name(NULL)
            status_project_creation(NULL) 
            status_user_selection(NULL)
            project_name_accepted(NULL) 
            project_user_selection_accepted(NULL)
            project_user_percentage_accepted(NULL) 
            status_upload_file(NULL)
            status_upload_accepted(NULL) 
            updateTextInput(session = session, "new_project_name", value="")
            updateTextInput(session = session, "project_description", value="")
            status_project_name("")
            # updateCheckboxInput(session, "prenormalised_data",value =FALSE)
            status_upload_file("")
            status_user_selection("")
            updatePrettySwitch(session,"bool_perc_agreement",value=FALSE)
            updatePrettySwitch(session,"bool_perc_users",value=FALSE)
            status_project_creation("")
            # Show ok modal
            showModal(modalDialog(id = "project_created", 
                                  HTML("Project successfully created.\n Reload the page to display it in the menu.") , 
                                  size="s")
            )
            
        } else {
            showModal(modalDialog(id = "project_created", 
                                  HTML("Project don't created.\n Verify all values in the form.") , 
                                  size="s")
            )
            return()
        }
        
    },ignoreInit=TRUE, ignoreNULL = TRUE)
    
    # Actualizar el texto de salida de estado
    output$status_name_project_output <- renderUI({
        HTML(status_project_name())
    })
    
    # Actualizar el texto de salida de estado de nombre de proyecto
    output$status_project_creation_output <- renderUI({
        HTML(status_project_creation())
    })
    # Actualizar el texto de salida de estado de selección de usuarios
    output$status_user_selection_output <- renderUI({
        HTML(status_user_selection())
    })
    
}