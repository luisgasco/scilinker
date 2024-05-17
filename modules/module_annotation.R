library(shinyjs)
library(DT)
library(mongolite)
library(jsonlite)
library(dplyr)
library(shinyWidgets)
library(fontawesome)
shinyjs::useShinyjs()

# Load submodules for annotation module
source(paste0(base_path,"/scilinker/modules/error_handling.R"))
source(paste0(base_path,"/scilinker/modules/annotation_utils/annotation_ui_utils.R"))
source(paste0(base_path,"/scilinker/modules/annotation_utils/annotation_data_utils.R"))
source(paste0(base_path,"/scilinker/modules/annotation_utils/annotation_backend_utils.R"))

# General Annotator Interace module ----
# Module to generate the graphical interface for normalization.
generalAnnotatorInterfaceUI <- function(id)
{
    ns <- NS(id)
    tabItem(
        tabName = "anotaciones", fluidRow(
            column(
                8, 
                box(
                    width = 12, status = "danger",
                    height = 500, shinycssloaders::withSpinner(tableAnnotatorUI(ns("mytable")))
                ),
                box(
                    width = 12, title = "Text",
                    height = 250, textAnnotatorUI(ns("texto_output"))
                )
            ),
            column(
                4,
                box(
                    width = 12, height = 750,
                    pannelAnnotatorUI(ns("pannel_output"))
                )
            )
        )
    )
}
generalAnnotatorInterface <- function(input, output, session,datos_reactive)
{
    ns <- session$ns
    
    ## Initialize datos_reactive and annotation_reactive inside the Annotator module
    # data_reactive are the mentions that the user must normalize for the project in question. 
    # annotation_reactive are the annotations carried out by the user for the active project. 
    # They are loaded at the beginning to avoid input/output flows with the database.
    datos_reactive <- reactiveValues(data = data.frame())
    annotation_reactive <- reactiveValues(
        data = data.frame()
    )
    ## Initialize reactive value with their default values.
    # Reactive value for the selected row in the table of UI.
    sel_row <- reactiveVal()
    # Internal reactive values
    context_id <- reactiveVal()
    composite_id <- reactiveVal()
    abbrev_id <- reactiveVal()
    num_codes <- reactiveVal(1)
    show_text <- reactiveVal(FALSE)
    prev_annotated <- reactiveVal(FALSE)
    is_abb <- reactiveVal(FALSE)
    is_composite <- reactiveVal(FALSE)
    need_context <- reactiveVal(FALSE)
    is_wrong <- reactiveVal(FALSE)
    timer <- reactiveVal()
    annotation_comments <- reactiveVal()
    # Put reactive values in a list to easily access them 
    reactive_values <- list(context_id = context_id, composite_id = composite_id,
                            abbrev_id = abbrev_id, num_codes = num_codes,
                            show_text = show_text,prev_annotated = prev_annotated,
                            is_abb = is_abb, is_composite = is_composite,
                            need_context = need_context, is_wrong = is_wrong,
                            timer = timer, annotation_comments = annotation_comments)
    
    # Loading modal while data is being prepared
    observeEvent(session$userData$current_project, {
        # Show modal
        showModal(modalDialog(tags$div(style = "display: flex; flex-direction: column; align-items: center;",
                                       tags$div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                                                tags$img(src = "small_logo.png", title = "small logo", height = "40px"),
                                                tags$p(style = "font-weight: bold; text-align: center; margin-left: 0px;",
                                                       "Loading... This may take a while")
                                                ), 
                                       tags$p(HTML("<img src=\"loading.gif\">"))
                                       ), 
                              footer = NULL, size = "s", fade = TRUE)
                  )
        # Load data from database from current user and project using loadData function
        datos_reactive$data = loadData(session, session$userData$data, session$userData$user, session$userData$current_project)
        # Load annotations done by the user in the current project using loadAnnotations function
        annotation_reactive$data = loadAnnotations(session, session$userData$user, session$userData$current_project)
        # Reinitialize reactive variables each time user change project
        # TODO: Perhaps this is the problem of the application breaking when changing projects.
        context_id(gsub("[.#-]", "_", paste0("contextx_", datos_reactive$data[1, ]$document_id)))
        composite_id(gsub("[.#-]", "_", paste0("compositex_", datos_reactive$data[1, ]$document_id)))
        abbrev_id()
        num_codes(1)
        show_text(FALSE)
        prev_annotated(FALSE)
        is_abb(FALSE)
        is_composite(FALSE)
        need_context(FALSE)  
        is_wrong(FALSE)
        annotation_comments()
        # Load the gazzeteer when loading all the data
        current_gaz_id <- session$userData$projects_db_endpoint$find(toJSON(list(name = session$userData$current_project), auto_unbox  = TRUE))$gazetteer_id
        current_gaz_info <- session$userData$terminologies_db_endpoint$find(toJSON(list(name = current_gaz_id), auto_unbox  = TRUE))
        current_gaz_data <- session$userData$terms_db_endpoint$find(toJSON(list(gazetteer_id = current_gaz_id), auto_unbox  = TRUE))
        diccionario <<- loadDict2(current_gaz_data, current_gaz_info$hyperlink_pattern)
        # After loading data, remove modal
        removeModal()
    }, ignoreNULL = FALSE)
    
    # Create proxy object to update datatable
    proxy <- dataTableProxy(ns("mytable"))
    
    # Load tsv file dictionary. Later it will be done by reading from a collection in the database.
    # print(paste0("obtener terminos con esto: ", session$userData$terms))
    # print(paste0( "terminologías": session$userData$terminologies))
    # print(paste0("current project", session$userData$current_project))
    # Vamos a utilizar el endpoint para encontrar el gazzeteer id del current project
    # print(session$userData$projects_db_endpoint)
    # asdasd<<- session$userData$projects_db_endpoint
    # current_gaz_id = session$userData$projects_db_endpoint$find(paste0('{"name":"',session$userData$current_project,'"}'))$gazetteer_id
    # print(paste0("CURRENT gazzeter_name ", current_gaz_id))
    # curent_gaz_data = session$userData$terminologies_db_endpoint$find(paste0('{"name":"',current_gaz_id,'"}'))
    # print(paste0("CURRENT gzzeter_data ", curent_gaz_data))
    # # Datos de ese gazzeteer: 
    # datos_gaz_test<<- session$userData$terms_db_endpoint$find(paste0('{"gazetteer_id":"',current_gaz_id,'"}'))

    # Load gazzeteer_id from project. 
    
    # Load gazeeteer data from terms and terminology
    # diccionario2 <<- loadDict(abspath2dicc)
    
    # Show results for current user
    # print(session$userData$projects)
    
    # Call tableAnnotator module (the module for the datatable)
    callModule(tableAnnotator, "mytable", datos_reactive, sel_row)
    callModule(
        textAnnotator, "texto_output", datos_reactive, sel_row, reactive_values
    )
    # Call pannelAnnotator module (the module to build the right pannel with codes)
    callModule(
        pannelAnnotator, "pannel_output", datos_reactive, sel_row, reactive_values,
        annotation_reactive, proxy, diccionario
    )
    
}

# Table Annotator module ----
# Module to generate the table to show the mentions to be normalize
tableAnnotatorUI <- function(id)
{
    ns <- NS(id)
    DT::DTOutput(ns('mytable'),height = "500px")
}
tableAnnotator <- function(input, output, session,datos_reactive,sel_row)
{
    ns <- session$ns
    # Observe the row that is clicked using the UI
    observe({
        sel_row(input$mytable_rows_selected)
    },priority=5)
   
    # Show table on the interface applying some filters and ordering
    output$mytable = DT::renderDataTable({  
        DT::datatable(datos_reactive$data,
                      rownames=FALSE,
                      options = list(deferRender = TRUE,
                                     scrollY = 400,
                                     scrollX=TRUE,
                                     scroller = TRUE,
                                     autoWidth = TRUE,
                                     columnDefs = list(list(visible=FALSE, targets=c( 6, 7, 8, 9, 10))),#2,
                                     order = list(list(11,'asc'),list(1,'asc'))
                                     ),
                      selection ="single",
                      extensions = c("Scroller")) %>% 
        DT::formatStyle( 'validated',
                         target = 'row',
                         backgroundColor = DT::styleEqual(c(0, 1,2,3), c('#f4f4f4', '#cbffe0','#fffddc','#dd6f64')),
                         fontWeight = 'bold')
    },server = TRUE
    )
    return(list(sel_row=sel_row)) # Return sel_row value to the parent module
}

# Text Annotator module ----
# Module show the content of the document if needed.
textAnnotatorUI <- function(id)
{
    ns <- NS(id)
    htmlOutput(ns("texto_output"),style = "height:190px;overflow-y: scroll;")
}
textAnnotator <- function(input, output, session, datos_reactive, sel_row, reactive_values)
{
    ns <- session$ns
    output$texto_output <- renderUI(
        {
            sel_row <- sel_row()
            ## Compute needed filters No need to re-select
            # Get filename_id of the selected mention
            file_name <- unlist(datos_reactive$data[sel_row, ]$document_id)[1] 
            # Get the text related to the current mention
            texto <- datos_reactive$data[sel_row, ]$text[1]
            # If sel_row is NULL (not clicked)
            if (!is.null(sel_row) )
            {   # Compute the html text by marking the mention in yellow
                HTML(calcula_texto(TRUE, datos_reactive$data[sel_row, ], texto, "clase_show"))
            }
        }
    )
}

# Pannel Annotator module ----
# This module builds the right panel in the user interface that will be used to 
# select/normalize each mention and display the previously normalized ones
pannelAnnotatorUI <- function(id)
{
    ns <- NS(id)
    uiOutput(ns("info_code"))
}
pannelAnnotator <- function(input, output, session,datos_reactive,sel_row,reactive_values,annotation_reactive,proxy, diccionario)
{
    ns <- session$ns
    
    # Observe event to update reactive values used to build the UI
    observeEvent(sel_row(),{
        reactive_values$is_abb(FALSE)
        reactive_values$is_composite(FALSE)
        reactive_values$need_context(FALSE)
        reactive_values$show_text(FALSE)
        reactive_values$prev_annotated(FALSE)
        reactive_values$num_codes(1)
        reactive_values$is_wrong(FALSE)
        reactive_values$annotation_comments("")
    })
    # 
    # Observe event to enable/disable the "full_text" button
    observeEvent(input[[reactive_values$context_id()]],{
        if (input[[reactive_values$context_id()]]==TRUE){
            shinyjs::enable("full_text")
        }else if(input[[reactive_values$context_id()]]==FALSE){
            shinyjs::disable("full_text")
        }
    })
    
    # Observe event to enable buttons related to composite normalization
    observeEvent(input[[reactive_values$composite_id()]],{
        if (input[[reactive_values$composite_id()]]==TRUE){
            shinyjs::enable("number_codes")
            shinyjs::enable("update_codes")
            updateAwesomeCheckbox(session, reactive_values$composite_id(),TRUE)
            # Logs for tracing errors
            # print("Observe - is_composite - TRUE")
        }else if(input[[reactive_values$composite_id()]]==FALSE){
            shinyjs::disable("number_codes")
            shinyjs::disable("update_codes")
            # Logs for tracing errors
            # print("Observe - is_composite - FALSE")
            updateAwesomeCheckbox(session, reactive_values$composite_id(),FALSE)
        }
    }, ignoreNULL = TRUE)
    
    # Observe event to update reactive values needed to generate code annotation panels
    observeEvent(input[["update_codes"]],{
        updateNumericInput(session,"number_codes",value = input[["number_codes"]])
        reactive_values$num_codes(input[["number_codes"]])
        updateAwesomeCheckbox(session, reactive_values$composite_id(),TRUE)
        reactive_values$is_composite(TRUE)
        reactive_values$prev_annotated(input[["previously_annotated"]])
        reactive_values$is_wrong(input[["wrong_mention"]])
        reactive_values$need_context(input[["need_context"]])
        reactive_values$annotation_comments(input[["annotation_comments"]])
        # We update the data frame so that there are no problems when saving data.
        update_logical_values_df(input,session, annotation_reactive,proxy,sel_row(),datos_reactive,reactive_values)
        # If num_codes is 1, update the composite input (to false)
        if(input[["number_codes"]]==1){
            reactive_values$is_composite(FALSE)
            updateAwesomeCheckbox(
                session = session, inputId = reactive_values$composite_id(),
                value = FALSE
            )
        }
    })
    
    # Observe event to enable/disable the code candidate list if previously 
    # annotated is not clicked
    observeEvent(input[["previously_annotated"]],{
        if(input[["wrong_mention"]] || input[["previously_annotated"]]){
            # If True disable the candidate code list
            shinyjs::disable("candidate_list_elem")
        }
        else{
            shinyjs::enable("candidate_list_elem")
        }
        
    })
    
    # Observe event to enable/disable the code candidate list if wrong_mention
    # is clicker
    observeEvent(input[["wrong_mention"]],{
        if(input[["wrong_mention"]] || input[["previously_annotated"]]){
            #Don't change state, because it was previously disable
            shinyjs::disable("candidate_list_elem")
        }
        else{
            shinyjs::enable("candidate_list_elem")
        }
    })
    
    # Render the UI for the actual mention
    output$info_code <- renderUI({
        # Logs for tracing errors
        # print(sel_row())
        # If user click on a row of the datatable (sel_row!=NULL)
        if(!is.null(sel_row())){
            # Filter dictionary
            dicc_filt <- filtra_dict(datos_reactive$data,diccionario,sel_row()) 
            # Update reactive values needed for the input ids using data of the actual mention
            reactive_values$abbrev_id(gsub("[.#-]","_",paste0("abbrevx_",datos_reactive$data[sel_row(),]$document_id,"#",datos_reactive$data[sel_row(),]$span_ini,"#",datos_reactive$data[sel_row(),]$span_end)))
            reactive_values$composite_id(gsub("[.#-]","_",paste0("compositex_",datos_reactive$data[sel_row(),]$document_id,"#",datos_reactive$data[sel_row(),]$span_ini,"#",datos_reactive$data[sel_row(),]$span_end)))
            reactive_values$context_id(gsub("[.#-]","_",paste0("contextx_",datos_reactive$data[sel_row(),]$document_id,"#",datos_reactive$data[sel_row(),]$span_ini,"#",datos_reactive$data[sel_row(),]$span_end)))
            # Logs for tracing errors
            #asd <<- datos_reactive$data
            #asd2 <<- annotation_reactive$data
            #print(annotation_reactive$data)
            #asd22<<- dicc_filt
            # Get current system time to calculate time spent when saving
            reactive_values$timer(Sys.time())
            # print("Tiempo obtenido en renderUI")
            # print(reactive_values$timer())
            # Función para generar la interfaz reactiva para cada uno de las menciones.
            # Cada mención tendrá su propia lista de codigos, etc.
            # Los datos_reactivos serán data_reactive$data
            # Seleccionamos sel_row(). Si el identificador está en annotation_reactive
            # cambiamos los reactive_values y los cargamos en la interfazç
            
            # Obtain current annotation_id by document_id and span_ini/span_end
            annotation_id_current <- paste0(datos_reactive$data[sel_row(),]$document_id,"_",
                                            datos_reactive$data[sel_row(),]$span_ini, "_",
                                            datos_reactive$data[sel_row(),]$span_end)
            # Logs for tracing errors
            # print("Current annotation al cargar")
            # print(annotation_id_current)
            # print(annotation_reactive$data)
            
            # If this is not the first annotation of the user in this project
            # (annotation_reactive has more dan one-row).
            if (nrow(annotation_reactive$data) > 0) {
                # Get the existing annotation if possible
                anotacion_existente <- annotation_reactive$data %>% 
                                        filter(user_id == session$userData$user,
                                               annotation_id == annotation_id_current) %>% unique()

                # If there is an annotation, update reactive values to be used
                # when generating the normalization panel
                if (nrow(anotacion_existente) == 1) {
                    reactive_values$prev_annotated(anotacion_existente$previously_annotated)
                    reactive_values$is_wrong(anotacion_existente$is_wrong)
                    reactive_values$is_abb(anotacion_existente$is_abrev)
                    reactive_values$is_composite(anotacion_existente$is_composite)
                    reactive_values$need_context(anotacion_existente$need_context)
                    reactive_values$num_codes(anotacion_existente$num_codes)
                    reactive_values$show_text(TRUE)
                    reactive_values$show_text(FALSE)
                    reactive_values$annotation_comments(anotacion_existente$annotation_comments)
                    
                    # Logs for tracing errors
                    # test_dicc_filt_erase <<- dicc_filt
                    # anotacion_existente_output <<- anotacion_existente
                    # output_reactive_values <<- reactive_values
                    # browser()
                    # Generate panel with update=TRUE 
                    generate_panel(ns, datos_reactive, reactive_values, dicc_filt, sel_row(), anotacion_existente %>% select(codes, sem_rels), update = TRUE)
                    
                } else {
                    # If there is not existing annotation, we will call to 
                    # generate panel with update=FALSE
                    generate_panel(ns, datos_reactive, reactive_values, dicc_filt, sel_row(), anotacion_existente, update = FALSE)
                }
            } else {
                # If this is the first annotation of the user, annotation_reactive
                # will be empty and the code will generate the normalization panel using
                # update=FALSE with default annotation values (NULL)
                generate_panel(ns, datos_reactive, reactive_values, dicc_filt, sel_row(), NULL, update = FALSE)
            }
        }
    })
    
    # Observe event to update internal dataframes and save data in the database
    observeEvent(input[["save_data"]],{
        # Filter dictionary
        dicc_filt <- filtra_dict(datos_reactive$data,diccionario, sel_row())
        # Logs for tracing errors
        # print("Observe - save_data")
        # dicc_filt_OUT<<- dicc_filt

        # Code to validate input (to see if there are errors and inconsistencies
        # when introducing data)
        error_occurred <- reactiveVal(FALSE)
        input_validation(input, dicc_filt, reactive_values,datos_reactive, sel_row(), error_occurred)
        # If error_occured() is TRUE, we leave the observeEvent to give the user
        # the posibility to fix it
        if (error_occurred()) {
            return()
        }
        
        # Get and prepare codes and relations from the UI
        code_list <- list()
        semrel_list <- list()
        code_list <- lapply(1:input[["number_codes"]], function(i) {
            id_code = i
            nocode_id = paste0("code_num_",id_code,"_no_code_", datos_reactive$data$document_id[sel_row()],"_",datos_reactive$data$span_ini[sel_row()],"_",datos_reactive$data$span_end[sel_row()])
            nonorm_id = paste0("code_num_",id_code,"_no_norm_", datos_reactive$data$document_id[sel_row()],"_",datos_reactive$data$span_ini[sel_row()],"_",datos_reactive$data$span_end[sel_row()])
            writtencode_id = paste0("code_num_",id_code,"_","written_code_", datos_reactive$data$document_id[sel_row()],"_",datos_reactive$data$span_ini[sel_row()],"_",datos_reactive$data$span_end[sel_row()])
            writtenrel_id = paste0("code_num_",id_code,"_","written_relation_", datos_reactive$data$document_id[sel_row()],"_",datos_reactive$data$span_ini[sel_row()],"_",datos_reactive$data$span_end[sel_row()])
            
            
            code_id <- lapply(1:nrow(dicc_filt), function(j, id_code, datos_reactive,input) {
                code_id = gsub("[.#-]","_",paste0("code_num_",id_code,"_check_",dicc_filt$code[j],"_", datos_reactive$data$document_id[sel_row()],"_",datos_reactive$data$span_ini[sel_row()],"_",datos_reactive$data$span_end[sel_row()]))
                if (input[[code_id]]) {
                    return(code_id)
                } else{
                    
                }
            }, id_code = id_code, datos_reactive = datos_reactive, input=input)
            code_id <- tryCatch({Filter(function(x) !is.null(x), code_id)[[1]]},error=function(e){return("")})
            #print("AUILLEGA_ DENTRO DE LAPPLY primerop")
            #print(input[["number_codes"]])
            #print(code_id)
            #print(nocode_id)
            #print(nonorm_id)
            if (code_id!=""){
                # Add codes
                second_split = gsub("[.#-]","_",paste0("_",datos_reactive$data$document_id[sel_row()]))
                code <- unlist(strsplit(unlist(strsplit(code_id, split = "_check_"))[2],split=second_split))[1]
                code_list <- append(code_list,code)
            } else if(input[[nocode_id]]) {
                code_list <- append(code_list, input[[writtencode_id]])
            } else if (input[[nonorm_id]]){
                code_list <- append(code_list, "NO_CODE")
            }
            return(code_list)
        })
        semrel_list <- lapply(1:input[["number_codes"]], function(i) {
            id_code = i
            semrel_id <- lapply(1:nrow(dicc_filt), function(j, id_code, datos_reactive,input) {
                semrel_id = gsub("[.#-]","_",paste0("code_num_",id_code,"_rel_",dicc_filt$code[j],"_", datos_reactive$data$document_id[sel_row()],"_",datos_reactive$data$span_ini[sel_row()],"_",datos_reactive$data$span_end[sel_row()]))
                if (!is.null(input[[semrel_id]])) {
                    return(semrel_id)
                }else{
                    
                }
            }, id_code = id_code, datos_reactive = datos_reactive, input=input)
            semrel_id <- tryCatch({Filter(function(x) !is.null(x), semrel_id)[[1]]},error=function(e){return("")})
            
            nocode_id = paste0("code_num_",id_code,"_no_code_", datos_reactive$data$document_id[sel_row()],"_",datos_reactive$data$span_ini[sel_row()],"_",datos_reactive$data$span_end[sel_row()])
            nonorm_id = paste0("code_num_",id_code,"_no_norm_", datos_reactive$data$document_id[sel_row()],"_",datos_reactive$data$span_ini[sel_row()],"_",datos_reactive$data$span_end[sel_row()])
            writtenrel_id = paste0("code_num_",id_code,"_","written_relation_", datos_reactive$data$document_id[sel_row()],"_",datos_reactive$data$span_ini[sel_row()],"_",datos_reactive$data$span_end[sel_row()])
            
            if (semrel_id!=""){
                # Add semrels
                semrel_list <- append(semrel_list,input[[semrel_id]])
            } else if(input[[nocode_id]]) {
                semrel_list <- append(semrel_list, input[[writtenrel_id]])
            } else if (input[[nonorm_id]]){
                semrel_list <- append(semrel_list, "NO_CODE")
            }
            return(semrel_list)
        })
        code_list <- unlist(code_list)
        semrel_list <- unlist(semrel_list)
        # Logs for tracing errors
        # print(paste0("Lista de códigos seleccionada: ", code_list))
        # print(paste0("Lista de rels seleccionada: ", semrel_list))
        
        
        
        # Update mentions Dataframe (datos_reactive)
        # This code will change the "user_id","validated" and "previously_annotated"
        # values. This will also update the "validated_by" value for the current user
        datos_reactive$data[sel_row(),] <- datos_reactive$data %>%
            filter(document_id == datos_reactive$data$document_id[sel_row()],
                   span_ini == datos_reactive$data$span_ini[sel_row()],
                   span_end == datos_reactive$data$span_end[sel_row()]) %>%
            mutate(user_id = session$userData$user,
                   validated = ifelse(input[["wrong_mention"]], 3,
                                      ifelse(input[["previously_annotated"]], 2, 1)),
                   previously_annotated = ifelse(input[["previously_annotated"]], TRUE, FALSE))
        # Get the new sate (1 or 2) of the normalized mention
        new_state <- datos_reactive$data %>%  filter(user_id == session$userData$user,
                                                     document_id == datos_reactive$data$document_id[sel_row()],
                                                     span_ini == datos_reactive$data$span_ini[sel_row()],
                                                     span_end == datos_reactive$data$span_end[sel_row()] ) %>% select(validated)
        # Get the value of previously_annotated for the normalized mention
        new_prevannotated <- datos_reactive$data %>%  filter(user_id == session$userData$user,
                                                             document_id == datos_reactive$data$document_id[sel_row()],
                                                             span_ini == datos_reactive$data$span_ini[sel_row()],
                                                             span_end == datos_reactive$data$span_end[sel_row()] ) %>% select(previously_annotated)
        
        # Update the validated_by field for the current user in the dataframe
        datos_reactive$data[sel_row(),]$validated_by[[1]] <- datos_reactive$data[sel_row(),]$validated_by[[1]] %>%
            mutate(state= ifelse(user_id==session$userData$user,new_state$validate,state),
                   previously_annotated = ifelse(user_id==session$userData$user,new_prevannotated$previously_annotated,previously_annotated))
        
        
        # Update annotation dataframe
        # Generate a current annotation_id
        current_annotation_id <- paste0(datos_reactive$data[sel_row(),]$document_id,"_",datos_reactive$data[sel_row(),]$span_ini,"_",datos_reactive$data[sel_row(),]$span_end)
        # Check if this annotation existed before in the dataframe
        is_present <- current_annotation_id %in% annotation_reactive$data$annotation_id[annotation_reactive$data$user_id == session$userData$user]
        # Calculate duration of annotation in seconds
        duration <- as.numeric(difftime(Sys.time(), reactive_values$timer(), units = "secs"))
        
        
        # Logs for tracing errors
        # print("Estamos guardando la current annotation:")
        # print(current_annotation_id)
        # print("Tiempo obtenido al cargar UI")
        # print(reactive_values$timer())
        # print("Tiempo en este momento")
        # print(Sys.time())
        # print("Tiempo duration en el save")
        # print(duration)
        # print(paste0("ESTE ELEMENTO ESTá PRESENTE? ", is_present))
        # If anotation is present in the dataframe, we update the data
        if(is_present){
            # Logs for tracing errors
            # print("Datos existentes en el annotation_data. Actualizamos")
            annotation_reactive$data <- annotation_reactive$data %>% 
                mutate(
                    project_id = ifelse(user_id==session$userData$user &
                                            document_id == datos_reactive$data$document_id[sel_row()] &
                                            span_ini == datos_reactive$data$span_ini[sel_row()] &
                                            span_end == datos_reactive$data$span_end[sel_row()],session$userData$current_project, project_id),
                    annotation_id = ifelse(user_id==session$userData$user &
                                               document_id == datos_reactive$data$document_id[sel_row()] &
                                               span_ini == datos_reactive$data$span_ini[sel_row()] &
                                               span_end == datos_reactive$data$span_end[sel_row()],current_annotation_id, annotation_id),
                    num_codes =  ifelse(user_id==session$userData$user &
                                            document_id == datos_reactive$data$document_id[sel_row()] &
                                            span_ini == datos_reactive$data$span_ini[sel_row()] &
                                            span_end == datos_reactive$data$span_end[sel_row()], input[["number_codes"]], num_codes),
                    is_abrev =  ifelse(user_id==session$userData$user &
                                           document_id == datos_reactive$data$document_id[sel_row()] &
                                           span_ini == datos_reactive$data$span_ini[sel_row()] &
                                           span_end == datos_reactive$data$span_end[sel_row()], input[[reactive_values$abbrev_id()]], is_abrev),
                    is_composite =  ifelse(user_id==session$userData$user &
                                               document_id == datos_reactive$data$document_id[sel_row()] &
                                               span_ini == datos_reactive$data$span_ini[sel_row()] &
                                               span_end == datos_reactive$data$span_end[sel_row()], input[[reactive_values$composite_id()]], is_composite),
                    need_context =  ifelse(user_id==session$userData$user &
                                               document_id == datos_reactive$data$document_id[sel_row()] &
                                               span_ini == datos_reactive$data$span_ini[sel_row()] &
                                               span_end == datos_reactive$data$span_end[sel_row()], input[[reactive_values$context_id()]], need_context),
                    previously_annotated =  ifelse(user_id==session$userData$user &
                                                       document_id == datos_reactive$data$document_id[sel_row()] &
                                                       span_ini == datos_reactive$data$span_ini[sel_row()] &
                                                       span_end == datos_reactive$data$span_end[sel_row()], input[["previously_annotated"]], previously_annotated),
                    is_wrong =  ifelse(user_id==session$userData$user &
                                           document_id == datos_reactive$data$document_id[sel_row()] &
                                           span_ini == datos_reactive$data$span_ini[sel_row()] &
                                           span_end == datos_reactive$data$span_end[sel_row()], input[["wrong_mention"]], is_wrong),
                    codes =  ifelse(user_id==session$userData$user &
                                        document_id == datos_reactive$data$document_id[sel_row()] &
                                        span_ini == datos_reactive$data$span_ini[sel_row()] &
                                        span_end == datos_reactive$data$span_end[sel_row()], list(code_list), codes),
                    sem_rels =  ifelse(user_id==session$userData$user &
                                           document_id == datos_reactive$data$document_id[sel_row()] &
                                           span_ini == datos_reactive$data$span_ini[sel_row()] &
                                           span_end == datos_reactive$data$span_end[sel_row()], list(semrel_list), sem_rels),
                    total_time = ifelse(user_id==session$userData$user &
                                      document_id == datos_reactive$data$document_id[sel_row()] &
                                      span_ini == datos_reactive$data$span_ini[sel_row()] &
                                      span_end == datos_reactive$data$span_end[sel_row()], total_time+duration, total_time),
                    list_times = ifelse(user_id==session$userData$user &
                                            document_id == datos_reactive$data$document_id[sel_row()] &
                                            span_ini == datos_reactive$data$span_ini[sel_row()] &
                                            span_end == datos_reactive$data$span_end[sel_row()], lapply(list_times, function(existing_list) c(existing_list, duration)), list_times),
                    annotation_comments = ifelse(user_id==session$userData$user &
                                                 document_id == datos_reactive$data$document_id[sel_row()] &
                                                 span_ini == datos_reactive$data$span_ini[sel_row()] &
                                                 span_end == datos_reactive$data$span_end[sel_row()],input[["annotation_comments"]],annotation_comments )
                    
                )

            
        }else{
            # If anotation is NOT present in the dataframe, we incorpore a new row
            # Logs for tracing errors
            # print("Datos NO existentes en el annotation_data. INCORPORAMOS")
            # Logs for tracing errors (to see the format of code and semrel list)
            # asd_code_list <<- code_list
            # asd_semrel_list <<- semrel_list
            # Generate the row with the data
            new_row<- list(
                project_id= session$userData$current_project,
                document_id = datos_reactive$data$document_id[sel_row()],
                span_ini = datos_reactive$data$span_ini[sel_row()],
                span_end = datos_reactive$data$span_end[sel_row()],
                annotation_id = current_annotation_id,
                user_id = session$userData$user,
                num_codes = input[["number_codes"]],
                is_abrev = input[[reactive_values$abbrev_id()]],
                is_composite = input[[reactive_values$composite_id()]],
                need_context = input[[reactive_values$context_id()]],
                previously_annotated = input[["previously_annotated"]],
                is_wrong = input[["wrong_mention"]],
                codes = ifelse(!is.null(code_list), list(code_list), list("prev_annotated")) ,
                sem_rels = ifelse(!is.null(semrel_list), list(semrel_list), list("prev_annotated")),
                text = datos_reactive$data$span[sel_row()],
                total_time = duration,
                list_times = list(duration),
                annotation_comments = input[["annotation_comments"]]
            )
            names(new_row$codes) <- "codes"
            names(new_row$sem_rels) <- "sem_rels"
            names(new_row$list_times) <- "list_times"
            # Include the row into annotation_reactive dataframe
            annotation_reactive$data <- rbind(annotation_reactive$data, new_row)
        }
        # Logs for tracing errors
        # asdasdasd <<- annotation_reactive$data
        
        # Update the datatable to see changes
        proxy %>% DT::replaceData(datos_reactive$data)
        
        # Get current annotation in the dataframe to save into database
        current_annotation <- annotation_reactive$data%>% filter(annotation_id == current_annotation_id,
                                                                 project_id == session$userData$current_project,
                                                                 user_id == session$userData$user)
        # # Save the annotation in annotation collection
        save_annotation_in_db(session$userData$annotation_db_endpoint,current_annotation, datos_reactive$data$span[sel_row()],UPDATE_ALL)
        # # print("ANOTACION GUARDADA")
        # 
        # # Update the status in mentions collection (changing the field associated to the user_id inside validated_by)
        update_mention_in_db(session$userData$mentions_db_endpoint,datos_reactive$data[sel_row(),],session$userData$user, UPDATE_ALL)
        # print("MENCION ACTUALIZADA")
        sel_row(NULL)
        reactive_values$timer(NULL)
        # Update reactive value of prev_annotated and wrong_mention (that inhabilitate part of the inputs)
        reactive_values$prev_annotated(input[["previously_annotated"]])
        reactive_values$is_wrong(input[["wrong_mention"]])
        reactive_values$annotation_comments("")
    })
}


