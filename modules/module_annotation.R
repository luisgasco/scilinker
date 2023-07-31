library(shinyjs)
library(DT)
library(mongolite)
library(jsonlite)
library(dplyr)
library(shinyWidgets)
library(fontawesome)

shinyjs::useShinyjs()
print(getwd() )
source("/scilinker/modules/error_handling.R")
source("/scilinker/modules/annotation_utils/annotation_ui_utils.R")
source("/scilinker/modules/annotation_utils/annotation_data_utils.R")
source("/scilinker/modules/annotation_utils/annotation_backend_utils.R")


generalAnnotatorInterfaceUI <- function(id)
{
    ns <- NS(id)
    tabItem(
        tabName = "anotaciones", fluidRow(
            column(
                8, box(
                    width = 12, status = "danger",
                    height = 500, shinycssloaders::withSpinner(tableAnnotatorUI(ns("mytable")))
                ),
                box(
                    width = 12, title = "Text",
                    height = 250, textAnnotatorUI(ns("texto_output"))
                )
            ),
            column(
                4, box(
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
    
    # Inicializo los datos_reactive y annotation_reactive, y después cada vez se cambie el proyecto
    # se cambian
    datos_reactive <- reactiveValues(data = data.frame())
    annotation_reactive <- reactiveValues(
        data = data.frame()
    )
    # sel_row reactive value
    sel_row <- reactiveVal()
    # Creamos reactive values
    context_id <- reactiveVal()
    composite_id <- reactiveVal()
    abbrev_id <- reactiveVal()
    num_codes <- reactiveVal(1)
    show_text <- reactiveVal(FALSE)
    prev_annotated <- reactiveVal(FALSE)
    is_abb <- reactiveVal(FALSE)
    is_composite <- reactiveVal(FALSE)
    need_context <- reactiveVal(FALSE)
    # Creamos lista de reactive_values para acceder 
    reactive_values <- list(
        context_id = context_id, composite_id = composite_id, abbrev_id = abbrev_id,
        num_codes = num_codes, show_text = show_text, prev_annotated = prev_annotated,
        is_abb = is_abb, is_composite = is_composite, need_context = need_context
    )
    
    # Update reactive_data when changing project
    observeEvent(session$userData$current_project, {
        showModal(modalDialog(tags$div(
                                            style = "display: flex; flex-direction: column; align-items: center;",
                                            tags$div(
                                                style = "display: flex; align-items: center; margin-bottom: 10px;",
                                                tags$img(src = 'small_logo.png', title = "small logo", height = "40px"), # Ajusta el valor de height para cambiar el tamaño de la imagen
                                                tags$p(style = "font-weight: bold; text-align: center; margin-left: 0px;", "     Loading... This may take a while")
                                            ),
                                        tags$p(HTML('<img src="loading.gif">'))
                                    ),
                              footer=NULL, size="s", fade=TRUE))
        print("LLEGA al observe_event_module_annotation")
        # Reiniciar los datos reactivos cargando los datos del nuevo proyecto
        datos_reactive$data = loadData(session, session$userData$data, session$userData$user, session$userData$current_project)
        annotation_reactive$data = loadAnnotations(session, session$userData$user, session$userData$current_project)
        # Reinitialize reactive variables each time user change project
        context_id(
            gsub(
                "[.#-]", "_", paste0("contextx_", datos_reactive$data[1, ]$document_id)
            )
        )
        composite_id(
            gsub(
                "[.#-]", "_", paste0("compositex_", datos_reactive$data[1, ]$document_id)
            )
        )
        abbrev_id()  
        num_codes(1)    
        show_text(FALSE) 
        prev_annotated(FALSE) 
        is_abb(FALSE)    
        is_composite(FALSE)   
        need_context(FALSE)  
        
        print("fin_primer_observe")
        
        removeModal()
    }, ignoreNULL = FALSE)
    
    
    # Realizar la consulta a la base de datos y obtener los
    # resultados Unimos los datos de texto con el de menciones
    # print(paste('outer = ', datos_reactive$data))
    proxy <- dataTableProxy(ns("mytable"))
    
    # Load diccionario. más adelante se hará leyendo de BB.DD
    diccionario <<- loadDict(abspath2dicc)
    
    # cntinue
    print(session$userData$projects)
    
    # datos_annotation <<- isolate(annotation_reactive$data)
    callModule(tableAnnotator, "mytable", datos_reactive, sel_row)
    callModule(
        textAnnotator, "texto_output", datos_reactive, sel_row, reactive_values
    )
    callModule(
        pannelAnnotator, "pannel_output", datos_reactive, sel_row, reactive_values,
        annotation_reactive, proxy, diccionario
    )
    
}

tableAnnotatorUI <- function(id)
{
    ns <- NS(id)
    DT::DTOutput(ns('mytable'),height = "500px")
}
tableAnnotator <- function(input, output, session,datos_reactive,sel_row)
{
    ns <- session$ns
    observe({
        sel_row(input$mytable_rows_selected)
    })
    output$mytable = DT::renderDataTable({  
        DT::datatable(datos_reactive$data,rownames=FALSE,
                      options = list(
                          deferRender = TRUE,
                          scrollY = 400,
                          scrollX=TRUE,
                          scroller = TRUE,
                          autoWidth = TRUE,
                          columnDefs = list(list(visible=FALSE, targets=c( 6, 7, 8, 9, 10))),#2,
                          order = list(list(1,'asc'))
                      ),
                      selection ="single",
                      extensions = c("Scroller")) %>% #'Responsive',
            DT::formatStyle( 'validated',
                             target = 'row',
                             backgroundColor = DT::styleEqual(c(0, 1,2), c('#f4f4f4', '#cbffe0','#fffddc')),
                             fontWeight = 'bold')
    },server = TRUE
    )
    return(list(sel_row=sel_row)) # Return sel_row value to the parent module
}

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
            ## thisrow_sel <- input$mytable_rows_selected Get
            ## filename related to the mention from dataframe.
            file_name <- unlist(datos_reactive$data[sel_row, ]$document_id)[1]  #'es-S0210-56912007000900007-3'
            # Build the query
            query_get_text <- paste0("{", "\"filename_id\"", ":\"", file_name, "\"}")
            # Get Text
            texto <- datos_reactive$data[sel_row, ]$text[1]
            # DATA WILL BE SAVE WITH A BUTTON If row is not selected,
            # don't show anything.
            if (!is.null(sel_row) )
            {
                HTML(
                    calcula_texto(TRUE, datos_reactive$data[sel_row, ], texto, "clase_show")
                )
            }
        }
    )
}

pannelAnnotatorUI <- function(id)
{
    ns <- NS(id)
    uiOutput(ns("info_code"))
}
pannelAnnotator <- function(input, output, session,datos_reactive,sel_row,reactive_values,annotation_reactive,proxy, diccionario){
    ns <- session$ns
    
    
    
    # Reactivity change of row:
    observeEvent(sel_row(),{
        reactive_values$is_abb(FALSE)
        reactive_values$is_composite(FALSE)
        reactive_values$need_context(FALSE)
        reactive_values$show_text(FALSE)
        reactive_values$prev_annotated(FALSE)
        reactive_values$num_codes(1)
    })
    
    
    # Reactivity need_context button.  # FUNCIONA
    observeEvent(input[[reactive_values$context_id()]],{
        if (input[[reactive_values$context_id()]]==TRUE){
            shinyjs::enable("full_text")
        }else if(input[[reactive_values$context_id()]]==FALSE){
            shinyjs::disable("full_text")
        }
    })
    
    # # Reactivity is_composite buttons:
    observeEvent(input[[reactive_values$composite_id()]],{
        print("Observe - is_composite")
        if (input[[reactive_values$composite_id()]]==TRUE){
            shinyjs::enable("number_codes")
            shinyjs::enable("update_codes")
            updateAwesomeCheckbox(session, reactive_values$composite_id(),TRUE)
            print("Observe - is_composite - TRUE")
            # reactive_values$is_composite(TRUE)
        }else if(input[[reactive_values$composite_id()]]==FALSE){
            shinyjs::disable("number_codes")
            shinyjs::disable("update_codes")
            print("Observe - is_composite - FALSE")
            print(reactive_values$composite_id())
            updateAwesomeCheckbox(session, reactive_values$composite_id(),FALSE)
            # reactive_values$is_composite(FALSE)
        }
    }, ignoreNULL = TRUE)
    observeEvent(input[["update_codes"]],{
        updateNumericInput(session,"number_codes",value = input[["number_codes"]])
        reactive_values$num_codes(input[["number_codes"]])
        updateAwesomeCheckbox(session, reactive_values$composite_id(),TRUE)
        reactive_values$is_composite(TRUE)
        reactive_values$prev_annotated(input[["previously_annotated"]])
        reactive_values$need_context(input[["need_context"]])
        # Actualizamos data frame paraque no haya problemas al guardar datos
        update_logical_values_df(input,session, annotation_reactive,proxy,sel_row(),datos_reactive,reactive_values)
        # Actualizar is_composite si num_codes == 1
        if(input[["number_codes"]]==1){
            reactive_values$is_composite(FALSE)
            updateAwesomeCheckbox(
                session = session, inputId = reactive_values$composite_id(),
                value = FALSE
            )
            
        }
        
        
    })
    
    # Reactivity previously_annotated button
    observeEvent(input[["previously_annotated"]],{
        print("Observe - previously_annotated")
        # Si el valor es TRUE
        if (input[["previously_annotated"]] ){
            #Deshabilitamos el bloque de anotaciones
            shinyjs::disable("candidate_list_elem")
            print("Observe - previously_annotated - TRUE")
        }
        else{
            shinyjs::enable("candidate_list_elem")
            print("Observe - previously_annotated - FALSE")
        }
        
    })
    
    output$info_code <- renderUI({
        print(sel_row())
        
        if(!is.null(sel_row())){
            # Filter dataframe
            dicc_filt <- filtra_dict(datos_reactive$data,diccionario,sel_row()) 
            print(reactive_values)
            # Get input ids
            reactive_values$abbrev_id(gsub("[.#-]","_",paste0("abbrevx_",datos_reactive$data[sel_row(),]$document_id,"#",datos_reactive$data[sel_row(),]$span_ini,"#",datos_reactive$data[sel_row(),]$span_end)))
            reactive_values$composite_id(gsub("[.#-]","_",paste0("compositex_",datos_reactive$data[sel_row(),]$document_id,"#",datos_reactive$data[sel_row(),]$span_ini,"#",datos_reactive$data[sel_row(),]$span_end)))
            reactive_values$context_id(gsub("[.#-]","_",paste0("contextx_",datos_reactive$data[sel_row(),]$document_id,"#",datos_reactive$data[sel_row(),]$span_ini,"#",datos_reactive$data[sel_row(),]$span_end)))
            ## RENDER PART OF THE PANEL
            asd <<- datos_reactive$data
            asd2 <<- annotation_reactive$data
            asd22<<- dicc_filt
            # Función para generar la interfaz reactiva para cada uno de las menciones.
            # Cada mención tendrá su propia lista de codigos, etc.
            # Los datos_reactivos serán data_reactive$data
            # Seleccionamos sel_row(). Si el identificador está en annotation_reactive
            # cambiamos los reactive_values y los cargamos en la interfaz
            fila_elegida = datos_reactive$data[sel_row(),]
            annotation_id_current = paste0(fila_elegida$document_id,"_",fila_elegida$span_ini, "_", fila_elegida$span_end)
            print("CURRENT ANNOTATION")
            print(annotation_id_current)
            print(annotation_reactive$data)
            # reactive_values$show_text(FALSE)
            # Verificar si annotation_reactive$data está vacío
            if (nrow(annotation_reactive$data) > 0) {
                anotacion_existente <- annotation_reactive$data %>% 
                    filter(user_id == session$userData$user,
                           annotation_id == annotation_id_current)
                print(nrow(anotacion_existente))
                if (nrow(anotacion_existente) == 1) {
                    print("DATOS_DE_INICIO")
                    print("reactive")
                    print(reactive_values$num_codes())
                    print("annotation")
                    print(anotacion_existente$num_codes)
                    print("TOCA ACTUALIZAR")
                    reactive_values$prev_annotated(anotacion_existente$previously_annotated)
                    reactive_values$is_abb(anotacion_existente$is_abrev)
                    reactive_values$is_composite(anotacion_existente$is_composite)
                    reactive_values$need_context(anotacion_existente$need_context)
                    reactive_values$num_codes(anotacion_existente$num_codes)
                    reactive_values$show_text(TRUE)
                    reactive_values$show_text(FALSE)
                    print("FALLO_ACTUAL_FUERA_GENERATE_PANEL")
                    print(anotacion_existente)
                    print(anotacion_existente %>% select(codes, sem_rels))
                    test_dicc_filt_erase <<- dicc_filt
                    anotacion_existente_output <<- anotacion_existente
                    output_reactive_values <<- reactive_values
                    # browser()
                    generate_panel(ns, datos_reactive, reactive_values, dicc_filt, sel_row(), anotacion_existente %>% select(codes, sem_rels), update = TRUE)
                } else {
                    
                    generate_panel(ns, datos_reactive, reactive_values, dicc_filt, sel_row(), anotacion_existente, update = FALSE)
                }
            } else {
                
                # Si annotation_reactive$data está vacío, se llama a generate_panel con update=FALSE
                generate_panel(ns, datos_reactive, reactive_values, dicc_filt, sel_row(), NULL, update = FALSE)
            }
            
            
            
        }
    })
    
    # Reactivity save_data button
    observeEvent(input[["save_data"]],{
        print("Observe - save_data")
        dicc_filt <- filtra_dict(datos_reactive$data,diccionario, sel_row())
        dicc_filt_OUT<<- dicc_filt
        print("llega aquí")
        
        ## INPUT VALIDATION
        error_occurred <- reactiveVal(FALSE)
        input_validation(input, dicc_filt, reactive_values,datos_reactive, sel_row(), error_occurred)
        # Si la bandera de error_occurred es TRUE, salimos del observeEvent
        if (error_occurred()) {
            return()
        }
        
        # First, let's read the data!
        # Iteramos para los valores
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
        
        print(paste0("Lista de códigos seleccionada: ", code_list))
        print(paste0("Lista de rels seleccionada: ", semrel_list))
        
        # ACtualizamos datos reactivos
        # Si user toca save, cambiamos valor de "user_id", "validated" y "previously_annotated". Además cambiamos valor de validated_by que corresponda.
        datos_reactive$data[sel_row(),] <- datos_reactive$data %>%
            filter(document_id == datos_reactive$data$document_id[sel_row()],
                   span_ini == datos_reactive$data$span_ini[sel_row()],
                   span_end == datos_reactive$data$span_end[sel_row()]) %>%
            mutate(user_id = session$userData$user,
                   validated = ifelse(input[["previously_annotated"]], 2, 1),
                   previously_annotated = ifelse(input[["previously_annotated"]], TRUE, FALSE))
        # Tomamos valor new_state y new_preannotated para guardar dentro del validated_by
        new_state <- datos_reactive$data %>%  filter(user_id == session$userData$user,
                                                     document_id == datos_reactive$data$document_id[sel_row()],
                                                     span_ini == datos_reactive$data$span_ini[sel_row()],
                                                     span_end == datos_reactive$data$span_end[sel_row()] ) %>% select(validated)
        new_prevannotated <- datos_reactive$data %>%  filter(user_id == session$userData$user,
                                                             document_id == datos_reactive$data$document_id[sel_row()],
                                                             span_ini == datos_reactive$data$span_ini[sel_row()],
                                                             span_end == datos_reactive$data$span_end[sel_row()] ) %>% select(previously_annotated)
        
        # Actualizamos el validated_by dataframe
        datos_reactive$data[sel_row(),]$validated_by[[1]] <- datos_reactive$data[sel_row(),]$validated_by[[1]] %>%
            mutate(state= ifelse(user_id==session$userData$user,new_state$validate$state,state),
                   previously_annotated = ifelse(user_id==session$userData$user,new_prevannotated$previously_annotated$previously_annotated,previously_annotated))
        
        
        # Actualizamos datos menciones TODO
        # Checkamos si existe un identificador de mención ya guardado dentro. 
        # Si existe lo actualizamos
        current_annotation_id <- paste0(datos_reactive$data[sel_row(),]$document_id,"_",datos_reactive$data[sel_row(),]$span_ini,"_",datos_reactive$data[sel_row(),]$span_end)
        is_present <- current_annotation_id %in% annotation_reactive$data$annotation_id[annotation_reactive$data$user_id == session$userData$user]
        print(paste0("ESTE ELEMENTO ESTá PRESENTE? ", is_present))
        if(is_present){
            print("Datos existentes en el annotation_data. Actualizamos")
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
                    codes =  ifelse(user_id==session$userData$user &
                                        document_id == datos_reactive$data$document_id[sel_row()] &
                                        span_ini == datos_reactive$data$span_ini[sel_row()] &
                                        span_end == datos_reactive$data$span_end[sel_row()], list(code_list), codes),
                    sem_rels =  ifelse(user_id==session$userData$user &
                                           document_id == datos_reactive$data$document_id[sel_row()] &
                                           span_ini == datos_reactive$data$span_ini[sel_row()] &
                                           span_end == datos_reactive$data$span_end[sel_row()], list(semrel_list), sem_rels),
                    
                    
                )
            
            
            asdasdasdafter <<- annotation_reactive$data
            
        }else{
            print("Datos NO existentes en el annotation_data. INCORPORAMOS")
            print("USUARIO_ACTUAL:")
            print(session$userData$user)
            print(annotation_reactive$data)
            print("VALORES code_list y sem_rels")
            print(list(code_list))
            print(list(semrel_list))
            asd_code_list <<- code_list
            asd_semrel_list <<- semrel_list
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
                codes = ifelse(!is.null(code_list), list(code_list), list("prev_annotated")) ,
                sem_rels = ifelse(!is.null(semrel_list), list(semrel_list), list("prev_annotated")),
                text = datos_reactive$data$span[sel_row()]
            )
            names(new_row$codes) <- "codes"
            names(new_row$sem_rels) <- "sem_rels"
            # VAMOS A VER CUAL ES EL RESULTADO AL GUARDAR EN PREVIOUSLY_ANNOTATED ==TRUE e intentar replicar ese formato en codes y sem_rels
            # con la condición anterior
            print("PRINTEMAMOS NEW_ROW")
            print(new_row)
            # SI input[["previously_annotated"]] es true y nrows(annotation_reactiva$data es 0, hay que modificar los sem_rels)
            annotation_reactive$data <- rbind(annotation_reactive$data, new_row)
            print("USUARIO_ACTUAL_FINAL")
            
        }
        
        asdasdasd <<- annotation_reactive$data
        
        # Actualizamos tabla
        proxy %>% DT::replaceData(datos_reactive$data)
        asdasdasdasdsadasd_Despues<<-datos_reactive$data
        current_annotation <- annotation_reactive$data%>% filter(annotation_id == current_annotation_id,
                                                                 project_id == session$userData$current_project,
                                                                 user_id == session$userData$user)
        
        # Guardamos la anotación en annotation collection
        save_annotation_in_db(session$userData$annotation_db_endpoint,current_annotation, datos_reactive$data$span[sel_row()])
        print("ANOTACION GUARDADA")
        # Actualizamos el estado en menciones collection (cambiando el campo asociado al user_id dentro de validated_by)
        update_mention_in_db(session$userData$mentions_db_endpoint,datos_reactive$data[sel_row(),],session$userData$user)
        print("MENCION ACTUALIZADA")
        reactive_values$prev_annotated(input[["previously_annotated"]])
    })
    
    
}
    
    # Esta para la de update mention https://sharegpt.com/c/mRSpjko



