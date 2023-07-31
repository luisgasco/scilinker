# Obten lista de sinonimos en formato output dada una lista de strings
get_list_sins <- function(lista_sinonimos){
    lista = div(
        lapply(1:length(lista_sinonimos), function(j) {
            p(HTML(paste0(j,". ",lista_sinonimos[j])))}))
    return(lista)
}
# Genera el output box para cada concepto
code_box2<- function(termino, sem_tag, codigo, lista_sinonimos_html, icon = NULL, color = "aqua", width = 4, href = NULL){
    boxContent <- div(class = "estilo_sct",
                      div(class="dot"),
                      div(class = "inner",
                          p(style="width: 100%", 
                            HTML(paste0("<b>",termino," (",sem_tag,")</b>"))),
                      ),
                      div(class = "codigos",
                          p(HTML(paste0("SCTID: ",codigo)))
                      ),
                      div(class = "sinonimos",
                          lista_sinonimos_html,
                      ),
                      
                      if (!is.null(icon)) div(class = "icon-large", icon)
    )
    
    if (!is.null(href))
        boxContent <- a(boxContent, href = href, target="_blank")
    
    div(class = if (!is.null(width)) paste0("col-sm-", width),
        boxContent
    )
}


# UI codes
listcodes <- function(ns,id_code, datos_reactive, reactive_values, dicc_filt,sel_row,current_annotation, update ){
    # fluidRow(
    if (update == FALSE){
        box(width = 12,
            class = "12_especial", 
            tags$div(style = "overflow-y: scroll;  max-height:300px",
                     tags$table(style="width:100%",
                                tags$colgroup(
                                    tags$col(span="1",style="width: 85%;"),
                                    tags$col(span="1",style="width: 7%;"),
                                    tags$col(span="1",style="width: 8%;")
                                ),
                                tags$tr(style="position: sticky;top: 0px; background-color: white;",
                                        tags$th("code", style="position: None;"),
                                        tags$th("check", style="position: None;"),
                                        tags$th("sem_tag", style="position: None;")
                                ),
                                lapply(1:nrow(dicc_filt), function(i) {
                                    lista_sinonimos <- unlist(dicc_filt$sinonimo.y[i])
                                    lista_sinonimos_html = get_list_sins(lista_sinonimos)
                                    tags$tr(
                                        tags$td(
                                            code_box2(codigo = dicc_filt$code[i], 
                                                      termino=dicc_filt$term[i],
                                                      sem_tag = dicc_filt$semantic_tag[i],
                                                      lista_sinonimos =lista_sinonimos_html,
                                                      href = dicc_filt$url[i],
                                                      width = 12)
                                        ),
                                        tags$td(
                                            prettyCheckbox(inputId = ns(gsub("[.#-]","_",paste0("code_num_",id_code,"_check_",dicc_filt$code[i],"_", datos_reactive$data$document_id[sel_row],"_",datos_reactive$data$span_ini[sel_row],"_",datos_reactive$data$span_end[sel_row]))),
                                                           label = "",
                                                           value = FALSE,#if(code==dicc_filt$code[i]) TRUE else FALSE,
                                                           icon = icon("check"),
                                                           status = "success",
                                                           animation = "rotate",
                                                           bigger = TRUE)
                                        ),
                                        tags$td(
                                            prettyRadioButtons(inputId = ns(gsub("[.#-]","_",paste0("code_num_",id_code,"_rel_",dicc_filt$code[i],"_", datos_reactive$data$document_id[sel_row],"_",datos_reactive$data$span_ini[sel_row],"_",datos_reactive$data$span_end[sel_row]))),
                                                               label= "",
                                                               choices = c("EXACT", "NARROW","NO_IDEA"),
                                                               selected = "",#if ((code==dicc_filt$code[i])&(sem_rel %in% c("EXACT", "NARROW","NO_IDEA"))) sem_rel else "",
                                                               icon = icon("check"),
                                                               status = "success",
                                                               animation = "jelly")
                                        )
                                    )
                                })
                                
                     ),
                     box(width = 12,
                         tags$tr(
                             tags$td(
                                 awesomeCheckbox(inputId = ns(paste0("code_num_",id_code,"_no_code_", datos_reactive$data$document_id[sel_row],"_",datos_reactive$data$span_ini[sel_row],"_",datos_reactive$data$span_end[sel_row])),
                                                 label = "Code not found in candidates", 
                                                 value = FALSE#no_code
                                 ),
                                 awesomeCheckbox(inputId = ns(paste0("code_num_",id_code,"_no_norm_", datos_reactive$data$document_id[sel_row],"_",datos_reactive$data$span_ini[sel_row],"_",datos_reactive$data$span_end[sel_row])),
                                                 label = "Code not found in ontology", 
                                                 value = FALSE#no_norm
                                 )
                                 
                             ),
                             tags$td(
                                 searchInput(
                                     inputId = ns(paste0("code_num_",id_code,"_","written_code_", datos_reactive$data$document_id[sel_row],"_",datos_reactive$data$span_ini[sel_row],"_",datos_reactive$data$span_end[sel_row])), 
                                     label = "Enter your search :", 
                                     value = "",
                                     placeholder = "Paste your code and click check", 
                                     btnSearch = icon("check"), 
                                     btnReset = icon("remove"), 
                                     width = "70%"
                                 ),
                                 prettyRadioButtons(inputId = ns(paste0("code_num_",id_code,"_","written_relation_", datos_reactive$data$document_id[sel_row],"_",datos_reactive$data$span_ini[sel_row],"_",datos_reactive$data$span_end[sel_row])),
                                                    label= "",
                                                    choices = c("EXACT", "NARROW","NO_IDEA"),
                                                    selected =  "",
                                                    icon = icon("check"),
                                                    status = "success",
                                                    animation = "jelly")
                             )
                             
                         )),
            ),
        )
    }else{
        # UPDATE VALUES DEPENDING VALUE OF CODES AND SEMANTIC_RELATION
        code2load <- current_annotation[1]
        semrel2load <- current_annotation[2]
        print("code2load")
        print(code2load)
        print("semrel2load")
        print(semrel2load)
        print("COMPROBACIONES_EXTRA")
        # if(is.na(code2load) || code2load != dicc_filt_OUT$code[2]){
        #    print("ESNA Y SERIA FALSE")
        # }  else {
        #    print("NO ES NA Y SERIA FALSE")
        # }
        
        box(width = 12,
            class = "12_especial", 
            tags$div(style = "overflow-y: scroll;  max-height:300px",
                     tags$table(style="width:100%",
                                tags$colgroup(
                                    tags$col(span="1",style="width: 85%;"),
                                    tags$col(span="1",style="width: 7%;"),
                                    tags$col(span="1",style="width: 8%;")
                                ),
                                tags$tr(style="position: sticky;top: 0px; background-color: white;",
                                        tags$th("code", style="position: None;"),
                                        tags$th("check", style="position: None;"),
                                        tags$th("sem_tag", style="position: None;")
                                ),
                                lapply(1:nrow(dicc_filt), function(i) {
                                    lista_sinonimos <- unlist(dicc_filt$sinonimo.y[i])
                                    lista_sinonimos_html = get_list_sins(lista_sinonimos)
                                    
                                    tags$tr(
                                        tags$td(
                                            code_box2(codigo = dicc_filt$code[i], 
                                                      termino=dicc_filt$term[i],
                                                      sem_tag = dicc_filt$semantic_tag[i],
                                                      lista_sinonimos =lista_sinonimos_html,
                                                      href = dicc_filt$url[i],
                                                      width = 12)
                                        ),
                                        tags$td(
                                            prettyCheckbox(inputId = ns(gsub("[.#-]","_",paste0("code_num_",id_code,"_check_",dicc_filt$code[i],"_", datos_reactive$data$document_id[sel_row],"_",datos_reactive$data$span_ini[sel_row],"_",datos_reactive$data$span_end[sel_row]))),
                                                           label = "",
                                                           value = if(is.na(code2load) || code2load != dicc_filt$code[i]) FALSE else TRUE,
                                                           icon = icon("check"),
                                                           status = "success",
                                                           animation = "rotate",
                                                           bigger = TRUE)
                                        ),
                                        tags$td(
                                            prettyRadioButtons(inputId = ns(gsub("[.#-]","_",paste0("code_num_",id_code,"_rel_",dicc_filt$code[i],"_", datos_reactive$data$document_id[sel_row],"_",datos_reactive$data$span_ini[sel_row],"_",datos_reactive$data$span_end[sel_row]))),
                                                               label= "",
                                                               choices = c("EXACT", "NARROW","NO_IDEA"),
                                                               selected = if (is.na(code2load) | is.na(semrel2load)) "" else if (code2load == dicc_filt$code[i] & semrel2load %in% c("EXACT", "NARROW", "NO_IDEA")) semrel2load else "",
                                                               icon = icon("check"),
                                                               status = "success",
                                                               animation = "jelly")
                                        )
                                    )
                                })
                                
                     ),
                     box(width = 12,
                         tags$tr(
                             tags$td(
                                 awesomeCheckbox(inputId = ns(paste0("code_num_",id_code,"_no_code_", datos_reactive$data$document_id[sel_row],"_",datos_reactive$data$span_ini[sel_row],"_",datos_reactive$data$span_end[sel_row])),
                                                 label = "Code not found in candidates", 
                                                 value =  if((!(code2load %in% dicc_filt$code) & (code2load != "NO_CODE") & !is.na(code2load))) TRUE else FALSE
                                 ),
                                 awesomeCheckbox(inputId = ns(paste0("code_num_",id_code,"_no_norm_", datos_reactive$data$document_id[sel_row],"_",datos_reactive$data$span_ini[sel_row],"_",datos_reactive$data$span_end[sel_row])),
                                                 label = "Code not found in ontology", 
                                                 value = if ((code2load == "NO_CODE") ||is.na(code2load)) TRUE else FALSE,#no_norm
                                 )
                                 
                             ),
                             tags$td(
                                 searchInput(
                                     inputId = ns(paste0("code_num_",id_code,"_","written_code_", datos_reactive$data$document_id[sel_row],"_",datos_reactive$data$span_ini[sel_row],"_",datos_reactive$data$span_end[sel_row])), 
                                     label = "Enter your search :", 
                                     value = if((!(code2load %in% dicc_filt$code) & (code2load != "NO_CODE"))||is.na(code2load)) code2load else "",
                                     placeholder = "Paste your code and click check", 
                                     btnSearch = icon("check"), 
                                     btnReset = icon("remove"), 
                                     width = "70%"
                                 ),
                                 prettyRadioButtons(inputId = ns(paste0("code_num_",id_code,"_","written_relation_", datos_reactive$data$document_id[sel_row],"_",datos_reactive$data$span_ini[sel_row],"_",datos_reactive$data$span_end[sel_row])),
                                                    label= "",
                                                    choices = c("EXACT", "NARROW","NO_IDEA"),
                                                    selected =  if(!(code2load %in% dicc_filt$code) & (code2load != "NO_CODE")&(semrel2load %in% c("EXACT", "NARROW","NO_IDEA"))) semrel2load else "",
                                                    icon = icon("check"),
                                                    status = "success",
                                                    animation = "jelly")
                             )
                             
                         )),
            ),
        )
    }
}

generate_panel <- function(ns,datos_reactive, reactive_values, dicc_filt, sel_row,current_annotation,update){
    # ns <- session$ns
    tags$div(
        fluidRow(
            column(6,tags$p(HTML(paste0("Mention: <b>",datos_reactive$data$span[sel_row],"</b>")))),
            column(6,
                   shinyWidgets::materialSwitch(
                       inputId = ns("previously_annotated"),
                       label = "Previously annotated", 
                       value = if(is.null(reactive_values$prev_annotated())) FALSE else reactive_values$prev_annotated(),
                       status = "warning"
                   )
            )
        ),
        tags$div(id=ns("candidate_list_elem"),
                 tags$p(HTML(paste0("<b>Normalization needed data:</b>"))),
                 fluidRow(
                     column(4,
                            awesomeCheckbox(inputId = ns(reactive_values$abbrev_id()), 
                                            label = "Is abbreviature?",
                                            value = reactive_values$is_abb())
                     ),
                     column(4,
                            req(awesomeCheckbox(inputId = ns(reactive_values$composite_id()),
                                                label = "Is composite?", 
                                                value = if(reactive_values$num_codes() !=1) TRUE else reactive_values$is_composite()))
                     ),
                     column(4,
                            awesomeCheckbox(inputId = ns(reactive_values$context_id()),
                                            label = "Need context?", 
                                            value = reactive_values$need_context())
                     ),
                 ),
                 fluidRow(
                     column(3,
                            shinyjs::disabled(
                                shinyWidgets::materialSwitch(
                                    inputId = ns("full_text"),
                                    label = "Show text", 
                                    status = "success",
                                    value = reactive_values$show_text(),
                                    right = FALSE
                                )
                            )
                     ),
                     column(9,
                            fluidRow(
                                
                                if(reactive_values$num_codes() ==1){
                                    div(
                                        column(6, 
                                               shinyjs::disabled(
                                                   numericInput(
                                                       ns("number_codes"),
                                                       label = "Number of codes",
                                                       value = reactive_values$num_codes(),
                                                       min = 1,
                                                       max = 10,
                                                       step = 1
                                                   )
                                               )),
                                        column(6,
                                               actionButton(ns("update_codes"),"Update codes:",style="")
                                        )
                                    )
                                    
                                    
                                    
                                    
                                }else{
                                    div(
                                        column(6, 
                                               numericInput(
                                                   ns("number_codes"),
                                                   label = "Number of codes",
                                                   value = reactive_values$num_codes(),
                                                   min = 1,
                                                   max = 10,
                                                   step = 1
                                               )),
                                        column(6,
                                               actionButton(ns("update_codes"),"Update codes:", style="")
                                        )
                                    )
                                }, style="display: flex;align-items: center;"
                            )
                            
                     )
                     
                     
                 ),
                 tags$p(HTML(paste0("<b>Normalization:</b>"))),
                 tags$div(style = "overflow-y: scroll;  max-height:475px",
                          num <- isolate(reactive_values$num_codes()),
                          if (update == FALSE){
                              lapply(1:num, function(i) {
                                  div( 
                                      HTML(paste0("<b>Code ",i, ":</b>")),
                                      listcodes(ns,i, datos_reactive, reactive_values, dicc_filt, sel_row,current_annotation, update)
                                  )
                                  # callModule(listcodes, "list_codes",datos_reactivos,num_codes(),dicc_filt,list_codes,
                                  #             row_sel, code, sem_rel,no_code,no_norm)
                              })
                          }else{
                              lapply(1:num, function(i) {
                                  #print("FALLO_ACTUAL")
                                  #print(current_annotation)
                                  i_code <- current_annotation$codes[[1]][i]
                                  i_sem_rel <- current_annotation$sem_rels[[1]][i]
                                  div( 
                                      HTML(paste0("<b>Code ",i, ":</b>")),
                                      listcodes(ns,i, datos_reactive, reactive_values, dicc_filt, sel_row,list(i_code,i_sem_rel), update)
                                  )
                                  # callModule(listcodes, "list_codes",datos_reactivos,num_codes(),dicc_filt,list_codes,
                                  #             row_sel, code, sem_rel,no_code,no_norm)
                              })
                          }
                 )
        ),
        fluidRow(
            column(12,
                   actionButton(ns("save_data"), "Save annotation", width='100%')
            )
        ),
        
    )
    
}
