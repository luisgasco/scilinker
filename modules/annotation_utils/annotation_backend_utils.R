# Calcula el texto con la mención highlightes
calcula_texto = function(valor_entrada,datos_filtrados_fila,texto, style_class) {
    # HABRIA QUE HACER LA QUERYT A LA BASE DE DATOS DE TEXTOS: filename_id = unlist(strsplit(datos_filtrados_fila$filename_id, split = "#"))[1])
    # Recuperar ese texto y guardarlo en "texto"
    if(valor_entrada){
        #IF True, cogemos valor inicial y final
        span_ini = as.integer(datos_filtrados_fila$span_ini)
        span_end = as.integer(datos_filtrados_fila$span_end)
        # texto = datos_filtrados_fila$text
        subcadena <- substr(texto, span_ini, span_end)
        parte1 <- substr(texto, 1, span_ini-1)
        parte2 <- substr(texto, span_end+1, nchar(texto))
        label_ini = paste0("<span class=",style_class,">")
        label_end = paste0("</span class=",style_class,">")
        resultado <- paste0(parte1," ",label_ini, subcadena,label_end ," ",parte2)
    }else{
        resultado=""
    }
    return(resultado)
}

# Actualiza valores dataframe 
update_logical_values_df <- function(input,session, annotation_reactive,proxy,row_sel,datos_reactive,reactive_values){
    dicc_filt <- filtra_dict(datos_reactive$data,diccionario, row_sel)
    # First, let's read the data!
    # Iteramos para los valores
    
    code_list <- rep("", input[["number_codes"]])
    semrel_list <- rep("", input[["number_codes"]])
    
    print(paste0("Lista de códigos seleccionada: ", code_list))
    print(paste0("Lista de rels seleccionada: ", semrel_list))
    
    # Actualizamos valores del dataframe para evitar errores al actualizar reactive values.
    current_annotation_id <- paste0(datos_reactive$data[row_sel,]$document_id,"_",datos_reactive$data[row_sel,]$span_ini,"_",datos_reactive$data[row_sel,]$span_end)
    is_present <- current_annotation_id %in% annotation_reactive$data$annotation_id[annotation_reactive$data$user_id == session$userData$user]
    if(is_present){
        print("Datos existentes en el annotation_data. Actualizamos")
        annotation_reactive$data <- annotation_reactive$data %>% 
            mutate(
                project_id = ifelse(user_id==session$userData$user &
                                        document_id == datos_reactive$data$document_id[row_sel] &
                                        span_ini == datos_reactive$data$span_ini[row_sel] &
                                        span_end == datos_reactive$data$span_end[row_sel],session$userData$current_project, project_id),
                annotation_id = ifelse(user_id==session$userData$user &
                                           document_id == datos_reactive$data$document_id[row_sel] &
                                           span_ini == datos_reactive$data$span_ini[row_sel] &
                                           span_end == datos_reactive$data$span_end[row_sel],current_annotation_id, annotation_id),
                num_codes =  ifelse(user_id==session$userData$user &
                                        document_id == datos_reactive$data$document_id[row_sel] &
                                        span_ini == datos_reactive$data$span_ini[row_sel] &
                                        span_end == datos_reactive$data$span_end[row_sel], input[["number_codes"]], num_codes),
                is_abrev =  ifelse(user_id==session$userData$user &
                                       document_id == datos_reactive$data$document_id[row_sel] &
                                       span_ini == datos_reactive$data$span_ini[row_sel] &
                                       span_end == datos_reactive$data$span_end[row_sel], input[[reactive_values$abbrev_id()]], is_abrev),
                is_composite =  ifelse(user_id==session$userData$user &
                                           document_id == datos_reactive$data$document_id[row_sel] &
                                           span_ini == datos_reactive$data$span_ini[row_sel] &
                                           span_end == datos_reactive$data$span_end[row_sel], input[[reactive_values$composite_id()]], is_composite),
                need_context =  ifelse(user_id==session$userData$user &
                                           document_id == datos_reactive$data$document_id[row_sel] &
                                           span_ini == datos_reactive$data$span_ini[row_sel] &
                                           span_end == datos_reactive$data$span_end[row_sel], input[[reactive_values$context_id()]], need_context),
                previously_annotated =  ifelse(user_id==session$userData$user &
                                                   document_id == datos_reactive$data$document_id[row_sel] &
                                                   span_ini == datos_reactive$data$span_ini[row_sel] &
                                                   span_end == datos_reactive$data$span_end[row_sel], input[["previously_annotated"]], previously_annotated),
                codes =  ifelse(user_id==session$userData$user &
                                    document_id == datos_reactive$data$document_id[row_sel] &
                                    span_ini == datos_reactive$data$span_ini[row_sel] &
                                    span_end == datos_reactive$data$span_end[row_sel], list(code_list), codes),
                sem_rels =  ifelse(user_id==session$userData$user &
                                       document_id == datos_reactive$data$document_id[row_sel] &
                                       span_ini == datos_reactive$data$span_ini[row_sel] &
                                       span_end == datos_reactive$data$span_end[row_sel], list(semrel_list), sem_rels),
                
            )
    }else{
        print("Datos NO existentes en el annotation_data. INCORPORAMOS")
        new_row<- list(
            project_id= session$userData$current_project,
            document_id = datos_reactive$data$document_id[row_sel],
            span_ini = datos_reactive$data$span_ini[row_sel],
            span_end = datos_reactive$data$span_end[row_sel],
            annotation_id = current_annotation_id,
            user_id = session$userData$user,
            num_codes = as.numeric(input[["number_codes"]]),
            is_abrev = input[[reactive_values$abbrev_id()]],
            is_composite = input[[reactive_values$composite_id()]],
            need_context = input[[reactive_values$context_id()]],
            previously_annotated = input[["previously_annotated"]],
            codes = list(code_list),
            sem_rels = list(semrel_list),
            text = datos_reactive$data$span[row_sel]
        )
        names(new_row$codes) <- "codes"
        names(new_row$sem_rels) <- "sem_rels"
        # Check data types of dataframe columns
        annotation_reactive$data <- rbind(annotation_reactive$data, new_row)
    }
    
    # Actualizamos tabla
    # proxy %>% DT::replaceData(datos_reactive$data)
}

get_user_status <- function(validated_by_df,username) {
    user_rows <- validated_by_df[validated_by_df$user_id == username, ]
    state_value <- user_rows$state
    previously_annotated_value <- user_rows$previously_annotated
    return(data.frame(state = user_rows$state, previously_annotated = user_rows$previously_annotated))
}
update_mention_in_db <- function(db_con, mention, user){
    query <- toJSON(list(
        project_id = mention$project_id[1],
        document_id = mention$document_id[1],
        span_ini = mention$span_ini[1],
        span_end = mention$span_end[1],
        mention_class = mention$mention_class[1],
        "validated_by.user_id" = user
    ), auto_unbox  = TRUE)
    
    user_status <- lapply(mention$validated_by, get_user_status,username=user)
    
    # Datos para actualizar en el documento
    update_data <- toJSON(list(
        "$set" = list(
            "validated_by.$.state" = user_status[[1]]$state,
            "validated_by.$.previously_annotated" = user_status[[1]]$previously_annotated
        )
    ), auto_unbox  = TRUE)
    
    # Realiza la actualización en MongoDB
    db_con$update(
        query = query,
        update = update_data
    )
}

save_annotation_in_db <- function(db_con, anotation, texto){
    # Generate query
    query <- toJSON(list(
        project_id = anotation$project_id[1],  # Accede al primer elemento de la lista
        annotation_id = anotation$annotation_id[1],  # Accede al primer elemento de la lista
        user_id = anotation$user_id[1]  # Accede al primer elemento de la lista
    ), auto_unbox  = TRUE)
    
    # Verify if entry exists in connection (given in session attribute)
    existing_entry <- db_con$find(query)
    # Update existing entry or create a new one
    if (length(existing_entry) > 0) {
        # Si ya existe una entrada con datos iguales, actualizamos esa entrada
        db_con$update(query, 
                      jsonlite::toJSON(list(
                          "$set" = list(
                              document_id = anotation$document_id[1],
                              span_ini = anotation$span_ini[1],
                              span_end = anotation$span_end[1],
                              num_codes = anotation$num_codes[1],
                              is_abrev = anotation$is_abrev[1],
                              is_composite = anotation$is_composite[1],
                              need_context = anotation$need_context[1],
                              previously_annotated = anotation$previously_annotated[1],
                              codes = c(anotation[1,]$codes), #list(ifelse(is.list(anotation[1,]$codes),anotation[1,]$codes,list(anotation[1,]$codes))),
                              sem_rels = c(anotation[1,]$sem_rels) #list(ifelse(is.list(anotation[1,]$sem_rels),anotation[1,]$sem_rels,list(anotation[1,]$sem_rels))) ,
                          )
                      ), auto_unbox  = TRUE))
        cat("Entrada actualizada.")
    } else {
        # Si no existe una entrada con datos iguales, incorporamos la anotacion
        # que está guarda en el reactive "anotacion"
        
        
        anotation[1,]$codes = c(anotation[1,]$codes)#list(ifelse(is.list(anotation[1,]$codes),anotation[1,]$codes,list(anotation[1,]$codes)))
        anotation[1,]$sem_rels = c(anotation[1,]$sem_rels)#list(ifelse(is.list(anotation[1,]$sem_rels),anotation[1,]$sem_rels,list(anotation[1,]$sem_rels)) )
        db_con$insert(anotation[1,])
        #anotacion$text tiene que ser una lista
        cat("Nueva entrada incorporada.")
    }
}

