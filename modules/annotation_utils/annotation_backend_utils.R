# Generate text with a mention highlighted
calcula_texto <- function(valor_entrada,datos_filtrados_fila,texto, style_class) {
    # If valor_entrada ==True
    if(valor_entrada){
        # Take initial and end character of the current mention
        span_ini = as.integer(datos_filtrados_fila$span_ini)
        span_end = as.integer(datos_filtrados_fila$span_end)
        # Obtain the preceding and following text of the mention
        subcadena <- substr(texto, span_ini, span_end)
        parte1 <- substr(texto, 1, span_ini-1)
        parte2 <- substr(texto, span_end+1, nchar(texto))
        # Generate a html label to highlight the text
        label_ini = paste0("<span class=",style_class,">")
        label_end = paste0("</span class=",style_class,">")
        # Rebuild the final text
        resultado <- paste0(parte1," ",label_ini, subcadena,label_end ," ",parte2)
    }else{
        resultado=""
    }
    return(resultado)
}

# Function to update dataframe values
# This is an auxiliar function to avoid problems when saving data
update_logical_values_df <- function(input,session, annotation_reactive,proxy,row_sel,datos_reactive,reactive_values){
    # Filter dictionary
    dicc_filt <- filtra_dict(datos_reactive$data,diccionario, row_sel)
    # Generate empty lists for codes and semantic relations
    code_list <- rep("", input[["number_codes"]])
    semrel_list <- rep("", input[["number_codes"]])
    # Logs for tracing errors
    # print(paste0("Lista de códigos seleccionada: ", code_list))
    # print(paste0("Lista de rels seleccionada: ", semrel_list))
    
    # Update dataframe values to avoid errores when updating reactive values. 
    # Get the current_annotation_id and compute in annotation is present in annotation dataframe.
    current_annotation_id <- paste0(datos_reactive$data[row_sel,]$document_id,"_",
                                    datos_reactive$data[row_sel,]$span_ini,"_",
                                    datos_reactive$data[row_sel,]$span_end)
    is_present <- current_annotation_id %in% annotation_reactive$data$annotation_id[annotation_reactive$data$user_id == session$userData$user]
    if(is_present){
        # Logs for tracing errors
        # print("Datos existentes en el annotation_data. Actualizamos")
        # Update data with current annotation
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
                is_wrong =  ifelse(user_id==session$userData$user &
                                       document_id == datos_reactive$data$document_id[row_sel] &
                                       span_ini == datos_reactive$data$span_ini[row_sel] &
                                       span_end == datos_reactive$data$span_end[row_sel], input[["wrong_mention"]], is_wrong),
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
        # Logs for tracing errors
        # print("Datos NO existentes en el annotation_data. INCORPORAMOS")
        # Create new row to incorpore to the annotation dataframe.
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
            is_wrong = input[["wrong_mention"]],
            codes = list(code_list),
            sem_rels = list(semrel_list),
            text = datos_reactive$data$span[row_sel]
        )
        names(new_row$codes) <- "codes"
        names(new_row$sem_rels) <- "sem_rels"
        # Incorpore new row in the dataframe
        annotation_reactive$data <- rbind(annotation_reactive$data, new_row)
    }
}

# This function return a dataframe with the validated_by field of a mention to
# ease the update process of the mention in the MongoDB collection
get_user_status <- function(validated_by_df,username) {
    user_rows <- validated_by_df[validated_by_df$user_id == username, ]
    state_value <- user_rows$state
    previously_annotated_value <- user_rows$previously_annotated
    return(data.frame(state = user_rows$state, previously_annotated = user_rows$previously_annotated))
}

# Function to update the mentions collection information. Specifically it updates
# the validated_by field of the mention to update the state and the previously_annotated
# field.
update_mention_in_db <- function(db_con, mention, user, update_all){
    if (update_all){
        # TODO: 
        # Update all mentions that are written in the same way and are not abbrv.
    } else {
        # Create the mongoDB query
        query <- toJSON(list(
            project_id = mention$project_id[1],
            document_id = mention$document_id[1],
            span_ini = mention$span_ini[1],
            span_end = mention$span_end[1],
            mention_class = mention$mention_class[1],
            "validated_by.user_id" = user
        ), auto_unbox  = TRUE)
        # Get current user status
        user_status <- lapply(mention$validated_by, get_user_status,username=user)
        
        # Generate the update mongodb query
        update_data <- toJSON(list(
            "$set" = list(
                "validated_by.$.state" = user_status[[1]]$state,
                "validated_by.$.previously_annotated" = user_status[[1]]$previously_annotated
            )
        ), auto_unbox  = TRUE)
        
        # Update MongoDB
        db_con$update(
            query = query,
            update = update_data
        )
    }
    
}

# Function to save the annotation in the annotation collections. If an annotation
# exists it will update that entry, if it doesn't, it will create a new document.
save_annotation_in_db <- function(db_con, anotation, texto, update_all){
    if (update_all){
        # TODO: 
        # Include/update new annotations that are written in the same way and are not abbrv.
    } else {
        # Create search quert
        query <- toJSON(list(
            project_id = anotation$project_id[1],  # Accede al primer elemento de la lista
            annotation_id = anotation$annotation_id[1],  # Accede al primer elemento de la lista
            user_id = anotation$user_id[1]  # Accede al primer elemento de la lista
        ), auto_unbox  = TRUE)
        
        # Verify if annotation exists in connection
        existing_entry <- db_con$find(query)
        if (length(existing_entry) > 0) {
            # If there is an existing entry, update it. 
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
                                  is_wrong = anotation$is_wrong[1],
                                  codes = c(anotation[1,]$codes), 
                                  sem_rels = c(anotation[1,]$sem_rels) 
                              )
                          ), auto_unbox  = TRUE))
            # Logs for tracing errors
            # cat("Entrada actualizada.")
        } else {
            # If there is not an existing entry, create a new one
            anotation[1,]$codes = c(anotation[1,]$codes)
            anotation[1,]$sem_rels = c(anotation[1,]$sem_rels)
            db_con$insert(anotation[1,])
            # Logs for tracing errors
            # cat("Nueva entrada incorporada.")
        }
    }
    
}

