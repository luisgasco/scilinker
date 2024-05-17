
# Aux function for error handling
# This function takes the input data after clicking save button and validates
# that there are no inconsistencies before updating and saving annotations

input_validation <- function(input, dicc_filt, reactive_values, datos_reactive, row_sel, error_occurred){
    icon_error = '<span style="color: red;"><i class="fas fa-exclamation-circle"></i> '
    ## CHECK 1
    # If composite is TRUE and num_codes is == 1 output a showModal
    if (input[[reactive_values$composite_id()]] == TRUE && input[["number_codes"]]==1){
        showModal(modalDialog(
            title =  HTML(paste0(icon_error, 'Error 8 - Inconsistent Mention Selection: Composite Mention with Single Code')),
            easyClose = TRUE,
            HTML("You have selected the mention as composite but only considered a single code. Increase the number of codes, click on 'update' and fill in the information for the second code, or deselect the composite option.")
        ))
        error_occurred(TRUE)
        return()
    }
    ## CHECK 2
    for (i in 1:input[["number_codes"]]) {
        # Obtenemos los ids de los elementos de códigos
        id_code <- i
        nocode_id <- paste0("code_num_", id_code, "_no_code_", datos_reactive$data$document_id[row_sel], "_", datos_reactive$data$span_ini[row_sel], "_", datos_reactive$data$span_end[row_sel])
        nonorm_id <- paste0("code_num_", id_code, "_no_norm_", datos_reactive$data$document_id[row_sel], "_", datos_reactive$data$span_ini[row_sel], "_", datos_reactive$data$span_end[row_sel])
        writtencode_id <- paste0("code_num_", id_code, "_", "written_code_", datos_reactive$data$document_id[row_sel], "_", datos_reactive$data$span_ini[row_sel], "_", datos_reactive$data$span_end[row_sel])
        writtenrel_id <- paste0("code_num_", id_code, "_", "written_relation_", datos_reactive$data$document_id[row_sel], "_", datos_reactive$data$span_ini[row_sel], "_", datos_reactive$data$span_end[row_sel])
        
        # BUCLE 1
        contador_codigos <- reactiveVal(0)
        for (j in seq_len(nrow(dicc_filt))) {
            check_code_id <- gsub("[.#-]", "_", paste0("code_num_", id_code, "_check_", dicc_filt$code[j], "_", datos_reactive$data$document_id[row_sel], "_", datos_reactive$data$span_ini[row_sel], "_", datos_reactive$data$span_end[row_sel]))
            if (input[[check_code_id]]) {
                contador_codigos(contador_codigos() + 1)
            }
        }
        ## CHECK 2a
        # Check that all code blocks have a code selected
        if ((contador_codigos()==0 && input[[nonorm_id]]==FALSE && input[[nocode_id]]==FALSE && (input[[writtencode_id]]=="" || is.null(input[[writtenrel_id]])) && (input[["previously_annotated"]]!=TRUE) && (input[["wrong_mention"]]!=TRUE) )){
            texto <- paste0("In code block number ",id_code, " you have forgotten to select a code. <br> Please complete this information.")
            showModal(modalDialog(
                title = HTML(paste0(icon_error, 'Error 6 - No code selected in Code ', id_code)),
                easyClose = TRUE,
                HTML(texto)
            ))
            error_occurred(TRUE)
            return()
        }
        
        # BUCLE 2
        contador_codigos <- reactiveVal(0)
        for (j in seq_len(nrow(dicc_filt))) {
            check_code_id <- gsub("[.#-]", "_", paste0("code_num_", id_code, "_check_", dicc_filt$code[j], "_", datos_reactive$data$document_id[row_sel], "_", datos_reactive$data$span_ini[row_sel], "_", datos_reactive$data$span_end[row_sel]))
            check_relbox_id <- gsub("[.#-]", "_", paste0("code_num_", id_code, "_rel_", dicc_filt$code[j], "_", datos_reactive$data$document_id[row_sel], "_", datos_reactive$data$span_ini[row_sel], "_", datos_reactive$data$span_end[row_sel]))
            
            if (input[[check_code_id]]) {
                contador_codigos(contador_codigos() + 1)
            }
            # CHECK 2b
            # Check that no more than one code is checked at the same time.
            if (contador_codigos() > 1) {
                texto <- " You have selected two codes from the list simultaneously. <br> It is not possible to save the result without correcting this error, uncheck one of the codes"
                showModal(modalDialog(
                    title = HTML(paste0(icon_error,'Error 1 - Multiple codes selected in Code ',id_code,"</b>")),
                    easyClose = TRUE,
                    HTML(texto)
                ))
                error_occurred(TRUE)
                return()  # Salir del bucle for
            }
            # Check if a code has been marked, but not its semantic relation
            if (input[[check_code_id]] && is.null(input[[check_relbox_id]])){
                texto <- "You have selected a code but have not checked the associated semantic relation.<br> Please select a semantic relationship for that code and try saving again."
                showModal(modalDialog(
                    title = HTML(paste0(icon_error,"Error 2 - Code selected without semantic relation in Code ",id_code)),
                    easyClose = TRUE,
                    HTML(texto)
                ))
                error_occurred(TRUE)
                return()
            }
            # Check that a code is not checked together with no_code and/or no_ontology
            if (contador_codigos()>=1 && (input[[nocode_id]]==TRUE  || input[[nonorm_id]]==TRUE)){
                texto <- paste0("In Code ", id_code, " you have selected a candidate code and you have also marked that no candidate is valid or that the mention is not normalizable with the working ontology.<br> Please check these fields and try to save again.")
                showModal(modalDialog(
                    title = HTML(paste0(icon_error, 'Error 3 - Code selected along with "no_code" or "no_ontology" marked in Code ', id_code)),
                    easyClose = TRUE,
                    HTML(texto)
                ))
                error_occurred(TRUE)
                return()
            }
            # Check that if no_code is True, the string and code are marked manually.
            if ((contador_codigos()==0 && input[[nocode_id]]==TRUE && (input[[writtencode_id]]=="" || is.null(input[[writtenrel_id]]) ))&& (input[["previously_annotated"]]!=TRUE)){
                texto <- paste0("In code block number ",id_code, " you have selected that the code is not among the candidates, but you have not typed the associated code in the text field or you have not selected the semantic relationship of the manually typed code and the mention.<br> Please complete this information.<br> Make sure you have clicked the tick button once you have typed the code so that it is saved correctly.")
                showModal(modalDialog(
                    title = HTML(paste0(icon_error, 'Error 4 - No candidate but code not written in Code ', id_code)),
                    easyClose = TRUE,
                    HTML(texto)
                ))
                error_occurred(TRUE)
                return()
            }
            # Check If code has been written, but code_not_found is not checked
            if ((contador_codigos()==0 && (input[[writtencode_id]]!="") && (input[[nocode_id]]==FALSE )) && (input[["previously_annotated"]]!=TRUE) ){
                texto <- paste0("In code block number ",id_code, " you have manually written the code and/or the semantic relation, but you have forgotten to check the 'Code not found in candidates' option.<br> Please complete this information.")
                showModal(modalDialog(
                    title = HTML(paste0(icon_error, 'Error 5 - Code written but checkbox not marked in Code ', id_code)),
                    easyClose = TRUE,
                    HTML(texto)
                ))
                error_occurred(TRUE)
                return()
            }
            
            # NOTA_ERROR: UNA VEZ SE CAMBIA DE NONORM_ID A SELECCIONAR UN CODIGO, EL SISTEMA
            # DA ERROR PORQUE ESTE CODIGO SALTA ANTES DE QUE EL CONTADOR PUEDA HABER
            # LLEGADO AL ULTIMO CODIGO. QUIZÄ HABRÏA QUE ITERAR POR TODOS LOS CODIGOS PRIMERO
            # Y POSTERIORMENTE VERIFICAR ESTOS IFs. SEGURAMENTE ESO IMPLIQUE
            # UN SEGUNDO BUCLE.
            # Checkear si no se ha marcado nada.
            
        }
        
        
    }
}
