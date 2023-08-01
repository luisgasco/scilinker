

generalValidationInterfaceUI <- function(id)
{
    ns <- NS(id)
    
    # users_of_project <- con$find(sprintf('{"projects.project": "%s"}', "proyecto1_test"))
    
    fluidRow(
        column(12,
               tabBox( 
                   title="",
                   id = "valid_tab",
                   height = "620",
                   tabPanel("Project stats", 
                            div(
                                uiOutput(ns("project_stats")),
                                
                            )
                   ),
                   tabPanel("Validation interface", 
                            pickerInput(
                                inputId = ns("user_filter"),
                                label = "User selection", 
                                choices = c("asd","asd2"),#c(users_of_project[users_of_project!="admin"],"all"),
                                multiple = TRUE
                            ),
                            uiOutput(ns("validation_interface"))
                   )
               )
        )
    )

}

generalValidationInterface<- function(input, output, session, con)
{
    ns <- session$ns
    
    output$project_stats <- renderUI({
        column(12,
            fluidRow(
                h4("General"),
                valueBoxOutput(ns("documents")),
                valueBoxOutput(ns("current_annotations")),
                downloadButton(ns("download_annotations"), "Download annotations tsv")
                    ),
            hr(),
            fluidRow(
                h4("User stats"),
                uiOutput(ns("user_stats"))
            )
        )
    })
    
    # Render values
    output$documents <- renderValueBox({
        print(session$userData$current_project)
        query <- toJSON(list(
            project_id = session$userData$current_project
        ), auto_unbox  = TRUE)
        docs_df = session$userData$documents_db_endpoint$find(query)
        valueBox(
            nrow(docs_df), "Documents", icon = icon("file"),
            color = "purple"
        )
    })
    output$current_annotations <- renderValueBox({
        # Query current project in mentions
        query <- toJSON(list(
            project_id = session$userData$current_project
        ), auto_unbox  = TRUE)
        mentions_df = session$userData$annotation_db_endpoint$find(query)
        valueBox(
            nrow(mentions_df), "Annotations", icon = icon("file"),
            color = "olive"
        )
    })
    output$user_stats <- renderUI({
        users_of_project <- con$find(sprintf('{"projects.project": "%s"}', session$userData$current_project))$user_name
        users_of_project <- users_of_project[users_of_project!="admin"]
        # Calculate number of 
        print("USERS_INSIDE RENDER_STATS")
        print(users_of_project)
        lapply(seq_along(users_of_project), function(i) {
            #Calculate percentajes
            query_mentions <- sprintf('{"project_id": "%s", "validated_by.user_id": "%s"}', session$userData$current_project, users_of_project[i])
            num_menciones <-  nrow(session$userData$mentions_db_endpoint$find(query_mentions))
            query_anotations <- sprintf('{"project_id": "%s", "user_id": "%s"}', session$userData$current_project, users_of_project[i])
            num_anotations <- nrow(session$userData$annotation_db_endpoint$find(query_anotations))
            percentage_mentions <- (num_anotations / num_menciones) * 100
            fluidRow(
                column(6,
                        p(users_of_project[i]),
                        progressBar(
                            paste0("pb", users_of_project[i]),
                            value = percentage_mentions,
                            size = "s",
                            # display_pct = TRUE
                        )
                    ),
                column(6,
                       p(paste0(num_anotations, " / ", num_menciones)),
                )
            )
        })
    })
    tsv_content <- reactive({
        # Select data from mongodb
        query <- toJSON(list(
            project_id = session$userData$current_project
        ), auto_unbox  = TRUE)
        mentions_df = session$userData$annotation_db_endpoint$find(query)
        
        # Transform list in comma-separated values
        for (col in names(mentions_df)) {
            if (is.list(mentions_df[[col]])) {
                mentions_df[[col]] <- sapply(mentions_df[[col]], function(x) {
                    if (length(x) > 0) {
                        paste0("[", paste(x, collapse = ","), "]")
                    } else {
                        ""
                    }
                })
            }
        }
        mentions_df
    })
    
    # Main data downloader
    output$download_annotations <- downloadHandler(
        # Prepare data to be downloaded as a tsv
        filename =  function() {
            paste0("annotations_",session$userData$current_project,".tsv")
            },
        content = function(file){
            write.table(tsv_content(), file, sep = "\t", row.names = FALSE, quote=FALSE)
        }
    )
    
    
    output$config_tab <- renderUI({
        h3("To be developed")
    })
}