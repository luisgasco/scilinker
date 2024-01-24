
# generalValidationInterface module  ----
# Module to show stats and validate mentions of users of an specific project
generalValidationInterfaceUI <- function(id)
{
    ns <- NS(id)
    # Show the UI (a tab-box with tabs)
    fluidRow(
        column(12,
               tabBox( 
                   title="",
                   id = "valid_tab",
                   height = "840",
                   tabPanel("Project stats", 
                            div(
                                uiOutput(ns("project_stats")),
                                
                            ),
                            fluidRow(
                                column(12,
                                       actionButton(ns("delete_project"), "Delete project", class = "btn-danger", style = "float: right; color:white")
                                )
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
generalValidationInterface<- function(input, output, session, con, con_projects, con_mentions)
{
    ns <- session$ns
    
    # Render the tab "project_stats" where admin can visualize number of documents
    # in the project, number of annotations done by annotator users, download 
    # those annotations in tsv format, and show stats per each user.
    output$project_stats <- renderUI({
        query_project <- con_projects$find(toJSON(list(name = session$userData$current_project), auto_unbox  = TRUE))
        query_mention <- con_mentions$find(toJSON(list(project_id = session$userData$current_project),auto_unbox=TRUE))
        
        # If candidate_codes field is different to the project's "k", we will show a loading gif.
        # Si query_project$k es precomputed, directamente habrá que poner el tick
        show_loading <- ifelse(query_project$k=="candidates precomputed", 
                               FALSE, 
                               as.character(length(query_mention$candidate_codes[[1]])) != query_project$k
                               )
        status_calculation <- ifelse(show_loading, 
                                     ' <img src="loading.gif" alt="Loading..."  style="height:30px; margin-left: -10px"/>In progress  
                                     <div class="hover-text"> <i class="fas fa-question-circle"></i><span class="tooltip-text" id="righttooltip">
                                     Candidate calculation time may vary depending on the number of mentions and size of the gazetteer. If it
                                     remains "In progress" for too long, it is possible that there was a problem during the calculation process,
                                     delete the project and try to generate it again. </span></div>',
                                     '<i class="fas fa-check"  style="color: green;"> </i> Completed')
                              
        print("SHOWLOADING")
        print(show_loading)
        print(as.character(length(query_mention$candidate_codes[[1]])))
        print(query_project$k)
        
        
        column(12,
            fluidRow(
                h4("General"),
                valueBoxOutput(ns("documents")),
                valueBoxOutput(ns("current_annotations")),
                downloadButton(ns("download_annotations"), "Download annotations tsv")
                    ),
            hr(),
            fluidRow(
                class = "align-items-center",
                column(5,HTML("<b>Gazetteer used in the project:</b>")),
                column(7,HTML(query_project$gazetteer_id))
            ),
            fluidRow(
                class = "align-items-center",
                column(5,HTML("<b>Model used to generate candidates:</b>")),
                column(7,HTML(query_project$model))
            ),
            fluidRow(
                class = "align-items-center",
                column(5,HTML("<b>Number of candidates generated per mention:</b>")),
                column(7,HTML(query_project$k))
            ),
            fluidRow(
                class = "align-items-center",
                column(5,HTML("<b>Status of candidate codes calculation:</b>")),
                column(7,HTML(status_calculation))
            ),
            hr(),
            fluidRow(
                div(
                    h4("User stats"),
                    div(
                        column(8, uiOutput(ns("user_stats"))),
                        column(4, uiOutput(ns("user_timegraphUI")))
                    )
                )
            )
        )
       
    })
    
    # Render value Box of number of documents associated to the current project
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
    # Render value Box of number of annotations associated to the current project
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
    # Render a UI with stats per user
    output$user_stats <- renderUI({
        users_of_project <- con$find(sprintf('{"projects.project": "%s"}', session$userData$current_project))$user_name
        users_of_project <- users_of_project[users_of_project!="admin"]
        # Logs for tracing errors
        # print("USERS_INSIDE RENDER_STATS")
        # print(users_of_project
        
        # Generate stats UI per user of the project (one row per user)
        lapply(seq_along(users_of_project), function(i) {
            #Calculate percentajes
            query_mentions <- sprintf('{"project_id": "%s", "validated_by.user_id": "%s"}', session$userData$current_project, users_of_project[i])
            num_menciones <-  nrow(session$userData$mentions_db_endpoint$find(query_mentions))
            query_anotations <- sprintf('{"project_id": "%s", "user_id": "%s"}', session$userData$current_project, users_of_project[i])
            annotations <<- session$userData$annotation_db_endpoint$find(query_anotations)
            num_anotations <- nrow(annotations)
            percentage_mentions <- (num_anotations / num_menciones) * 100
            average_time <- ifelse(is.nan(sum(annotations$total_time) / num_anotations), 0, sum(annotations$total_time) / num_anotations )
            # Compute
            div(
                column(7,
                        p(users_of_project[i]),
                        progressBar(
                            paste0("pb", users_of_project[i]),
                            value = percentage_mentions,
                            size = "s",
                            display_pct = TRUE
                        )
                    ),
                column(5,
                       div(
                           p(paste0(num_anotations, " / ", num_menciones)),
                           p(paste0(round(average_time,2), " secs/mention"))
                       )
                )
            )
        })
    })
    # Render a UI with a boxplot
    output$user_timegraphUI <- renderUI({
        query_anotations <- sprintf('{"project_id": "%s"}', session$userData$current_project)
        annotations_out <- session$userData$annotation_db_endpoint$find(query_anotations)
        if ("total_time" %in% names(annotations_out)) {
            plotOutput(ns("user_timegraph_plot"))
        } else {
            # Mostrar un mensaje si total_time no está presente
            div(p("When the user starts annotating, a box-plot will appear to view the distribution of annotation times."))
        }
        
    })
    
    output$user_timegraph_plot <- renderPlot({
        query_anotations <- sprintf('{"project_id": "%s"}', session$userData$current_project)
        annotations_out <- session$userData$annotation_db_endpoint$find(query_anotations)
        par(mar = c(2, 4, 0, 2)) 
        boxplot(total_time ~ user_id, data = annotations_out)
    })
    # Reactive data to prepare the tsv content with annotations
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
    
    # Delete project:
    # If delete button is clicked show modal to confirm
    observeEvent(input$delete_project, {
        showModal(
            modalDialog(
                id = "confirmation_modal",
                title = "Confirm",
                tagList(
                    "This is a destructive action, but annotations will not be deleted.",
                    "If you want to continue, type the name of the project in the text field and press 'Delete' ",
                    textInput(ns("confirm_project_name"),label = "",value = ""),
                    uiOutput(ns("status_confirm_project_name")),
                    awesomeCheckbox(inputId = ns("delete_annotations"), label = "Delete user annotations",
                                    value = FALSE,status = "danger")
                ),
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton(ns("confirm_delete_project"), "Delete", class = "btn-danger",style = "color:white")
                )
                
            )
        )
    },ignoreInit=TRUE, ignoreNULL = TRUE)
    
    # IF confirm_delete is clicked, verify the input text field and delete gazzeteer from db.
    observeEvent(input$confirm_delete_project, {
        # Borrar la entrada de la base de datos mongo con_terminologies cuyo "name" sea "hola"
        # con$remove('{"name": "hola"}')
        if (input$confirm_project_name == session$userData$current_project){
            
            delete_project(mongo_url = paste0("mongodb://",mongo_host,":",mongo_port),
                           db_name = mongo_database,
                           projects_collection = mongo_projects_collection,
                           documents_collection = mongo_documents_collection,
                           mentions_collection = mongo_mentions_collection,
                           users_collection = mongo_user_collection,
                           project_to_delete = session$userData$current_project,
                           annotations_collection = mongo_annotations_collection, 
                           delete_annotations = input$delete_annotations)

            
            # Search active users of the project (plus the admin, present in every project)
            # db.getCollection('proyectos').find({"name":"asd"}) and get users
            # Delete entry from project collection.
            # db.getCollection('proyectos').remove({"name":"asd"})
            
            # Delete documents from documents collection.
            # db.getCollection('documentos').remove({"project_id":"pruebaA"})
            
            # Delete mentions from mentions collection
            # db.getCollection('menciones').remove({"project_id":"pruebaA"})
            
            # Update project field in users collection. 
            # https://bard.google.com/chat/312f25ea284ba39d
            print("everythin deleted")
            
            removeModal()
            # Show info. modal
            showModal(modalDialog(id = "project_deleted", 
                                  HTML("Project successfully deleted\n Reload the page to update the menu.") , 
                                  size="s")
            )
        } else {
            output$status_confirm_project_name <- renderUI({
                HTML(paste("<p style='color:red;'>",
                           "The written text does not match the name of the project",
                           "</p>"))
            })
        }
    },ignoreInit=TRUE, ignoreNULL = TRUE)
    
    # Download handler when pressing Download 
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