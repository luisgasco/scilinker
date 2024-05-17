library(shinyjs)
library(DT)
library(mongolite)
library(jsonlite)
library(dplyr)
library(shinyWidgets)
library(fontawesome)
shinyjs::useShinyjs()

# Gazzeteer View module  ----
# Module to view in table the existing gazzeteers. 
generalGazzeteerInterfaceUI <- function(id, con_terminologies)
{
    ns <- NS(id)
    fluidRow(
        column(12,
               tabBox( 
                   title="",
                   id = "config_tab",
                   height = "620",
                   
                   tabPanel("Info.", 
                            tagList(
                                title = h4("Gazzeteer info"),
                                fluidRow(
                                    class = "align-items-center",
                                    column(3, 
                                           tags$div(
                                                style = "display: flex; justify-content: space-between;",
                                                HTML("<b>Gazzeteer name: </b>")
                                                )
                                           ),
                                    column(9,
                                            tags$div(
                                                style = "display: flex; justify-content: space-between;",
                                                htmlOutput(ns("gaz_name"))
                                                )
                                    )
                                ),
                                fluidRow(
                                    class = "align-items-center",
                                    column(3, 
                                           tags$div(
                                               style = "display: flex; justify-content: space-between;",
                                               HTML("<b>Gazzeteer description: </b>")
                                           )
                                    ),
                                    column(9,
                                           tags$div(
                                               style = "display: flex; justify-content: space-between;",
                                               htmlOutput(ns("gaz_description"))
                                           )
                                    ),
                                ),
                                fluidRow(
                                    class = "align-items-center",
                                    column(3, 
                                           tags$div(
                                               style = "display: flex; justify-content: space-between;",
                                               HTML("<b>Hyperlink pattern to online terminology viewer: </b>")
                                           )
                                    ),
                                    column(9,
                                           tags$div(
                                               style = "display: flex; justify-content: space-between;",
                                               htmlOutput(ns("gaz_pattern_link"))
                                           )
                                    ),
                                )
                            ),
                            tags$hr(style = 'width:100%; border-top: 1px solid #e0e0e0;'),
                            tagList(
                                title = h4("Basic stats."),
                                fluidRow(
                                    class = "align-items-center",
                                    infoBoxOutput(ns("gaz_concepts")),
                                    infoBoxOutput(ns("gaz_lexical_entries")),
                                    infoBoxOutput(ns("gaz_amb_lexical_entries"))
                                )
                            ),
                            fluidRow(
                                column(12,
                                       actionButton(ns("delete_gaz"), "Delete gazzeteer", class = "btn-danger", style = "float: right; color:white")
                                )
                            )
                   ),
                   tabPanel("Terms", 
                            DT::DTOutput(ns('mytable_gaz'),height = "500px")
                   )
               )
        )
    )

}
generalGazzeteerInterface <- function(input, output, session, con_terms, con_terminologies)
{
    ns <- session$ns
    
    # Generate reactive data for the terms.
    gaz_data_reactive <- reactiveValues(data = data.frame())
 

    observeEvent(session$userData$current_gazzeteer, {
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
        # Load terminology data from database 
        gaz_data_reactive$data = con_terms$find(query = paste("{\"gazetteer_id\": \"",
                                                session$userData$current_gazzeteer, "\"}",sep = ""))
        # Get stats about the current gazzeteer:
        terminology_data <- con_terminologies$find(query = paste("{\"name\": \"",
                                                   session$userData$current_gazzeteer, "\"}",sep = ""))
        
        # Show name and description in interface
        output$gaz_name <- renderUI({
            HTML(terminology_data$name)
        })
        output$gaz_description <- renderUI({
            HTML(terminology_data$description)
        })
        output$gaz_pattern_link <- renderUI({
            tagList(
                fluidRow(
                    class = "align-items-center",
                    textAreaInput(ns("text_hyperlink_pattern"),"",value=terminology_data$hyperlink_pattern),
                    actionButton(ns("update_hyperlink_pattern"), "Update pattern", width = "100%")
                )
            )
            
        })
        # Update value in database if update pattern is clicked:
        observeEvent(input$update_hyperlink_pattern,{
            update_gaz_hyperlink(db_url = paste0("mongodb://",mongo_host,":",mongo_port),
                                 db_name = mongo_database,
                                 collection = mongo_terminologies_collection,
                                 gaz_id = session$userData$current_gazzeteer,
                                 new_pattern = input$text_hyperlink_pattern)
            runjs('$("#y-text_hyperlink_pattern").css("color", "green");')
            
        })
        # Show values in boxes in renderValueBox
        output$gaz_concepts <- renderInfoBox({
            infoBox(
                "Concepts",length(unique(gaz_data_reactive$data$code)), icon = icon("tags",lib="glyphicon"),
                color = "navy"
            )
        })
        output$gaz_lexical_entries <- renderInfoBox({
            infoBox(
                "Lexical entries", nrow(gaz_data_reactive$data), icon = icon("stats",lib="glyphicon"),
                color = "aqua"
            )
        })
        output$gaz_amb_lexical_entries <- renderInfoBox({
            amb_terms <- duplicated(gaz_data_reactive$data[, c("term")]) | duplicated(gaz_data_reactive$data[, c("term", "code")], fromLast = TRUE)
            infoBox(
                "Ambigous entries", sum(amb_terms),  icon = icon("duplicate",lib="glyphicon"),
                color = "orange"
            )
        })
        
        
        
        
        # After loading data, remove modal
        removeModal()
    }, ignoreNULL = FALSE)
    
    # Create proxy object to update datatable
    proxy <- dataTableProxy(ns("mytable"))
    
    removeModal()
    
    # Generate content for ValueBox with number of projects associated to the user
    output$mytable_gaz = DT::renderDataTable({  
        DT::datatable(gaz_data_reactive$data,
                      rownames=FALSE,
                      options = list(deferRender = TRUE,
                                     scrollY = 400,
                                     scrollX=TRUE,
                                     scroller = TRUE,
                                     autoWidth = TRUE
                                     # columnDefs = list(list(visible=FALSE, targets=c( 6, 7, 8, 9, 10))),#2,
                                     # order = list(list(11,'asc'),list(1,'asc'))
                      ),
                      selection ="single",
                      extensions = c("Scroller")) 
        },server = TRUE
    )
    
    # If delete button is clicked show modal to confirm
    observeEvent(input$delete_gaz, {
        showModal(
            modalDialog(
                id = "confirmation_modal",
                title = "Confirm",
                tagList(
                   "This is a destructive action and could break some of the active annotation projects.",
                   "If you want to continue, type the name of the gazzeteer in the text field and press 'Delete' ",
                   textInput(ns("confirm_gaz_name"),label = "",value = ""),
                   uiOutput(ns("status_confirm_gaz_name"))
                ),
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton(ns("confirm_delete"), "Delete", class = "btn-danger",style = "color:white")
                )

            )
        )
    },ignoreInit=TRUE, ignoreNULL = TRUE)
    # IF confirm_delete is clicked, verify the input text field and delete gazzeteer from db.
    observeEvent(input$confirm_delete, {
        # Borrar la entrada de la base de datos mongo con_terminologies cuyo "name" sea "hola"
        # con$remove('{"name": "hola"}')
        if (input$confirm_gaz_name == session$userData$current_gazzeteer){
            # Delete gazzeteer entry from con_terminologies
            con_terminologies$remove(paste0('{"name": "',input$confirm_gaz_name, '"}'))
            # Delete term entries from con_terms
            con_terms$remove(paste0('{"gazetteer_id": "',input$confirm_gaz_name, '"}'))
            removeModal()
            # Show info. modal
            showModal(modalDialog(id = "terminology_deleted", 
                                  HTML("Gazzeteer successfully deleted\n Reload the page to update the menu.") , 
                                  size="s")
            )
        } else {
            output$status_confirm_gaz_name <- renderUI({
                HTML(paste("<p style='color:red;'>",
                           "The written text does not match the name of the gazzeteer.",
                           "</p>"))
            })
        }
    },ignoreInit=TRUE, ignoreNULL = TRUE)
}
  




