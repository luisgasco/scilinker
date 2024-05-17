# SIDEMENU_MODuLES
# menuItem module  ----
# Module to generate the sidebar menu
menuItemModuleUI <- function(id) {
    ns <- NS(id)
    sidebarMenuOutput(ns("menu"))
}
menuItemModule <- function(input, output, session) {
    # Logs for tracing errors
    # print(session$userData$projects)
    # If user has a role, generate menu
    if(length(session$userData$role) > 0){
        # Get project-tab items based on projects associated to current_user
        project_items <<- menuItem("Projects", tabName="projects", icon=icon("bar-chart"),
                                   c(
                                       lapply(session$userData$projects, function(item) {
                                           menuSubItem(item, tabName = item)
                                       }),
                                       if (session$userData$role == "admin") {
                                           list(menuSubItem("Create new project", tabName = "new_project",  icon = icon("plus")))
                                       }
                                   )
        )
        
        config_item <- menuItem("Config", icon = icon("gear"), tabName="config")
        about_item <- menuItem("About", icon = icon("info"), tabName="about")
 
        # Logs for tracing errors
        # print(session$userData$role)
        # If user is admin, show the users tab.
        # TODO: Include another item called "Create project"
        # asdasdasd <<- session$userData$terminologies
        # print("LLGA1?")
        if(length(session$userData$role) > 0 && session$userData$role=="admin"){
            
            admin_items_a <-menuItem("Users", icon = icon("users"), tabName="users")
            admin_items_b <- menuItem("Gazzeteers", tabName = "gazzeteers", icon = icon("list"),
                                c(
                                    lapply(session$userData$terminologies, function(item) {
                                        menuSubItem(item, tabName = item)
                                    }),
                                    list(menuSubItem("Create new terminology", tabName = "new_terminology",  icon = icon("plus")))
                                )
                                )
        } else {
            admin_items_a <- NULL
            admin_items_b <- NULL
        }
        # Render menu
        output$menu <- renderMenu({
            sidebarMenu(
                about_item,
                admin_items_a,
                admin_items_b,
                project_items,
                config_item
                
            )
        })
    } else {
        # If user has not a role, render an empty menu
        output$menu <- renderMenu({
            sidebarMenu(
            )
        })
    }
}
