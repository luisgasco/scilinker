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
        project_items <- menuItem("Projects", tabName="projects", icon=icon("bar-chart"),
                                  lapply(session$userData$projects, function(item) {
                                      menuSubItem(item, tabName = item)
                                  })
        )
        config_item <- menuItem("Config", icon = icon("gear"), tabName="config")
        about_item <- menuItem("About", icon = icon("info"), tabName="about")
        
        # Logs for tracing errors
        # print(session$userData$role)
        # If user is admin, show the users tab.
        # TODO: Include another item called "Create project"
        if(length(session$userData$role) > 0 && session$userData$role=="admin"){
            admin_items <- list(
                menuItem("Users", icon = icon("users"), tabName="users")
            )
            
        } else {
            admin_items <- NULL
        }
        # Render menu
        output$menu <- renderMenu({
            sidebarMenu(
                about_item,
                admin_items,
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
