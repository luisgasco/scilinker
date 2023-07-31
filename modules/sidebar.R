# SIDEMENU_MODuLES
menuItemModuleUI <- function(id) {
    ns <- NS(id)
    sidebarMenuOutput(ns("menu"))
}

menuItemModule <- function(input, output, session) {
    print(session$userData$projects)
    if(length(session$userData$role) > 0){
        project_items <- menuItem("Projects", tabName="projects", icon=icon("bar-chart"),
                                  lapply(session$userData$projects, function(item) {
                                      menuSubItem(item, tabName = item)
                                  })
        )
        config_item <- menuItem("Config", icon = icon("gear"), tabName="config")
        about_item <- menuItem("About", icon = icon("info"), tabName="about")
        
        
        print(session$userData$role)
        if(length(session$userData$role) > 0 && session$userData$role=="admin"){
            admin_items <- list(
                menuItem("Users", icon = icon("users"), tabName="users")
            )
            
        } else {
            admin_items <- NULL
        }
        output$menu <- renderMenu({
            sidebarMenu(
                about_item,
                admin_items,
                project_items,
                config_item
                
            )
        })
    } else {
        output$menu <- renderMenu({
            sidebarMenu(
            )
        })
    }
}
