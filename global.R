library(shiny)
library(shinydashboard)
library(shinyjs)
library(jsonlite)
library(shinycssloaders)
library(tidyr)
library(mongolite)
library(dplyr)
library(DT)
library(shinyWidgets)
library(fontawesome)
library(RColorBrewer)
library(stringr)
library(data.table)

shinyjs::useShinyjs()



# Change to false if running on docker
LOCAL = TRUE 

# Read environment variables
readRenviron(ifelse(LOCAL,"~/scilinker/data/.config_file","/srv/shiny-server/scilinker/data/.config_file"))
mongo_host <- Sys.getenv("MONGODB_HOST")
mongo_port <- Sys.getenv("MONGODB_PORT")
mongo_user <- Sys.getenv("MONGODB_USER")
mongo_pass <- Sys.getenv("MONGODB_PASSWORD")
mongo_database <- Sys.getenv("MONGODB_DATABASENAME")
mongo_user_collection <- Sys.getenv("MONGODB_USER_COLLECTION")
mongo_projects_collection <- Sys.getenv("MONGODB_PROJECTS_COLLECTION")
mongo_mentions_collection <- Sys.getenv("MONGODB_MENTIONS_COLLECTION")
mongo_documents_collection <- Sys.getenv("MONGODB_DOCUMENTS_COLLECTION")
mongo_annotations_collection  <- Sys.getenv("MONGODB_ANNOTATIONS_COLLECTION")
timeoutSeconds <- as.integer(Sys.getenv("TIMEOUT_SECONDS"))
abspath2dicc  <- Sys.getenv("DICCIONARY_ABS_PATH")
base_path <- Sys.getenv("ABS_PATH_LOCAL")

# Import modules
source(paste0(base_path,"/scilinker/modules/login.R"))
source(paste0(base_path,"/scilinker/modules/sidebar.R"))
source(paste0(base_path,"/scilinker/modules/module_annotation.R"))
source(paste0(base_path,"/scilinker/modules/module_adminvalidation.R"))
source(paste0(base_path,"/scilinker/modules/module_users.R"))
source(paste0(base_path,"/scilinker/modules/module_config.R"))

# If this variable is TRUE, the app will update all equal mentions assign to user 
# (if they are not abbreviations or need context). NOT DEVELOP YET
UPDATE_ALL = as.logical(Sys.getenv("UPDATE_ALL"))

# Function to verify user/password in mongodb database
authenticate_user <- function(username, password, con)
{
    user <- con$find(query = paste("{\"user_name\": \"", username, "\"}", sep = ""))
    
    if (is.null(user))
    {
        return(FALSE)  # Usuario no encontrado
    }
    
    if (password == user$password)
    {
        return(TRUE)  # Contraseña correcta
    } else
    {
        return(FALSE)  # Contraseña incorrecta
    }
}
