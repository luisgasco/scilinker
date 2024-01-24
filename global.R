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
library(reticulate)
library(httr)
library(future)
library(promises)
shinyjs::useShinyjs()
future::plan(future::multisession)  

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
mongo_terminologies_collection <- Sys.getenv("MONGODB_TERMINOLOGIES_COLLECTION")
mongo_terms_collection <- Sys.getenv("MONGODB_TERMS_COLLECTION")
maximum_upload_size <- as.numeric(Sys.getenv("MAX_UPLOAD_SIZE_MB"))
timeoutSeconds <- as.integer(Sys.getenv("TIMEOUT_SECONDS"))
abspath2dicc  <- Sys.getenv("DICCIONARY_ABS_PATH")
base_path <- Sys.getenv("ABS_PATH_LOCAL")
available_models <- Sys.getenv("AVAILABLE_MODELS")
available_models <- unlist(strsplit(available_models, ","))
max_candidates <- as.numeric(Sys.getenv("MAX_CANDIDATES"))

# Check if database exist. It it isn't, create it with the user admin:
test_mongodb <- mongo(url = paste0("mongodb://",mongo_host,":",mongo_port),db="admin")
if (!(mongo_database %in% test_mongodb$run('{"listDatabases":1}')$databases$name)){
    temp_connection <- mongo(db = mongo_database, url = paste0("mongodb://",mongo_host,":",mongo_port, collection = mongo_user_collection))
    # Inserta un documento en una colección (esto creará la base de datos si no existe)
    admin_data <- toJSON(list(user_name = "admin", password = "admin",
                              email="admin", role="admin",
                              projects=list()),auto_unbox=TRUE)
    temp_connection$insert(admin_data)
    temp_connection$disconnect()
}
# Check if needed collections exist. If they does not, create it. 
#conection_tests <- mongo(db = mongo_database, url = paste0("mongodb://",mongo_host,":",mongo_port))
#existing_collections  <- conection_tests$run('{"listCollections":1}')$cursor$firstBatch$name
#collections_to_check <- c(mongo_user_collection, mongo_projects_collection, mongo_mentions_collection, 
#                          mongo_documents_collection, mongo_annotations_collection, mongo_terminologies_collection,
#                          mongo_terms_collection)
# 
# for (collection_name in collections_to_check) {
#     if (!(collection_name %in% existing_collections)) {
#         conection_tests_creation <<- mongo(db = mongo_database, collection = collection_name, url = paste0("mongodb://",mongo_host,":",mongo_port))
#         cat(sprintf("La colección '%s' no existe. Creándola...\n", collection_name))
#         conection_tests_creation$insert(toJSON(list(dummy = "dummy"),auto_unbox=TRUE))
#         conection_tests_creation$disconnect()
#     }
# }
# conection_tests$disconnect()

# Check if there is any user with "admin" role:
temp_connection <- mongo(collection = mongo_user_collection,
                         db = mongo_database,
                         url = paste0("mongodb://",mongo_host,":",mongo_port))
admin_user <- temp_connection$find('{"role": "admin"}', limit = 1)
if (length(admin_user) == 0) {
    admin_data <- toJSON(list(user_name = "admin", password = "admin",
                       email="admin", role="admin",
                       projects=list()),auto_unbox=TRUE)
    temp_connection$insert(admin_data)
    temp_connection$disconnect()
}

# Import modules
source(paste0(base_path,"/scilinker/modules/login.R"))
source(paste0(base_path,"/scilinker/modules/sidebar.R"))
source(paste0(base_path,"/scilinker/modules/module_annotation.R"))
source(paste0(base_path,"/scilinker/modules/module_adminvalidation.R"))
source(paste0(base_path,"/scilinker/modules/module_users.R"))
source(paste0(base_path,"/scilinker/modules/module_config.R"))
source(paste0(base_path,"/scilinker/modules/module_projectcreation.R"))
source(paste0(base_path,"/scilinker/modules/module_gazzeteercreation.R"))
source(paste0(base_path,"/scilinker/modules/module_gazzeteerview.R"))

# Maximum upload 
options(shiny.maxRequestSize = maximum_upload_size*1024^2)

#Import python functions
source_python('~/scilinker/python/gaz_functions.py')
source_python('~/scilinker/python/proj_functions.py')

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
