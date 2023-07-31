loadData <- function(session,query_mongo_user, user_ide, project_id){

    # Filtrar el dataframe por user_id
    filtered_df = query_mongo_user[query_mongo_user$user_name == user_ide, ]
    # Obtener el dataframe de proyectos del user_id específico
    projects_df = filtered_df$projects[[1]]
    # Filtrar el dataframe de proyectos por project_id
    filtered_projects_df = projects_df[projects_df["project"] == project_id,]
    # Obtener los documentos asociados al project_id para el user_id específico
    documentos = filtered_projects_df$documents[[1]]
    # Get the list of documents
    documentos_text = distinct(session$userData$documents_db_endpoint$find(paste0('{"$and": [{"document_id": {"$in": ', toJSON(documentos), '}}, {"project_id": "', project_id, '"}]}',sep="")))
    menciones_text = session$userData$mentions_db_endpoint$find(paste0('{"$and": [{"document_id": {"$in": ', toJSON(documentos), '}}, {"project_id": "', project_id, '"}]}',sep=""))
    output = menciones_text %>% left_join(documentos_text,by="document_id")%>%
        filter(sapply(validated_by, function(df) any(df$user_id == user_ide))) %>%
        mutate(
            user_id = sapply(validated_by, function(df) df %>% filter(user_id==user_ide) %>% select(user_id) ),
            validated = sapply(validated_by, function(df) df %>% filter(user_id==user_ide) %>% select(state) ),
            previously_annotated =  sapply(validated_by, function(df) df %>% filter(user_id==user_ide) %>% select(previously_annotated)),
            no_code = sapply(validated_by, function(df) df %>% filter(user_id==user_ide) %>% select(no_code))
        ) %>%
        rename("span"="text.x",
               "text" = "text.y",
               "project_id"="project_id.x"
        )

    output
}

loadAnnotations <- function(session, user_ide, project_id){
    
    output = session$userData$annotation_db_endpoint$find(paste0('{"project_id": "', project_id,'", "user_id": "', user_ide,'"}'))
    if (nrow(output) == 0) {
        # Si el dataframe está vacío, crea un nuevo dataframe vacío para evitar errores posteriores
        output <- data.frame(
            project_id = character(),
            document_id = character(),
            span_ini = character(),
            span_end = character(),
            annotation_id = character(),
            user_id = character(),
            num_codes = numeric(),
            is_abrev = logical(),
            is_composite = logical(),
            need_context = logical(),
            previously_annotated = logical(),
            codes = I(rep(list("232674004"), 0)),  # Lista vacía con 0 elementos
            sem_rels = I(rep(list("EXACT"), 0)),    # Lista vacía con 0 elementos
            text = character()
        )
    }
    output
}


# Filtra diccionario
filtra_dict <- function(datos,diccionario, row_sel) {
    cadena <- datos$candidate_codes[row_sel][[1]]
    # cadena_caracteres = gsub("]", "", cadena)
    # cadena_caracteres = gsub('\\[', "", cadena_caracteres)
    # cadena_caracteres = gsub(' ', "", cadena_caracteres)
    # lista <- unlist(strsplit(cadena_caracteres, split = ","))
    dicc_filt <- diccionario %>%
        filter(code %in% cadena)
    dicc_filt$order <- match(dicc_filt$code,cadena)
    dicc_filt <- dicc_filt %>% arrange(order)
    return(dicc_filt)
}


loadDict  <- function(abspath2dicc) {
    # /srv/shiny-server/bioMnorm/data/diccionario.tsv
    diccionario <- read.csv(abspath2dicc,sep="\t",
                            colClasses = c("code" = "character")) 
    
    # Apply function para obtener URL de diccionario.
    parte_web1="https://browser.ihtsdotools.org/?perspective=full&conceptId1="
    parte_web2="&edition=MAIN/SNOMEDCT-ES/2022-10-31&release=&languages=es"
    diccionario<-  diccionario %>% 
        rowwise()  %>%
        mutate(url = paste0(parte_web1,code,parte_web2))
    main_terms<-  diccionario  %>%
        group_by(code)  %>%
        filter(mainterm == 1) %>%
        mutate(term = first(term)) %>%
        mutate(sinonimo = "")
    sinonimos <- diccionario  %>%
        group_by(code)  %>%
        filter(mainterm == 0) %>%
        mutate(sinonimo = list(term))  %>%
        distinct(code, sinonimo)  
    diccionario <- merge(main_terms, sinonimos %>% 
                             group_by(code) %>%
                             mutate(sinonimo = list(sinonimo)), by = "code", all.x=TRUE)
    
    diccionario
}
