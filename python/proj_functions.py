import random
import numpy as np
from pymongo import MongoClient
from pathlib import Path

def create_annotation_project(mongo_url, db_name, collection_name, project_name, project_description, users_list, gazetteer_id, model_name, number_candidates):
    client = MongoClient(mongo_url)
    db = client[db_name]
    collection = db[collection_name]
    
    # Transform users_list to the correct python format:
    users_list = [value["user"] for key, value in users_list.items()]
    
    # Buscar si el proyecto ya existe en la colección
    existing_project = collection.find_one({"name": project_name})

    if existing_project:
        # Si el proyecto existe, actualizamos la descripción y la lista de usuarios
        collection.update_one(
            {"name": project_name},
            {"$set": {"description": project_description, "users": users_list}}
        )
    else:
        # Si el proyecto no existe, lo creamos
        project = {
            "name": project_name,
            "description": project_description,
            "gazetteer_id": gazetteer_id,
            "users": users_list,
            "model": model_name,
            "k": number_candidates
        }
        collection.insert_one(project)
    
    client.close()

def create_project_documents(mongo_url, db_name, collection_name, text_files_path, project_name):
    client = MongoClient(mongo_url)
    db = client[db_name]
    collection = db[collection_name]
    
    for text_file_path in text_files_path:
        with open(text_file_path, 'r', encoding='utf-8') as file:
            text = file.read()
            document_id = Path(text_file_path).stem
            document = {
                "document_id": document_id,
                "project_id": project_name,
                "text": text
            }
            
            collection.insert_one(document)
    
    client.close()    
    return None
    

def distribute_elements(lista, porcentajes, porcentaje_compartido):
    """
    Function to divide documents with a percentage of documents as agreement
    """
    total_elementos = len(lista)
    n_elementos_compartidos = int(total_elementos * porcentaje_compartido)

    if n_elementos_compartidos > total_elementos:
        raise ValueError("El porcentaje de elementos compartidos es muy alto.")

    # Repartir los elementos compartidos entre todos los conjuntos
    elementos_compartidos = random.sample(lista, n_elementos_compartidos)
    lista_sin_elementos_compartidos = [elem for elem in lista if elem not in elementos_compartidos]

    # Repartir los elementos en cada conjunto
    resultados = []
    lista_copia = lista_sin_elementos_compartidos.copy()

    for p in porcentajes[:-1]:
        n = int((total_elementos - n_elementos_compartidos) * (p))
        muestra = random.sample(lista_copia, n)
        resultados.append(muestra)
        lista_copia = [elem for elem in lista_copia if elem not in muestra]

    resultados.append(lista_copia)  # Agregar el resto de elementos a la última muestra
    output = [list(set(resultado+elementos_compartidos)) for resultado in resultados]
    return output

def import_annotations_to_mongodb( mongo_url, db_name, collection_name, project_id,admin_user_name, users_list, agreement_perc,candidate_df):
    """
    """
    client = MongoClient(mongo_url)
    db = client[db_name]
    collection = db[collection_name]
    # Transform users_list to the correct python format:
    users_list = [value for key, value in users_list.items()]
    
    # Calcular la cantidad de documentos que se asignará a cada usuario
    total_documents = len(candidate_df.filenameid.unique())
    total_users = len(users_list)
    percs_per_user = [user['perc_elements'] for user in users_list]
    # Separate column
    candidate_df[['filename', 'span_ini', 'span_end']] = candidate_df['filenameid'].str.split('#', expand=True)
    
    # Repartir elementos por usuario
    docs_per_user = distribute_elements(list(np.unique(candidate_df.filename.to_list())), percs_per_user, agreement_perc)
    
    
    user_info_out = list()
    for user, docs in zip(users_list, docs_per_user):
        user_name = user["user"]
        user_info_out.append({"user": user_name, "docs": docs})
        print("User {} will have assigned {} documents".format(user_name, len(docs)))
        
        # Filter rows of candidate_df that are part of the docs assigned to the user
        filter_docs = candidate_df[candidate_df.filename.isin(docs)]

        for index, row in filter_docs.iterrows():
            document_id = row['filename']
            mention_class = row["mention_class"]
            span_ini = row['span_ini']
            span_end = row['span_end']
            text = row["span"]
            # Check if the mention already exists for the current user and document
            existing_mention = collection.find_one({
                "document_id": document_id,
                "text": text,
                "project_id": project_id,
                "span_ini": span_ini,
                "span_end": span_end,
                "mention_class": mention_class,
                #"validated_by.user_id": user_name
            })
            if existing_mention:
                # If the mention already exists, add the user to the 'validated_by' list if not already present
                if not any(entry["user_id"] == user_name for entry in existing_mention["validated_by"]):
                    collection.update_one(
                        {"_id": existing_mention["_id"]},
                        {"$push": {"validated_by": {"user_id": user_name, "state": 0,
                                                    "previously_annotated": False, "no_code": False}}}
                    )
            else:
                # If the mention doesn't exist, insert a new mention with the user's validation info
                if filter_docs[(filter_docs.filename==document_id) & (filter_docs.span_ini==str(span_ini))  &  (filter_docs.span_end==str(span_end))].shape[0] == 0:
                    continue
                mention = {
                    "document_id": document_id,
                    "text": text,
                    "project_id": project_id,
                    "span_ini": span_ini,
                    "span_end": span_end,
                    "mention_class": mention_class,
                    "candidate_codes": filter_docs[(filter_docs.filename==document_id) & (filter_docs.span_ini==str(span_ini))  &  (filter_docs.span_end==str(span_end))].codes.iloc[0],
                    "validated_by": [
                        {
                            "user_id": user_name,
                            "state": 0,
                            "previously_annotated": False,
                            "no_code": False
                        }
                    ]
                }

                collection.insert_one(mention)
    # Close conection
    client.close()
    # Append all docs for admin.
    user_info_out.append(
        {"user": admin_user_name, "docs": list(np.unique(candidate_df.filename.to_list()))}
    )
     
    return user_info_out
                            
def create_update_user(mongo_url,db_name,collection_name,user_name, project_info):
    # Configurar la conexión con la base de datos MongoDB
    client = MongoClient(mongo_url)  # Cambia los parámetros según tu configuración
    db = client[db_name]  # Reemplaza 'tu_base_de_datos' con el nombre de tu base de datos
    collection = db[collection_name]  # Reemplaza 'users' con el nombre de tu colección

    try:
        # Intentar actualizar la entrada existente
        result = collection.update_one(
            {"user_name": user_name, "projects.project": project_info["project"]},
            {"$set": {"projects.$": project_info}}
        )

        if result.matched_count == 0:
            # Si no se encuentra una entrada para el proyecto, crear una nueva
            collection.update_one(
                {"user_name": user_name},
                {"$push": {"projects": project_info}}
            )

    except Exception as e:
        # En caso de error, imprimir el mensaje de error y cerrar la conexión
        print(f"Error: {str(e)}")
        

    client.close()   
    
    

def delete_project(mongo_url, db_name, projects_collection, documents_collection,
                    mentions_collection, users_collection, project_to_delete,
                    annotations_collection, delete_annotations=False):
    client = MongoClient(mongo_url)
    db = client[db_name]

    # Eliminar entrada con "name" "proyecto1" de la collection "proyectos"
    db[projects_collection].delete_one({"name": project_to_delete})

    # Eliminar entradas cuyo "project_id" sea "proyecto1" de la collection "documents"
    db[documents_collection].delete_many({"project_id": project_to_delete})

    # Eliminar entradas cuyo "project_id" sea "proyecto1" de la collection "menciones"
    db[mentions_collection].delete_many({"project_id": project_to_delete})

    # Actualizar las entradas de la collection "users"
    db[users_collection].update_many(
        {"projects.project": project_to_delete},
        {"$pull": {"projects": {"project": project_to_delete}}}
    )
    
    # Borrar datos de la collection "annotations" si delete_annotations es True
    if delete_annotations:
        db[annotations_collection].delete_many({"project_id": project_to_delete})


    client.close()

