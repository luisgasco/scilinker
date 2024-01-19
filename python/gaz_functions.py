import pandas as pd
from pymongo import MongoClient

def create_gaz_info(gaz_name, gaz_description, gaz_hyperlink, db_url, db_name, collection):
    """
    Function to introduce to database info about terminology
    """
    # Initialize mongo object
    client = MongoClient(db_url)
    db = client[db_name]
    collection = db[collection]
    
    # Create structure to introduce
    insertion = {
        "name":gaz_name,
        "description": gaz_description,
        "hyperlink_pattern":gaz_hyperlink
    }
    # insert 
    collection.insert_one(insertion)
    
def create_gaz_entries(data, gaz_name, db_url, db_name, collection):
    """
    Function to introduce to database lexical entries of terminology
    """
    records = data.to_dict(orient='records')
    for record in records:
        record['gazetteer_id'] = gaz_name
    # Initialize mongo object
    client = MongoClient(db_url)
    db = client[db_name]
    collection = db[collection]
    # insert
    collection.insert_many(records)
    
    
def update_gaz_hyperlink(db_url, db_name, collection,gaz_id, new_pattern):
    # Initialize mongo object
    client = MongoClient(db_url)
    db = client[db_name]
    collection = db[collection]
    
    collection.update_one(
            {"name": gaz_id},
            {"$set": {"hyperlink_pattern": new_pattern}}
        )
    
    client.close()
        

    
