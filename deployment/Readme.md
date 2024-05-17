---
output:
  pdf_document: default
  html_document: default
---

## Readme for deployment with docker-compose

To install the application using docker-compose you can use the `docker-compose_scilinker_with_mongo.yml` file. The mongo db contains two services: *shiny-app*, which is the scilinker application itself; and *mongo_scilinker*, which raises a mongodb service specific to the scilinker application where the annotation data will be stored.

The docker-compose has 3 variables parameterized in the `.env_app` file, which can be customized to launch the application:

-   **APP_PORT**: This is the port that will be used to access through the browser. By default it has the value `8002`. If the application is running locally it would be accessed using the url [**http://localhost:8002/scilinker/**](http://localhost:8002/scilinker/){.uri}.
-   **MONGO_VOLUME_PATH**: This is the system folder where the data from the MongoDB database used by the application will be stored. The folder must exist in the system, otherwise errors will appear.
-   **EXTERNAL_PORT_MONGO**: This is the port exposed by the MongoDB service so that we can access the data from the host. By default it has the value 27017.

Once the environment files are configured, before launching the docker-compose, it is necessary to check the application configuration. For that we need to validate the values of the application configuration file `scilinker/data/.config_file`. If we are going to launch the complete docker-compose service (with shiny-app and mongo_scilinker), we should test the following:

-   **MONGODB_HOST**, which is used by the application to connect to the MongoDB database. It should have the value "mongo_scilinker". If we are working locally, running the application from RStudio with a local MongoDB database we should use the value "localhost".

Once those steps have been finished, to launch the application we must follow the following steps:

1.  Build the docker images:

    1.  Go to the deployment folder: `cd deployment`

    2.  Build the image using the .env_app file: `docker compose -f docker-compose_scilinker_with_mongo.yml --env-file .env_app build --no-cache`

2.  Run the docker-compose:

    1.  Run in background: `docker compose -f docker-compose_scilinker_with_mongo.yml --env-file .env_app up -d`

    2.  Check that docker services are running: `docker ps`

    3.  If you want to stop the services: `docker compose -f docker-compose_scilinker_with_mongo.yml --env-file .env_app down`

If you try to enter the application and you get an error, you can access the scilinker service in this way:

1.  Enter scilinker service: `docker exec -u root -it scilinker_docker_id bash`
2.  Once inside, you can verify that 'data/.config_file' has the correct values. You can also see the scilinker logs in the path `cd /var/log/shiny-server`
3.  If you modify anything, you should restart the shiny server using `sudo service shiny-server restart`
