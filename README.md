#This is the readme
Si estoy en server:
- Apagar: sudo docker-compose -f docker-compose_server.yml down
- Borrar: sudo docker system prune
- Re-build: sudo docker-compose -f docker-compose_server.yml build --no-cache
- Encender: sudo docker-compose -f docker-compose_server.yml up


Una vez encendido:
docker exec -u root -it DOCKER_ID bash
nano /data/.config_file y update needed values
nano global.R y cambiar la variable global local
sudo service shiny-server restart



