FROM rocker/shiny:4.2
# Install system requirements for index.R as needed
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libsasl2-dev \
    libz-dev \
    pkg-config \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
RUN apt-get -y update
RUN apt-get -y install vim nano

ARG CACHEBUST=1

# GET FILES
RUN git clone https://github.com/luisgasco/scilinker.git

# ENVIRONMMENT
ENV RENV_VERSION 0.17.3
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"


#Install
RUN rm -r /srv/shiny-server/* && mkdir -p /srv/shiny-server/scilinker/renv && mkdir -p /srv/shiny-server/scilinker/data &&  mkdir -p /srv/shiny-server/scilinker/modules &&  mkdir -p /srv/shiny-server/scilinker/modules/annotation_utils && mkdir -p /srv/shiny-server/scilinker/www && chown -R shiny:shiny /srv/shiny-server/scilinker

WORKDIR scilinker/
RUN pwd && echo "output pwd asv" 
RUN ls -lh && echo "Ofasfasfautput of ls command above aasf" 

COPY renv.lock /srv/shiny-server/scilinker/
COPY renv/activate.R  /srv/shiny-server/scilinker/renv/
#COPY .Rprofile /home/shiny/

# CHANGE TO USER
# AQUI CAMBIA DE USUARIO/PROPIETARIO EL ROOT https://www.r-bloggers.com/2021/08/setting-up-a-transparent-reproducible-r-environment-with-docker-renv/
# Habrá qu cambiar la ruta relativa de UI.R
COPY ui.R /srv/shiny-server/scilinker/
COPY global.R /srv/shiny-server/scilinker/
COPY server.R /srv/shiny-server/scilinker/
COPY renv/* /srv/shiny-server/scilinker/renv/
COPY data/* /srv/shiny-server/scilinker/data/
COPY www/* /srv/shiny-server/scilinker/www/
COPY modules/annotation_utils/* /srv/shiny-server/scilinker/modules/annotation_utils/
COPY modules/* /srv/shiny-server/scilinker/modules/


WORKDIR /srv/shiny-server/scilinker
#RUN R -e "renv::hydrate()"
RUN R  -e "renv::restore(prompt=FALSE)"
#RUN R -e "renv::isolate()"


USER shiny
RUN R  -e "renv::restore(prompt=FALSE)"



# Entrar a srv/shiny-server/bioMnorm . A R y ejecutar renv:repair()
# Change to USER shiny and add permises so folder (ver error entrando dentro de BioMnorm a R (con R) y poniendo renv::restore(). Ahí dirá que no puede acceder a X folder)


#EXPOSE 3838

CMD ["/usr/bin/shiny-server"]



# WORKDIR /srv/shiny-server/scilinker/
# RUN ls -lh && echo "Output of ls shiny-server"
# RUN pwd && echo "output pwd"

