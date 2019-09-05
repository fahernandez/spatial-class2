FROM rocker/geospatial:3.6.0

RUN apt-get update && apt-get install -y cargo

RUN install2.r --error \
    --deps TRUE \
    dismo \
    XML \
    knitr \
    here \
    ggspatial \
    tmap \
    && R -e "devtools::install_github('geocompr/geocompkg')"