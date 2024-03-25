# Needs to be run from and as in https://github.com/rocker-org/rocker-versioned2.git
# docker build -f ./dockerfiles/geodev.Dockerfile -t geodev:1.0 /path/to/rocker-versioned2
# Last built: 2/8/2024
FROM rocker/verse:4.3.2

LABEL org.opencontainers.image.licenses="GPL-2.0-or-later" \
      org.opencontainers.image.source="https://github.com/rocker-org/rocker-versioned2" \
      org.opencontainers.image.vendor="Rocker Project" \
      org.opencontainers.image.authors="Carl Boettiger <cboettig@ropensci.org>"

ENV PROJ_VERSION=9.3.1
ENV GDAL_VERSION=3.8.3
ENV GEOS_VERSION=3.12.1
ENV S6_VERSION=v2.1.0.2
ENV RSTUDIO_VERSION=2023.12.0+369
ENV DEFAULT_USER=rstudio
ENV PANDOC_VERSION=default
ENV SHINY_SERVER_VERSION=latest
ENV QUARTO_VERSION=latest

COPY scripts/experimental/install_geospatial_unstable.sh /rocker_scripts/experimental/install_geospatial_unstable.sh

RUN /rocker_scripts/experimental/install_geospatial_unstable.sh
RUN /rocker_scripts/install_rstudio.sh
RUN /rocker_scripts/install_shiny_server.sh
RUN /rocker_scripts/install_pandoc.sh
RUN /rocker_scripts/install_quarto.sh

RUN apt-get update &&\
    apt-get install -y binutils libproj-dev gdal-bin

RUN R -q -e 'install.packages("devtools")'
RUN R -q -e 'install.packages("remotes")'
RUN R -q -e 'install.packages("BiocManager")'
RUN R -q -e 'install.packages("rmarkdown")'
RUN R -q -e 'install.packages("knitr")'
RUN R -q -e 'install.packages("xml2")'
RUN R -q -e 'install.packages("glue")'
RUN R -q -e 'install.packages("unglue")'
RUN R -q -e 'install.packages("tidyverse")'
RUN R -q -e 'install.packages("roxygen2")'
RUN R -q -e 'install.packages("workflowr")'
RUN R -q -e 'install.packages("usethis")'
RUN R -q -e 'install.packages("pkgdown")'
RUN R -q -e 'install.packages("httr")'
RUN R -q -e 'install.packages("dplyr")'
RUN R -q -e 'install.packages("lubridate")'
RUN R -q -e 'install.packages("stringi")'
RUN R -q -e 'install.packages("stringr")'
RUN R -q -e 'install.packages("sf")'
RUN R -q -e 'install.packages("sfheaders")'
RUN R -q -e 'install.packages("lwgeom")'
RUN R -q -e 'install.packages("terra")'
RUN R -q -e 'install.packages("arrow")'
RUN R -q -e 'install.packages("data.table")'
RUN R -q -e 'install.packages("leaflet")'
RUN R -q -e 'install.packages("leafem")'
RUN R -q -e 'install.packages("leafgl")'
RUN R -q -e 'install.packages("leafpop")'
RUN R -q -e 'install.packages("mapview")'
RUN R -q -e 'remotes::install_github("rstudio/webshot2")'
RUN R -q -e 'install.packages("cowplot")'
RUN R -q -e 'install.packages("patchwork")'
RUN R -q -e 'install.packages("foreach")'
RUN R -q -e 'install.packages("aws.s3")'
RUN R -q -e 'install.packages("doParallel")'
RUN R -q -e 'install.packages("concaveman")'

RUN R -q -e 'BiocManager::install("rhdf5")'

RUN R -q -e 'remotes::install_github("mikejohnson51/AOI")'

EXPOSE 8787

CMD ["/init"]