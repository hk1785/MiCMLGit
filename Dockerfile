FROM openanalytics/r-base

MAINTAINER Hyunwook Koh "hyunwook.koh@stonybrook.edu"

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libgsl-dev \
    libssh2-1-dev \
    libssl1.1 \
    libxml2-dev \
    build-essential \
    r-base-dev \
    pkgconf \
    cmake \
    libtiff5-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libgdal-dev \
    && rm -rf /var/lib/apt/lists/*
    
RUN apt-get update && apt-get install -y \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('rgl', 'ape', 'BiocManager', 'bios2mds', 'caret', 'checkmate', 'compositions', 'data.table', 'doParallel', 'DT', 'ecodist', 'fossil', 'fontawesome', 'GUniFrac', 'googleVis', 'ggplot2', 'ggplotify', 'grid', 'grf', 'htmltools', 'Matrix', 'MiRKAT', 'phangorn', 'picante', 'plotly', 'proxy', 'randomForest', 'remotes', 'reshape2', 'rpart', 'rpart.plot', 'rmarkdown', 'seqinr', 'shiny', 'shinydashboard', 'shinyjs', 'shinyWidgets', 'stringr', 'tidyverse', 'vegan', 'VGAM', 'xtable', 'zCompositions', 'zip'), repos='https://cloud.r-project.org/')"
RUN R -e "BiocManager::install('phyloseq')"
RUN R -e "remotes::install_github('joey711/biomformat')"
RUN R -e "remotes::install_github('nik01010/dashboardthemes', force = TRUE)"
RUN R -e "remotes::install_github('wdl2459/ConQuR', force = TRUE)"
RUN R -e "BiocManager::install('sva')"
RUN R -e "remotes::install_github('prise6/aVirtualTwins', build_vignettes = TRUE)"
RUN R -e "remotes::install_github('hk1785/MiVT', force = TRUE)"
RUN R -e "remotes::install_github('Zhiwen-Owen-Jiang/MiRKATMC', force = TRUE)"

RUN mkdir /root/app
COPY app /root/app
COPY Rprofile.site /usr/lib/R/etc/

COPY app/Data/Phyloseq/biom.Rdata /root/app
COPY app/Data/Individual/otu.tab.txt /root/app
COPY app/Data/Individual/sam.dat.txt /root/app
COPY app/Data/Individual/tax.tab.txt /root/app
COPY app/Data/Individual/tree.tre /root/app

COPY app/www/home.png /root/app

COPY app/MiDataProc.DataInput.R /root/app
COPY app/MiDataProc.Descriptive.R /root/app
COPY app/MiDataProc.GLM.R /root/app
COPY app/MiDataProc.CML.R /root/app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/app')"]
