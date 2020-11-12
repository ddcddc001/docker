# Build image for R based on rocker-rver-3.6.3

# Pull R base image
FROM rocker/r-ver:3.6.3


# Update apt
RUN apt-get -y update


# Install java
RUN apt-get -y install default-jdk
#RUN R CMD javareconf


# Install libs for building 'rJava'
RUN apt-get -y install libpcre3-dev
RUN apt-get -y install liblzma-dev
RUN apt-get -y install libbz2-dev
RUN apt-get -y install libz-dev
RUN apt-get -y install libicu-dev


# Install R packages
RUN R -e "install.packages('RJDBC')"
RUN R -e "install.packages('RSQLite')"

RUN R -e "install.packages('data.table')"
RUN R -e "install.packages('digest')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('stringdist')"

RUN R -e "install.packages('igraph')"

RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('visNetwork')"

RUN R -e "install.packages('DT')"
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('shinybusy')"







