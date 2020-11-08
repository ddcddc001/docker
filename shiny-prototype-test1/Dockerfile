FROM rocker-3.6.3-libs:20201106

RUN mkdir /home/shinyapp1

COPY *.R /home/shinyapp1
COPY *.csv /home/shinyapp1

WORKDIR /home/shinyapp1

CMD ["Rscript","shiny-Prototype-test1.R"]