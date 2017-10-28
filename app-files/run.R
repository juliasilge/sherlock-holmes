library(shiny)  
port <- Sys.getenv('PORT')

rmarkdown::run(  
    '/home/rstudio/sherlock-topic-model-app.Rmd',
    host = '0.0.0.0',
    port = as.numeric(port)
)
