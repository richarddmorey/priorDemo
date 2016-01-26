# priorDemo
A demo of various prior distributions for a Bayes factor t test.


To run, ensure your `shiny` installation is updated (this is important) 

    install.packages(c('shiny','shinyjs','Cairo'))

you'll also need to install the `bioconductor` package `svgAnnotation`:

    source("http://bioconductor.org/biocLite.R")
    biocLite("SVGAnnotation")

and then run in R:
    
    shiny::runGitHub( "priorDemo", "richarddmorey")

to run the `shiny` app. Explanation can be found on the "Instructions" tab.
