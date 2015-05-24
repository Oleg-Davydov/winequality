library(shiny);
library(RCurl);

url.white <- getURL("https://raw.githubusercontent.com/Oleg-Davydov/winequality/master/winequality-white.csv",
                    ssl.verifypeer = FALSE);
url.red <- getURL("https://raw.githubusercontent.com/Oleg-Davydov/winequality/master/winequality-red.csv",
                  ssl.verifypeer = FALSE);


white.wine <- read.table(textConnection(url.white), sep=";", header=TRUE);

red.wine <- read.table(textConnection(url.red), sep=";", header=TRUE);

white.wine$color <- "white";
red.wine$color <- "red";

wine <- rbind(white.wine, red.wine);

wine$color <- as.factor(wine$color);

wine <- wine[,c(1:11,13,12)];


shinyUI(fluidPage(
     
     # Application title
     titlePanel("Portugal wines quality assessment"),
     
     fluidRow(
          column(11, offset=0.1,
          div(paste("This application is made for assessing 
            Portugal wine quality (lowest = 1, highest = 9) by its physicochemical parameters.",
               "Here you can get prediction of wine 
            quality by particular combination of the parameters' values.
               Besides, you can make some inference
            about quality dependence of these parameters.
            For further information about data source and modelling tools, see Reference below."))
          )
     ),     
     
     fluidRow(
          column(2, wellPanel(
               radioButtons("color", "Color:",
                            levels(wine$color)),
               lapply(1:6, function(i)
               {
                    sliderInput(inputId=names(wine)[i], label=names(wine)[i], 
                                min = round(min(wine[,i]),2),
                                max = round(max(wine[,i]),2),
                                value = round(mean(wine[,i]),2),
                                step= (max(wine[,i])-min(wine[,i]))/100)
               })
          )),
          
          column(2, wellPanel(
               selectInput('parplot', 'Choose parameter for plot', names(wine[,1:11])),
               lapply(7:11, function(i)
               {
                    sliderInput(inputId=names(wine)[i], label=names(wine)[i], 
                                min = round(min(wine[,i]),2),
                                max = round(max(wine[,i]),2),
                                value = round(mean(wine[,i]),2),
                                step= (max(wine[,i])-min(wine[,i]))/500)
               })
          )),
          
          column(2,
               h1("Predicted Quality:"),
               uiOutput("prediction"),
               tableOutput("table")
          ),
          
          column(4, offset=1,
                 plotOutput("plot")
          )

     ),
     
     fluidRow(
          column(11, offset=0.1,
                 h1("Reference"),
                 p("Data for this app was taken from UCI repository:",
                   a(href="https://archive.ics.uci.edu/ml/datasets/Wine+Quality",
                     target="_blanc",
                     "https://archive.ics.uci.edu/ml/datasets/Wine+Quality")),
                 p("Original source: Paulo Cortez, University of Minho, Guimaraes, Portugal,",
                    a(href="http://www3.dsi.uminho.pt/pcortez",
                      target="_blanc",
                      "http://www3.dsi.uminho.pt/pcortez")),
                 p("A. Cerdeira, F. Almeida, T. Matos and J. Reis, Viticulture Commission
                   of the Vinho Verde Region(CVRVV), Porto, Portugal @2009"),
                 p("Preprocessing and modelling were performed in",
                   a(href="http://www.rstudio.com/", target="_blanc","RStudio."),
                   "Modelling algorithm - Random Forest (regression) via caret package."),
                 p("Application is made with the help of shiny package and published
                    via",
                    a(href="https://www.shinyapps.io/", target="_blanc", "shinyapps.io"),
                   "service of RStudio."),
                 p("Source code of the app can be found on my Github repository:",
                   a(href="https://github.com/Oleg-Davydov/winequality",
                     target="_blanc",
                     "https://github.com/Oleg-Davydov/winequality"))
                 )
                 )
))