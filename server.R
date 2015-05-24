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


# loading ready prediction model
url.model <- getBinaryURL("https://raw.githubusercontent.com/Oleg-Davydov/winequality/master/modelFit.rda", 
                          ssl.verifypeer = FALSE);
load(rawConnection(url.model));



shinyServer(function(input, output) {
     
     data <- reactive({
#           pm <- as.data.frame(setNames(
#                replicate(11,numeric(0), simplify = F), names(wine)[1:11]))
#           
#           for (col in names(wine)[1:11]) 
#           {
#                pm[,col] <- input[[col]]
#           }
#           pm
          
          res <- as.data.frame(t(sapply(names(wine)[1:11], function(i) input[[i]])))
          res$color <- as.factor(input$color)
          row.names(res) <- c("Current values")
          res
          
     })
     
     # Generate a plot of the data. Also uses the inputs to build
     # the plot label. Note that the dependencies on both the inputs
     # and the data reactive expression are both tracked, and
     # all expressions are called in the sequence implied by the
     # dependency graph
     output$plot <- renderPlot({
          x <- seq(min(wine[,input$parplot]), max(wine[,input$parplot]), length.out = 100);
          ds <- data()[rep(1,100),];
          ds[,input$parplot]<-x;
          y <- predict(modelFit, newdata=ds);
          plot(y ~ x, type="l", xlab=input$parplot, ylab="Quality",
               main=paste("Quality vs",input$parplot,"\n",
                          "with current values of other parameters"));
     })
     
     # Generate a summary of the data
     output$summary <- renderPrint({
          summary(data()) # summary(data())
     })
     
     # Generate a summary of the data
     output$prediction <- renderUI({
          h1(round(predict(modelFit,newdata=data()),5));
     })


     # Generate an HTML table view of the data
     output$table <- renderTable({
          t(data())
     })
     
})