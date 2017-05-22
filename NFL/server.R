
library(shiny)
library(datasets)
library(reshape, quietly=TRUE)
library(dplyr)

# Taking input from a csv file.
dataset<-read.csv("Incident.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
#ds_player <- read.csv("file:///G:/1RIT/Big data/project/NFL-Fines-and-Suspensions/dataset/PLayer.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")
#ds_Team <- read.csv("file:///G:/1RIT/Big data/project/NFL-Fines-and-Suspensions/dataset/Team.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

                            
# Segregating dataset into numeric and categoric variables
nobs <- nrow(dataset)

sample <- train <- sample(nrow(dataset), 0.7*nobs)

input <- c("first_name", "last_name", "position", "team","fine_Category", "games_Suspended_From", "fined_amount", "time_title","date", "year")

numeric <- c("games_Suspended_From", "fined_amount", "year")

categoric <- c("first_name", "last_name", "position", "team","fine_Category", "time_title", "date")

set.seed(42)


shinyServer <- function(input, output) {
  
  
  
  
  output$contents <- renderTable({
    # Summarizes the dataset
    summary(dataset)
    
    
  })
  
  
  
  
  
  #kmeans <- kmeans(sapply(na.omit(dataset[sample, numeric]), rescaler, "range"), 10)
  # Applying K means clustering Algorithm on year, games suspended from and Fined Amount
  km <- reactive({
    if(input$cluster == "games_Suspended_From"){count <- 7}
    else{count <- 8}
     kmeans(dataset[,c(count,11)], input$clusters)
     #cluster_count <- input$clusters
  })
  
  # Plotting dataset after applying k means algorithm.
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    
    #par(mar = c(5.1, 4.1, 0, 1))
    plot(sapply(na.omit(dataset[,c(7,11)]), rescaler, "range"),
         col = km()$cluster,
         #ylim=c(0, 144),
         #xlim = c(0,50),
         #height = 600, width = 600,
         xlab = input$cluster,
         pch = 20, cex = 3)
    
    points(km()$centers, pch = 4, cex = 4, lwd = 4)
  }) 
 
  
  #plot(sapply(na.omit(dataset[,c(7,11)]), rescaler, "range"), col = km$cluster)
  
  # Plotting descriptive analysis graphs based on user inputs.
  output$result <- renderText({
    paste("You chose", input$FineCategory)
    seed <- 42
    set.seed(seed)
    ds <- rbind(summary(na.omit(dataset$input$category)))
    ord <- order(ds[1,], decreasing=TRUE)
    hist(ds,breaks = ds,ylim=c(0, 144), col = 'darkgray', border = 'white')
    
     })
  output$result1 <- renderPlot({
    
    # Render a barplot
    
    ds <- rbind(summary(na.omit(dataset[,input$category])))
    ord <- order(ds[1,], decreasing=TRUE)
    #barplot(ds[,ord])
    barplot(ds[,ord],
            main="Frequency of Fines and Suspensions for different categories.",
            ylab="Frequency",
            xlab=input$category)
    
    ds <- rbind(summary(na.omit(dataset[,input$category])))
    ord <- order(ds[1,], decreasing=TRUE)
    #barplot(ds[,ord])
    barplot(ds[,ord],
            main="Frequency of Fines and Suspensions for different categories.",
            ylab="Frequency",
            xlab=input$category)
    
    ds <- rbind(summary(na.omit(dataset[,input$category])))
    ord <- order(ds[1,], decreasing=TRUE)
    #barplot(ds[,ord])
    barplot(ds[,ord],
            main="Frequency of Fines and Suspensions for different categories.",
            ylab="Frequency",
            xlab=input$category)
            
  })
  output$role <- renderPlot({
    
    # Render a barplot
    ds <- rbind(summary(na.omit(dataset[,input$category])))
    ord <- order(ds[1,], decreasing=TRUE)
    #barplot(ds[,ord])
    barplot(ds[,ord],
            main="Frequency of Fines and Suspensions for different categories.",
            col=colorspace::rainbow_hcl,
            ylab="Frequency",
            xlab=input$category)
    
  })
  output$role <- renderPlot({
    
    # Render a barplot
    ds_player <- rbind(summary(na.omit(ds_player[,input$category])))
    ord <- order(ds_player[1,], decreasing=TRUE)
    #barplot(ds[,ord])
    barplot(ds_player[,ord],
            main="Frequency of Fines and Suspensions for different categories.",
            col=colorspace::rainbow_hcl,
            ylab="Frequency",
            xlab=input$category)
    
  })
  
}




