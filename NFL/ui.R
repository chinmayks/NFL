library(shiny)
library(datasets)
shinyUI <- fluidPage(
  titlePanel("NFL Fines and Suspensions"),
  sidebarLayout(
    sidebarPanel(
      
      h2('Data Exploration'),
      
      selectInput("category", 
                  label = "Select a Category",
                  choices = list("Most Common type of fine" = "fine_Category",
                                 "Role when fined" = "position",
                                  "Fine Across Teams"="team",
                                 "Most fined players" = "first_name",
                                 "Timeline of Fines" = "time_title",
                                 "Fined Date" = "date"
                                 #"Distribution of teams" = "teams",
                                 #"Distribution of Role" = "role",
                                 #"Age of first fine"="age_of_first_fine",
                                 #"number of fines" = "number_of_fines"
                                 ),
                  selected = "Role when fined"),
      strong(h2('Data Clustering')),
      
      selectInput("cluster",
                  label = "Cluster Analysis for:",
                  choices = list("Games Suspended from"="games_Suspended_From",
                                 "Fined Amount"="fined_amount"),
                  selected = "Games Suspended from"),
      sliderInput("clusters", label = "Cluster count",
                  min = 2, max = 10, value = 5)
      
    ),
    mainPanel(
      strong("Descriptive analysis of dataset"),
      plotOutput("result1"),
      strong("Data classifications into clusters"),
      plotOutput("plot1"),
      strong("Summary of dataset"),
      tableOutput("contents")
      
    )
  )
)