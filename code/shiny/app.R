rm(list = ls())

library(readxl)
library(tidyverse)
library(ggplot2)
library(stringr)
library(reshape2)
library(gifski)
library(gganimate)
library(shiny)
library(shinyWidgets)

dat <- read.csv("yearlyMelt.csv")
unique(dat$variable)

ui <- fluidPage(
  h1("Adjust the Billboard 100 Plot to Your Choosing"),
  plotOutput(outputId = "plt"),
  fluidRow(
    column(4,
           sliderInput("year", label = h3("Years Included"), min = min(dat$Year), 
                       max = max(dat$Year), value=c(min(dat$Year), max(dat$Year)), sep="", step=1)
    ),
    column(4,
           checkboxGroupInput("genres", label = h3("Genres Included"),
                              choices = unique(dat$variable),
                              selected = unique(dat$variable))
    ),
  )
) 

server <- function(input, output) {
  output$plt <- renderPlot({
    subsetted <- subset(dat, dat$Year <= input$year[2] & dat$Year >= input$year[1])
    subsettedAgain <- subsetted[subsetted$variable %in% input$genres, ]
    ggplot(subsettedAgain, aes(Year, value, color=variable)) +
      geom_line() +
      labs(x="Year",y="Number of Songs in Hot 100", color='Genre') +
      ggtitle("Number of Songs in the Billboard Hot 100 by Genre over Time")
  })
}

shinyApp(ui = ui, server = server)

