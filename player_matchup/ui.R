library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
library(plotly)
library(DT)

shotlog <- read.csv("data/shot_logs2.csv")
df_type <- read.csv("data/df_type.csv")


shinyUI(
    fluidPage(
        # Application title
        titlePanel("NBA Player Matchup Analysis"),
        fluidRow(column(3,
            wellPanel(
                helpText("Please select player."),
                selectInput(
                    "player",
                    label = "Player",
                    choices = sort(unique(shotlog$player_name))
                ),
                submitButton("Submit")
            )
        ),
        column(10,
            wellPanel(
                textOutput("pl1"),
        
        tabsetPanel(
                    tabPanel("Statistics",
                             titlePanel("Player Performance"),
                             plotOutput("chart1"),
                             titlePanel('Shot Distance'),
                             DT::dataTableOutput("datatab"),
                             titlePanel('First Quarter Performers'),
                             DT::dataTableOutput("datatab1"),
                             titlePanel('Fourth Quarter Performers'),
                             DT::dataTableOutput("datatab2"),
                             titlePanel("Top 100 3-Point Shooters"),
                             plotlyOutput("chart3",width = 1500,height = 1200),
                    tabPanel("Defense",
                             )
                            )
                    )
        ))
    ))

)

