library(shiny)

shotlog <- read.csv("data/shot_logs2.csv")
View(shotlog)


shinyUI(
    fluidPage(
        # Application title
        titlePanel("NBA Player Matchup Analysis"),
        titlePanel(
            img(
                src = "http://murphguide.com/wp-content/uploads/2012/11/nyknicks-300x240.png",
                height = 100,
                width = 180,
                align = "top",
                style = "float:middle"
            )
        ),
        
        fluidRow(column(3,
            wellPanel(
                helpText("Please select player."),
                selectInput(
                    "player",
                    label = "Player",
                    choices = sort(unique(shotlog$player_name)),
                    selected = "STEPHEN CURRY"
                ),
                submitButton("Submit")
            )
        ),
        column(10,
            wellPanel(
                textOutput("pl1"),
        
        tabsetPanel(
                    tabPanel("Offense",
                             plotOutput("chart1"),
                             DT::dataTableOutput("datatab"),
                             plotlyOutput("chart3",width = 1400,height = 1200),
                    tabPanel("Defense"
                             )
                            )
                    )
        ))
    ))

)

