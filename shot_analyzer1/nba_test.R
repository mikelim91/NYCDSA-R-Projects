library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
library(lubridate)


# shot distance plot of all players
plot_ly(data = shot_logs_2, y = ~SHOT_DIST, color = ~player_name, type = "box") %>% 
  layout(legend = list(x = 0.2, y = 1.0))


# close defender distance of all plaers against shooter
plot_ly(data = shot_logs_2, y = ~CLOSE_DEF_DIST, color = ~player_name, type = "box") %>% 
  layout(legend = list(x = 0.2, y = 1.0))

shot_logs_2$GAME_CLOCK

gc <- shot_logs_2$GAME_CLOCK
# cleans gc column to mins sec
gc_new <- gc %>%  sub(":00$","",.) %>% ms() 
# replaces na with value 0 secs
gc_new[is.na(gc_new)] <- 0
gc_new

#### shiny server ####


shinyServer(function(input, output) {
  
  output$pl1 <- renderText({
    paste("You have selected", input$player, "as your player of interest\n")
    
  })

  output$shotchart1 <- renderPlot({
    # generate data
    shot_logs_2 %>%
      group_by(player_name, LOCATION, SHOT_RESULT) %>%
      summarize(countmm = n()) %>%
      mutate(prop = countmm / sum(countmm))
    
    shotmvm <- as.data.frame(shotmvm)
    colnames(shotmvm) <-
      c("Player",
        "Location",
        "Shot_Result",
        "Shot_Count",
        "Shot_Pct")
    shotmvm$Location <-
      with(shotmvm, factor(Location, levels = rev(levels(Location))))

    # generate shots taken (made vs. missed)
    
    ggplot(shotmvm, aes(x = Shot_Result, y = Shot_Pct)) + facet_grid(.~ Location) +
      ggtitle(paste("%Shots Made vs. Missed by ", unique(input$player),"\n Home vs Away Games" , sep = "")) +
      geom_bar(stat = "identity", fill = "orange", alpha = 0.8) +
      xlab("Shots Made vs. Missed") + ylab("% Shots")
    
  })

})

  output$shotchart2 <- renderPlot({
    # generate data
    shotmvmw <- shotlog %>%
      group_by(player_name, W, SHOT_RESULT) %>%
      summarize(countmm = n()) %>%
      mutate(prop = countmm / sum(countmm)) %>%
      filter(player_name == input$player)
    shotmvmw <- as.data.frame(shotmvmw)
    colnames(shotmvmw) <-
      c("Player", "WL", "Shot_Result", "Shot_Count", "Shot_Pct")
    shotmvmw$WL <-
      with(shotmvmw, factor(WL, levels = rev(levels(WL))))
    
    
    # generate shots taken (made vs. missed)
    
    ggplot(shotmvmw, aes(x = Shot_Result, y = Shot_Pct)) + facet_grid(.~ WL) +
      ggtitle(paste("%Shots Made vs. Missed by ", unique(input$player),"\n Win vs.Loss Games", sep = "")) +
      geom_bar(stat = "identity", fill = "orange", alpha = 0.8) +
      xlab("Shots Made vs. Missed") + ylab("% Shots")
    
  })

  
  


# Update values to GAME_CLOCK
shot_logs_2$GAME_CLOCK <- gc
shot_logs_2$GAME_CLOCK[1]

# choose target columns
df <-  select(shot_logs_2,-c(GAME_ID,FINAL_MARGIN,PERIOD,Column2,Column1,CLOSEST_DEFENDER_PLAYER_ID,player_id))
View(df)

# close defender distance of all plaers against shooter
plot_ly(data = df, y = ~CLOSE_DEF_DIST, color = ~player_name, type = "box") %>% 
  layout(legend = list(x = 0.2, y = 1.0))


# use helper file to make court
source("helpers.R")
gg_court <- make_court()


df %>% select(.,player_name,SHOT_RESULT) %>% filter(.,SHOT_RESULT == 'made')

class(df$SHOT_RESULT)

player_data <- filter(df$SHOT_RESULT, player_name == "brian roberts")

gg_court + geom_point(
  data = player_data,
  alpha = 0.75,
  size = 2.5,
  aes(loc_x, loc_y, color = shot_made_flag)
) + scale_color_manual("", values = c(made = "blue", missed = "orange"))


# Count the number of misses and made
df %>%
  group_by(player_name, SHOT_DIST, SHOT_RESULT) %>%
  summarize(Count = n())

df <- df %>%
  group_by(player_name, distrange, SHOT_RESULT) %>%
  summarize(Count = n())
df <- data.frame(df,Value = TRUE)
df <- reshape(df, idvar = c("player_name","distrange"),
              timevar = "SHOT_RESULT", direction = "wide")
df <- df[,c(1,2,3,5)]
colnames(df) <- c("Player Name", "Distance Range","Shots Made", "Shots Missed")



df$SHOT_RESULT
df$SHOT_RESULT %>% sub('made',1,.) %>% gsub('missed',0)

df$SHOT_RESULT %>% as.integer(as.logical(c("made","missed")))

gg_court + geom_point(data = player_data, alpha = 0.75, size = 2.5,
                      aes(loc_x, loc_y, color = shot_made_flag)) +
  scale_color_manual("", values = c(made = "blue", missed = "orange"))
