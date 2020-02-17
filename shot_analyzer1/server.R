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



shinyServer(function(input, output) {
    output$pl1 <- renderText({
        paste(toupper(input$player))
    })
    
    output$chart1 <- renderPlot({
        # shots made vs missed at away or home 
        madevmissed <- shotlog %>%
            group_by(player_name, LOCATION, SHOT_RESULT) %>%
            summarize(countratio = n()) %>%
            mutate(ratio = countratio / sum(countratio)) %>%
            filter(player_name == input$player)
        madevmissed <- as.data.frame(madevmissed)
        colnames(madevmissed) <- c("Player","Location","Shot_Result","Shot_Count","Shot_Pct")
        madevmissed$Location <-
            with(madevmissed, factor(Location, levels = rev(levels(Location))))
        
        g1 <- ggplot(madevmissed, aes(x = Shot_Result, y = Shot_Pct)) + facet_grid(. ~ Location) +
            ggtitle(paste("%Shots Made vs. Missed by ",toupper(unique(input$player)),
                "\n Home vs Away Games" ,sep = "")) +
            geom_bar(stat = "identity",
                     fill = "blue",
                     alpha = 0.8) +
            xlab("Made vs. Missed") + 
            ylab("% Shots")
        
        ## Defender graphs
        shotdef <- shotlog %>%
            filter(player_name == input$player)%>%
            group_by(player_name, CLOSEST_DEFENDER, SHOT_RESULT) %>%
            summarize(Count = n())
        
        shotdef <- as.data.frame(shotdef)
        shotdef <- data.frame(shotdef,Value = TRUE)
        shotdef <- reshape(shotdef, idvar = c("player_name","CLOSEST_DEFENDER"),
                           timevar = "SHOT_RESULT", direction = "wide")
        shotdef <- shotdef[,c(1,2,3,5)]
        colnames(shotdef) <- c("Player Name", "Defender","Shots Made", "Shots Missed")
        
        Bestdef <- shotdef %>%
            arrange(desc(`Shots Missed`))
        Bestdef <- Bestdef[1:10,]       
        
        Worstdef <- shotdef %>%
            arrange(desc(`Shots Made`))
        Worstdef <- Worstdef[1:10,]
        
        g2 <- ggplot(Bestdef, aes(reorder(Defender,-`Shots Missed`), `Shots Missed`)) +
            geom_bar(stat = "identity", fill = "blue", alpha=0.8) + 
            xlab("Top Defender") + 
            ylab("# Shots Missed") + ylim(0,20) + 
            ggtitle(paste("10 Best Defenders against")) +
            theme(axis.text.x = element_text(angle = 60, hjust = 1))
        g3 <- ggplot(Worstdef, aes(reorder(Defender,-`Shots Made`), `Shots Made`)) +
            geom_bar(stat = "identity", fill = "blue", alpha=0.8) + 
            xlab("Defender") + 
            ylab("# Shots Made") +  ylim(0,20) + 
            ggtitle(paste("10 Worst Defender")) +
            theme(axis.text.x = element_text(angle = 60, hjust = 1))
        grid.arrange(g1, g2,g3, ncol=3)
        
        
    })
    output$datatab <- DT::renderDataTable({
        shot.dist <- shotlog %>%
            filter(player_name == input$player)
        shot.dist <- as.data.frame(shot.dist)
        shot.dist$distrange <- cut(shot.dist$SHOT_DIST, c(0,5,10,15,20,25,30,50)) #create inteverals of 5
        levels(shot.dist$distrange) <- c("Less than 5Ft","5-9Ft","10-14Ft","15-19Ft","20-24Ft","25-29Ft","30Ft+") #convert to levels
        shot.dist <- shot.dist[,c(2:4,6:8,10:14,18,20,22)] #target cols
        shot.dist <- shot.dist %>%
            group_by(player_name, distrange, SHOT_RESULT) %>%
            summarize(Count = n())
        shot.dist <- data.frame(shot.dist,Value = TRUE)
        shot.dist <- reshape(shot.dist, idvar = c("player_name","distrange"),
                            timevar = "SHOT_RESULT", direction = "wide")
        shot.dist <- shot.dist[,c(1,2,3,5)]
        colnames(shot.dist) <- c("Player Name", "Shot Distance","Shots Made", "Shots Missed")
        DT::datatable(shot.dist, options = list(pageLength = 10), rownames = FALSE)
    })

    output$chart3 <- renderPlotly({
        plot_ly(data = df_type, y = ~three_pointers_made, color = ~player_name, type = "bar",colors = c("#132B43", "#56B1F7")) %>% 
            layout(legend = list(x = -.4, y = 1.0),
                   title = list(title='Top Three Point Shooters'),
                   xaxis = list(title='Players'))
        
    })



    output$chart4 <- renderPlot({
        shottype <- shotlog %>%
            filter(player_name == input$player)%>%
            group_by(player_name, PTS_TYPE, SHOT_RESULT) %>%
            summarize(Count = n())
        
        shottype <- as.data.frame(shottype)
        shottype <- data.frame(shottype,Value = TRUE)
        shottype <- reshape(shottype, idvar = c("player_name","Type"),
                           timevar = "SHOT_RESULT", direction = "wide")
        shottype <- shottype[,c(1,2,3,5)]
        colnames(shottype) <- c("Player Name", "Type","Made", "Missed")
        
        three <- shottype %>% filter('Type' == 3) %>% 
        arrange(desc('Made'))
        three <- three[1:10,]      
        
        two <- shottype %>% filter('Type' == 2) %>% 
            arrange(desc('Made'))
        two <- two[1:10,]
        
        g4 <- ggplot(three, aes(reorder('Pts Type',-'Missed'), 'Missed')) +
            geom_bar(stat = "identity", fill = "blue", alpha=0.8) + 
            xlab("Shot Type") + 
            ylab("# Shots Missed") + ylim(0,20) + 
            ggtitle(paste("Three Pointers")) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        g5 <- ggplot(two, aes(reorder('Type',-'Made'), 'Made')) +
            geom_bar(stat = "identity", fill = "blue", alpha=0.8) + 
            xlab("Shot Type") + 
            ylab("# Shots Made") +  ylim(0,20) + 
            ggtitle(paste("Two Pointers")) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        grid.arrange(g4,g5, ncol=2)
        
        ?reorder
    })

})


    ## First Quarter Top Shooters
    
    first_q <- shotlog %>% %>% filter(PERIOD == 1, SHOT_DIST > 5) %>% group_by(player_name) %>% 
        summarise(made = sum(FGM), 
                  total_attempts = length(FGM),
                  ave_defense_dist = round(mean(CLOSE_DEF_DIST, na.rm = T),2),
                  ave_touch = round(mean(TOUCH_TIME, na.rm = T),2),
                  ave_dribble = round(mean(DRIBBLES, na.rm = T),2),
                  ave_shot_clock = round(mean(SHOT_CLOCK, na.rm = T),2),
                  ave_distance = round(mean(SHOT_DIST, na.rm = T),2)) %>%
        mutate(percentage = round(made/total_attempts,2)) %>%
        arrange(desc(percentage)) %>% filter(total_attempts > 150)
    best_1st <- data.frame(first_q)
    datatable(best_1st[1:20,])


    ## Fourth Quarter Top Shooters
    
    
    fourth_q <- shotlog %>% filter(PERIOD == 4, SHOT_DIST > 5) %>% group_by(player_name) %>% 
        summarise(made = sum(FGM), 
                  total_attempts = length(FGM),
                  ave_defense_dist = round(mean(CLOSE_DEF_DIST, na.rm = T),2),
                  ave_touch = round(mean(TOUCH_TIME, na.rm = T),2),
                  ave_dribble = round(mean(DRIBBLES, na.rm = T),2),
                  ave_shot_clock = round(mean(SHOT_CLOCK, na.rm = T),2),
                  ave_distance = round(mean(SHOT_DIST, na.rm = T),2)) %>%
        mutate(percentage = round(made/total_attempts,2)) %>%
        arrange(desc(percentage)) %>% 
        filter(total_attempts > 150)
    best_4th <- data.frame(fourth_q)
    datatable(best_4th[1:20,])
