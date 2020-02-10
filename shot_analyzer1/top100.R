
shotlog <- read.csv("data/shot_logs2.csv")
#initiate
df_type <- cbind(top.threes.made,top.threes.missed,top.twos.made,top.twos.missed)

#top 100 two and three pointers made
two.made <- shotlog %>% group_by(player_name) %>% unique() %>% filter(.,PTS_TYPE == 2,SHOT_RESULT == 'made') %>% summarise(., two_pointers_made = n())
two.missed <- shotlog %>% group_by(player_name) %>% unique() %>% filter(.,PTS_TYPE == 2,SHOT_RESULT == 'missed') %>% summarise(., two_pointers_missed = n())
three.made <- shotlog %>% group_by(player_name) %>% unique() %>% filter(.,PTS_TYPE == 3,SHOT_RESULT == 'made') %>% summarise(., three_pointers_made = n())
three.missed <- shotlog %>% group_by(player_name) %>% unique() %>% filter(.,PTS_TYPE == 3,SHOT_RESULT == 'missed') %>% summarise(., three_pointers_missed = n())

#top 100 players two pointers
top.twos.made <- two.made %>% arrange(.,desc(two_pointers_made)) %>% top_n(.,n=100)
#top 100 players with missed shots
top.twos.missed <- two.missed  %>% arrange(.,desc(two_pointers_missed)) %>% top_n(.,n=100)
#top 100 players three pointers
top.threes.made <- three.made %>% arrange(.,desc(three_pointers_made)) %>% top_n(.,n=100)
#top 100 players with missed shots
top.threes.missed <- three.missed %>% arrange(.,desc(three_pointers_missed)) %>% top_n(.,n=100) %>% head(.,100)




#Top 100 player total shots made away & home
made.away <- shotlog %>% group_by(player_name) %>% unique() %>% filter(.,LOCATION == 'A',SHOT_RESULT == 'made') %>% summarise(made.away = n())%>% arrange(.,desc(made.away)) %>% top_n(.,100)
made.home <- shotlog %>% group_by(player_name) %>% unique() %>% filter(.,LOCATION == 'H',SHOT_RESULT == 'made') %>% summarise(made.home = n())%>% arrange(.,made.home) %>% top_n(.,100)
missed.away <- shotlog %>% group_by(player_name) %>% unique() %>% filter(.,LOCATION == 'A',SHOT_RESULT == 'missed') %>% summarise(missed.away = n())%>% arrange(.,missed.away) %>% top_n(.,100)
missed.home <- shotlog %>% group_by(player_name) %>% unique() %>% filter(.,LOCATION == 'H',SHOT_RESULT == 'missed') %>% summarise(missed.home = n())%>% arrange(.,missed.home) %>% top_n(.,100)


# top 100 overall successful shots
top.100.made <- shot.made %>% arrange(.,desc(made)) %>% top_n(.,n=100)
top.100.made <- top.100.made %>% rename(best.scorer = player_name)

# top 100 overall failed shots
top.100.missed <- shot.missed %>% arrange(.,desc(missed)) %>% top_n(.,n=100) %>% head(.,100)
top.100.missed <- top.100.missed %>% rename(worst.scorer = player_name)

#top 100 players with missed shots
top.threes.missed <- three.missed %>% arrange(.,desc(three_pointers_missed)) %>% top_n(.,n=100) %>% head(.,100)
#top 100 players with least amount of missed shots
top.threes.least <- three.missed %>% arrange(.,three_pointers_missed) %>% top_n(.,n=100) %>% head(.,100)

#Top 100 players fg avg .4526786
(sum(top.100.made[,2])/100)/(sum(top.100.made[,2]+top.100.missed[,2])/100)
# data frame
top.100.made[,2]/(top.100.made[,2]+top.100.missed[,2])

# Top 100 closest defender which caused a miss
top.100.defender <- shotlog %>% group_by(CLOSEST_DEFENDER) %>% unique() %>% filter(.,SHOT_RESULT == 'missed') %>% summarise(defense = n()) %>% arrange(.,desc(defense)) %>% top_n(.,100)
# Top 100 worst defender
worst.100.defenders <- shotlog %>% group_by(CLOSEST_DEFENDER) %>% unique() %>% filter(.,SHOT_RESULT == 'made') %>% summarise(no_defense = n()) %>% arrange(.,no_defense) %>% top_n(.,100)
worst.100.defenders <- worst.100.defenders %>% rename(worst.defender = CLOSEST_DEFENDER)
top.100.defender <- top.100.defender %>% rename(best.defender = CLOSEST_DEFENDER)

levels(shotlog$SHOT_DIST)

aaronbrooks.dist <- shotlog %>%
  filter(player_name == 'aaron brooks')


aaronbrooks.dist <- as.data.frame(aaronbrooks.dist) #converts to dataframe
aaronbrooks.dist$distrange <- cut(aaronbrooks.dist$SHOT_DIST, c(0,5,10,15,20,25,30,50)) # rename and converts range of x into distance shot intervals of 5 ft

levels(aaronbrooks.dist$distrange) <- c("Less than 5Ft","5-9Ft","10-14Ft","15-19Ft","20-24Ft","25-29Ft","30Ft+") #convert distance to ft levels for grouping

aaronbrooks.dist <- aaronbrooks.dist[,c(2:4,6:8,10:14,18,20,22)] #target cols
aaronbrooks.dist

View(cbind(
top.100.defender,
worst.100.defenders,
top.threes.made,
top.threes.missed,
top.threes.least,
top.twos.made,
top.twos.missed,
top.two.least))
worst.100.defenders[1] <- 'Worst Defenders'
woprst

plot_ly(data = df_type, y = ~three_pointers_made, color = ~player_name, type = "bar") %>% 
  layout(legend = list(x = -.2, y = 1.0))

#initiate
df_type <- cbind(top.threes.made,top.threes.missed,top.twos.made,top.twos.missed)




df_type
df_defender <- cbind(top.100.defender,worst.100.defenders)
df_defender[c('CLOSEST_DEFENDER ','CLOSEST_DEFENDER ') <- c('Best Defender','Worst Defender')
df_defender


plot_ly(data = df_defender, y = ~defense, color = ~best.defender, type = "bar") %>% 
  layout(legend = list(x = -.2, y = 1.0))

colnames(shotlog)

shotlog %>% plot_ly(x=~SHOT_RESULT, y = ~PTS_TYPE,type = "bar") %>% 
  layout(legend = list(x = -.2, y = 1.0))


plot_ly(data = df_defender, y = ~three_pointers_made, color = ~player_name, type = "bar") %>% 
  layout(legend = list(x = -.2, y = 1.0))

View(df_type)
colnames(df_type)
1] "player_name"           "three_pointers_made"   "player_name"           "three_pointers_missed" "player_name"           "three_pointers_missed"
[7] "player_name"           "two_pointers_made"     "player_name"           "two_pointers_missed"   "player_name"           "two_pointers_missed"


View(shotlog %>% select(.,player_name,SHOT_DIST,SHOT_RESULT) %>% group_by(.,player_name) %>% filter(.,SHOT_RESULT == 'missed') %>% arrange(.,desc(SHOT_DIST)) %>% head(.,100))

shotlog %>% select(.,player_name,SHOT_DIST,SHOT_RESULT) %>% group_by(.,player_name) %>% unique() %>% filter(.,SHOT_RESULT == 'made') %>% arrange(.,desc(SHOT_DIST)) %>% head(.,100)




shotlog %>%
  filter(player_name == 'aaron brooks')%>%
  group_by(player_name, PTS_TYPE, SHOT_RESULT) %>%
  summarize(Count = n()) %>% 
  filter(PTS_TYPE == 3) %>% 
  
  #Top 100 players fg avg .4526786
  (sum(top.100.made[,2])/100)/(sum(top.100.made[,2]+top.100.missed[,2])/100)
# data frame
top.100.made[,2]/(top.100.made[,2]+top.100.missed[,2])

top.100.made %>% 
  filter(player_name == 'aaron brooks') %>% 
  group_by()




library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
library(lubridate)
library(plotly)
shotlog <- read.csv("data/shot_logs3.csv",stringsAsFactors = FALSE)
# cleans gc column to mins sec
shotlog$GAME_CLOCK <- shotlog$GAME_CLOCK %>%  ms() 
View(shotlog)



# shot distance plot of all players
plot_ly(data = shotlog, y = ~SHOT_DIST, color = ~player_name, type = "bar") %>% 
  layout(legend = list(x = 0.2, y = 1.0))


# close defender distance of all plaers against shooter
plot_ly(data = shotlog, y = ~PTS_TYPE, color = ~player_name, type = "box") %>% 
  layout(legend = list(x = 0.2, y = 1.0))



# total number of made shots in the league
league_made <- shotlog %>% filter(.,shotlog$SHOT_BOOLEAN == 1) %>% count(SHOT_BOOLEAN)
# total number of mised shots in the league
league_missed <- shotlog %>% filter(.,shotlog$SHOT_BOOLEAN == 0) %>% count(SHOT_BOOLEAN)

#league average
league_made/(league_missed + league_made)


missed_a <- shotlog %>% filter(.,LOCATION == 'A',player_name == 'aaron brooks',SHOT_BOOLEAN == 0) %>% count(SHOT_BOOLEAN)
made_a <- shotlog %>% filter(.,LOCATION == 'A',player_name == 'aaron brooks',SHOT_BOOLEAN == 1) %>% count(SHOT_BOOLEAN)

missed_h <- shotlog %>% filter(.,LOCATION == 'H',player_name == 'aaron brooks',SHOT_BOOLEAN == 0) %>% count(SHOT_BOOLEAN)
made_h <- shotlog %>% filter(.,LOCATION == 'H',player_name == 'aaron brooks',SHOT_BOOLEAN == 1) %>% count(SHOT_BOOLEAN)


#missed fg away & home 
missed_a/(made_a + missed_a)
missed_h/(made_h + missed_h)

# made fg away & home
made_a/(made_a + missed_a)
made_h/(made_h + missed_h)

player_made <- shotlog %>%
  group_by(player_name) %>%
  unique() %>%
  filter(., SHOT_BOOLEAN == 1) %>%
  summarise(made_shot = n())

player_missed <- shotlog %>%
  group_by(player_name) %>%
  unique() %>%
  filter(., SHOT_BOOLEAN == 0) %>%
  summarise(made_shot = n())



player_fg <- player_made[,2]/(player_missed[,2] + player_made[,2])


View(shotlog %>%
       group_by(player_name) %>%
       unique())



shotlog %>% filter(.,LOCATION == 'A',player_name == 'aaron brooks',SHOT_BOOLEAN == 0) %>% count(SHOT_BOOLEAN)

# player names
player.names <- data.frame(unique(shotlog$player_name))

##TEST####

#Top 100 player total shots made away & home
made.away <- shotlog %>% 
  group_by(player_name) %>%
  unique() %>%
  filter(.,LOCATION == 'A',SHOT_RESULT == 'made') %>%
  summarise(made.away = n())%>%
  arrange(.,desc(made.away)) %>%
  top_n(.,100)

made.home <- shotlog %>%
  group_by(player_name) %>%
  unique() %>%
  filter(.,LOCATION == 'H',SHOT_RESULT == 'made') %>%
  summarise(made.home = n())%>%
  arrange(.,made.home) %>%
  top_n(.,100)

missed.away <- shotlog %>%
  group_by(player_name) %>%
  unique() %>%
  filter(.,LOCATION == 'A',SHOT_RESULT == 'missed') %>%
  summarise(missed.away = n())%>%
  arrange(.,desc(missed.away)) %>%
  top_n(.,100)

missed.home <- shotlog %>%
  group_by(player_name) %>%
  unique() %>%
  filter(.,LOCATION == 'H',SHOT_RESULT == 'missed') %>%
  summarise(missed.home = n())%>%
  arrange(.,missed.home) %>%
  top_n(.,100)


#top 100 two and three pointers made
two.made <- shotlog %>% group_by(player_name) %>% unique() %>% filter(.,PTS_TYPE == 2,SHOT_RESULT == 'made') %>% summarise(., two_pointers_made = n())
two.missed <- shotlog %>% group_by(player_name) %>% unique() %>% filter(.,PTS_TYPE == 2,SHOT_RESULT == 'missed') %>% summarise(., two_pointers_missed = n())
three.made <- shotlog %>% group_by(player_name) %>% unique() %>% filter(.,PTS_TYPE == 3,SHOT_RESULT == 'made') %>% summarise(., three_pointers_made = n())
three.missed <- shotlog %>% group_by(player_name) %>% unique() %>% filter(.,PTS_TYPE == 3,SHOT_RESULT == 'missed') %>% summarise(., three_pointers_missed = n())


#### overall fg and top scorers ####


# total number of made shots in the league
shot.made <- shotlog %>% group_by(player_name) %>% unique() %>% filter(.,SHOT_RESULT == 'made') %>% summarise(made = n()) %>% summarise()
# total number of mised shots in the league
shot.missed <- shotlog %>% group_by(player_name) %>% unique() %>% filter(.,SHOT_RESULT == 'missed') %>% summarise(missed = n())

# total fg average 0.4521391
(sum(shot.made[,2])/281)/(sum(shot.made[,2] + shot.missed[,2])/281)

shot.made #281
shot.missed #281

# closest defender which caused a miss
shotlog %>% group_by(CLOSEST_DEFENDER) %>% unique() %>% filter(.,SHOT_RESULT == 'missed') %>% summarise(blocked = n()) %>% arrange(.,desc(blocked))
#worst defender for successful shot
shotlog %>% group_by(CLOSEST_DEFENDER) %>% unique() %>% filter(.,SHOT_RESULT == 'missed') %>% summarise(blocked = n()) %>% arrange(.,blocked)



### TOP OVERALL ###



# top 100 overall successful shots
top.100.made <- shot.made %>% arrange(.,desc(made)) %>% top_n(.,n=100)
# top 100 overall failed shots
top.100.missed <- shot.missed %>% arrange(.,desc(missed)) %>% top_n(.,n=100) %>% head(.,100)

#Top 100 players fg avg .4526786
(sum(top.100.made[,2])/100)/(sum(top.100.made[,2]+top.100.missed[,2])/100)
# data frame
top.100.made[,2]/(top.100.made[,2]+top.100.missed[,2])

#top 100 players two pointers
two.made %>% arrange(.,desc(two_pointers_made)) %>% top_n(.,n=100)
#top 100 players with missed shots
two.missed %>% arrange(.,desc(two_pointers_missed)) %>% top_n(.,n=100)
#top 100 players with least amount of missed shots
two.missed %>% arrange(.,two_pointers_missed) %>% top_n(.,n=100)


#top 100 players three pointers
three.made %>% arrange(.,desc(three_pointers_made)) %>% top_n(.,n=100)
#top 100 players with missed shots
three.missed %>% arrange(.,desc(three_pointers_missed)) %>% top_n(.,n=100)
#top 100 players with least amount of missed shots
three.missed %>% arrange(.,three_pointers_missed) %>% top_n(.,n=100)


# Top 100 closest defender which caused a miss
top.100.defender <- shotlog %>% group_by(CLOSEST_DEFENDER) %>% unique() %>% filter(.,SHOT_RESULT == 'missed') %>% summarise(defense = n()) %>% arrange(.,desc(defense)) %>% top_n(.,100)
# Top 100 worst defender
worst.100.defenders <- shotlog %>% group_by(CLOSEST_DEFENDER) %>% unique() %>% filter(.,SHOT_RESULT == 'made') %>% summarise(no_defense = n()) %>% arrange(.,no_defense) %>% top_n(.,100)



plot_ly(data = shotlog, y = ~CLOSE_DEF_DIST, color = ~player_name, type = "box") %>% 
  layout(legend = list(x = 0.2, y = 1.0))



### server code###
shotlog %>%
  filter(player_name == input$player)%>%
  group_by(player_name, CLOSEST_DEFENDER, SHOT_RESULT) %>%
  summarize(Count = n())

shotlog %>% group_by(player_name) %>% unique() %>% filter(.,CLOSEST_DEFENDER,SHOT_RESULT == 'missed') %>% summarise(., three_pointers_missed = n())






colname <- c(made.away,missed.away,made.home,missed.home,shot.made,shot.missed,two.made,two.missed,three.made,three.missed)

df <- cbind.data.frame(colname)
colnames(df)    

df <- df[-c(3,5,7,9,11,13,15)]
df
df <- df %>% mutate(., avg_fg = made/(made+missed),away_fg = made.away/(made.away+missed.away),home_fg = made.home/(made.home+missed.home),two_fg = two_pointers_made/(two_pointers_made+two_pointers_missed),three_fg = three_pointers_made/(three_pointers_made+three_pointers_missed))
View(df)



#league fg
shot.made[,2]/(shot.missed[,2]+shot.made[,2])

#Ratio away vs home
shot.away[,2]/(shot.away[,2]+shot.home[,2])


ggplot(shotmvm, aes(x = Shot_Result, y = Shot_Pct)) + facet_grid(.~ Location) +
  ggtitle(paste("%Shots Made vs. Missed by ", unique('aaron brooks'),"\n Home vs Away Games" , sep = "")) +
  geom_bar(stat = "identity", fill = "orange", alpha = 0.8) +
  xlab("Shots Made vs. Missed") + ylab("% Shots")

##TEST####

# data for made/missed home & away
shotmvm <- shotlog %>% 
  group_by(player_name, LOCATION, SHOT_BOOLEAN, TOUCH_TIME) %>%
  summarize(countmm = n()) %>%
  mutate(prop = countmm / sum(countmm))


unique(shotmvm$player_name)

View(shotlog %>% 
       group_by(player_name) %>%
       unique() %>% 
       filter(., SHOT_BOOLEAN == 1) %>% 
       summarize(made_shot = n()) %>%
       mutate(prop = countmm / sum(countmm))) 


#### TESTTTT END ######



shotmvm <- shotlog %>%
  group_by(player_name,LOCATION,SHOT_RESULT) %>%
  summarize(countmm = n()) %>%
  mutate(prop = countmm/sum(countmm))%>%
  filter(player_name == input$player)
shotmvm <- as.data.frame(shotmvm)
colnames(shotmvm) <- c("Player","Location", "Shot_Result","Shot_Count","Shot_Pct")
shotmvm$Location <- with(shotmvm, factor(Location, levels = rev(levels(Location))))



# generate shots taken (made vs. missed)

ggplot(shotmvm, aes(x = Shot_Result, y = Shot_Pct)) + facet_grid(.~ Location) +
  ggtitle(paste("%Shots Made vs. Missed by ", unique(input$player),"\n Home vs Away Games" , sep = "")) +
  geom_bar(stat = "identity", fill = "orange", alpha = 0.8) +
  xlab("Shots Made vs. Missed") + ylab("% Shots")

})


shotdist <- shotlog %>% group_by(player_name,SHOT_DIST)


shotdist <- shotdist[,c(2:4,6:8,10:14,18,20,22)]

shotlog %>%
  group_by(player_name, SHOT_DIST, SHOT_RESULT) %>%
  summarize(Count = n())




colnames(shotlog)

ggplot(data=shotlog,aes(x=SHOT_RESULT,y= LOCATION)) + geom_bar(stat="identity", fill="steelblue")

ggplot(data=shotlog,aes(x=SHOT_RESULT,y= CLOSE_DEF_DIST)) + geom_point()

ggplot(data=shotlog,aes(x=SHOT_RESULT,y= CLOSE_DEF_DIST)) + geom_boxplot()





ggplot(data=shotlog,aes(x=CLOSE_DEF_DIST,y= SHOT_RESULT)) + geom_bar(stat="identity", fill="steelblue")

ggplot(data=shotlog,aes(x=CLOSE_DEF_DIST,y= SHOT_RESULT)) + geom_point()


ggplot(data=shotlog,aes(x=CLOSE_DEF_DIST,y= SHOT_RESULT)) + geom_boxplot()

shotlog %>%
  group_by(player_name, LOCATION, SHOT_RESULT) %>%
  summarize(countmm = n()) %>%
  mutate(prop = countmm / sum(countmm)) %>% arrange(desc(countmm))

shotlog %>%
  group_by(player_name,SHOT_DIST,CLOSE_DEF_DIST) %>%
  summarize(countmm = n()) %>%
  mutate(prop = countmm / sum(countmm)) %>% arrange(desc(countmm))


shotmvm %>% group_by(.,player_name) %>% filter(prop)




options(digits = 10)
View(shotmvm %>%
       group_by(player_name) %>%
       summarise(.,mean_fg = format(weighted.mean(prop),nsmall = 4)))

?mean
mean(0.664,0.34434,0.3423423)


shotmvm$prop





shotmvm %>% group_by(player_name,countmm,prop) %>% summarise(.,mean(prop))

?top_n
shotmvm


top10 <- 
  
  ggplot(data=shotmvm,aes(x = player_name,y = prop)) + geom_boxplot()


View(shotmvm)


shotmvm$prop[1:2]





shotmvm <- as.data.frame(shotmvm)
colnames(shotmvm) <- c("Player","Location", "Shot_Result","Shot_Count","Shot_Pct")
shotmvm$Location <- with(shotmvm, factor(Location, levels = rev(levels(Location))))

#### shiny server ####




shotlog %>% filter(.,player_name == "LeBron James",SHOT_NUMBER)

gg_court + geom_point(data = player_data, alpha = 0.75, size = 2.5,
                      aes(loc_x, loc_y, color = shot_made_flag)) +
  scale_color_manual("", values = c(made = "blue", missed = "orange"))
