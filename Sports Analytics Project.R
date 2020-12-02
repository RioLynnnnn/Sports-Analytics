library(tidyverse)
library(SportsAnalytics)
library(kableExtra)
library(XML)
library(stringr)
library(rvest)
library(ggmap)
library(googleVis)
library(devtools)

#Using SportsAnalytics API in R i.e. function fetch_NBAPlayerStatistics() 
nba.data <- fetch_NBAPlayerStatistics(season = "13-14")    #Retrieving the NBA data for year 2013-14

#Subset the data for your favourite team
fav.team <- nba.data %>% 
  filter(Team=="SAN")     #Favourite team is San Antiano Spurs for which code is "SAN" 

# a - Which player has the best three point percentage
player.3pp <- fav.team %>% 
  mutate(three_p_p=ThreesMade/ThreesAttempted) %>%
  filter(three_p_p==max(three_p_p,na.rm=T)) %>%
  select(Name,three_p_p)       
#mutate to form a function of three point percentage
#filter to filter the player with maximum 3-point percentage
#select to select that player

player.3pp

# b - Which player has played the largest number of minutes?
player.lnm <- fav.team %>%
  filter(TotalMinutesPlayed==max(TotalMinutesPlayed))%>%
  select(Name,TotalMinutesPlayed)

player.lnm

# c - Which player has the most Steals?
player.ms <- fav.team %>%
  filter(Steals==max(Steals))%>%
  select(Name,Steals)

player.ms


#Show 5 teams for the 2007-2008 season that have the most wins in descending order.
url <- read_html("https://www.landofbasketball.com/yearbyyear/2013_2014_standings.htm")
tbls <- html_nodes(url, "table")
head(tbls)

tbls_ls <- url %>%
  html_nodes("table") %>%
  .[1:2] %>%
  html_table(fill = TRUE)
str(tbls_ls)

team.data <- rbind(tbls_ls[[1]], tbls_ls[[2]])
team.data <- team.data[-c(1,17),2:3]
names(team.data) <- c("Team", "Wins")
team.data <- team.data[order(-as.numeric(team.data$Wins)),]
top_5_teams <- team.data[1:5,]
top_5_teams

#Use at least 5 Google charts (your choice) to show relevant data from this dataset

#Position point 1
pp <- nba.data %>% 
  group_by(Position) %>% 
  summarise(Total=mean(TotalPoints))   
#Google Chart 1
line.chart <- gvisLineChart(pp, options = list(colors = "['#2ca25f']", title = "Total Points w.r.t. Positions"))
plot(line.chart)    

#Position point 2
nba.pp <- nba.data %>% 
  group_by(Position) %>% 
  summarise(Total=mean(TotalPoints))
nba.san.pp <- fav.team %>% 
  group_by(Position) %>% 
  summarise(Total=mean(TotalPoints))
pp2 <- inner_join(nba.pp, nba.san.pp, by="Position")
names(pp2) <- c("Position", "Average Total Points", "San Antiano Spurs Total Points")
#Google Chart 2
column.chart <- gvisColumnChart(pp2, options = list(colors = "['#c3c3c3','#2ca25f']", legend = "bottom", 
                                                      title = "Total Points w.r.t. Positions"))

plot(column.chart)

#Position point 3
nba.data$poptooltip <- sprintf("Points:%d Fouls:%d", nba.data$TotalPoints, nba.data$PersonalFoulsDrawn) #setting hovertext
nba.scatterplot <- nba.data %>%
  select(PersonalFoulsDrawn, TotalPoints)
#Google Chart 3
Scatter <- gvisScatterChart(nba.scatterplot, options = list(pointSize=1, hAxis = "{title:'Personal fouls Drawn'}",
                            vAxis = "{title:'Total points'}", colors = "['#2ca25f']",
                            tooltip = "{isHtml:'True'}", title = "Personal Fouls Drawn VS Total Points"))
plot(Scatter)


#Position Block Point 4
team.bp <- nba.data %>% 
  group_by(Team) %>% 
  summarise(total.blocks = sum(Blocks),total.points = sum(TotalPoints)) 
#Google Chart 4
Bubble <- gvisBubbleChart(team.bp, idvar="Team", 
                          xvar="total.points", yvar="total.blocks",
                          sizevar="total.points", colorvar = "total.blocks",
                          options=list(colors="['#2ca25f']", 
                                       vAxis='{minValue:0, maxValue:700}', 
                                       title="Relation among Blocks and Points for all 30 teams"))
plot(Bubble)


#Position Block Point 5
devtools::install_github("dkahle/ggmap", force = TRUE) 
register_google(key = "AIzaSyAqgiuue0gXTpVfyeFnDDHHefJwoTrOCAc")
#Google Chart 5
geochartdata <- team.data %>% 
  mutate(city = sub("\\s+[^ ]+$", "", Team)) #to get the city of each team
geochartdata$win <- as.numeric(as.character(geochartdata$Wins)) 
geochartdata <- geochartdata %>% 
  arrange(win) 
geochartdata <- geochartdata[-c(2,10,16,26),]
geochartdata$locationvar <- 
c("25.7616798:-80.1917902","46.729553:-94.6858998","35.1495343:-90.0489801","40.7127753:-74.0059728",
"34.0522342:-118.2436849","43.0389025:-87.9064736","41.8781136:-87.6297982",
"40.2671941:-86.1349019","33.7489954:-84.3879824","38.5815719:-121.4943996","39.9525839:-75.1652215",
"43.653226:-79.3831843","30.4581881:-97.7544841","47.7510741:-120.7401385","41.49932:-81.6943605",
"36.778261:-119.4179324","39.7392358:-104.990251","32.7766642:-96.7969879","28.5383355:-81.3792365",
"39.3209801:-111.0937311","33.4483771:-112.0740373","29.7604267:-95.3698028","29.4241219:-98.4936282",
"34.0522342:-118.2436849","42.331427:-83.0457538","42.3600825:-71.0588801")

GeoMarker <- gvisGeoChart(geochartdata ,"locationvar",
                          sizevar = 'win',
                          hovervar = "Team",
                          options=list(region="US", resolution="provinces"))
plot(GeoMarker)


#Use gvisGeoChart function to display the location on the world map of the last 5 Basketball World Cup Champions
url2 <- read_html("http://www.landofbasketball.com/world_cup_stats/medals_by_year.htm")
new.data <- html_nodes(x = url2, css = "td:nth-child(3) a , .a-right a") %>% 
  html_text()
year <- word(new.data[c(TRUE,FALSE)], -1)
country <- gsub("\r\n\t\t \r\n\t  ","", new.data[c(FALSE,TRUE)])
champion <- data.frame(year, country)
lastchamp.r <- champion
lastchamp.r$country <- recode(lastchamp.r$country, "FR of Yugoslavia"="Republic of Serbia")
last5champ <- lastchamp.r %>% 
  group_by(country) %>%
  summarise(num=n())

#gvisGeoChart 
GeoChart <- gvisGeoChart(last5champ, locationvar = "country", colorvar = "num", 
                    options = list(width = 700, height = 700, projection = "kavrayskiy-vii", 
                                   datalessRegionColor = "#e5f5f9",
                                   colorAxis = "{values:[1,2],colors:[\'#99d8c9', \'#2ca25f\']}"))
table <- gvisTable(lastchamp.r, options = list(width = 220, height = 300))
finalchart <- gvisMerge(GeoChart, table, horizontal = TRUE) 
plot(finalchart)

