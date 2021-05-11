rm(list = ls())
setwd("/Users/rossbechtel/Downloads")

library(readxl)
library(tidyverse)
library(ggplot2)
library(stringr)
library(reshape2)
library(gifski)
library(gganimate)
library(plotly)
library(crosstalk)
library(htmlwidgets)
library(ggeffects)

# Read data and merge
hot100 <- read.csv("Hot Stuff.csv")
extraInfo <- read_excel("Hot 100 Audio Features.xlsx")
all <- merge(hot100, extraInfo, by='SongID')

# Clean data
inOrder <- all[order(all$WeekID),]
genres <- subset(inOrder, !is.na(inOrder$spotify_genre) & inOrder$spotify_genre != "[]")

# Better define genres
genres$isBlues <- ifelse(grepl("blues", genres$spotify_genre), 1, 0)
genres$isJazz <- ifelse(grepl("jazz", genres$spotify_genre), 1, 0)
genres$isRock <- ifelse(grepl("rock", genres$spotify_genre), 1, 0)
genres$isCountry <- ifelse(grepl("country", genres$spotify_genre), 1, 0)
genres$isSoul <- ifelse(grepl("soul",genres$spotify_genre), 1, 0)
genres$isDance <- ifelse(grepl("dance", genres$spotify_genre), 1, 0)
genres$isPop <- ifelse(grepl("pop", genres$spotify_genre), 1, 0)
genres$isRap <- ifelse(grepl("rap", genres$spotify_genre) | grepl("hip hop", genres$spotify_genre), 1, 0)
genres$isRnB <- ifelse(grepl("r&b", genres$spotify_genre), 1, 0)

# Calculate year and remove incomplete 2020 data
genres$Year <- as.numeric(str_sub(genres$WeekID, -4))
genres <- subset(genres, genres$Year != 2020)

# Group by year and compute counts
yearly <- genres %>%
  group_by(Year) %>%
  summarise(blues = sum(isBlues),
            jazz = sum(isJazz),
            rock = sum(isRock),
            country = sum(isCountry),
            soul = sum(isSoul),
            dance = sum(isDance),
            pop = sum(isPop),
            rap = sum(isRap),
            rnb = sum(isRnB))

# Transform data to graphable format
yearlyMelt <- melt(yearly, id='Year')
yearlyMelt$variable <- str_to_title(yearlyMelt$variable)

# Save for use in rShiny app
write.csv(yearlyMelt, "./yearlyMelt.csv")

# Make first plot
allGenres <- ggplot(yearlyMelt, aes(Year, value, color=variable)) +
  geom_line() +
  labs(x="Year",y="Number of Songs in Hot 100", color='Genre') +
  ggtitle("Number of Songs in the Billboard Hot 100 by Genre over Time")

# Animate
allGenrsanim<-allGenres+transition_reveal(Year)
animate(allGenrsanim, renderer=gifski_renderer())
anim_save("./allGenres.gif",animation=allGenrsanim, renderer=gifski_renderer())

# Figure out which genre each artist belongs to
artistGenre <- genres %>%
  group_by(Performer.x) %>%
  summarise(blues = sum(isBlues),
            jazz = sum(isJazz),
            rock = sum(isRock),
            country = sum(isCountry),
            soul = sum(isSoul),
            rap = sum(isRap),
            pop = sum(isPop),
            dance = sum(isDance),
            rnb = sum(isRnB),
            count = n()) %>%
  arrange(desc(count))
bot <- subset(artistGenre, select = -1)
artistGenre$theirGenre <- colnames(bot)[apply(bot,1,which.max)]
artistGenre$theirGenre <- str_to_title(artistGenre$theirGenre)

# Plot the top 20 artists 
ggplot(head(artistGenre, n=20), aes(reorder(Performer.x, count), count, fill=theirGenre)) +
  geom_col() + 
  coord_flip() +
  labs(y="Songs in the Billboard 100", x="Artist/Group", fill="Genre") +
  ggtitle("Top 20 Billboard 100 Artists of All Time")

# Madonna's plots
maddona <- subset(genres, genres$Performer.x == 'Madonna')
mgroup <- maddona %>%
  group_by(Year) %>%
  summarise(count = n(),
            Peak = min(Week.Position),
            `Top Song` = Song.x[which.min(Week.Position)])
mplt <- ggplot(mgroup, aes(Year, count, color="blue")) +
  geom_line() +
  geom_point(aes(text3=`Top Song`, text1=Peak)) +
  labs(x="Year",y="Songs in the Billboard 100") +
  ggtitle("Madonna's Billboard 100 History") +
  theme(legend.position = 'none')
ggplotly(mplt, tooltip = c("text3","text1"))

# Drake's plots
drake <- subset(genres, genres$Performer.x == 'Drake')
dgroup <- drake %>%
  group_by(Year) %>%
  summarise(count = n(),
            Peak = min(Week.Position),
            `Top Song` = Song.x[which.min(Week.Position)])
dplt <- ggplot(dgroup, aes(Year, count, color="blue")) +
  geom_line() +
  geom_point(aes(text3=`Top Song`, text1=Peak)) +
  labs(x="Year",y="Songs in the Billboard 100") +
  ggtitle("Drake's Billboard 100 History") +
  theme(legend.position = 'none') + 
  scale_color_manual(values=c("blue"))
ggplotly(dplt, tooltip = c("text3","text1"))

# Elton John's plots
elton <- subset(genres, genres$Performer.x == 'Elton John')
egroup <- elton %>%
  group_by(Year) %>%
  summarise(count = n(),
            Peak = min(Week.Position),
            `Top Song` = Song.x[which.min(Week.Position)])
eplt <- ggplot(egroup, aes(Year, count, color="blue")) +
  geom_line() +
  geom_point(aes(text3=`Top Song`, text1=Peak)) +
  labs(x="Year",y="Songs in the Billboard 100") +
  ggtitle("Elton John's Billboard 100 History") +
  theme(legend.position = 'none') + 
  scale_color_manual(values=c("green"))
ggplotly(eplt, tooltip = c("text3","text1"))

# Calculate song stat averages per year
stats <- genres %>%
  group_by(Year) %>%
  summarise(energy = mean(energy, na.rm=T),
            danceability = mean(danceability, na.rm=T),
            liveness = mean(liveness, na.rm = T),
            tempo = mean(tempo, na.rm=T),
            valence = mean(valence, na.rm=T))

# Plot effects plot for all 5 stats
ggplot(stats, aes(Year, energy)) +
  geom_point(aes(color="blue"))+
  ggtitle("Average Energy of Hot 100 Songs over Time")+
  labs(y= "Average Energy Score of Charting Songs", x = "Year")+
  geom_smooth(method=lm) +
  scale_color_manual(values=c("blue")) +
  theme(legend.position = 'none')

ggplot(stats, aes(Year, danceability)) +
  geom_point(aes(color="blue"))+
  ggtitle("Average Danceability of Hot 100 Songs over Time")+
  labs(y= "Average Danceability Score of Charting Songs", x = "Year")+
  geom_smooth(method=lm, color="red") +
  scale_color_manual(values=c("red")) +
  theme(legend.position = 'none')

ggplot(stats, aes(Year, liveness)) +
  geom_point(aes(color="green"))+
  ggtitle("Average Liveness of Hot 100 Songs over Time")+
  labs(y= "Average Liveness Score of Charting Songs", x = "Year")+
  geom_smooth(method=lm, color="green") +
  scale_color_manual(values=c("green")) +
  theme(legend.position = 'none')

ggplot(stats, aes(Year, tempo)) +
  geom_point(aes(color="orange"))+
  ggtitle("Average Tempo of Hot 100 Songs over Time")+
  labs(y= "Average Tempo Score of Charting Songs", x = "Year")+
  geom_smooth(method=lm, color="orange") +
  scale_color_manual(values=c("orange")) +
  theme(legend.position = 'none')

ggplot(stats, aes(Year, valence)) +
  geom_point(aes(color="purple"))+
  ggtitle("Average Valence of Hot 100 Songs over Time")+
  labs(y= "Average Valence Score of Charting Songs", x = "Year")+
  geom_smooth(method=lm, color="purple") +
  scale_color_manual(values=c("purple")) +
  theme(legend.position = 'none')

# Read and clean platform data
platform <- read.csv('/Users/rossbechtel/Desktop/Revenue_Chart_data.csv')
platformGood <- subset(platform, platform$Total.Value.For.Year != "")
platformGood$type <- sapply(platformGood$Format, function(x) {
  if(x %in% c("LP/EP","Vinyl Single","8 - Track"))
    return("Vinyl")
  if(x %in% c("Cassette","Cassette Single","Other Tapes"))
    return("Cassette")
  if(x %in% c("CD", "CD Single", "SACD", "DVD Audio", "Music Video (Physical)"))
    return("CD")
  if(x %in% c("Download Album", "Download Single", "Ringtones & Ringbacks", "Download Music Video", "Other Digital", "Kiosk"))
    return("Digital Download")
  if(x %in% c("Paid Subscription", "On-Demand Streaming (Ad-Supported)","Other Ad-Supported Streaming","SoundExchange Distributions",
              "Limited Tier Paid Subscription","Synchronization"))
    return("Streaming Platform")
  return("ERROR")
})
platformGood$sales <- as.numeric(str_sub(platformGood$Total.Value....Billion., 2, -2)) 

# Get proportions
grouped <- platformGood %>%
  group_by(Year, type) %>%
  summarise(sales = sum(sales)) %>%
  mutate(prop = prop.table(sales))

# Only show every 5 years for readability
grouped <- subset(grouped, grouped$Year %% 5 == 0)

# Make the series of pie charts
ggplot(grouped,aes("", prop, fill=type)) + 
  geom_col()+
  facet_wrap(~Year)+
  ggtitle("Proportion of Music Sales Value by Sales Type") +
  coord_polar("y", start=0) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()) +
  labs(x="",y="", fill="Sales Type")
  
  
