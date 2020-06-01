data = read.csv("KingGizzStats.csv",header = T)
library(plyr)

# first need to convert length column from milliseconds to minutes (located at index 6 in csv file)
a <- data[6]
newlength <- apply(a,2,function(x) x / 60000)
data$length <- newlength


# Plot number of tracks over time
years <- format(as.Date(data$release_date, format="%m/%d/%Y"),"%Y") # just need the years from release date
data$release_date <- years # replace release date with just year
years

#create sub-datasets for each album
rats_nest <- data %>% slice(0:9)
fishies <- data %>% slice(10:18)
gumboot <- data %>% slice(19:29)
poly <- data %>% slice(30:39)
sketches <- data %>% slice(40:52)
motu <- data %>% slice(53:73)
fmb <- data %>% slice(74:82)
nonagon <- data %>% slice(83:91)
paper_mache <- data %>% slice(92:103)
quart <- data %>% slice(104:107)
mind_fuzz <- data %>% slice(108:117)
oddments <- data %>% slice(118:129)
float_along <- data %>% slice(130:137)
eyes <- data %>% slice(138:147)
twelve_bar <- data %>% slice(148:159)

library(ggplot2)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(viridis)
library(hrbrthemes)
# create a table of year/album/number of songs
album_years <- c(2012,2013,2013,2014,2014,2015,2015,2016,2017,2017,2017,2017,2017,2019,2019)
album_name <- c("12 Bar","Eyes","Float Along","Oddments","Mind Fuzz", "Paper Mâché", "Quarters!","Nonagon", "FMB","MotU","Sketches","Poly","Gumboot","Fishies","Rats' Nest")
album_num_songs <- c(12,10,8,12,10,12,4,9,9,21,13,10,11,9,9)
albums_df <- data.frame(album_years,album_name,album_num_songs)

# plot
ggplot(albums_df, aes(fill=album_name, y=album_num_songs, x=album_years)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Number of Songs Over Time") +
  xlab("year") + ylab("number of songs") +
  scale_x_continuous("year", labels = as.character(album_years), breaks = album_years)
ggplotly()


# Average track length over time
song_length_df <- data.frame("album" = c("12 Bar","Eyes","Float ALong",
                                         "Oddments","Mind Fuzz","Quarters",
                                         "Paper Mache", "Nonagon", "FMB",
                                         "MotU", "Sketches", "Poly", "Gumboot",
                                         "Fishies", "Rats' Nest"),
                             "avg track length" = c(mean(twelve_bar$length),mean(eyes$length),
                                                    mean(float_along$length),mean(oddments$length),
                                                    mean(mind_fuzz$length),mean(quart$length),
                                                    mean(paper_mache$length),mean(nonagon$length),
                                                    mean(fmb$length),mean(motu$length),mean(sketches$length),
                                                    mean(poly$length),mean(gumboot$length),
                                                    mean(fishies$length),mean(rats_nest$length) ))
# order album by release
song_length_df$album <- factor(song_length_df$album, levels = song_length_df$album)

#plot
ggplot(song_length_df, aes(x=album, y=avg.track.length,group=1)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, formula = y ~ log(x)) +
  ggtitle("Average Track Length by Album") +
  xlab("album") + ylab("avg track length (minutes)")
ggplotly()


# Happiest sounding song ( use sonic happy index = \sqrt(valence*energy) )
happy_index <- (data$valence*data$energy)^.5
happy_df <- data.frame(data$name, happy_index)
happy_df %>% arrange(desc(happy_index))

ggplot(data=happy_df, aes(x=data$name, y=happy_index, colour=data$album)) +
  geom_point() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  ggtitle("Happiest Sounding Songs") +
  ylab("happy index") + xlab("song")


# Most energetic sounding song
energy_index <- data$energy
energy_df <- data.frame(data$name, energy_index)
energy_df %>% arrange(desc(energy_index))

ggplot(data=energy_df, aes(x=data$name, y=energy_index, colour=data$album)) +
  geom_point() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  ggtitle("Most Energetic Sounding Songs") +
  ylab("energy index") + xlab("song")


# Find King Gizz's angriest sounding song ( use sonic happy index = \sqrt(1-valence*energy) )
anger_index <- ((1-data$valence)*data$energy)^.5
anger_df <- data.frame(data$name, anger_index)
anger_df %>% arrange(desc(anger_index))

ggplot(data=anger_df, aes(x=data$name, y=anger_index, colour=data$album)) +
  geom_point() +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
  ggtitle("Angriest Sounding Songs") +
  ylab("anger index") + xlab("song")


# Plot avg danceability/energy over time
avg_energy <- data.frame(album_name, c(mean(twelve_bar$energy),
                                       mean(eyes$energy),
                                       mean(float_along$energy),
                                       mean(oddments$energy),
                                       mean(mind_fuzz$energy),
                                       mean(quart$energy),
                                       mean(paper_mache$energy),
                                       mean(nonagon$energy),
                                       mean(fmb$energy),
                                       mean(motu$energy),
                                       mean(sketches$energy),
                                       mean(poly$energy),
                                       mean(gumboot$energy),
                                       mean(fishies$energy),
                                       mean(rats_nest$energy)))
colnames(avg_energy) <- c("album_name", "avg_energy")
avg_energy$album_name <- factor(avg_energy$album_name, levels = avg_energy$album_name)

ggplot(avg_energy, aes(x=album_name, y=avg_energy,group=1)) +
  geom_point() +
  stat_smooth(method = 'lm', aes(x=album_name, y=avg_energy), se = FALSE, formula = y ~ poly(x,3)) +
  labs(title = "Average Energy per Album", subtitle = "Sorted by Release Date") +
  xlab("album") + ylab("average energy")
ggplotly()

f <- avg_dance <- data.frame(album_name, c(mean(twelve_bar$danceability),
                                       mean(eyes$danceability),
                                       mean(float_along$danceability),
                                       mean(oddments$danceability),
                                       mean(mind_fuzz$danceability),
                                       mean(quart$danceability),
                                       mean(paper_mache$danceability),
                                       mean(nonagon$danceability),
                                       mean(fmb$danceability),
                                       mean(motu$danceability),
                                       mean(sketches$danceability),
                                       mean(poly$danceability),
                                       mean(gumboot$danceability),
                                       mean(fishies$danceability),
                                       mean(rats_nest$danceability)))
colnames(avg_dance) <- c("album_name", "avg_danceability")
avg_dance$album_name <- factor(avg_dance$album_name, levels = avg_dance$album_name)

ggplot(avg_dance, aes(x=album_name, y=avg_danceability,group=1)) +
  geom_point() +
  stat_smooth(method = 'lm', aes(x=album_name, y=avg_danceability), se = FALSE, formula=y~poly(x,3)) +
  labs(title = "Average Danceability per Album", subtitle = "Sorted by Release Date") +
  xlab("album") + ylab("average danceability")
ggplotly()
