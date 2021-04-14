# Clear the console
cat('\014')

#Set Driectory
setwd('/Users/anupamarao/Desktop/OneDrive - Syracuse University/MayMester/Session 1 - IST 719/Final Project/4549_466349_bundle_archive')
# Clear all user objects from the environment
rm(list=ls())  

#Reading the data file
#Great Britain Files
gb = read.csv("GBvideos.csv", stringsAsFactors = FALSE)
#India Files
ind = read.csv("INvideos.csv", stringsAsFactors = FALSE)
#USA Files
us = read.csv("USvideos.csv", stringsAsFactors = FALSE)

#Adding Location column to the dataset
gb$Location= "GB"
View(gb)
ind$Location= "IN"
View(ind)
us$Location= "US"
View(us)

#Libraries
install.packages("data.table")
library(data.table)
install.packages("dplyr")
library(dplyr)
install.packages("maptools")
library(maptools)
install.packages("ggplot2")
library(ggplot2)
install.packages("lubridate")
library(lubridate)

#Comvbining files into a data set
YouTubevideos = as.data.table(rbind(gb,us,ind))
View(YouTubevideos)

#Adding categories based on numbers
YouTubevideos = YouTubevideos %>% 
  mutate(category = case_when(
    category_id== '1' ~ 'Film and Animation',
    category_id== '2' ~ 'Autos and Vehicles',
    category_id== '10'~ 'Music',
    category_id== '15'~ 'Pets and Animals',
    category_id== '17'~ 'Sports',
    category_id== '18'~ 'Short Movies',
    category_id== '19'~ 'Travel and Events',
    category_id== '20'~'Gaming',
    category_id== '21'~'Videoblogging',
    category_id== '22'~ 'People and Blogs',
    category_id== '23'~ 'Comedy',
    category_id== '24'~ 'Entertainment',
    category_id== '25'~ 'News and Politics',
    category_id== '26'~ 'How to and Style',
    category_id== '27'~ 'Education',
    category_id== '28'~ 'Science and Technology',
    category_id== '29'~ 'Nonprofits & Activism',
    category_id== '30'~ 'Movies',
    category_id== '31'~ 'Anime/Animation',
    category_id== '32'~ 'Action/Adventure',
    category_id== '33'~ 'Classics',
    category_id== '34'~ 'Comedy',
    category_id== '35'~ 'Documentary',
    category_id== '36'~ 'Drama',
    category_id== '37'~ 'Family',
    category_id== '38'~ 'Foreign',
    category_id== '39'~ 'Horror',
    category_id== '40'~ 'Sci-Fi/Fantasy',
    category_id== '41'~ 'Thriller',
    category_id== '42'~ 'Shorts',
    category_id== '43'~ 'Shows',
    category_id== '44'~ 'Trailers'))


#WorldMap
data(wrld_simpl)
myCountries = wrld_simpl@data$NAME %in% c("India", "United Kingdom", "United States")
plot(wrld_simpl, col = c(gray(.80), "red")[myCountries+1])
# Donut Charts

#Worldwide (Top 5 Categories)
WorldWide = YouTubevideos %>% count(category)%>% 
  arrange(desc(n))%>% head(5)
WorldWide$fraction = WorldWide$n/ sum(WorldWide$n)
WorldWide$ymax = cumsum(WorldWide$fraction)
WorldWide$ymin = c(0, head(WorldWide$ymax, n=-1))
WorldWide$labelPosition = (WorldWide$ymax + WorldWide$ymin) / 2
WorldWide$label = paste0(WorldWide$category, "\n count: ", WorldWide$n)
#Plot
ggplot(WorldWide, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette="Reds") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void()+ 
  theme(legend.position = "none")

##USA (Top 5 Categories)
USATop5 =filter(YouTubevideos,Location=='US') %>%
  count(category) %>% arrange(desc(n))%>% head(5)
USATop5$fraction = USATop5$n/ sum(USATop5$n)
USATop5$ymax = cumsum(USATop5$fraction)
USATop5$ymin = c(0, head(USATop5$ymax, n=-1))
USATop5$labelPosition = (USATop5$ymax + USATop5$ymin) / 2
USATop5$label = paste0(USATop5$category, "\n count: ", USATop5$n)
#Plot
ggplot(USATop5, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette="Reds") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void()+ 
  theme(legend.position = "none")

#GREAT BRITAIN (Top 5 Categories)
GBTop5 =filter(YouTubevideos,Location=='GB') %>%
  count(category) %>% arrange(desc(n))%>% head(5)
GBTop5$fraction = GBTop5$n/ sum(GBTop5$n)
GBTop5$ymax = cumsum(GBTop5$fraction)
GBTop5$ymin = c(0, head(GBTop5$ymax, n=-1))
GBTop5$labelPosition = (GBTop5$ymax + GBTop5$ymin) / 2
GBTop5$label = paste0(GBTop5$category, "\n count: ", GBTop5$n)
#Plot
ggplot(GBTop5, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette="Reds") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void()+ 
  theme(legend.position = "none")

#INDIA (Top 5 Categories)
IndTop5 =filter(YouTubevideos,Location=='IN') %>%
  count(category) %>% arrange(desc(n))%>% head(5)
IndTop5$fraction = IndTop5$n/ sum(IndTop5$n)
IndTop5$ymax = cumsum(IndTop5$fraction)
IndTop5$ymin = c(0, head(IndTop5$ymax, n=-1))
IndTop5$labelPosition = (IndTop5$ymax + IndTop5$ymin) / 2
IndTop5$label = paste0(IndTop5$category, "\n count: ", IndTop5$n)
#Plot
ggplot(IndTop5, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette="Reds") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void()+ 
  theme(legend.position = "none")

#Top 5 popular channels (WorldWide)
WorldWidePopChannels = YouTubevideos %>% count(channel_title)%>% 
  arrange(desc(n))%>% head(5)%>%rename("No. of Views" = n)
labels = c("Jimmy Kimmel Live","The Tonight Show Starring Jimmy Fallon","TheEllenShow","The Late Show with Stephen Colbert","WWE")
ggplot(WorldWidePopChannels,aes(channel_title,No.of.Videos))+
  geom_bar(stat = "identity",aes(fill = channel_title ), color="black", size=0.2)+
  scale_x_discrete(labels = labels)+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))+
  scale_fill_brewer(palette="Reds")

#Top 5 popular channels (USA)
USPopChannels = filter(YouTubevideos,Location=='US') %>% 
  count(channel_title)%>% 
  arrange(desc(n))%>% head(5)%>%rename("No.of.Videos" = n)
labels = c("ESPN","The Tonight Show Starring Jimmy Fallon","Netflix"," TheEllenShow","Vox")
ggplot(USPopChannels,aes(channel_title,No.of.Videos))+
  geom_bar(stat = "identity",aes(fill = channel_title ), color="black", size=0.2)+
  scale_x_discrete(labels = labels)+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))+
  scale_fill_brewer(palette="Reds")

#Top 5 popular channels (GREAT BRITAIN)
GBPopChannels = filter(YouTubevideos,Location=='GB') %>% 
  count(channel_title)%>% 
  arrange(desc(n))%>% head(5)%>%rename("No.of.Videos" = n)
labels = c("The Tonight Show Starring Jimmy Fallon","Jimmy Kimmel Live"," TheEllenShow","Saturday Night Live","WWE")
ggplot(GBPopChannels,aes(channel_title,No.of.Videos))+
  geom_bar(stat = "identity",aes(fill = channel_title), color="black", size=0.2)+
  scale_x_discrete(labels = labels)+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))+
  scale_fill_brewer(palette="Reds")

#Top 5 popular channels (INDIA)
INDPopChannels = filter(YouTubevideos,Location=='IN') %>% 
  count(channel_title)%>% 
  arrange(desc(n))%>% head(5)%>%rename("No.of.Videos" = n)
labels = c("VikatanTV","etvteluguindia","Flowers Comedy","ETV Plus India","SAB TV")
ggplot(INDPopChannels,aes(channel_title,No.of.Videos))+
  geom_bar(stat = "identity",aes(fill = channel_title), color="black", size=0.2)+
  scale_x_discrete(labels = labels)+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))+
  scale_fill_brewer(palette="Reds")

#Time between Trending and publishing
YouTubevideos$trending_date <- ydm(YouTubevideos$trending_date)
YouTubevideos$publish_time <- ymd(substr(strsplit(YouTubevideos$publish_time,"T"),
                                         start = 1, stop =15))
YouTubevideos$trendingPublishDateDifference <- YouTubevideos$trending_date - YouTubevideos$publish_time

ggplot(YouTubevideos[YouTubevideos$trendingPublishDateDifference<30],
       aes(as.factor(trendingPublishDateDifference)))+
  geom_bar(fill="#FF0000",colour="black",size=0.3) +
  coord_flip()+
  labs(title=" Time between published and trending",
       subtitle="In days")+
  xlab(NULL)+ylab(NULL)















