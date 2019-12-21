#### EDA on business dataset

## Packages
library(jsonlite)
library(tidycensus)
library(tidyr)
library(dplyr)
library(sf)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(stringr)
library(geojsonio)
library(sp)
library(mice)
library(VIM)
library(moments)
library(stringr) #  string manipulation
library(lubridate) #  date manipulation
library(wordcloud) #  wordcloud
library(tidytext) # tidy implementation of NLP methods
library(leaflet) # maps
library(igraph) #  graphs
library(ggraph) #  graphs
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(textcat)
#install.packages('ndjson')
library(ndjson)
library(RJSONIO) 
#install.packages("DT")
library(DT)
library(plyr)

# Loading files
#Business file
Lines_business <- readLines("business.json") 
business <- as.data.frame(t(sapply(Lines_business, fromJSON)))

#Trying a different method to load
business2 <-stream_in('business.json')
write.csv(business2,'Business_data.csv')

#Tip File
tip <- stream_in('tip.json')
write.csv(tip,"tip_data.csv")

datatable(head(business2), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


#trying to find most common businesses
categories = strsplit(business2$categories,",")
categories = as.data.frame(unlist(categories))
colnames(categories) = c("Name")

categories %>%
  group_by(Name) %>%
  dplyr::summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) %>%
  head(10) %>%
  
  
  ggplot(aes(x = Name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill ='light blue') +
  geom_text(aes(x = Name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of Category', y = 'Count', 
       title = 'Top 10 Categories of Business') +
  coord_flip() + 
  theme_bw()


#Top cities
business2 %>%
  group_by(city) %>%
  dplyr::summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(City = reorder(city,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = City,y = Count)) +
  geom_bar(stat='identity',colour="white", fill ='orange') +
  geom_text(aes(x = City, y = 1, label = paste0("(",round(Count/1e3)," K )",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'City', y = 'Count of Reviews', 
       title = 'Top Ten Cities') +
  coord_flip() + 
  theme_bw()

### Filter for LA/LV only
LACoords = business2 %>% filter(city == "Los Angeles")
LV = business2 %>% filter(city == 'Las Vegas')

center_lon = median(LV$longitude,na.rm = TRUE)
center_lat = median(LV$latitude,na.rm = TRUE)

leaflet(LV) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude) 

## Looking for restaurants
vegasfood <- subset(LV, grepl("Restaurants", LV$categories))

Vegas_categories = strsplit(vegasfood$categories,",")
Vegas_categories = as.data.frame(unlist(Vegas_categories))
colnames(Vegas_categories) = c("Name")


## Dont think this following snippet is useful
Vegas_categories %>%
  group_by(Name) %>%
  dplyr::summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) %>%
  head(10) %>%
  
  
  ggplot(aes(x = Name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill ='light blue') +
  geom_text(aes(x = Name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of Category', y = 'Count', 
       title = 'Top 10 Categories of Business') +
  coord_flip() + 
  theme_bw()

## Mapping the restaurants
center_lon_rest = median(vegasfood$longitude,na.rm = TRUE)
center_lat_rest = median(vegasfood$latitude,na.rm = TRUE)

leaflet(vegasfood) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude) 


## Looking at the top categories
foodvector <- unlist(strsplit(vegasfood$categories, ","))
topfood <- head(sort(table(foodvector), decreasing = TRUE), n = 20)
topfoodtable <- data.table(topfood)
colnames(topfoodtable)[1] <- "Category"
colnames(topfoodtable)[2] <- "Frequency"
topfoodtable


top6cuisine <- topfoodtable[c(6,9,10,15,16,17),]

ggplot(top6cuisine, aes(x = reorder(Category,Frequency), y = Frequency)) + 
  geom_bar(stat = "identity", color = "grey", fill = "light green") + 
  geom_text(aes(label = Frequency), hjust = 2, fontface ="bold") + coord_flip() + 
  labs(title = "Top 6 Cuisines in Las Vegas", x = "Cuisine Type")

#Finding the restaurants in each category 

vegasfood <- vegasfood[,c(1:5,6,8:18)]
vegasmexican <- subset(vegasfood, grepl("Mexican", vegasfood$categories)) 
vegasamerican <- subset(vegasfood, grepl("American", vegasfood$categories))
vegaschinese <- subset(vegasfood, grepl("Chinese", vegasfood$categories))
vegasitalian <- subset(vegasfood, grepl("Italian", vegasfood$categories))
vegasjapanese <- subset(vegasfood, grepl("Japanese", vegasfood$categories))

#Top restaurants by star rating
topstarmexican <- head(vegasmexican[order(vegasmexican$stars, decreasing = TRUE),], n=25) 
topstaramerican <- head(vegasamerican[order(vegasamerican$stars, decreasing = TRUE),], n=25)
topstarchinese <- head(vegaschinese[order(vegaschinese$stars, decreasing = TRUE),], n=25)
topstaritalian <- head(vegasitalian[order(vegasitalian$stars, decreasing = TRUE),], n=25)
topstarjapanese <- head(vegasjapanese[order(vegasjapanese$stars, decreasing = TRUE),], n=25)

##mapping
topstarmap <- addProviderTiles(leaflet(), "Stamen.Toner", group = "OSM (default)")
topstarmap <- addCircleMarkers(setView(topstarmap, lng=center_lon_rest, lat=center_lat_rest,zoom = 11), lng = topstarmexican$longitude, 
                               lat = topstarmexican$latitude, color = "red", radius = 5, fillOpacity = 4, 
                               group = "Mexican (Red)")

topstarmap <- addCircleMarkers(topstarmap, lng = topstaramerican$longitude, lat = topstaramerican$latitude, 
                               color = "orange", radius = 5, fillOpacity = 4, group = "American (Orange)")

topstarmap <- addCircleMarkers(topstarmap, lng = topstarchinese$longitude, lat = topstarchinese$latitude, 
                               color = "yellow", radius = 5, fillOpacity = 4, group = "Chinese (Yellow)")

topstarmap <- addCircleMarkers(topstarmap, lng = topstaritalian$longitude, lat = topstaritalian$latitude, 
                               color = "green", radius = 5, fillOpacity = 4, group = "Italian (Green)")

topstarmap <- addCircleMarkers(topstarmap, lng = topstarjapanese$longitude, lat = topstarjapanese$latitude, 
                               color = "blue", radius = 5, fillOpacity = 4, group = "Japanese (Blue)")

topstarmap <- addLayersControl(topstarmap, overlayGroups = c("Mexican", "American", "Chinese", "Italian", "Japanese"))

topstarmap














