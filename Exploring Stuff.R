### Libraries

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
library(ndjson)
library(RJSONIO) 
library(DT)
library(plyr)

## Reading the trim files

user_trim <- read_csv('user_trim.csv')
res_trim <- read_csv('restaurant_trim.csv')
review_trim <- read_csv('review_trim.csv')

## number of observations
nrow(user_trim)
summary(user_trim)

typeof(user_trim$date_diff)

## Converting to datetime format and Adding new columns for current date abd difference in dates in days
user_trim$yelping_since <- format(as.POSIXct(user_trim$yelping_since,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
user_trim$Current_Date <- format(as.POSIXct(user_trim$Current_Date,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')

user_trim[ , "Current_Date"] <- Sys.Date()

user_trim$date_diff <- as.Date(as.character(user_trim$Current_Date), format='%m/%d/%Y')-
                        as.Date(as.character(user_trim$yelping_since), format='%m/%d/%Y')

user_trim['normalized'] <- user_trim$review_count/as.double(user_trim$date_diff)

user_trim['normalized'] <- scale(user_trim$normalized, center = TRUE, scale = TRUE)

write_csv(user_trim,'user_trim_normalized.csv')













