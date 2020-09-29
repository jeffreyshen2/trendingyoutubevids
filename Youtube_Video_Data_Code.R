### LOADING THE NECESSARY LIBRARIES
library(tidytext)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(stringr)
library(jsonlite)
library(wordcloud)
library(RColorBrewer)




#### IMPORTING THE DATA
yt_data <- read.csv("USvideos.csv", header = TRUE)

# Filtering out unnecessary columns
yt_data <- yt_data %>%
  select(-thumbnail_link, -comments_disabled, -ratings_disabled, -video_error_or_removed)




### CLEANING DATA
# Converting date into readable format 
yt_data <- yt_data %>%
  mutate(trending_date = as.Date(yt_data$trending_date, format = "%y.%d.%m"))%>%
  mutate(trending_date = as.character(trending_date))

# Converting time into readable format
yt_data <- yt_data %>%
  mutate(publish_time = strptime(yt_data$publish_time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))

# Changing the separater character in the tags column
yt_data <- yt_data %>%
  mutate(tags = gsub("\\|", "//", tags))
head(yt_data)




####### TIME RELATED GRAPHS
### TRENDING HOUR OF THE DAY GRAPH
yt_data <- yt_data %>%
  mutate(publish_hour = yt_data$publish_time$hour)
ggplot(data = yt_data, aes(x = factor(publish_hour), fill = factor(publish_hour)), na.rm = TRUE) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Number of Trending Videos by Hour of the Day Posted", x = "Hour", y = "# of Videos") +
  scale_fill_viridis_d()

### TRENDING DAY OF WEEK GRAPH
yt_data <- yt_data %>%
  mutate(publish_day = weekdays(as.Date(yt_data$publish_time)))
ggplot(data = yt_data, aes(x= publish_day, fill = factor(publish_day)), na.rm = TRUE) +
  geom_bar(show.legend = FALSE) + 
  labs(title = "Number of Trending Videos by Day of the Week Posted", x = "Day", y = "# of Videos") +
  scale_fill_viridis_d()

### DAYS UNTIL TRENDING GRAPH
yt_data <- yt_data %>%
  mutate(trending_date_datetype = strptime(yt_data$trending_date, format = "%Y-%m-%d", tz = "UTC")) %>%
  mutate(trending_time_difference = trending_date_datetype-publish_time) %>%
  mutate(trending_time_difference = as.numeric(trending_time_difference)) %>%
  mutate(trending_time_difference = trending_time_difference / 86400) %>%
  filter(trending_time_difference < 30, trending_time_difference > 0 )

ggplot(data = yt_data, aes(x = trending_time_difference)) +
  geom_histogram(binwidth = 2, show.legend = FALSE, fill = rainbow(16)) +
  geom_vline(aes(xintercept = median(trending_time_difference)), color = "black", linetype = "dashed", size = 1) +
  labs(title = "Time Until Video Goes Trending", x = "# of Days", y = "# of Videos") +
  scale_fill_viridis_d()

### TRENDING MONTH OF THE YEAR GRAPH 
yt_data <- yt_data %>%
  mutate(publish_month = months(as.Date(yt_data$publish_time)))
yt_data <- yt_data %>%
  mutate(publish_day_of_month = substr(publish_time, 9, 10)) %>%
  mutate(publish_day_of_month = as.numeric(publish_day_of_month))

ggplot(data = yt_data, aes(x = publish_month, y = publish_day_of_month, fill = views)) +
  geom_tile() +
  scale_fill_gradient(low = "light blue", high = "navy") +
  labs(title = "Views for Trending Videos by Month", x = "Month", y = "Day") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




####### CATEGORICAL GRAPHS
#Loading JSON file with category names 
categories_file <- fromJSON("US_category_id.json")
categories_file <- as.data.frame(categories_file)
categories_flatten <- flatten(categories_file)

#Creating dataframe with category names related to each category id
categories_tbl <- as_data_frame(categories_flatten)
categories_tbl <- categories_tbl %>%
  select(-kind, -etag, -items.kind, -items.etag,-items.snippet.channelId, -items.snippet.assignable) %>%
  mutate(items.id = as.numeric(items.id))

#Creates empty column of chr datatype
yt_data$categories <- character(40463)
for(i in 1:length(categories_tbl$items.id)) {
  id <- categories_tbl$items.id[i]
  category_name <- categories_tbl$items.snippet.title[i]
  yt_data$categories[yt_data$category_id == id] <- category_name
}

### MOST POPULAR CATEGORIES OF TRENDING VIDEOS GRAPH
ggplot(data = yt_data, aes(x = categories, fill = categories)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Number of Trending Videos by Category", x = "Category", y = "# of Videos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()

### VIEWS FOR CATEGORIES OF TRENDING VIDEOS GRAPH
ggplot(data = yt_data, aes(x = categories, y = views, fill = categories)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(title = "Views for Categories of Trending Videos", x = "Category", y = "# of Total Views") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()

### ENGAGEMENT RATE FOR CATEGORIES OF TRENDING VIDEOS GRAPH
ggplot(data = yt_data, aes(x = categories, y = engagement_rate, fill = categories)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Engagement Rate for Categories of Trending Videos", x = "Category", y = "Engagement Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()




######## TITLE RELATED GRAPHS
### TITLE LENGTH OF TRENDING VIDEOS GRAPH
yt_data <- yt_data %>%
  mutate(title_length = nchar(gsub(" ", "", title)))

ggplot(data = yt_data, aes(x = title_length)) + 
  geom_histogram(binwidth = 2, fill = rainbow(43)) +
  geom_vline(aes(xintercept = mean(title_length)), color = "black", linetype = "dashed", size = 1) +
  labs(title = "Title Length of Trending Videos", x = "Title Length", y = "# of Videos")

### VIEWS OF TRENDING VIDEOS BASED ON TITLE LENGTH GRAPH
ggplot(data = yt_data, aes(x = title_length, y = views)) +
  geom_point(size = 2, alpha = 0.7, color = "blue") +
  labs(title = "Views of Trending Videos Based on Title Length", x = "Title Length", y = "Views")+
  geom_vline(aes(xintercept = mean(title_length)), color = "black", linetype = "dashed", size = 1) +
  coord_flip()

### ENGAGEMENT RATE OF TRENDING VIDEOS BASED ON TITLE LENGTH GRAPH
yt_data <- yt_data %>%
  mutate(engagement_rate = (likes + dislikes + comment_count) / views)
ggplot(data = yt_data, aes(x = title_length, y = engagement_rate)) +
  geom_point(size = 2, alpha = 0.7, color = "blue") +
  labs(title = "Engagement Rate of Trending Videos Based on Title Length", x = "Title Length", y = "Engagement Rate") + 
  geom_vline(aes(xintercept = mean(title_length)), color = "black", linetype = "dashed", size = 1)

### TOP WORDS IN TRENDING VIDEO TITLES GRAPH
title_word <- tibble(video = 1:length(yt_data$title), text = as.character(yt_data$title))
title_word <- title_word %>%
  unnest_tokens(word, text)
data(stop_words)
title_word <- title_word %>%
  dplyr::anti_join(stop_words, by = "word")
head(title_word %>%
       dplyr::count(word, sort = TRUE), 50)
title_word %>%
  dplyr::count(word) %>%
  with(wordcloud(word, n, max.words = 125, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(6, "Dark2")))




####### USER RELATED GRAPHS
### CHANNELS WITH THE MOST TRENDING VIDEOS GRAPH
yt_data_creators <- yt_data %>%
  group_by(channel_title) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:15)
ggplot(data = yt_data_creators, aes(x = channel_title, y = count, fill = channel_title)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Channels with the Most Trending Videos", x = "Channel Name", y = "# of Videos") +
  coord_flip() +
  scale_fill_viridis_d()

### CHANNELS WITH THE MOST VIEWS GRAPH
yt_data_creators2 <- yt_data %>%
  group_by(channel_title) %>%
  summarise(total_views = sum(views)) %>%
  arrange(desc(total_views)) %>%
  slice(1:15)
ggplot(data = yt_data_creators2, aes(x = channel_title, y = total_views, fill = channel_title)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Channels with the Most Total Views", x = "Channel Name", y = "# of Total Views") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()

### CHANNELS WITH THE MOST ENGAGEMENT PER VIDEO GRAPH
yt_data_creators3 <- yt_data %>%
  group_by(channel_title) %>%
  summarise(channel_engagement_rate = mean(engagement_rate)) %>%
  arrange(desc(channel_engagement_rate)) %>%
  slice(1:15)
ggplot(data = yt_data_creators3, aes(x = channel_title, y = channel_engagement_rate, fill = channel_title)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Channels with the Highest Engagement Rate per Video", x = "Channel Name", y = "Engagement Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()




####### TAG RELATED GRAPHS
### TOP WORDS IN TRENDING VIDEO TAGS GRAPH
tags_word <- tibble(video = 1:length(yt_data$tags), text = as.character(yt_data$tags))
tags_word <- tags_word %>%
  unnest_tokens(word, text)
data(stop_words)
tags_word <- tags_word %>%
  dplyr::anti_join(stop_words, by = "word")
head(tags_word %>%
       dplyr::count(word, sort = TRUE), 50)
tags_word %>%
  dplyr::count(word) %>%
  with(wordcloud(word, n, max.words = 100, scale = c(3, 0.03), random.order = FALSE, rot.per = 0.25, colors = brewer.pal(4, "Dark2")))

### NUMBER OF TAGS IN TRENDING VIDEOS GRAPH
yt_data <- yt_data %>%
  mutate(tag_count = str_count(yt_data$tags, "//") + 1)
head(yt_data)

ggplot(data = yt_data, aes(x = tag_count)) + 
  geom_histogram(binwidth = 2, fill = rainbow(34)) +
  geom_vline(aes(xintercept = mean(tag_count)), color = "black", linetype = "dashed", size = 1) +
  labs(title = "Tag Counts of Trending Videos", x = "# of Tags", y = "# of Videos")