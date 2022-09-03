# load packages
library(tidyverse)
library(stringr)
library(base)
library(robotstxt) 
library(rvest) 
library(purrr) 
library(xml2)
library(utils)

# load data
data <- read_csv("data.csv")

# clean up 
data <- data %>%
  # add likeability ratio
  mutate(sentiment = likes/views) 

# remove titles with commas in them
for (i in 1:nrow(data)){
  if(str_detect(data$title[i], ",")){
    data <- data %>%
      slice(-i)
  }
}

data <- data %>%
  # add column for topics 
  mutate(topics = "")

# loop through the cleaned up data
for(i in 1:nrow(data)){
  #retrieve link for that TED talk
  link <- data$link[i]
  
  # get topics
  topic_scr <- link %>%
    read_html() %>%
    html_nodes('.underline') 
  
  # put them in a data frame/table
  t <- bind_rows(lapply(xml_attrs(topic_scr), function(x) data.frame(as.list(x), stringsAsFactors=FALSE))) %>%
    separate(href, c("sub", "topics"), sep = "topics/") %>%
    select(-c(class, sub))
  
  # put them in a vector
  ts <- toString(t$topics)
  
  # add them into the dataset
  data[i,8] <- ts
}

# remove rows with no topics found (html tags inconsistensies)
data <- data[!(data$topics==""), ]

# clean up topics presentation in dataset
data$topics <- gsub("\\+", " ", data$topics) 
data$topics <- gsub('NA, ', '', data$topics) 
data$topics <- gsub('ted ed', '', data$topics) 

write.csv(data, "datated.csv")
