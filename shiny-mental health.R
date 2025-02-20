
# Load necessary packages
library(tidyverse)
library(robotstxt)
library(rvest)


#import the data

mental_data <- read.csv("~/Desktop/STAT-231/shiny-mental-health/data_Mental/Mental_health.csv") %>%
  janitor::clean_names() 

# wrangle

mental_data_2 <- mental_data %>%
  filter(index < 6468) %>%

  select(entity, year, schizophrenia, bipolar_disorder, eating_disorders, anxiety_disorders, 
        drug_use_disorders, depression, alcohol_use_disorders)%>%
 filter(year > 1999)

 # save new dataset object as csv file

 write.csv(mental_data_2, "wrangled_dataset.csv", row.names = FALSE)
