# Assignment 1

title: "Summary of COVID-19 Cases in Toronto"
subtitle: "Examine Trend of COVID-19 Cases"
author: "Xinyi Xu"
date: "`r format(Sys.time(), '%d %B %Y')`"
thanks: "https://github.com/xuxinyi720/Assignment1.git"
abstract: "COVID-19 has been recognized as a global threat; thus, numerous studies have been conducted in order to control and prevent this pandemic. I am going to use the dataset from Toronto Public Healthto summarize and analyze data to find how the pandemic is evolving, and assess severity and risk in Toronto. It is crucial for the Ministry of Health and our local public health institution to have an in-depth understanding of the disease and develop the necessary interventions to prevent further spread."

The full Script of the assignment 1 is under "Scripts" folder.
The raw data is stored in the "inputs/data" folder.
All graphs, tables are in the "outputs/paper" folder.
The final PDF version is under "outputs/paper" folder.

### Set up ###
library(opendatatoronto)
library(tidyverse)
library(ggplot2) 
library(scales)
library(kableExtra)
library(dplyr)
library(gt)

### Get data ###
COVID_data<- 
  opendatatoronto::search_packages("COVID-19 Cases in Toronto")%>%
  opendatatoronto::list_package_resources()%>%
  filter(name=="COVID19 cases")%>%
  select(id)%>%
  opendatatoronto::get_resource()

### Split date ###
COVID_data<- 
  COVID_data %>%
  dplyr::mutate(year = lubridate::year(COVID_data$`Reported Date`), 
                month = lubridate::month(COVID_data$`Reported Date`), 
                day = lubridate::day(COVID_data$`Reported Date`))
                 # show year/months/days
                 
### Save data ###
write.csv(COVID_data, "inputs/data/COVID_raw_data.csv")

### Line Chart ###
data_2020<-
  COVID_data%>%
  filter(COVID_data$year == 2020)%>%
  group_by(month)%>%
  summarise(Total = n())

ggplot(data_2020, aes(x=month, y=Total))+
  geom_line( 
    color="red")+
  labs(x = "Months",
       y = "Number of Cases",
       title = "New Cases in Each Month in 2020")+  # see how the new case has increased in 2020
  scale_x_continuous(breaks= pretty_breaks())+
  theme(plot.title = element_text(hjust = 0.5))

### Save image ### 
ggsave(file="outputs/paper/line-1.jpeg", width=4, height=4, dpi=300)

### Bar Chart 1###
head(COVID_data)
COVID_data %>%
  ggplot(mapping = aes( x = `Age Group`))+
  geom_bar()+
  theme_minimal()+
  labs(x = "Age Group",
       y = "Number of Cases",
       fill = "Patient Gender",
       title = "Number of Patient in Each Age Group")+ # which age group is most vulnerable
  theme(axis.text.x = element_text(angle = 90))



### Save image ### 
ggsave(file="outputs/paper/bar-chart-1.jpeg", width=10, height=8, dpi=300)

### Bar Chart 2 ###
COVID_data %>%
  ggplot(mapping = aes( x = FSA ))+
  geom_bar( fill = "dark blue")+
  theme_minimal()+
  labs(x = "Neighbourhood Name", 
       y = "Number of cases",
       title = "Cases in Each Neighbourhood")+  # Risky neighbouhoods
  theme(axis.text.x=element_text(angle=90,margin = margin(1, unit = "cm"),siz=12,hjust = 0.5, vjust =1),
        title =element_text(size=40, face='bold'))

### Save image ### 
ggsave(file="outputs/paper/bar-chart-2.jpeg", width=10, height=8, dpi=300)

### Table 1 ###
COVID_sum_table <-
  COVID_data %>%
  group_by(Outcome)%>%
  summarize(
    Total= n()
  )

COVID_sum_table%>%
  knitr::kable(caption = "Total Number of Three Outcomes", 
               col.names = c("Month", "Total Cases"),
               align = c('l', 'l', 'l'))
  


### Save image ### 
# save.image("outputs/paper/Table-1.jpeg")

### Table 2 ###
COVID_table <-
  COVID_data %>% count(Outcome,`Age Group`)

COVID_table%>%
  knitr::kable(caption = "Outcomes of All Age Group", 
               col.names = c("Outcome", "Age Group", "Total"))

### Save image ### 
save.image("outputs/paper/Table-2.jpeg")

### Table 3 ###
data_2020<-
  COVID_data%>%
  filter(COVID_data$year == 2020)%>% #only show the data in 2020
  group_by(month)%>%
  summarise(Total = n())

data_2020%>%
  knitr::kable(caption = "Number of Cases in Each Month in 2020", 
               col.names = c("Month", "Total"))

