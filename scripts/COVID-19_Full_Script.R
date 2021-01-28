### Preamble ###
# Purpose: Use the opendatatoronto to get the data, make table and graph
# Author: Xinyi Xu
# Contact: xiny.xu@mail.utoronto.ca
# Date: 28 January 2021
# Pre-requisites: None
# To-dos: get raw data, save data, make bar chart, make table 


### Workplace set-up ###
# install.packages("opendatatoronto")
#install.packages("ggplot2")
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
       title = "New Cases in Each Month in 2020")+
  scale_x_continuous(breaks= pretty_breaks())+
  theme(plot.title = element_text(hjust = 0.5))

### Save image ### 
ggsave(file="outputs/paper/line-1.jpeg", width=4, height=4, dpi=300)

### Bar Chart 1###
head(COVID_data)
COVID_data %>%
  ggplot(mapping = aes( x = `Age Group`, fill = `Client Gender`))+
  geom_bar()+
  theme_minimal()+
  labs(x = "Age Group",
       y = "Number of Cases",
       fill = "Patient Gender",
       title = "Number of Patient in Each Age Group")+
  theme(axis.text.x = element_text(angle = 90))


### Save image ### 
ggsave(file="outputs/paper/bar-chart-1.jpeg", width=4, height=4, dpi=300)

### Bar Chart 2 ###
COVID_data %>%
  ggplot(mapping = aes( x = FSA, fill = `Source of Infection`))+
  geom_bar()+
  theme_minimal()+
  labs(x = "Neighbourhood Name",
       y = "Number of cases",
       fill = "Source of infection",
       title = "Cases with different source of infection")+
  theme(axis.text.x=element_text(angle=90,margin = margin(1, unit = "cm"),siz=12,hjust = 0.1, vjust =1),
        title =element_text(size=40, face='bold'))
### Save image ### 
ggsave(file="outputs/paper/bar-chart-2.jpeg", width=15, height=8, dpi=300)

### Table Set-up ###
# install.packages("kableExtra")
# install.packages("gt"
#library(kableExtra)
#library(dplyr)
#library(gt)

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
  
#gg
COVID_sum_table%>%
  gt()%>%
  tab_header(
    title = "Total Number of Three Outcomes ")

### Save image ### 
# save.image("outputs/paper/Table-1.jpeg")
# save_kable("outputs/paper/Table-1.jpeg")

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
  filter(COVID_data$year == 2020)%>%
  group_by(month)%>%
  summarise(Total = n())

data_2020%>%
  knitr::kable(caption = "Number of Cases in Each Month in 2020", 
               col.names = c("Month", "Total"))

  #gt()%>%
  tab_header(
    title = "Number of Cases in Each Month in 2020 ")




