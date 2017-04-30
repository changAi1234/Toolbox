library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

#Import data
h1b <- read_csv("h1b.csv")

#Explore data
head(h1b)

#Convert variables to factors
h1b$EMPLOYER_NAME <- factor(h1b$EMPLOYER_NAME)
h1b$WORKSITE <- factor(h1b$WORKSITE)
h1b$CASE_STATUS <- factor(h1b$CASE_STATUS)
h1b$YEAR <- factor(h1b$YEAR)
h1b$FULL_TIME_POSITION <- factor(h1b$FULL_TIME_POSITION)

#Remame levels in CASE_STATUS
levels(h1b$CASE_STATUS)
levels(h1b$CASE_STATUS)[levels(h1b$CASE_STATUS)=="PENDING QUALITY AND COMPLIANCE REVIEW - UNASSIGNED"] <- "PENDING"

#Bar chart of CASE_STATUS
h1b %>% filter(!is.na(CASE_STATUS)) %>% group_by(CASE_STATUS) %>%
  summarise(Percentage = round(n()*100/nrow(h1b),1)) %>% ggplot(aes(CASE_STATUS, Percentage, fill = CASE_STATUS)) + geom_bar(stat = "identity") + 
  labs(x = "Case Status", y = "Percentage", title = "Status of Petitioned Applications") +scale_y_continuous(breaks = seq(0,100,10))+coord_flip()
certified <- as.data.frame(h1b%>% filter(CASE_STATUS=="CERTIFIED"))

#Bar chart of top 10 employers
certified %>% group_by(EMPLOYER_NAME) %>%summarise(Count = n(), Percentage = round(Count*100/nrow(certified),1)) %>%arrange(desc(Count)) %>% top_n(10, wt = Count) %>% ggplot(aes(EMPLOYER_NAME,Percentage, fill = EMPLOYER_NAME)) +
  geom_bar(stat = "identity") +
  labs(x = "EMPLOYER_NAME", y = "Certified(in percentage)") + 
  theme(legend.position = "none") +
  coord_flip()

#top10e <- as.data.frame(certified %>% group_by(EMPLOYER_NAME, WORKSITE) %>%summarise(Count = n(), Percentage = round(Count*100/nrow(certified),1)) %>%arrange(desc(Count)))
#top30 <- top10e[1:30,]

ggplot(data = top30, aes(WORKSITE,Percentage, fill = WORKSITE)) +
  geom_bar(stat = "identity") +
  labs(x = "EMPLOYER_NAME", y = "Certified(in percentage)") + 
  theme(legend.position = "none") +
  coord_flip()+facet_wrap(~ EMPLOYER_NAME)

#List of top 10 locations
certified %>% group_by(WORKSITE) %>%summarise(Count = n(), Percentage = round(Count*100/nrow(certified),1)) %>%arrange(desc(Count))%>% top_n(30, wt = Count) %>% ggplot(aes(WORKSITE,Percentage, fill = WORKSITE)) +
  geom_bar(stat = "identity") +
  labs(x = "WORKSITE", y = "Certified(in percentage)") + 
  theme(legend.position = "none") +
  coord_flip()

#top30l <- as.data.frame(certified %>% group_by(WORKSITE) %>%summarise(Count = n()) %>%arrange(desc(Count))%>% top_n(30, wt = Count))
#top30l

#summary(h1b$PREVAILING_WAGE)
summary(certified$PREVAILING_WAGE)
Wage <- certified %>% group_by(YEAR) %>% summarise(Avg_Wage=mean(PREVAILING_WAGE, na.rm = T)) 
ggplot(Wage,aes(YEAR, Avg_Wage, fill=YEAR))+geom_bar(stat = "identity")

