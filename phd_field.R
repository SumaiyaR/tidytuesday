setwd('/home/sumaiya/Cozy Drive/tidytuesday/')

# Because all the things
library(tidyverse)
library(dplyr)
library(gganimate)

medyrs <- read.csv('medianyears_phd.csv', skip = 3, header = T, nrows = 18) 
medyrs <- medyrs[-c(1,2,4,7,10),-c(3,5,7,9,11,13,15,17,19)]

major_fields <- c("All fields",
                  "Life sciences", 
                  "Physical Sciences and earth sciences", 
                  "Mathematics and computer sciences",
                  "Psychology and social sciences", 
                  "Engineering",
                  "Education",
                  "Humanities and arts",
                  "Other")

names(medyrs) <- c("Time", major_fields)

sex_phd <- medyrs[1:3,] %>%
  gather(key = 'field', value = 'yrs', -Time) %>%
  mutate(yrs = as.numeric(yrs)) %>%
  filter(Time != "All doctorate recipientsc")

sex_phd <- sex_phd %>%
  group_by(field) %>%
  mutate(diff = yrs - lag(yrs, default = first(yrs))) %>%
  filter(Time != "Male")

p <- ggplot(data = sex_phd, 
            aes(x = factor(field), y = diff, 
                fill = factor(Time))) + 
  geom_bar(stat="identity", position = "dodge", fill = '#ffb428') + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  scale_y_continuous(expand = c(0,0)) +
  theme(legend.position = 'bottom',
        legend.justification = "left",
        legend.text = element_text(size = 8),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 11),
        axis.ticks = element_blank(),
        plot.title = element_text(color = "Black", size = 12),
        plot.subtitle=element_text(color = "Black", size = 10, face = "italic")) +
  geom_hline(yintercept = 0, size = 1, colour = "#1380A1") +
  labs(title = "Do women take longer to start their PhDs?", subtitle = "Median difference in years taken since bachelor's with men") +
  xlab("") + 
  ylab("") 
  
p

ggsave('phdyrs.PNG', p,  width = 210, height = 150, units = "mm")



eth_phd <- medyrs[-c(1,2,3,4,5,7, 12, 13),] %>%
  gather(key = 'field', value = 'yrs', -Time)%>%
  mutate(yrs = as.numeric(yrs)) 

eth_phd <- eth_phd %>%
  group_by(field) %>%
  mutate(white = case_when(Time == 'White' ~ yrs)) %>%
  fill(white, .direction = "up") %>% 
  fill(white, .direction = "down") %>%
  mutate (diff = yrs - white) %>%
  filter(Time != 'White')

e <- ggplot(data = eth_phd, 
            aes(fill=Time, y=diff, x=field)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_viridis_d() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + 
  scale_y_continuous(expand = c(0,0)) +
  theme(legend.position = 'bottom',
        legend.justification = "left",
        legend.text = element_text(size = 8),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 11),
        axis.ticks = element_blank(),
        plot.title = element_text(color = "Black", size = 12),
        plot.subtitle=element_text(color = "Black", size = 10, face = "italic")) +
  geom_hline(yintercept = 0, size = 1, colour = "#1380A1") +
  labs(title = "Median years to PhD by Ethnicities", subtitle = "Median difference in years taken since bachelor's with whites") +
  xlab("") + 
  ylab("") 

e

ggsave('phdyrs_eth.PNG', e,  width = 210, height = 150, units = "mm")



empsec <- read.csv('empsec_phd.csv', skip = 5, header = T) 
names(empsec) <- c("Field", "All Recipients", "With Commitments", "Total in US",
                   "Postdoc", "Academe", "Industry", "Other", "Abroad", "Unknown")

empsec_phd <- empsec %>%
  mutate(Field = as.character(Field)) %>%
  mutate(Sex = case_when(Field == "Male" ~ Field,
                         Field == "Female" ~ Field)) %>%
  fill(Sex, .direction = "down") %>% 
  mutate_if(is.factor, as.numeric) %>%
  mutate(Pct = round(`With Commitments`/`All Recipients` * 100, 0)) %>%
  mutate_if(is.numeric, funs(./as.numeric(`With Commitments`) * 100)) %>%
  mutate(Field = case_when(Field == "Othero" ~ "Other",
                           TRUE ~ Field),
         major_field = case_when(Field %in% major_fields ~ Field,
                                 TRUE ~ NA_character_)) %>%
  fill(major_field, .direction = "down") %>% 
  filter(!Field %in% major_fields) %>%
  filter(Sex != "") %>%
  select(-`All Recipients`, -`With Commitments`, -Field, -Pct) 

data <- empsec_phd %>%
  gather(key = type, value = pct, -Sex, - major_field) %>%
  filter(type != "Total in US") 

s <- ggplot(data,
            aes(x = major_field,
                y = pct,
                fill = type)) +
  geom_bar(stat = "identity", position = "fill", width = 0.9) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0), labels = function(x) str_wrap(x, width = 10)) + 
  theme(legend.position = 'bottom',
        legend.justification = "left",
        legend.text = element_text(size = 8),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(color = "Black", size = 12),
        plot.subtitle=element_text(color = "Black", size = 10, face = "italic")) +
  #guides(fill = guide_legend(reverse = F, nrow = 5, ncol = 3)) +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  labs(title = "Trend in R&D Expenditure", subtitle = "% Share of R&D Spending, 1976 - 2017") +
  xlab("") + 
  ylab("") +
  scale_fill_viridis_d(option="plasma") + 
  facet_wrap(~Sex)
s
rm(list = ls())


esec <- read.csv('esec_phd.csv', skip = 5, header = T) 
esec <- esec[7:30,1:4]

type <- c("Academe",
          "Government",
          "Industry or business",
          "Nonprofit organization",
          "Unknown")
esec_x <- esec %>%
  mutate(X = as.character(X),
         type = case_when(nchar(X)>4 ~ X)) %>%
  fill(type, .direction = "down") %>%
  mutate_if(is.factor, as.numeric) %>%
  filter(X.1 != 1) %>%
  select(-X.1)

esec_x$type <- gsub("\\(%).*", "", esec_x$type)

dt <- esec_x %>%
  gather(key = sex, value = pct, -type, -X)


t <- ggplot(dt,
            aes(x = sex,
                y = pct,
                fill = type)) +
  #geom_label(aes(label = pct)) +
  geom_bar(stat = "identity", position = "fill", width = 0.9) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0), labels = function(x) str_wrap(x, width = 10)) + 
  theme(legend.position = 'right',
        legend.justification = "left",
        legend.text = element_text(size = 8, colour = 'white'),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(color = "Black", size = 12),
        plot.subtitle=element_text(color = "Black", size = 10, face = "italic")) +
  #guides(fill = guide_legend(reverse = F, nrow = 5, ncol = 3)) +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  labs(title = "Which sector do PhDs join?", subtitle = "Employment sector of doctorate recipients with definite postgraduation commitments for employment in the United States, by sex") +
  xlab("") + 
  ylab("") +
  geom_text(aes(label = paste0(pct,"%")),
            position = position_fill(vjust = 0.5), size = 5) + 
  scale_fill_viridis_d(option="plasma") +
  labs(x ='{closest_state}') +
  transition_states(X, transition_length = 1, state_length = 5) 
t
animate(t, nframes = 20, height = 800, width = 800)

anim_save("t.gif")
