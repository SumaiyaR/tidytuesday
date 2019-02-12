
rm(list = ls())
library(dplyr)
library(ggplot2)
options("scipen" = 100)

setwd("/home/sumaiya/Cozy Drive/tidytuesday/")

fed <- read.csv("fed_r_d_spending.csv")

fedx <- fed %>%
  group_by(year) %>%
  mutate(rd_frac = round(rd_budget/sum(rd_budget) * 100, 2)) %>%
  select(department, year, rd_frac) %>%
  ungroup() %>%
  mutate(department = as.factor(department),
         year = as.numeric(year),
         rd_frac = as.numeric(rd_frac)) %>%
  arrange(year, rd_frac) 
  
df <- fedx
df$department <- factor(df$department, 
                        levels = c("DHS", "VA", "DOC", "EPA", "DOT", "Interior",
                                   "Other", "USDA", "NSF", "NIH", "HHS",
                                   "DOE", "NASA", "DOD"))

s <- ggplot(df,
            aes(x = year,
                y = rd_frac,
                fill = department)) +
  geom_bar(stat = "identity", position = "fill", width = 0.9) +
  scale_y_continuous(expand = c(0,0), labels = scales::percent) +
  scale_x_continuous(expand = c(0,0), breaks = round(seq(1976, 2017, by = 1),1)) + 
  theme(legend.position = 'bottom',
        legend.justification = "left",
        legend.text = element_text(size = 8),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.text.y = element_text(size = 11),
        axis.ticks = element_blank(),
        plot.title = element_text(color = "Black", size = 12),
        plot.subtitle=element_text(color = "Black", size = 10, face = "italic")) +
  guides(fill = guide_legend(reverse = F, nrow = 5, ncol = 3)) +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  labs(title = "Trend in R&D Expenditure", subtitle = "% Share of R&D Spending, 1976 - 2017") +
  xlab("") + 
  ylab("") +
  scale_fill_viridis_d(option="plasma",
    #direction = -1,
    name = "",
    labels = c(
      "Deparment of Defense",
      "National Aeronautics and Space Administration",
      "Department of Energy",
      "Department of Health and Human Services",
      "National Institute of Health",
      "National Science Foundation",
      "US Department of Agriculture",
      "Department of Interior",
      "Deparment of Transportation",
      "Environmental Protection Agency",
      "Department of Corrections",
      "Department of Homeland Security",
      "Department of Veterands Affairs",
      "Other"
    ),
    breaks = c(
      "DOD",
      "NASA",
      "DOE",
      "HHS",
      "NIH",
      "NSF",
      "USDA",
      "Interior",
      "DOT",
      "EPA",
      "DOC",
      "DHS",
      "VA",
      "Other"
    )
  )
ggsave('s.PNG', s,  width = 210, height = 150, units = "mm")

  
s


