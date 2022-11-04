library(tidyverse)
library(worldfootballR)
library(extrafont)

# Scrape data
bun_table <- tm_matchday_table(country_name = "Germany", start_year = "2022", matchday = c(1:11))

# Select columns and create a second one for team
bun_table <- bun_table %>%
  select(Team = squad, Match = matchday, Points = pts, Team2 = squad)

# Create a 0 for matchday to let every line start at 0
temp <- data.frame(Team = unique(bun_table$Team), Match = 0, Points = 0, Team2 = unique(bun_table$Team))

# Combine the two tables
df <- rbind(temp, bun_table)

# Plot data
ggplot(df, aes(Match, Points, group = Team)) +
  geom_line(data = df[,2:4], aes(Match, Points, group = Team2), color = "grey70", alpha = 0.1) +
  geom_line(color = "#ffc504", size = 1) +
  scale_x_continuous(breaks = c(2, 6, 10)) +
  facet_wrap(~ Team) +
  labs(title = "Number of points over time in the Bundesliga",
       subtitle = "Cumulative sum of points per matchday 2022-23 season",
       caption = "Data: Transfermarkt | Viz: Evan Gower",
       x = "Points",
       y = "Matchday") +
  theme(plot.background = element_rect(fill = "grey15", color = "grey15"),
        panel.background = element_rect(fill = "grey15", color = "grey15"),
        text = element_text(family = "Consolas Bold", color = "white"),
        plot.title = element_text(family = "Consolas Bold", color = "white", size = 16),
        plot.subtitle = element_text(family = "Consolas Bold", color = "white"),
        axis.text = element_text(color = "white", size = 6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#aa3b45"),
        strip.text = element_text(color = "white"))
        