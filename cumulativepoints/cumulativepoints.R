library(tidyverse)
library(worldfootballR)
library(showtext)

# Add custom font
font_add_google("Cairo", "cairo")
showtext_auto()

# Scrape data
bun_table <- tm_matchday_table(country_name = "Germany", start_year = "2022", matchday = c(1:27))

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
  geom_line(color = "#e3170a", size = 0.4) +
  scale_x_continuous(breaks = seq(2, 28, 4)) +
  facet_wrap(~ Team) +
  labs(title = "Number of points over time in the Bundesliga",
       subtitle = "Cumulative sum of points per matchday 2022-23 season",
       caption = "Data: Transfermarkt | Viz: Evan Gower",
       x = "Matchday",
       y = "Points") +
  theme(text = element_text(family = "cairo", color = "black"),
        plot.title = element_text(family = "cairo", color = "black", size = 20),
        plot.subtitle = element_text(family = "cairo", color = "black", size = 11.5),
        plot.caption = element_text(family = "cairo", color = "black", size = 7.35),
        axis.text = element_text(color = "black", size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(color = "black", size = 10))
