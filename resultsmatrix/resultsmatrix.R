# Packages and fonts
library(tidyverse)
library(showtext)

font_add_google('Cairo', 'cairo')
showtext_auto()

# Set image sizing
options(repr.plot.width = 8, repr.plot,height = 8)

# Import data
soccer <- read_csv("soccer21-22.csv")

# Select data for dataframe
df <- soccer %>%
  select(HomeTeam, AwayTeam, FTR, FTHG, FTAG) %>%
  mutate(GD = FTHG-FTAG)

# Add score line column
df$score <- paste(df$FTHG, "-", df$FTAG)

# View data
head(df)

# Combine two copies of orginal datafram
combine_df <- rbind(
  df %>% select(team = HomeTeam, opp = AwayTeam, gf = FTHG, ga = FTAG),
  df %>% select(team = AwayTeam, opp = HomeTeam, gf = FTAG, ga = FTHG))

# Create league table
league_table <- combine_df %>%
  mutate(gd = gf-ga) %>%
  group_by(team) %>%
  summarize(mp = n(), wins = sum(gd > 0), draws = sum(gd == 0), losses = sum(gd < 0),
            pts = sum(draws + (wins * 3)), gf = sum(gf), ga = sum(ga), gd = sum(gf - ga))
  arrange(desc(pts), desc(gd))

# View league table
league_table

# Order teams in datafram by league table
df$HomeTeam <- factor(df$HomeTeam, levels = rev(league_table$team))
df$AwayTeam <- factor(df$AwayTeam, levels = rev(league_table$team))

# Plot data
ggplot(df, aes(HomeTeam, AwayTeam, fill = FTR)) +
  geom_tile(stat = "identity", size = 0.5, width = 1, height = 1, color = "grey20") +
  geom_text(data = df, aes(HomeTeam, AwayTeam, label = score), family = "cairo", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("#fd9b9d", "#ffee90", "#80d3a9")) +
  scale_x_discrete(expand = c(0, 0 )) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_flip() +
  labs(title = "English Premier League Results Matrix",
       subtitle = "Match results from the 2021-22 season",
       caption = "Data: English Premier League | Viz: Evan Gower") +
  theme(plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, color = "gray20", size = 0.5, linetype = "solid"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 18, face = "bold"),
        text = element_text(family = "cairo", size = 10, color = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
