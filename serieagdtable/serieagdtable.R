# Load packages and add custom font
library(tidyverse)
library(worldfootballR)
library(showtext)

font_add_google('Sora', 'sora')
showtext_auto()

# Load data using worldfootballR
serie_a <- fb_match_results(country = "ITA", gender = "M", season_end_year = c(2009:2023), tier = "1st")

# Prepare data
# Select vars
serie_a_df <- serie_a %>%
  select(Season_End_Year, Home, Away, HomeGoals, AwayGoals) %>%
  set_names(names(.) %>% str_to_lower())

# Summarise number of goals scored and conceded by season and club
# Home goals
serie_a_home <- serie_a_df %>%
  group_by(season_end_year, home) %>%
  summarise(home_goals = sum(homegoals, na.rm = TRUE),
            home_goals_conceded = sum(awaygoals, na.rm = TRUE),
            .groups = "drop")

# Away goals
serie_a_away <- serie_a_df %>%
  group_by(season_end_year, away) %>%
  summarise(away_goals = sum(awaygoals, na.rm = TRUE),
            away_goals_conceded = sum(homegoals, na.rm = TRUE),
            .groups = "drop")

# Join tables
serie_a_gd <- serie_a_home %>%
  left_join(serie_a_away, by = c("season_end_year", "home" = "away")) %>%
  rename(`club` = home) %>%
  mutate(goal_diff = (home_goals + away_goals) - (home_goals_conceded + away_goals_conceded))

# Select data that has clubs in the current season
serie_a_latest <- serie_a_gd %>%
  mutate(current_season = case_when(season_end_year == max(season_end_year) ~ 1, TRUE ~ 0)) %>%
  group_by(club) %>%
  mutate(club_current_season = max(current_season)) %>%
  filter(club_current_season == 1) %>%
  ungroup() %>%
  arrange(season_end_year, desc(goal_diff)) %>%
  mutate(club = club %>% fct_reorder2(season_end_year, desc(goal_diff)),
         season = str_glue("{season_end_year - 1} - {str_sub(season_end_year, 3, 4)}"))

# Plot data
ggplot(serie_a_latest, aes(season, club, fill = goal_diff)) +
  geom_tile(height = 0.7) +
  scale_x_discrete(position = "top") +
  scale_fill_gradient(low = "#dddddd", high = "#004346", n.breaks = 12) +
  coord_cartesian(expand = FALSE) +
  labs(title = "Italy's Serie A - Goal Difference",
       subtitle = "Current Serie A Clubs Goals Difference Since 2008-09",
       caption = "Clubs ordered by the GD from Week 29 of the 2022-23 season\n Data: FBref.com | Viz: Evan Gower",
       x = "", y = "") +
  theme_minimal() +
  theme(plot.title.position = "plot",
        plot.title = element_text(family = 'sora', face = 'bold'),
        plot.subtitle = element_text(family = 'sora', size = 10.5),
        plot.caption.position = "plot",
        plot.caption = element_text(family = 'sora', size = 6, color = 'grey30', lineheight = 1),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(family = 'sora', size = 8),
        legend.margin = margin(l = -70),
        panel.grid = element_blank(),
        plot.margin = margin(0.5, 1, 0.5, 0.5, unit = "cm"),
        axis.text.x = element_text(family = 'sora', size = 6, color = "black"),
        axis.text.y = element_text(family = 'sora', size = 8, color = "black"),
        axis.ticks.y = element_blank()) +
  guides(fill = guide_colorbar(label.position = "top", 
                               title.hjust = 0.6, 
                               barwidth = unit(12, "lines"), 
                               barheight = unit(0.4, "lines")))