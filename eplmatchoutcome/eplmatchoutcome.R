# Load packages and add custom font
library(tidyverse)
library(showtext)
library(scales)

font_add_google('Cairo', 'cairo')

# Read csv file
df <- read_csv('eplmatchs.csv')

# Filter for last 10 years of match data
# Mutate percentage of count by type of outcome in match, grouped by season and ftr
count_df <- df %>%
  filter(Season_End_Year > 2012) %>%
  count(Season_End_Year, FTR) %>%
  group_by(Season_End_Year) %>%
  mutate(share = n / sum(n),
         descript = case_when(FTR == 'A' ~ 'Away',
                              FTR == 'D' ~ 'Draw',
                              FTR == 'H' ~ 'Home'))

# Make Season_End_Year a character
count_df$Season_End_Year <- as.character(count_df$Season_End_Year)

# Set image sizing options
options(repr.plot.width = 8, repr.plot.height = 5)

# Plot data
ggplot(count_df, aes(Season_End_Year, share, fill = descript)) +
  geom_bar(stat = "identity") +
  geom_text(data = . %>%
              filter(share > 1), aes(label = percent(share, accuracy = 1L)),
            family = 'cairo', fontface = 'bold', size = 5, color = "black", position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#fd9b9d", "#ffee90", "#80d3a9")) +
  labs(title = "Home Teams Win More in the English Premier League",
       subtitle = "Over the past 10 EPL seasons, the home team winning the match was the most likely outcome, which averaged\n at around 40% of all match outcomes. Away only beat home once during this period and draws never hit the 30% mark.",
       caption = "Data: English Premier League | Viz: Evan Gower",
       x = "", y = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = 'cairo', face = 'bold', size = 22, hjust = 0.5),
        plot.subtitle = element_text(family = 'cairo', size = 10, hjust = 0.5),
        plot.caption = element_text(family = 'cairo', size = 8),
        legend.title = element_blank(),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(family = 'cairo', face = 'bold', size = 14, vjust = 0.8)) +
  guides(fill = guide_legend(
    keywidth = 1.6, keyheight = 0.2,
    defualt.unit = "cm", label.position = 'top', nrow = 1))
