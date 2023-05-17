library(tidyverse)
library(showtext)
library(gt)

# Read data file
data <- read_csv("efl-championship-2022-23.csv")

# Load custom font
font_add_google("Roboto Slab", "font")
showtext_auto()

# Select required data
data <- data %>% select(Date:AR)

# Create function to create league standings
create_standings <- function(data) {
  # Create a new dataframe to store the standings
  standings <- data.frame(TEAM = character(),
                          MP = integer(),
                          W = integer(),
                          D = integer(),
                          L = integer(),
                          GF = integer(),
                          GA = integer(),
                          GD = integer(),
                          PTS = integer(),
                          stringsAsFactors = FALSE)
  
  # Get a list of all the teams in the data
  teams <- unique(c(data$HomeTeam, data$AwayTeam))
  
  # Loop through each team and calculate their stats
  for (team in teams) {
    # Get all the matches where the team was the home team
    home_matches <- data %>% filter(HomeTeam == team)
    # Get all the matches where the team was the away team
    away_matches <- data %>% filter(AwayTeam == team)
    
    # Calculate the team's stats
    MP <- nrow(home_matches) + nrow(away_matches)
    W <- sum(home_matches$FTR == "H") + sum(away_matches$FTR == "A")
    D <- sum(home_matches$FTR == "D") + sum(away_matches$FTR == "D")
    L <- sum(home_matches$FTR == "A") + sum(away_matches$FTR == "H")
    GF <- sum(home_matches$FTHG) + sum(away_matches$FTAG)
    GA <- sum(home_matches$FTAG) + sum(away_matches$FTHG)
    GD <- GF - GA
    PTS <- (W * 3) + (D * 1)
    
    # Add the team's stats to the standings dataframe
    standings <- rbind(standings, data.frame(TEAM = team,
                                             MP = MP,
                                             W = W,
                                             D = D,
                                             L = L,
                                             GF = GF,
                                             GA = GA,
                                             GD = GD,
                                             PTS = PTS))
  }
  
  # Order the standings by points, then goal difference, then goals for
  standings <- standings %>% arrange(desc(PTS), desc(GD), desc(GF))
  
  # Add a column for the team's position in the standings
  standings$RANK <- 1:nrow(standings)
  
  # Return the standings dataframe
  standings
}

# Apply create_standings to data
standings <- create_standings(data)

# Table standings using gt()
standings_table <- standings %>%
  gt() %>%
  cols_align(
    align = "center",
    columns = c(MP:RANK)
  ) %>%
  tab_style(
    style = list(
      
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(2)
      )
      
    ),
    locations = list(
      cells_body(
        columns = c(MP)
      )
    )
  ) %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      cell_borders(sides = "bottom", weight = px(2)),
      cell_text(weight = "bold")
    )
  ) %>% tab_style(
    locations = list(
      cells_body(
        columns = c(RANK)
      )
    ),
    style = list(
      cell_text(weight = "bold")
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#002642"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = RANK,
      rows = RANK <= 2
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#8EA8C8"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = RANK,
      rows = RANK %in% c(3, 4, 5, 6)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#928B6C"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = RANK,
      rows = RANK %in% c(7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#840032"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = RANK,
      rows = RANK >= 22
    )
  ) %>%
  opt_table_font(font = google_font("Rubik")) %>%
  tab_options(
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    source_notes.font.size = 12,
    heading.align = "left",
    row.striping.background_color = "#F6F8FA",
    row.striping.include_table_body = TRUE,
    data_row.padding = px(2),
    table.border.top.style = "hidden",
    #table.border.bottom.style = "hidden",
    table.font.size = "12px"
  ) %>%
  tab_header(md("**EFL CHAMPIONSHIP STANDINGS 2022-23**")) %>%
  tab_source_note(html('<pre><span style="background-color: #002642;"
                       >        </span> = Promoted</pre>', '<pre><span style="background-color: #8EA8C8;"
                       >        </span> = Playoff</pre>', '<pre><span style="background-color: #928B6C;"
                       >        </span> = Reamin in League</pre>', '<pre><span style="background-color: #840032;"
                       >        </span> = Relegated</pre>')) %>%
  tab_source_note(
    source_note = "Data: football-data.co.uk"
  )

standings_table

# Save the table to an HTML file
gtsave(standings_table, "standings_table.html")
