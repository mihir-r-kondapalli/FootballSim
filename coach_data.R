# Load required packages
library(nflfastR)
library(dplyr)
library(readr)

# Define seasons to analyze
seasons <- c(2018, 2019, 2021, 2022, 2023, 2024)

# Load play-by-play data for the specified seasons
pbp_data <- purrr::map_dfr(seasons, function(season) {
  load_pbp(season)
})

# Filter for plays with more than 300 seconds remaining
filtered_data <- pbp_data %>%
  filter(game_seconds_remaining > 300) %>%
  select(season, down, ydstogo, yardline_100, play_type, qb_scramble)

# Assign coach decision categories with new mapping
filtered_data <- filtered_data %>%
  mutate(
    coach_decision = case_when(
      play_type %in% c("run") & qb_scramble == 0 ~ 0,  # Run (not a QB scramble)
      play_type %in% c("pass") | qb_scramble == 1 ~ 1,  # Pass or QB scramble
      play_type %in% c("field_goal") ~ 2,  # Field goal attempt
      play_type %in% c("punt") ~ 3  # Punt
    )
  ) %>%
  filter(!is.na(coach_decision))

# Group by (down, ydstogo, yardline_100) and get the most common decision
grouped_data <- filtered_data %>%
  group_by(down, ydstogo, yardline_100) %>%
  summarise(`Optimal Choice` = as.integer(names(sort(table(coach_decision), decreasing = TRUE)[1])),
            .groups = "drop") %>%
  rename(Down = down, Distance = ydstogo, Yardline = yardline_100)

# Save to CSV with proper column names
write_csv(grouped_data, "coach_decisions_grouped.csv")

# Print success message
print("CSV file 'coach_decisions_grouped.csv' has been successfully created.")
