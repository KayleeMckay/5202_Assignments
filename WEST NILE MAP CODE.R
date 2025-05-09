library(tidyverse)
library(maps)

# Load and clean data
data <- read_csv("C:/Users/Owner/Downloads/West Nile virus human disease cases reported by state of residence, 2024 (1).csv")

data <- data %>%
  mutate(state = tolower(state.name[match(State, state.abb)])) %>%
  filter(!is.na(state))

# Load US map
us_states <- map_data("state")

# Join case data with map
map_data_joined <- us_states %>%
  left_join(data, by = c("region" = "state"))

# Create plot without grid
wnv_map <- ggplot(map_data_joined, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = `Reported Cases`), color = "white") +
  scale_fill_gradient(
    low = "#ffe0f0",  # light pink
    high = "#d1006f", # dark pink
    na.value = "grey90"
  ) +
  theme_minimal() +
  labs(
    title = "West Nile Virus Human Cases by State (2024)",
    fill = "Cases"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()  # <- this removes grid lines
  )

# Save as PNG
ggsave("west_nile_2024_map.png", plot = wnv_map, width = 8, height = 5, dpi = 300)
