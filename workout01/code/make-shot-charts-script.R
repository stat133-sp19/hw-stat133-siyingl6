#title: shots-charts
#description: shots chart of the players
#inputs: data frames - curry, iguodala, durant, thompson, green
#outputs: stephen-curry-shot-chart.pdf, andre-iguodala-shot-chart.pdf, kevin-durant-shot-chart.pdf, klay-thompson-shot-chart.pdf, draymond-green-shot-chart.pdf, gsw-shot-charts.pdf, gsw-shot-charts.png 

library(jpeg)
library(grid)

# court image (to be used as background of plot)
court_file <- "../images/nba-court.jpg"

# create raste object
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))

# 4.1) Short charts of each player
# Stephen Curry
curry_shot_chart <- ggplot(data = curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal() +
  ggsave("../images/stephen-curry-shot-chart.pdf", width = 6.5, height = 5)

# Andre Iguodala
iguodala_shot_chart <- ggplot(data = iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal() +
  ggsave("../images/andre-iguodala-shot-chart.pdf", width = 6.5, height = 5)

# Kevin Durant
durant_shot_chart <- ggplot(data = durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal() +
  ggsave("../images/kevin-durant-shot-chart.pdf", width = 6.5, height = 5)

# Klay Thompson
thompson_shot_chart <- ggplot(data = thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal() +
  ggsave("../images/klay-thompson-shot-chart.pdf", width = 6.5, height = 5)

# Draymond Green
green_shot_chart <- ggplot(data = green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal() +
  ggsave("../images/draymond-green-shot-chart.pdf", width = 6.5, height = 5)

# 4.2) Facetted Shot Chart
# PDF format
shots_data <- read.csv("../data/shots-data.csv", stringsAsFactors = FALSE)
ggplot(data = shots_data) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: GSW (2016 season)') +
  theme_minimal() +
  facet_wrap(~name, ncol=3) + 
  ggsave("../images/gsw-shot-charts.pdf", width = 8, height = 7)

# PNG format
ggplot(data = shots_data) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle('Shot Chart: GSW (2016 season)') +
  theme_minimal()+
  facet_wrap(~name,ncol=3) +
  ggsave("../images/gsw-shot-charts.png", width = 8, height = 7)
