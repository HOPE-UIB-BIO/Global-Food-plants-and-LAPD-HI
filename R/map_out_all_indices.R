#-----------------------------------------------------------------#
#
#
#                  Make map of human indices 
#
#                         Suzette Flantua
#                         November 2023
#
#-----------------------------------------------------------------#

# 1. Load packages

library(ggplot2)

# 2. Load dataset

human_indices <- readxl::read_excel("data/human_impact_indices_coordinates.xlsx")

# Convert ref-ID to numeric 
human_indices$`ref-ID` <- as.numeric(human_indices$`ref-ID`)

# Fetch world map data
world_map <- map_data("world")

# Filter out Latin American countries
latin_america <- subset(world_map, region %in% c("Mexico", "Central America", "South America"))

# Create the base map with Latin American boundaries
base_map <- ggplot() +
  geom_polygon(data = world_map,
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "gray60") +
  geom_polygon(data = latin_america,
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "gray60") +
  coord_fixed(ylim = c(-60, 35), xlim = c(-130, -35)) +
  scale_x_continuous(breaks = seq(-130, -35, by = 20)) +
  scale_y_continuous(breaks = seq(-60, 35, by = 20)) +
  theme_void()

# Add points for lakes with ref-ID values by color legend
lake_map1 <- base_map +
  geom_point(data = human_indices, aes(x = Longitude, y = Latitude, color = `ref-ID`),
             size = 3) +
  labs(title = "Lakes in Latin America with ref-ID Values") +
  scale_color_continuous(name = "ref-ID")  # Add color scale legend

# Display the map
print(lake_map1)

# -------------------------------

# Add points for lakes with ref-ID values
lake_map2 <- base_map +
  geom_point(data = human_indices, aes(x = Longitude, y = Latitude),
             size = 3) +
  geom_text(data = human_indices, aes(x = Longitude, y = Latitude, label = `ref-ID`),
            color = "black", vjust = -0.5, size = 3) +  # vjust to adjust text position
  labs(title = "Lakes in Latin America with ref-ID Values")  +
  scale_color_continuous(name = "ref-ID")  # Add color scale legend

# Display the map
print(lake_map2)

# -------------------------------

# adjust bounding box and add coordinates
base_map <- ggplot() +
  geom_polygon(data = world_map,
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "gray60") +
  geom_polygon(data = latin_america,
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "gray60") +
  geom_hline(yintercept = seq(-60, 60, by = 20), color = "black", linetype = "dotted") +
  geom_vline(xintercept = seq(-130, -20, by = 20), color = "black", linetype = "dotted") +
  coord_fixed(ylim = c(-60, 30), xlim = c(-135, -30)) +
  annotate("text", x = seq(-130, -30, by = 20), y = -63, label = c("130°W", "110°W", "90°W", "70°W", "50°W", "30°W")) +
  annotate("text", x = -135, y = seq(-60, 40, by = 20), label = c("60°S", "40°S", "20°S", "0°", "20°N", "40°N")) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),  
        plot.background = element_rect(fill = "white"))



# Add blue colored points and avoid overlap
lake_map3 <- base_map +
  geom_point(data = human_indices, aes(x = Longitude, y = Latitude),
             size = 3, color = "lightblue") +
  geom_text(data = human_indices, aes(x = Longitude, y = Latitude, label = `ref-ID`),
            color = "black", vjust = -0.5, size = 3, check_overlap = TRUE) +
  labs(title = "Lakes in Latin America with ref-ID Values")

# Display the map
print(lake_map3)


# point to all numbers with a line 
lake_map4 <- base_map +
  geom_segment(data = human_indices, aes(x = Longitude, y = Latitude + 1,
                                         xend = Longitude, yend = Latitude - 3),
               color = "darkblue", size = 0.5) +
  geom_point(data = human_indices, aes(x = Longitude, y = Latitude, color = factor(`ref-ID`)),
             size = 3) +
  geom_text(data = human_indices, aes(x = Longitude, y = Latitude - 3, label = `ref-ID`),
            color = "black", size = 3, check_overlap = TRUE, vjust = 1) +
  scale_color_discrete(name = "ref-ID") +  # Add color scale legend
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),  # Adjust margin around the plot
        plot.background = element_rect(fill = "white"))

# Display the map
print(lake_map4)

ggsave("plots/lake_map.pdf", plot = lake_map4, width = 10, height = 7)
