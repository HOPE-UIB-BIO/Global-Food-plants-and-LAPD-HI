#-----------------------------------------------------------------#
#
#
#         Make plots of frequency of detected human indicators
#                   in fossil pollen records in Latin America
#                         Suzette Flantua
#                         August 2023
#
#-----------------------------------------------------------------#


#----------------------------------------------------------#
# 1. Set up  -----
#----------------------------------------------------------

# load libraries
library(ggplot2)
library(tidyverse)

#----------------------------------------------------------#
# 2. Get human indicators -----
#----------------------------------------------------------#

# load human indicator counts
data <- read.csv("data/human_indicators_count.csv", sep =",")

#----------------------------------------------------------#
# 2. Create different versions of box and facet plots -----
#----------------------------------------------------------

### PLOT 1 BOX PLOT - NOT ORDERED ###
# PLOT 1: Bar plot showing the number of times each human indicator was identified in individual references
frequency_plot <- ggplot(data, aes(x = human_indicator, y = number_refs)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Number of Times Each Human Indicator was Identified",
       x = "Human Indicator",
       y = "Number of References")


### PLOT 2 BOX PLOT - ORDERED ###

# Order human indicators from high to low based on number_refs
data$human_indicator <- reorder(data$human_indicator, -data$number_refs)

# Create the plot
human_bar_plot <- ggplot(data, aes(x = human_indicator, y = number_refs)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3.5)) +
  labs(title = "Number of times each human indicator was identified",
       x = "Human Indicator",
       y = "Number of References")

# Save the plot as a .png file in the "plots" folder
ggsave(filename = "plots/human_indicators_bar_plot.png", plot = human_plot, width = 8.75, height = 3.15)


### PLOT 3 FACETS ###

# Create the plot
human_bar_facets <- ggplot(data, aes(x = perc, y = human_indicator)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~ perc, scales = "free_y", nrow = 2) +
  labs(title = "Frequency of Human Indicators in Fossil Pollen Records",
       x = "Percentage of total papers with human indicator",
       y = "Human Indicator") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 4),
        strip.text = element_text(size = 8))

# Save the plot as a PNG file
ggsave(filename = "plots/human_indicators_facets_plot.png", plot = human_bar_facets, width = 10, height = 7)

# Save the plot as a pdf file
ggsave(filename = "plots/human_indicators_facets_plot.pdf", plot = human_bar_facets, width = 10, height = 7)


### PLOT 4 FACETS GROUPS ###

# Group human indicators into percentage ranges
data <- data %>%
  mutate(percentage_range = cut(perc,
                                breaks = c(0, 1, 2, 5, 10, 20, 100),
                                labels = c("<1%", "1-2%", "2-5%", "5-10%", "10-20%", ">20%")),
         human_indicator = reorder(human_indicator, perc, function(x) -mean(x)))

# Create facetted horizontal bar plot with groups
human_bar_facets2 <- ggplot(data, aes(x = perc, y = human_indicator)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~ percentage_range, scales = "free", ncol = 1) +
  labs(x = "Percentage", y = "Human Indicators") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),
        panel.spacing = unit(0.5, "lines"))

# Export the plot as both PNG and PDF
ggsave(filename = "plots/human_indicators_facetted_plot2.png", plot = human_bar_facets2, width = 6, height = 10, dpi = 300)
ggsave(filename = "plots/human_indicators_facetted_plot2.pdf", plot = human_bar_facets2, width = 6, height = 10)


### PLOT 4 FACETS GROUPS RESHUFFLE ORDER ###

# Group human indicators into percentage ranges and name
data <- data %>%
  mutate(percentage_range = cut(perc,
                                breaks = c(0, 1, 2, 5, 10, 20, 100),
                                labels = c("<1%", "1-2%", "2-5%", "5-10%", "10-20%", ">20%")),
         human_indicator = factor(human_indicator, levels = unique(human_indicator)))

# Create facetted horizontal bar plot
human_bar_facets3 <- ggplot(data, aes(x = perc, y = reorder(human_indicator, perc))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~ percentage_range, scales = "free", ncol = 1) +
  labs(x = "Percentage", y = "Human Indicators") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),
        panel.spacing = unit(0.5, "lines"))

# Export the plot as both PNG and PDF
ggsave(filename = "plots/human_indicators_facetted_plot3.png", plot = human_bar_facets3, width = 6, height = 10, dpi = 300)
ggsave(filename = "plots/human_indicators_facetted_plot3.pdf", plot = human_bar_facets3, width = 6, height = 10)

