# Load necessary libraries
library(dplyr)
library(ggplot2)
library(viridis)

binos <- read.csv("Binocular_Review.csv") # Read in the CSV

bins <- binos %>% 
  filter(Use.For.Initial.Guide. == "Y") %>% 
  mutate(fov = FOV.ft...1000.yards., eye_relief = Eye.Relief..mm.)# Subset to only the bins used for the guide

bins <- bins[, c(1:10, 16)] %>% 
  select(-c("Link", "Maker")) %>%
  mutate(Price..MSRP.. = as.numeric(Price..MSRP..),
         rating = sample(1:10, n(), replace = TRUE)) %>% 
  arrange((rating))
# Subset further for only the columns we want

bins$Binocular.Name <- factor(bins$Binocular.Name, levels = bins$Binocular.Name)

# Create the bar plot
ggplot(data = bins) +
  aes(x = Binocular.Name, y = eye_relief, fill = eye_relief) +
  geom_bar(width = 0.9, stat = "identity", color = 'black') +
  geom_text(aes(label = eye_relief), vjust = -0.4) +
  theme_minimal()+
  labs(x = "Binocular Name", y = "Eye Relief (mm)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")+
  scale_fill_viridis_c(option = "magma", direction = -1)


ggplot(data = bins) +
  aes(x = Binocular.Name, y = fov, fill = fov) +
  geom_bar(width = 0.9, stat = "identity", color = 'black') +
  geom_text(aes(label = fov), vjust = -0.4) +
  theme_minimal()+
  labs(x = "Binocular Name", y = "Linear Fielf of View (ft)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")+
  scale_fill_viridis_c(option = "magma", direction = -1)

ggplot(data = bins) +
  aes(x = Binocular.Name, y = Close.Focus..ft., fill = Close.Focus..ft.) +
  geom_bar(width = 0.9, stat = "identity", color = 'black') +
  geom_text(aes(label = Close.Focus..ft.), vjust = -0.4) +
  theme_minimal()+
  labs(x = "Binocular Name", y = "Close Focus Distance (ft)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")+
  scale_fill_viridis_c(option = "magma", direction = -1)



# geom_col()# Create the grid table plot
ggplot(data = bins) +
  aes(x = Binocular.Name, y = factor(1), fill = rating) +
  geom_tile(color = "white", width = 1, height = 0.3) +
  geom_text(aes(label = rating), color = "black", size = 5) +
  scale_fill_gradient(low = "red", high = "green", limits = c(1, 10)) +
  labs(title = "Ratings of Binoculars", x = "Binocular Name", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(), 
        legend.position = "none")


