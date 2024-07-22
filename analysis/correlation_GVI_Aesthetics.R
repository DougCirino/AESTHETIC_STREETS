setwd('G:/Meu Drive/PESQUISA - Ecologia Urbana e Serviços Ecossistêmicos/Doutorado/Dados/Extrapolation')

# Read the data
data <- read.csv("Aesthetic+GVI+Demand.csv", sep = ",")

options(na.action = "na.omit")
cor(data$greenView_,data$aesthetic_)
 summary(data$greenView_)
 summary(data$aesthetic_)

correlation <- cor(data$greenView_, data$aesthetic_, use = "complete.obs")

# Install and load necessary packages if not already installed
# install.packages("ggplot2")
# install.packages("ggpubr")
library(ggplot2)
library(ggpubr)
library(viridis)  # For the viridis color scale
library(MASS)  # For density estimation

# Function to calculate density
get_density <- function(x, y, n = 100) {
  dens <- kde2d(x, y, n = n)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  dens$z[cbind(ix, iy)]
}

# Remove rows with missing or infinite values in greenView_ or aesthetic_
data <- data[is.finite(data$greenView_) & is.finite(data$aesthetic_), ]

# Calculate density
data$density <- get_density(data$greenView_, data$aesthetic_)

# Create a scatterplot with density-based coloring
scatter_plot <- ggplot(data) +
  geom_point(aes(x = greenView_, y = aesthetic_, color = density), alpha = 0.6) +
  scale_color_viridis(direction = 1) +  # Adjust the color scale using viridis
  geom_smooth(aes(x = greenView_, y = aesthetic_), method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(title = "",
       x = "Green View",
       y = "Aesthetic") +
  theme_minimal()  # Maintain the axes

# Calculate R^2
correlation <- cor(data$greenView_, data$aesthetic_, use = "complete.obs")
r_squared <- correlation^2

# Add R^2 to the plot
scatter_plot_with_r_squared <- scatter_plot +
  annotate("text", x = max(data$greenView_), y = min(data$aesthetic_), 
           label = paste("R² =", round(r_squared, 3)),
           vjust = 1, hjust = 1, color = "red")

# Print the plot
x11()
print(scatter_plot_with_r_squared)

# Create and summarize the linear model
model <- lm(greenView_ ~ aesthetic_, data = data)
summary(model)
