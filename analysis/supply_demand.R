################################################################
#
#  Creates a Biplot Map between the Population (demand) and 
#    the predicted values of aesthetics (supply)
#
#
#    Uses a 100x100m grid with the mean values of population and Aesthetics
#
#
#
#GRAPH -> Population vs. Aeshtetics biplot map


install.packages(c("faux","raster"))
library('sf')


## install just cowplot
install.packages("cowplot")

## install all suggested dependencies
install.packages("biscale", dependencies = TRUE)

install.packages("extrafont")
library(extrafont)

font_import(pattern = "Arial Unicode MS")

library(biscale)   # bivariate mapping
library(cowplot)   # combine ggplot2 objects
library(faux)      # create simulated data
library(ggplot2)   # create maps
library(raster)    # work with raster data
library(gridExtra)

#neon_harv <- as.data.frame(DSM_HARV, xy = TRUE)
sdb <- read.csv2("Aesthetic+GVI+Demand.csv", sep = ",", row.names = NULL)

summary(sdb)


sdb$aesthetic_ <- as.numeric(sdb$aesthetic_)
sdb$pop_17_0_1 <- as.numeric(sdb$pop17_0_1)
sdb$top <- as.numeric(sdb$top)
sdb$left <- as.numeric(sdb$left)


## reorder variables
sdb <- subset(sdb, select = c(aesthetic_, pop_17_0_1, left, top))

summary(sdb)

sdb <- bi_class(sdb, x = aesthetic_ , y = pop_17_0_1, style = "fisher")
help("bi_pal")

map <- ggplot() +
  geom_raster(data = sdb , aes(x = left, y = top, fill = bi_class)) +
  #geom_sf(data = shp) +
  bi_scale_fill(pal = "PinkGrn") +
  coord_cartesian() +
  labs(
    x = "",
    y = "") +
  bi_theme(base_size = 16) +
  theme(legend.position="none")

legend <- bi_legend(pal = "PinkGrn",
                    xlab = "Supply",
                    ylab = "Demand",
                    size = 12,
                    arrows = FALSE)

## construct final plot
finalPlot <-  plot_grid(
  map, legend,
  rel_widths = c(1, .4),
  nrow = 1
)

## print final plot
x11()
finalPlot


ggsave("Supply_demand_biplot.png", finalPlot, dpi = 500)


# Convert bi_class to factor
sdb$bi_class <- as.factor(sdb$bi_class)

# Count pixels in each bi_class
pixel_counts <- table(sdb$bi_class)

# Display the counts
print(pixel_counts)install.packages(c("faux","raster"))

  
  
  