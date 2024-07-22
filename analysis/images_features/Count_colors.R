###################################################################################################
#'Count_colors
#'
#'This script get the amount of green, blue and grey measures using the countcolors pck 
#'from the analysis of images 
#'https://cran.r-project.org/web/packages/countcolors/vignettes/Introduction.html
#'.  
#'
#'Produces  
#'.   - data_count_color.csv
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         
#'
#' @date 2023/10/17 first created
##################################################################################################
rm(list=ls())
source(here::here("R","functions.R"))

images_list <- list.files(here::here("data","BIG_FILES","images","jpg_sq"),full.names=T)

#EXEMPLE ----

exsp <- "001_1_52759182023_o"
n_im=which(grepl(paste0(exsp,".jpg"),images_list))


green.center <- c(0.2, 0.39, 0.2)
gray.center <- c(0.6, 0.6, 0.6)
blue.center <- c(0.55, 0.74, 0.94)
bg.lower <- c(0, 0, 0)


green <- countcolors::countColors(images_list[n_im], color.range="spherical", 
                                  center = c(green.center, gray.center,blue.center), radius = c(0.11,0.15,0.15),
                                  bg.lower=bg.lower, bg.upper=NULL, plotting = TRUE,
                                  target.color=c("green", "grey","blue"))
#----


#ANALYSE ALL IMAGES----

green.center <- c(0.2, 0.39, 0.2)
gray.center <- c(0.6, 0.6, 0.6)
blue.center <- c(0.55, 0.74, 0.94)
bg.lower <- c(0, 0, 0)

images_list <- list.files(here::here("data","BIG_FILES","images","jpg_sq"),full.names=T)
pict <-length(images_list)
data_count_color <- as.data.frame(do.call(rbind,pbmcapply::pbmclapply(1:pict, function(i) {
  
 # i=1
  
  img <- images_list[i]
  name <- gsub(".jpg","",strsplit(images_list[i],"/")[[1]][length(strsplit(images_list[i],"/")[[1]])])
  
  green <- countcolors::countColors(img, color.range="spherical", 
                                    center = green.center, radius = c(0.12),
                                    bg.lower=bg.lower, bg.upper=NULL, plotting = FALSE)
  green <- green$pixel.fraction
  
  gray <- countcolors::countColors(img, color.range="spherical", 
                                   center = gray.center, radius = c(0.15),
                                   bg.lower=bg.lower, bg.upper=NULL, plotting = FALSE)
  gray <- gray$pixel.fraction
  
  blue <- countcolors::countColors(img, color.range="spherical", 
                                   center = blue.center, radius = c(0.15),
                                   bg.lower=bg.lower, bg.upper=NULL, plotting = FALSE)
  blue <- blue$pixel.fraction
  
  cbind.data.frame(name=name,green=green,gray=gray,blue=blue)
  
},mc.cores = parallel::detectCores() )))

write.csv2(data_count_color,here::here("results","images_features","count_color.csv"),row.names = F)

#----

#FIGURES----

#FIG PCA_col
source(here::here('R','functions.R'))

  df <- data.frame(x = data_count_color$green,
                   y = data_count_color$blue,
                   names=data_count_color$name)
  
  tiff(here::here("figures_tables","images_features","FIG_GREEN_BLUE.tiff"), width = 1100, height = 1100)
  plot.2d(scale=0.05,size = -100L,coord <- df,pathphoto <- here::here("data","BIG_FILES","images","png_sq"),xlab="GREEN",ylab="BLUE")
  dev.off()
  
  df <- data.frame(x = data_count_color$green,
                   y = data_count_color$gray,
                   names=data_count_color$name)
  
  tiff(here::here("figures_tables","images_features","FIG_GREEN_GRAY.tiff"), width = 1100, height = 1100)
  plot.2d(scale=0.05,size = -100L,coord <- df,pathphoto <- here::here("data","BIG_FILES","images","png_sq"),xlab="GREEN",ylab="GRAY")
  dev.off()
  
  df <- data.frame(x = data_count_color$blue,
                   y = data_count_color$gray,
                   names=data_count_color$name)
  
  tiff(here::here("figures_tables","images_features","FIG_BLUE_GRAY.tiff"), width = 1100, height = 1100)
  plot.2d(scale=0.05,size = -100L,coord <- df,pathphoto <- here::here("data","BIG_FILES","images","png_sq"),xlab="GREEN",ylab="BLUE")
  dev.off()


#----





