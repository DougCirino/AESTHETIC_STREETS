################################################################################################
#' Data collection
#' 
#'This script assemble 
#'  - the features extracted in cluster.R, lumsat.R, Colors_pca.R
#'  - the Elo scores (aesthetic) 
#'  
#'into a single data frame used afterward in our analysis
#'
#'.   - data_belleza_all.csv
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr}#'        
#'
#' @date 2023/07/11 first created
################################################################################################


rm(list=ls(all=TRUE)) 

data_elo <- read.csv2(here::here("results","elo_scores.csv"))


cluster <- read.csv(here::here("results","images_features","cluster.csv"))
cluster <- cluster[,-1]
lumsat <- read.csv(here::here("results","images_features","lumsat.csv"))
lumsat <- lumsat[,-1]
colors <- read.csv2(here::here("results","images_features","data_color.csv"))
fluency <- read.csv2(here::here("results","images_features","fluency.csv"))
count_color <- read.csv2(here::here("results","images_features","count_color.csv"))


names(cluster)[names(cluster) == 'name'] <- 'Id_images'
names(lumsat)[names(lumsat) == 'name'] <- 'Id_images'
names(colors)[names(colors) == 'Image_id'] <- 'Id_images'
names(fluency)[names(fluency) == 'name'] <- 'Id_images'
names(count_color)[names(count_color) == 'name'] <- 'Id_images'

data_belleza_all <- merge(data_elo,cluster)
data_belleza_all <- merge(data_belleza_all,lumsat)
data_belleza_all <- merge(data_belleza_all,colors)
data_belleza_all <- merge(data_belleza_all,fluency)
data_belleza_all <- merge(data_belleza_all,count_color)


write.csv2(data_belleza_all,here::here("results","data_belleza_all.csv"),row.names = F)



