###################################################################################################
#' Get Google Street view images 
#'
#'This script 
#'  - get the google street view images using the google API and the R package googleway
#'
#'
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         
#'
#' @date 2023/06/21 first created
##################################################################################################
rm(list=ls(all=TRUE))

key <- #insert here your google key

street_raw <- read.csv(here::here('data','street_raw.csv'),sep=";")
street_raw$id <- paste0("lat_",street_raw$lat,"_long_",street_raw$long)
street_raw$lat <- as.numeric(street_raw$lat)
street_raw$long <- as.numeric(street_raw$long)
write.csv2(street_raw,here::here('data','street_id.csv'))

lapply(1:nrow(street_raw), function(i){
  
  
  # i=10
  url <- googleway::google_streetview(location = c(street_raw$lat[i], street_raw$long[i]),
                                      size = c(500,500), output = "html",
                                      key = key)
  im_file <- imager::load.image(url)
  imager::save.image(im_file,here::here("data","BIG_FILES","ggstreet","png",paste0(street_raw$id[i],'.png')))
  
})


#POSTER 

list_img <- list.files(here::here("data","BIG_FILES","ggstreet","png"),full.names=TRUE)
pbmcapply::pbmclapply(list_img, function(id){
  img <- png::readPNG(id)
  jpeg::writeJPEG(img,gsub("png","jpg",id))
},mc.cores=2)


source(here::here('R','functions.R'))
elo_scores <- read.csv(here::here("results","inference_beleza.csv"))


elo_scores$image_name<-do.call(rbind,lapply(strsplit(elo_scores$image_name,"/"),function(id){
  unlist(id)
  id[4]
}))
elo_scores$image_name <- gsub(".png","",elo_scores$image_name)

order_id<-elo_scores[order(elo_scores$predicted_score,decreasing = TRUE),"image_name"]
outputImageFileName <- here::here("figures_tables","Poster_pred.jpg")
poster(path_photo <- here::here("data","BIG_FILES","ggstreet","jpg"),
       tab_image <-  paste0(order_id,".jpg"),
       nb_row <- 6,
       nb_col <- 6,
       outputImageFileName <- outputImageFileName,
       sizered=1
)






