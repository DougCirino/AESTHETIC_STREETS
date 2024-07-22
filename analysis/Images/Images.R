####IMAGES 

list_img <- list.files(here::here('data','photos'),full.names=TRUE)
pbmcapply::pbmclapply(list_img, function(id){
  img <- magick::image_read(id)
  img <- magick::image_resize(img,"700x525!")
  magick::image_write(img,id)
  img <-png::readPNG(id)
  png::writePNG(img,id,dpi=96)
},mc.cores=10)
