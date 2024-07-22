###################################################################################################
#'Color_pca
#'
#'This script get the fluency metrics using the imagefluency pck 
#'from the analysis of images 
#'https://cran.r-project.org/web/packages/imagefluency/readme/README.html
#'.  
#'
#'Produces  
#'.   - data_fluency.csv
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         
#'
#' @date 2023/10/09 first created
##################################################################################################
rm(list=ls())
source(here::here("R","functions.R"))

images_list <- list.files(here::here("data","BIG_FILES","images","jpg_sq"),full.names=T)

#EXEMPLE ----

  exim <- "005_1_52758696951_o"
  n_im=which(grepl(paste0(exim,".jpg"),images_list))


    ex_im <- imagefluency::img_read(images_list[n_im])
    
    grid::grid.raster(ex_im)
    
      imagefluency::img_contrast(ex_im)
      imagefluency::img_symmetry(ex_im)
      imagefluency::img_complexity(ex_im)
      imagefluency::img_self_similarity(ex_im)
#----

      
#ANALYSE ALL IMAGES----
      
    images_list <- list.files(here::here("data","BIG_FILES","images","jpg_sq"),full.names=T)
    pict <-length(images_list)
    
    fluency_data <- as.data.frame(do.call(rbind,pbmcapply::pbmclapply(1:pict, function(i) {
      
      #i=1
      img <- imagefluency::img_read(images_list[i])
      name <- gsub(".jpg","",strsplit(images_list[i],"/")[[1]][length(strsplit(images_list[i],"/")[[1]])])
      
      contrast <- imagefluency::img_contrast(img)
      complexity <- imagefluency::img_complexity(img)
      symmetry <- imagefluency::img_symmetry(img)
      
      symmetry_v <- as.numeric(symmetry[1])
      symmetry_h <- as.numeric(symmetry[2])
      
      self_similarity <- imagefluency::img_self_similarity(img)
      
      cbind.data.frame(name=name,contrast=contrast,complexity=complexity,self_similarity=self_similarity,symmetry_v,symmetry_h)
      },mc.cores = parallel::detectCores() )))
    
    write.csv2(fluency_data,here::here("results","images_features","fluency.csv"),row.names = F)
      
#----    



