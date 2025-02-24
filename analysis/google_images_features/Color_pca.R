###################################################################################################
#'Color_pca
#'
#'This script get the colorimetric measures using the colordistance pck 
#'from the analysis of images 
#'.  
#'
#'Produces  
#'.   - data_color.csv
#'.   - FIG S4 C
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         
#'
#' @date 2023/03/17 first created
##################################################################################################
rm(list=ls())
source(here::here("R","functions.R"))

images_list <- list.files(here::here("data","BIG_FILES","images","jpg_sq"),full.names=T)

#EXEMPLE with colordistance pck----

  exsp <- "005_1_52758696951_o"
  n_im=which(grepl(paste0(exsp,".jpg"),images_list))


  ## Read in the first image with CIELab pixels
    H1 <- colordistance::loadImage(images_list[114], lower = rep(0.8, 3), upper = rep(1, 3),
                                 CIELab = TRUE, ref.white = "D65", sample.size = 10000)
  
  ##They can be visualized using the plotPixels function, specifying color.space = "lab":
    colordistance::plotPixels(H1, lower = rep(0.98, 3), upper = rep(1, 1), 
                            color.space = "lab", ref.white = "D65", 
                            main = "CIELab color space",
                            ylim = c(-10, 10), zlim = c(0, 10))


  ##Clustering in CIELab space
  ##https://cran.r-project.org/web/packages/colordistance/vignettes/lab-analyses.html

    ### Setting boundaries
      lab_hist <- colordistance::getLabHist(images_list[114], bins = 2, 
                                      sample.size = 10000, ref.white = "D65", bin.avg = TRUE, 
                                      plotting = TRUE, lower = rep(0.98, 3), upper = rep(1, 3),
                                      a.bounds = c(-100, 100), b.bounds = c(-100, 100))
    ### kMeanss
      lab_kmeans <- colordistance::getKMeanColors(images_list[114], n = 5, sample.size = 10000,
                                            lower = rep(0.98, 3), upper = rep(1, 3), 
                                            color.space = "CIELab", ref.white = "D65")

  ##Distance matrices

    par(mfrow = c(4, 4))
    lab_hist_list <- colordistance::getLabHistList(images_list[1:8], bins = 2, sample.size = 10000,
                                                   ref.white = "D65", lower = rep(0.98, 3), upper = rep(1, 3),
                                                   plotting = TRUE, pausing = FALSE)
    
    par(mfrow = c(1,1))
    lab_dist_matrix <- colordistance::getColorDistanceMatrix(lab_hist_list, plotting = TRUE)

#---- 

#PCA color----
    
  lab_hist_list <- colordistance::getLabHistList(images_list, bins = 2, sample.size = 10000,
                                                            ref.white = "D65", lower = rep(0.97, 3), upper = rep(1, 3),
                                                            plotting = FALSE, pausing = FALSE,color.weight=0.4, size.weight=0.6)
  
  lab_dist_matrix <- colordistance::getColorDistanceMatrix(lab_hist_list, plotting = FALSE)
  
  color_tab <- as.data.frame(lab_dist_matrix)
  colnames(color_tab) <- gsub(".jpg","",colnames(color_tab))
  rownames(color_tab) <- gsub(".jpg","",rownames(color_tab))
  color_tab[is.na(color_tab)] <- 0  

  
#need to select some images to feed the pca with constrated features 
  pca_color_tab<-ade4::dudi.pca(color_tab[,c(sample(1:250,20))], scannf=FALSE, nf = 5)

  factoextra::fviz_eig(pca_color_tab,main = "Eigenvalues")
  res.ind <- factoextra::get_pca_ind(pca_color_tab)

#FIG PCA_col
  source(here::here('R','functions.R'))
  E1 <- round(100*(pca_color_tab$eig[1]/sum(pca_color_tab$eig)),1)
  E2 <- round(100*(pca_color_tab$eig[2]/sum(pca_color_tab$eig)),1)
  
  df <- data.frame(x = res.ind$coord[,'Dim.1'],
                   y = jitter(res.ind$coord[,'Dim.2'],150),
                   names=rownames(res.ind$coord))
  
  tiff(here::here("figures_tables","images_features","FIG_PCA12.tiff"), width = 1100, height = 1100)
  plot.2d(scale=0.08,size = -15L,coord <- df,pathphoto <- here::here("data","BIG_FILES","images","png_sq"),xlab=paste0("PC1 (",E1," %)"),ylab=paste0("PC2 (",E2," %)"))
  dev.off()
  
  E2 <- round(100*(pca_color_tab$eig[2]/sum(pca_color_tab$eig)),1)
  E3 <- round(100*(pca_color_tab$eig[3]/sum(pca_color_tab$eig)),1)
  
  df <- data.frame(x = res.ind$coord[,'Dim.2'],
                   y = jitter(res.ind$coord[,'Dim.3'],150),
                   names=rownames(res.ind$coord))
  
  tiff(here::here("figures_tables","images_features","FIG_PCA23.tiff"), width = 1100, height = 1100)
  plot.2d(scale=0.08,size = -15L,coord <- df,pathphoto <- here::here("data","BIG_FILES","images","png_sq"),xlab=paste0("PC2 (",E2," %)"),ylab=paste0("PC3 (",E3," %)"))
  dev.off()
  
  E3 <- round(100*(pca_color_tab$eig[3]/sum(pca_color_tab$eig)),1)
  E4 <- round(100*(pca_color_tab$eig[4]/sum(pca_color_tab$eig)),1)
  df <- data.frame(x = res.ind$coord[,'Dim.3'],
                   y = jitter(res.ind$coord[,'Dim.4'],150),
                   names=rownames(res.ind$coord))
  tiff(here::here("figures_tables","images_features","FIG_PCA34.tiff"), width = 1100, height = 1100)
  plot.2d(scale=0.08,size = -35L,coord <- df,pathphoto <- here::here("data","BIG_FILES","images","png_sq"),xlab=paste0("PC3 (",E3," %)"),ylab=paste0("PC4 (",E4," %)"))
  dev.off()

#prepare the output
  data_color <- res.ind$coord

  data_color$Image_id <- rownames(data_color)
  data_color <- data_color[,c("Image_id","Dim.1","Dim.2","Dim.3","Dim.4")]
  colnames(data_color) <- c("Image_id","PCA_col_1","PCA_col_2","PCA_col_3","PCA_col_4")
  
  write.csv2(data_color,here::here("results","images_features","data_color.csv"),row.names = F)


#----  
