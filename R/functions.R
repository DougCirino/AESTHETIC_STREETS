###################################################################################################
#' Functions set
#'
# This script contain some basic functions used in this project
#'
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         
#'
#' @date 2023/01/30 first created
##################################################################################################

# NORM01
norm01 <- function(dat){
  min_dat <- min(dat,na.rm=TRUE)
  max_dat <- max(dat,na.rm=TRUE)
  dat <- dat+abs(min_dat)
  min_dat <- min(dat,na.rm=TRUE)
  max_dat <- max(dat,na.rm=TRUE)
  dat <- (dat-min_dat)/(max_dat-min_dat)
  return(dat)
}

# PLOT.2D : plot the photo on a 2D plot given two variables
plot.2d <- function(scale,size,coord,pathphoto,xlab,ylab)
{
  
  # scale=0.05
  # size = -15L
  # coord <- df_inflo
  # pathphoto <- here::here("data","images","inflo","pngnobg")
  # xlab="PC1"
  # ylab="PC2"

  labx <- colnames(coord)[1]
  laby <- colnames(coord)[2]
  minx <- min(na.omit(coord[,1]))
  maxx <- max(na.omit(coord[,1]))*1.03
  miny <- min(na.omit(coord[,2]))
  maxy <- max(na.omit(coord[,2]))*1.03
  
  plot(c(0,0),type="n",xlim=c(minx,maxx),ylim=c(miny,maxy),xlab=xlab,ylab=ylab)
  abline(v = 0, col="gray", lwd=2, lty=2)
  abline(h = 0, col="gray", lwd=2, lty=2)
  
  xp=(abs(maxx-minx))*scale
  yp=(abs(maxy-miny))*scale
  
  for (i in 1:nrow(coord)) {
    #i=1
    img <- imager::load.image(file.path(pathphoto,paste0(coord[i,3],".png")))
    img=imager::resize(img, size, size, size_z = -100L,size_c = -100L,interpolation_type =1L)
    img <- as.raster(img)
    img[img=='#FFFFFF']=NA  # pour forcer la transparence
    rasterImage(img, coord[i,1], coord[i,2], coord[i,1]+xp, coord[i,2]+yp,interpolate=TRUE)
    rm(img)
  }
  
}

plot.2d.lm_labels <- function(scale,size,coord,pathphoto,xlab,ylab,xR,yR,colR,labelx,labely,cexlab,cexaxis,colline,lwline,df)
{
  
  # scale=0.0
  # size = -15L
  # coord <- df_flowers
  # pathphoto <- here::here("data","images","png_nobg")
  # xlab=varx
  # ylab=vary
  
  labx <- colnames(coord)[1]
  laby <- colnames(coord)[2]
  minx <- min(na.omit(coord[,1]))
  maxx <- max(na.omit(coord[,1]))*1.03+0.05
  miny <- min(na.omit(coord[,2]))
  maxy <- max(na.omit(coord[,2]))*1.03+0.05
  
  par(mar=c(8, 9, 4.1, 2.1),mgp=c(6,2,0))
  plot(c(0,0),type="n",xlim=c(minx,maxx),ylim=c(miny,maxy),xlab=labelx,ylab=labely,cex.lab=cexlab,cex.axis=cexaxis)
  box(lwd=3)
  #abline(v = 0, col="gray", lwd=2, lty=2)
  #abline(h = 0, col="gray", lwd=2, lty=2)
  
  xp=(abs(maxx-minx))*scale
  yp=(abs(maxy-miny))*scale
  
  for (i in 1:nrow(coord)) {
    #i=1
    img <- imager::load.image(file.path(pathphoto,paste0(coord[i,3],".png")))
    img=imager::resize(img, size, size, size_z = -100L,size_c = -100L,interpolation_type =1L)
    img <- as.raster(img)
    img[img=='#FFFFFF']=NA  # pour forcer la transparence
    rasterImage(img, coord[i,1], coord[i,2], coord[i,1]+xp, coord[i,2]+yp,interpolate=TRUE)
    rm(img)
  }
  
  abline(lm(reformulate("x","y"), data = df),lty=2, lwd=lwline,col=colline)
  mods <- summary(lm(reformulate("x","y"), data = df))
  text(x=xR, y=yR, paste0("r2= ",round(mods$r.squared,2)),cex=cexlab,adj=0,col=colR)
  
}

# POSTER : create a 2D poster with all the photo following a particular order 
#images must be in jpg 
poster <- function(path_photo, tab_image, nb_row, nb_col, outputImageFileName,sizered)
{
   # path_photo <- here::here("data","_BIG_FILES","Images","jpg_topred")
   # tab_image <-  paste0(order_id[1:10],".jpg")
   # nb_row <- 10
   # nb_col <- 10
   # outputImageFileName <- here::here("figures_tables","Poster_pred_all.jpg")
   # sizered <- 1
  
  #compute the size of an image
  #image <- jpeg::readJPEG(file.path(paste0("/",path_photo),tab_image[1]))
  image <- EBImage::readImage(file.path(paste0("/",path_photo),tab_image[1]))
  image <- EBImage::resize(image, dim(image)[1]/sizered)
  image <- EBImage::flop(image)
  row_length <- dim(image[, , 1])[1]
  col_length <- dim(image[, , 1])[2]
  
  #EBImage::display(image)
  
  #dimension of the output image
  outputImage <- array(1,dim = c((nb_row * row_length), (nb_col * col_length), 3))
  
  #create the output image
  pos=0
  for (i in 1:nb_row){
    for (j in 1:nb_col)
    {
      #i=1
      #j=1
      pos=pos+1
      if (pos<=length(tab_image))
      {
        path_image <- file.path(paste0("/",path_photo),tab_image[pos])
        image <- EBImage::readImage(path_image)
        image <- EBImage::resize(image, dim(image)[1]/sizered)
        image <- EBImage::flop(image)
        image <- EBImage::rotate(image,-90)
        outputImage[(1+(i-1)*row_length):(row_length+(i-1)*row_length), (1+(j-1)*col_length):(col_length+(j-1)*col_length), ] <- image
      }
      
    }
  }
  
  #save the outputimage
  
  jpeg::writeJPEG(outputImage, outputImageFileName)
  
}

#JPG Name 
#' jpeg_name
#' add the name of the photo at the bottom of the photo and save in out_photo rep
#' @param path_photo where to find the original jpg photos
#' @param out_photo where to save the named photos
#' @param x x coordinate of the name on the photo
#' @param y y coordinate of the name on the photo
#' @param size size of the police 
#' @param who allow to adapt the script to the dataset of flowers (flo) or inflorescence (inflo)  
function(path_photo, out_photo, x, y, size,who){
  
  # path_photo <- here::here("data","images",who,"jpg")
  # out_photo <- here::here("data","images",who,"jpg_name")
  # x=0.5
  # y=0.005
  # size=2
  # who="flo"
  
  files <- dir(path = path_photo, pattern = ".jpg")
  
  for (i in 1:length(files)){
    #i=2
    #read file
    img <- jpeg::readJPEG(file.path(path_photo,files[i]))
    
    #get size
    h <- dim(img)[1]
    w <- dim(img)[2]
    
    #open new file for output
    jpeg(file.path(out_photo,files[i]), width = w, height = h)
    
    par(mar = c(0,0,0,0), xpd = NA, mgp = c(0,0,0), oma = c(0,0,0,0), ann = F)
    plot.new()
    plot.window(0:1, 0:1)
    
    #fill plot with image
    usr <- par("usr")
    rasterImage(img, usr[1], usr[3], usr[2], usr[4])
    
    #add text
    img_name <- gsub(paste0("_",who,".jpg"), "", files[i])
    img_name <- gsub("_", " ", img_name)
    img_name <- rutils::to_binomial_name(img_name)
    text(x, y, img_name, cex = size, col = "#595959",font=3)
    
    #close image
    dev.off()
  } # eo for
} # eo jpeg_name

#RENAME COL 
#' rename_dfc
#' rename some columns of a data frame df
#' @param df dataframe
#' @param old.col.names names of the columns to rename
#' @param new.col.names new names
rename_dfc <- function(df, old.col.names, new.col.names){
  for (i in 1:length(old.col.names)) names(df)[names(df) == old.col.names[i]] <- new.col.names[i]
  df
}

#SIGNI 
#' Produce produces significance symbols for the values of p (ns., *, **, ***)
#'
#' @param p a p value
#' 
#' @return a character "ns", "*", "**", "***" 
#' 
#' @export
#'
#' @examples
#' signi(p=0.004)
#' 

signi <- function(p){
  if (p>0.05) symbol="ns."
  if ((p<=0.05) & (p>0.01)) symbol="*"
  if ((p<=0.01) & (p>0.001)) symbol="**"
  if (p<=0.001) symbol="***"
  return(symbol)
}
