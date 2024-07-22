###################################################################################################
#'Main analysis presented in the article
#'
#'
#'Produces 
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         
#'
#' @date 2023/06/11 first created
##################################################################################################

rm(list=ls(all=TRUE)) 
library(ggplot2)

source(here::here("R","functions.R"))
data_belleza_all <- read.csv2(here::here("results","data_belleza_all.csv"))


#FEAFTURE ANALYSIS using LM FIG4acdef, FIG5acdef----

varall <- c("CL_cie_d_mean","SH_shape.index_mean","LS_mean_satu",
            "LS_mean_light","PCA_col_1","PCA_col_2","PCA_col_3",
            "contrast","self_similarity","green","gray","blue")

data_features_sub <- data_belleza_all[,c("Id_images","Elo_mean_all",varall)]
colnames(data_features_sub) <- c("Id_images","Aesthe","Color_heterogeneity","Complexity",
                                 "Color_saturation","Brightness","PCA_col_1","PCA_col_2",          
                                 "PCA_col_3","Contrast","Self_similarity","Green_fraction",              
                                 "Gray_fraction","Blue_fraction")

write.csv2(data_features_sub,here::here("results","data_features_sub.csv"),row.names = F)

varcor="aesthe"

  #normalize and log data 

    feat_norm <- cbind.data.frame(Id_images=data_belleza_all$Id_images,aesthe=data_belleza_all$Elo_mean_all, data_belleza_all[,varall])
    
    for (i in varall) feat_norm[,i] <- norm01(as.numeric(feat_norm[,i]))
    
    #feat_norm$SH_shape.index_mean <- 1-feat_norm$SH_shape.index_mean #must be inversed to represente image complexity 
    
    par(mfrow = c(4, 3))
    for(i in 1:length(varall))car::densityPlot(feat_norm[,varall[i]])
    
      # varall_sub <- c(1)
      # feat_norm[,varall[varall_sub]] <- log10(feat_norm[,varall[varall_sub]]+0.01)
      # par(mfrow = c(4, 4))
      # for(i in 1:length(varall))car::densityPlot(feat_norm[,varall[i]])

  #Look at the correlation between variables and remove the variable with pearson > 0.6

    GGally::ggpairs(feat_norm[,-1])
    
    cormat <- function(idcor,data,thr){
      
      #idcor=all_id
      #data=data_fisheyes
      #thr=0.7 
      
      remove <- function(id_name,rem) {
        for (i in 1:length(rem)) id_name=id_name[id_name!=rem[i]]
        return(id_name)
      }     
      
      id_temp <- idcor
      
      for (i in 1:length(id_temp))  {
        if (length(id_temp)==1) break
        cor_mat <- cor(data[,id_temp])
        if (i>= dim(cor_mat)[1]) break
        rem <- names(which(abs(cor_mat[,i])>thr)[-1])
        if (length(rem)>0) id_temp <- remove(id_name=id_temp,rem=rem)
      }  
      return(id_temp)
    }
    
    id_tps <- cormat(idcor=varall,data=feat_norm,thr=0.7)
    
    #Order the remaining variables in decreasing order of importance
    
      #varcor : variable to explain
      #all_id : vector of variable to use in the correlation
      #dat_cor: dataset to be used 
      #ord : stat to be used to sort the variable (R2, PERVAR percentage of total variance,PEARSON coefficient)
    
    look_cor <- function(all_id,dat_cor=datacor,varcor,ord){
      
      # all_id=id_tps
      # dat_cor=feat_flowe_rnorm
      # varcor=varcor
      # ord="PERVAR"
      
      modall <- lm(as.formula(paste(varcor," ~ ", paste(all_id, collapse= "+"))),data=dat_cor, na.action=na.omit)
      
      out_data <- as.data.frame(matrix(NA,ncol=4,nrow=length(all_id)))
      colnames(out_data) <- c("var","pervar","R2","pearson.p")
      out_data$var <- all_id
      
      for (i in 1:length(all_id))
      {
        #i=1
        cort <- cor.test(dat_cor[,varcor],dat_cor[,all_id[i]])
        out_data$pearson.p[i] <- cort$p.value
        modminus <- lm(as.formula(paste(varcor," ~ ", paste(all_id[-i], collapse= "+"))),data=dat_cor, na.action=na.omit)
        modalone <- lm(as.formula(paste(varcor," ~ ", all_id[i])),data=dat_cor, na.action=na.omit)
        out_data$pervar[i] <- (100-(summary(modminus)[[8]]/summary(modall)[[8]])*100)
        out_data$R2[i] <- summary(modalone)[[8]]
      }
      
      if (ord=="R2") corres <- out_data[order(-out_data$R2),]
      if (ord=="PERVAR") corres <- out_data[order(-out_data$pervar),]
      if (ord=="PEARSON") corres <- out_data[order(-out_data$pearson.p),]
      
      return(corres)
    }
    
    ord <- look_cor(id_tps,dat_cor=feat_norm,varcor,ord="PERVAR")
    
    id_final <- ord$var
    
    #Keep the variables that are significant till all variable are significant 
    signi=FALSE
    thr <- 0.05
    while (signi==FALSE){
      modint <- lm(as.formula(paste(varcor," ~ ", paste(id_final, collapse= "+"))),data=feat_norm)
      #summary(modint)
      sumod <- summary(modint)
      id_final <- rownames(sumod$coefficients)[sumod$coefficients[,4]<thr][-1]
      if (sum(!sumod$coefficients[,4]<thr)==0) signi=TRUE
    }
    
    id_final_lm <-  id_final
    
    #build the final model 
    modfinal <- lm(as.formula(paste(varcor," ~ ", paste(id_final_lm, collapse= "+"))),data=feat_norm)
    summary(modfinal)
    ##R2=0.63
    
    #FIGURE PCA_feat a
      data_scale <-  cbind.data.frame(aesthe=feat_norm$aesthe, feat_norm[,id_final_lm])
      modfinal <- lm(as.formula(paste(varcor," ~ ", paste(id_final_lm, collapse= "+"))),data=data_scale)
      sum_mod <- summary(modfinal)
      
      x <- rownames(sum_mod$coefficients)
      x <-x[-1]
      Estimate <- sum_mod$coefficients[-1,"Estimate"]
      sdev <- sum_mod$coefficients[-1,"Std. Error"]
      
      data_plot <- cbind.data.frame(name=x,Estimate=Estimate,sdev=sdev)
      data_plot <- data_plot[order(data_plot$Estimate,decreasing = FALSE),]
      data_plot$pos <- NA
     
        for (i in 1:length(data_plot$pos)) {if (data_plot$Estimate[i]<0) data_plot$pos[i]=200 else data_plot$pos[i]=-1900}
        data_plot$name <- c("Brightness","PCA_col_3","Color heterogeneity","Color saturation","Blue","Complexity")
        fill <- c("tomato","tomato","seagreen3","seagreen3","seagreen3","seagreen3")
      
      #Fig PCA_feat 
      
        library(ggplot2)
        theme_set(theme_bw())
        plot <-  ggplot(data_plot) +
          geom_bar( aes(x=name, y=Estimate), stat="identity",  fill=fill, alpha=0.8) +
          geom_errorbar( aes(x=name, ymin=Estimate-sdev, ymax=Estimate+sdev), width=0.3, colour="azure4", alpha=0.9, size=1) +
          scale_x_discrete(limits=data_plot$name)+
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text.y = element_blank(),
                axis.text=element_text(size=12),
                axis.title=element_text(size=12)
          ) +
          ylim(-2000,1900)+
          xlab("Images Features") +
          ylab("Estimates (scaled)") +
          coord_flip() + 
          geom_text(aes(x=name, y=pos,label = name),hjust = 0,size=3.5)
        
        ggplot2::ggsave(filename = here::here("figures_tables", "images_features","PCA_feat_a.jpg"),
                                        plot, width = 9, height = 10.5, units = "cm", dpi = 300)
                      

    #Fig feat 
        
    #feattoplot <-  rownames(data_plot)   
    feattoplot <- id_final_lm
        
    lapply(feattoplot,function(id){
      
      #id <- rownames(data_plot)[2]
      
      varx=id 
      vary="aesthe"
      
      df <- data.frame(x = feat_norm[,varx],
                       y = jitter(feat_norm[,vary],150),
                       names=feat_norm$Id_images)
      
        tiff(here::here("figures_tables","images_features",paste0("Fig_feat_",varx,".tiff")), width = 1100, height = 1100)
        plot.2d.lm_labels(scale=0.05,size = -100L,coord <- df,pathphoto <- here::here("data","BIG_FILES","images","png_sq"),xlab=varx,
                          ylab=vary,xR=0.8,yR=2300,colR="#7D7D7D",labelx=id,labely="Aesthetic",cexlab=4.5,cexaxis=3.5,
                          colline="#325395",lwline=8,df=df)
        dev.off()
        
    })

#----

#PCA FEAFTURES b ---- 

  data_pca_lm <-  cbind.data.frame(aesthe=feat_norm$aesthe, feat_norm[,id_final_lm])

  ##Change the names of the variables (see Text F in S1 File)
    
    colnames(data_pca_lm) <- c("aesthe","PCA_col_4","Brightness","Complexity","PCA_col_3","Light heterogeneity","PCA_col_1")
  
  ##Do the PCA
  
    pca_lm<-ade4::dudi.pca(data_pca_lm[,-1], scannf=FALSE, nf = 3)

    factoextra::fviz_eig(pca_lm,main = "Eigenvalues")

    GGally::ggpairs(cbind(data_pca_lm$aesthe,pca_lm$l1))


  ##with ggplot data_pca_glm and factoextra
  ##http://www.sthda.com/english/wiki/fviz-pca-quick-principal-component-analysis-data-visualization-r-software-and-data-mining

  pca_axis <- c(1,3) 
  ylim          = c(-6.5,6.5)
  xlim          = c(-6.5,6.5)
  legend.position = c(0.1, 0.85)
  
  library(ggplot2)
  plot <- factoextra::fviz_pca_biplot(pca_lm,
                                    axes = pca_axis,
                                    #col.var = "contrib", 
                                    col.ind       = data_pca_lm$aesthe,
                                    geom          = "point",
                                    gradient.cols = c("#325395", "#4575b4","#74add1","#abd9e9", "#e0f3f8",'#fee090','#fdae61','#f46d43','#d73027'),
                                    repel         = TRUE,
                                    col.var       = "darkblue",
                                    geom.var      = c("arrow", "text"),
                                    alpha.ind     = 0.9,
                                    ggtheme       = theme_bw(),
                                    ylim          = ylim,
                                    xlim          = xlim,
                                    pointsize=2) +
  labs(col="Aesthetic", title = " ") +
  theme(legend.position = legend.position)

  ggplot2::ggsave(filename = here::here("figures_tables","images_features","PCA_feat_b.jpg"),plot,
                  width = 18, height = 18, units = "cm", dpi = 300)
 
  
  ## find coordinates of image to illustrate the PCA figure 
    res.ind <- factoextra::get_pca_ind(pca_lm)
  
    identifyPch <- function(x, y = NULL, n = length(x), plot = FALSE, pch = 19, ...)
    {
      xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
      sel <- rep(FALSE, length(x))
      while(sum(sel) < n) {
        ans <- identify(x[!sel], y[!sel], labels = which(!sel), n = 1, plot = plot, ...)
        if(!length(ans)) break
        ans <- which(!sel)[ans]
        points(x[ans], y[ans], pch = pch)
        sel[ans] <- TRUE
      }
      ## return indices of selected points
      which(sel)
    }

    x <- res.ind$coord[,'Dim.1']
    y <- res.ind$coord[,'Dim.2']
    df <- data.frame(x=x, y=y)
    df$cop_col <- "grey"
    rownames(df) <- rownames(res.ind$coor)

    plot(y ~ x, data=df,ylim = ylim,xlim = ylim)
    abline(v=0)
    abline(h=0)
    id_point <- identifyPch(x=df$x,y=df$y)
    rownames(df)[id_point]

#end----