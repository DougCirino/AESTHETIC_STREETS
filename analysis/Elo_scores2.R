
###################################################################################################
#' Survey main dataset and Elo scores computation
#'
#' This script compute the Elo scores 
#'
#' Produces :
#'          - elo_scores.csv
#'          - Poster_24.jpg
#'          - Poster_8.jpg
#'          - Poster_all.jpg
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         
#'
#' @date 2023/05/17 first created
##################################################################################################

##############################################################

matches_all <- read.csv2(here::here("data","matches_all.csv"))

##Compute Elo scores (for final ELOs better to set eloruns to 1000)

eloruns=1000
mc.cores=12

matches_all$Loser <- NA
matches_all$Loser[matches_all$outcome==1]=matches_all$challenger_2[matches_all$outcome==1]
matches_all$Loser[matches_all$outcome==0]=matches_all$challenger_1[matches_all$outcome==0]

#calculate the mean number of matches per images (449)

images_list <- unique(matches_all$challenger_1)

match_dat <- do.call(rbind,pbmcapply::pbmclapply(images_list, function(id){
  data.frame(images=id,nb_matche=dim(matches_all[matches_all$challenger_1==id,])[1])
},mc.cores = mc.cores))

meanmatchs <- round(mean(match_dat$nb_matche),2)

#computing Elos

res_elo    <- EloChoice::elochoice(winner = matches_all$Winner, loser = matches_all$Loser,
                                   startvalue = 1500, runs = eloruns)
elo_scores <- cbind.data.frame(Idmages=names(EloChoice::ratings(res_elo, show="mean",drawplot = F)),
                               mean=EloChoice::ratings(res_elo, show="mean",drawplot = F),
                               var=EloChoice::ratings(res_elo, show="var",drawplot = T))
rm(res_elo)
elo_scores[,3] <- sqrt(elo_scores[,3])
colnames(elo_scores) <- c("Id_images","Elo_mean_all","Elo_sd_all") 

write.csv2(elo_scores,here::here("results","elo_scores.csv"),row.names = F)
write.csv2(elo_scores,here::here("figures_tables",paste0("table_elos_",length(unique(matches_all$judge_id)),"_respondents",".csv")),row.names = F)

elo_deep_beleza <- elo_scores[1:2]
write.table(elo_deep_beleza,here::here("results","elo_deep_beleza.txt"),row.names = F,col.names = F,quote = FALSE,sep=",")
#----

#Posters ----

source(here::here('R','functions.R'))
elo_scores <- read.csv2(here::here("results","elo_scores.csv"))

#do the poster
order_id<-elo_scores[order(elo_scores$Elo_mean_all,decreasing = TRUE),"Id_images"]
outputImageFileName <- here::here("figures_tables","Poster_all.jpg")
poster(path_photo <- here::here("data","BIG_FILES","images","jpg_sq"),
       tab_image <-  paste0(order_id,".jpg"),
       nb_row <- 17,
       nb_col <- 25,
       outputImageFileName <- outputImageFileName,
       sizered=1
)

#do the poster for 24 
order_id<-elo_scores[order(elo_scores$Elo_mean_all,decreasing = TRUE),"Id_images"]
im_24 <- order_id[c(1:6,seq(14,400,round(400/12)),415:420)]
outputImageFileName <- here::here("figures_tables","Poster_24.jpg")
poster(path_photo <- here::here("data","BIG_FILES","images","jpg_sq"),
       tab_image <-  paste0(im_24,".jpg"),
       nb_row <- 4,
       nb_col <- 6,
       outputImageFileName <- outputImageFileName,
       sizered=1
)

#do the poster for 8 
order_id<-elo_scores[order(elo_scores$Elo_mean_all,decreasing = TRUE),"Id_images"]
im_8 <- order_id[c(1:2,seq(90,419,round(420/5)),419:420)]
outputImageFileName <- here::here("figures_tables","Poster_8.jpg")
poster(path_photo <- here::here("data","BIG_FILES","images","jpg_sq"),
       tab_image <-  paste0(im_8,".jpg"),
       nb_row <- 4,
       nb_col <- 2,
       outputImageFileName <- outputImageFileName,
       sizered=1
)
#----
