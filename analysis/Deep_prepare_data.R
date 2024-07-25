###################################################################################################
#' Split the survey elo data to train the deep
#'
#'This script 
#'  - prepare data to run deep on beleza data
#'  - note that In general, putting 70% of the data in the training set, 15% in the validation set, 
#'    and 15% in the test set is a good split to start with
#'    #https://www.v7labs.com/blog/train-validation-test-set
#'  
#'Produces :
#'  - txt files in the results/deep/ folder to train the deep algo
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#'         
#'
#' @date 2023/06/21 first created
##################################################################################################

  beleza_all <- read.table(here::here('results','elo_deep_beleza.txt'), header = FALSE, sep = ",", dec = ".")
  
  beleza_train <- beleza_all[sample(1:dim(beleza_all)[1],round(dim(beleza_all)[1]*0.7)),]
  beleza_left <- beleza_all[!beleza_all$V1%in%beleza_train$V1,]
  beleza_test <- beleza_left[sample(1:dim(beleza_left)[1],round(dim(beleza_left)[1]*0.5)),]
  beleza_val <- beleza_left[!beleza_left$V1%in%beleza_test$V1,]
  
  write.table(beleza_train,here::here("results","deep","beleza_train.txt"),row.names = F,col.names = F,quote = FALSE,sep=",")
  write.table(beleza_test,here::here("results","deep","beleza_test.txt"),row.names = F,col.names = F,quote = FALSE,sep=",")
  write.table(beleza_val,here::here("results","deep","beleza_val.txt"),row.names = F,col.names = F,quote = FALSE,sep=",")

