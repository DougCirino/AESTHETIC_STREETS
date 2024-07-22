###################################################################################################
#' Respondents profiles & effects 
#'
#'This script 
#'  - format the raw data for the respondents
#'  - produce simple plots on the respondents profiles 
#'  - test the effects of the respondents profiles 
#'  - produce plots to illustrate some respondent effects
#'
#'
#'Produces - data_respondents.csv with the formatted respondents profiles 
#'         - table_S1.csv contain the results of the glmer test on the respondents profiles
#'         - Fig S1 on the distribution of respondents profiles
#'         - Figures in table_figures/respondents/ to illustrate some respondent effects
#'
#' @author Nicolas Mouquet, \email{nicolas.mouquet@@cnrs.fr},
#' @editing Douglas Cirino, \email{douglas.cirino@@usp.br}         
#'
#' @date 2023/05/16 first created
##################################################################################################


#----

#PLOT the respondent profiles RAW DATA Fig S1 -----
library(forcats)

respondents <- read.csv2(here::here("data","data_respondents_sub.csv"))

textsize=3
axissize=12
ticks=10
hjust=-0.2
perct=0.2


library(ggplot2)
##time serie
times <- as.data.frame(table(respondents$Time))
colnames(times) <- c("date","respondents")
times$date <- as.Date(times$date)
ts <- ggplot(data=times, aes(x = date, y = respondents)) +
  geom_col(fill="#74BDD6")+
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))
ggsave(file=here::here("figures_tables","Fig_time.tiff"), ts,width = 12, height = 8, dpi = 200, units = "cm", device='tiff') 

##gender 
##https://cran.r-project.org/web/packages/treemapify/vignettes/introduction-to-treemapify.html  
data_sub <- table(respondents$Gender)
data_sub <- data.frame(Gender=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")

data_sub$Gender <- as.factor(data_sub$Gender)
data_sub$Gender <- factor(data_sub$Gender,levels=c("Male","Female","Other"))

Gender <- ggplot(data_sub, aes(value,Gender)) +
  geom_col(aes(fill=Gender),show.legend = FALSE) + 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlim(0,max(data_sub[,2])+perct*max(data_sub[,2]))+
  xlab("# individuals")+ ylab("Gender")+
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)

##Ethnicity 
data_sub <- table(respondents$Ethnicity)
data_sub <- data.frame(Ethnicity=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")

data_sub$Ethnicity <- as.factor(data_sub$Ethnicity)

Ethnicity_plot <- ggplot(data_sub, aes(value,Ethnicity)) +
  geom_col(aes(fill=Ethnicity),show.legend = FALSE) + 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlim(0,max(data_sub[,2])+perct*max(data_sub[,2]))+
  xlab("# individuals")+ ylab("Ethnicity")+
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)




##Color_blind 
data_sub <- table(respondents$Color_blind)
data_sub <- data.frame(Color_blind=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")
data_sub$Color_blind <- as.factor(data_sub$Color_blind)
data_sub$Color_blind <- factor(data_sub$Color_blind, levels=c("Yes", "No"))

Color_blind <- ggplot(data_sub, aes(value,Color_blind)) +
  geom_col(aes(fill=Color_blind),show.legend = FALSE) + 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlim(0,max(data_sub[,2])+perct*max(data_sub[,2]))+
  xlab("# individuals")+ ylab("Color_blind")+
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)

##Age 
data_sub <- respondents
data_sub$Gender <- as.factor(data_sub$Gender)
data_sub$Gender <- factor(data_sub$Gender,levels=c("Male","Female","Other"))

xmax <- max(table(cut(data_sub$Age, breaks=seq(0,100,5), right = FALSE)))+10

Age <- ggplot(data_sub, aes(y=Age, color=Gender, fill=Gender)) +
  geom_histogram(alpha=0.6, binwidth = 5)+
  scale_y_continuous(breaks = seq(0,100,10), labels = seq(0,100, 10))+
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlim(0,xmax)+
  xlab("# individuals")+
  theme(legend.position = c(0.8, 0.75),legend.title=element_text(size=axissize), 
        legend.text=element_text(size=ticks))

##Age_class
data_sub <- table(respondents$Age_class)
data_sub <- data.frame(Age_class=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")
data_sub$Age_class <- as.factor(data_sub$Age_class)
data_sub$Age_class <- factor(data_sub$Age_class, levels=c("18_29","30_59","60_100"))

Age_class <- ggplot(data_sub, aes(value,Age_class)) +
  geom_col(aes(fill=Age_class),show.legend = FALSE) + 
  xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlab("# individuals")+ 
  ylab("Age_class")+ 
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)

##Education
data_sub <- table(respondents$Education)
data_sub <- data.frame(Education=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")
data_sub$Education <- as.factor(data_sub$Education)
data_sub$Education <- factor(data_sub$Education, levels=c("Elementary school","High school", "Bachelor","Master","PhD"))

Education <- ggplot(data_sub, aes(value,Education)) +
  geom_col(aes(fill=Education),show.legend = FALSE) + 
  xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlab("# individuals")+ 
  ylab("Education")+ 
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)

##Grew_up
data_sub <- table(respondents$Grew_up)
data_sub <- data.frame(Grew_up=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")
data_sub$Grew_up <- as.factor(data_sub$Grew_up)
data_sub$Grew_up <- factor(data_sub$Grew_up, levels=c("Rural place","Village", "Small city","Median city","Metropolis"))

Grew_up <- ggplot(data_sub, aes(value,Grew_up)) +
  geom_col(aes(fill=Grew_up),show.legend = FALSE) + 
  xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlab("# individuals")+ 
  ylab("Grew_up")+ 
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)

##Live_in
data_sub <- table(respondents$Live_in)
data_sub <- data.frame(Live_in=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")
data_sub$Live_in <- as.factor(data_sub$Live_in)
data_sub$Live_in <- factor(data_sub$Live_in, levels=c("Rural place","Village", "Small city","Median city","Metropolis"))

Live_in <- ggplot(data_sub, aes(value,Live_in)) +
  geom_col(aes(fill=Live_in),show.legend = FALSE) + 
  xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlab("# individuals")+ 
  ylab("Live_in")+ 
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)


##Social_class
data_sub <- table(respondents$Social_class)
data_sub <- data.frame(Social_class=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")
data_sub$Social_class <- as.factor(data_sub$Social_class)
data_sub$Social_class <- factor(data_sub$Social_class, levels=c("Low","Low-middle", "Middle","Upper-middle","Upper"))

Social_class <- ggplot(data_sub, aes(value,Social_class)) +
  geom_col(aes(fill=Social_class),show.legend = FALSE) + 
  xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlab("# individuals")+ 
  ylab("Social_class")+ 
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)

##Countries

data_sub <- table(respondents$Country)
data_sub <- data_sub[order(data_sub,decreasing = T)]
data_sub <- data.frame(Country=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")
data_sub$Country <- as.factor(data_sub$Country)
data_sub <- data_sub[1:8,]
data_sub$Country <- factor(data_sub$Country, levels=rev(as.character(data_sub$Country)))

Country <- ggplot(data_sub, aes(value,Country)) +
  geom_col(aes(fill=Country),show.legend = FALSE) + 
  xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlab("# individuals")+ 
  ylab("Country")+ 
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)

##Survey
data_sub <- table(respondents$Survey)
data_sub <- data.frame(Survey=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")
data_sub$Survey <- as.factor(data_sub$Survey)
data_sub$Survey <- factor(data_sub$Survey, levels=c("ES","EN","BR"))

Survey <- ggplot(data_sub, aes(value,Survey)) +
  geom_col(aes(fill=Survey),show.legend = FALSE) + 
  xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlab("# individuals")+ 
  ylab("Survey")+ 
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)

##Save_the plot 

library(gridExtra)
g <- arrangeGrob(Gender,Age,
                 Education,Social_class,
                 Ethnicity_plot,
                 Grew_up,Live_in,ncol=3) #generates g
ggsave(file=here::here("figures_tables","FigS1.1.tiff"), g,width = 35, height = 25, dpi = 200, units = "cm", device='tiff') 

#----

#PLOT the respondent profiles SUBSET DATA Fig S1_sub -----
library(forcats)

respondents <- read.csv2(here::here("data","data_respondents_sub.csv"))

textsize=3
axissize=12
ticks=10
hjust=-0.2
perct=0.2

##gender 
##https://cran.r-project.org/web/packages/treemapify/vignettes/introduction-to-treemapify.html  
data_sub <- table(respondents$Gender)
data_sub <- data.frame(Gender=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")

data_sub$Gender <- as.factor(data_sub$Gender)
data_sub$Gender <- factor(data_sub$Gender,levels=c("Male","Female","Other"))

Gender <- ggplot(data_sub, aes(value,Gender)) +
  geom_col(aes(fill=Gender),show.legend = FALSE) + 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlim(0,max(data_sub[,2])+perct*max(data_sub[,2]))+
  xlab("# individuals")+ ylab("Gender")+
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)

##Age_class
data_sub <- table(respondents$Age_class)
data_sub <- data.frame(Age_class=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")
data_sub$Age_class <- as.factor(data_sub$Age_class)
data_sub$Age_class <- factor(data_sub$Age_class, levels=c("18_24","25_59","60_100"))

Age_class <- ggplot(data_sub, aes(value,Age_class)) +
  geom_col(aes(fill=Age_class),show.legend = FALSE) + 
  xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlab("# individuals")+ 
  ylab("Age_class")+ 
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)

##Education
data_sub <- table(respondents$Education)
data_sub <- data.frame(Education=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")
data_sub$Education <- as.factor(data_sub$Education)
data_sub$Education <- factor(data_sub$Education, levels=c("High school", "Bachelor","Master","PhD"))

Education <- ggplot(data_sub, aes(value,Education)) +
  geom_col(aes(fill=Education),show.legend = FALSE) + 
  xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlab("# individuals")+ 
  ylab("Education")+ 
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)

##Grew_up
data_sub <- table(respondents$Grew_up)
data_sub <- data.frame(Grew_up=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")
data_sub$Grew_up <- as.factor(data_sub$Grew_up)
data_sub$Grew_up <- factor(data_sub$Grew_up, levels=c("Small city","Median city","Metropolis"))

Grew_up <- ggplot(data_sub, aes(value,Grew_up)) +
  geom_col(aes(fill=Grew_up),show.legend = FALSE) + 
  xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlab("# individuals")+ 
  ylab("Grew_up")+ 
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)

##Live_in
data_sub <- table(respondents$Live_in)
data_sub <- data.frame(Live_in=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")
data_sub$Live_in <- as.factor(data_sub$Live_in)
data_sub$Live_in <- factor(data_sub$Live_in, levels=c("Small city","Median city","Metropolis"))

Live_in <- ggplot(data_sub, aes(value,Live_in)) +
  geom_col(aes(fill=Live_in),show.legend = FALSE) + 
  xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlab("# individuals")+ 
  ylab("Live_in")+ 
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)


##Social_class
data_sub <- table(respondents$Social_class)
data_sub <- data.frame(Social_class=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")
data_sub$Social_class <- as.factor(data_sub$Social_class)
data_sub$Social_class <- factor(data_sub$Social_class, levels=c("Low","Low-middle", "Middle","Upper-middle","Upper"))

Social_class <- ggplot(data_sub, aes(value,Social_class)) +
  geom_col(aes(fill=Social_class),show.legend = FALSE) + 
  xlim(0,max(data_sub[,2])+(perct+0.02)*max(data_sub[,2]))+ 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlab("# individuals")+ 
  ylab("Social_class")+ 
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize)


##Ethnicity 
data_sub <- table(respondents$Ethnicity)
data_sub <- data.frame(Ethnicity=names(data_sub),value=as.vector(data_sub))
data_sub$per <- round((data_sub$value/sum(data_sub$value))*100,digit=1)
data_sub$per <- paste(data_sub$per,"%")
data_sub$Ethnicity <- as.factor(data_sub$Ethnicity)
data_sub$Ethnicity <- factor(data_sub$Ethnicity, levels=c("White","Non-white","NA"))

data_sub$Ethnicity <- as.factor(data_sub$Ethnicity)

Ethnicity_plot <- ggplot(data_sub, aes(value,Ethnicity)) +
  geom_col(aes(fill=Ethnicity),show.legend = FALSE) + 
  theme_bw()+
  theme(axis.text = element_text(size=ticks),axis.title=element_text(size=axissize))+
  xlim(0,max(data_sub[,2])+perct*max(data_sub[,2]))+
  xlab("# individuals")+ ylab("Ethnicity")+
  geom_text(aes(label = per),position=position_dodge(0.9),hjust=hjust,size=textsize) 


##Save_the plot 

library(gridExtra)
g <- arrangeGrob(Gender,Age_class,
                 Education,Grew_up,Live_in,Social_class,Ethnicity_plot,ncol=3) #generates g

plot(g)
ggsave(file=here::here("figures_tables","FigS1_sub2.tiff"), g,width = 35, height = 25, dpi = 200, units = "cm", device='tiff') 

#----

#TEST the respondent effects on matches outcomes GLMER Table S2----
#produce the Table S2 

# Install and load dplyr
library(dplyr)

# Read the data
matches_all <- read.csv2(here::here("data", "matches_all.csv"))
respondents <- read.csv2(here::here("data", "data_respondents_sub.csv"))

# Rename the column using dplyr
respondents <- respondents %>% rename(judge_id = ID_Judge)

# Select the desired columns and remove rows with missing values
list_var <- c("Gender", "Age_class", "Education", "Social_class", "Grew_up", "Live_in", "Ethnicity")
respondents_sub <- respondents %>% select(judge_id, all_of(list_var)) %>% na.omit()

##Keep only the judges for which we have intel in data_judges
matches_all <- matches_all[matches_all$judge_id %in% unique(respondents_sub$judge_id),]

##Merge with the data_judges and save 

data_matches_judge <- merge(matches_all, respondents_sub, by = "judge_id",all.x = T,all.y=F)
write.csv2(data_matches_judge,here::here('data','data_matches_judge.csv'),row.names = F)


##Variables that will be included in the test 

data_matches_judge$Gender <- as.factor(data_matches_judge$Gender)
data_matches_judge$Age_class <- as.factor(data_matches_judge$Age_class)
data_matches_judge$Education <- as.factor(data_matches_judge$Education)
data_matches_judge$Social_class <- as.factor(data_matches_judge$Social_class)
data_matches_judge$Grew_up <- as.factor(data_matches_judge$Grew_up)
data_matches_judge$Live_in <- as.factor(data_matches_judge$Live_in)
data_matches_judge$Ethnicity <- as.factor(data_matches_judge$Ethnicity)

data_matches_judge <- droplevels(data_matches_judge)

#run the test #WARNING set the blas_set_num_threads to your cores (parallel::detectCores() gives you this max)
whatvar=list_var
RhpcBLASctl::blas_set_num_threads(10) # set the total number of proc used by the BLAS (within the glmer function of the lme4 package) make sure to keep some proc free :) 
test_model <- lme4::glmer(as.formula(paste("outcome ~ ",paste(whatvar,collapse = "+"),"+ (1 | challenger_1)")), family = binomial,
                          data = data_matches_judge, na.action = na.fail)
summary(test_model)
restest <- car::Anova(test_model)

output <- data.frame(restest)
output <- cbind.data.frame(variables=rownames(output),output)
output$signi <- unlist(lapply(output$Pr..Chisq., signi))
output
#save the table 
write.csv2(output,here::here("figures_tables","table_S1.csv"),row.names = F)

#----


#PLOT some examples of respondents effects FIG X----
#run the Elo on some subsamples to illustrate the test of the 
#respondents effects 
#########WARNING! THE PROCESSING CAN TAKE TIME###########

matches_all <- read.csv2(here::here("data","matches_all.csv"))
respondents <- read.csv2(here::here("data","data_respondents_sub.csv"))
names(respondents)[names(respondents) == "ID_Judge"] <- "judge_id"
list_var <- c("Gender","Age_class","Education","Social_class","Grew_up","Live_in","Ethnicity")

respondents_sub <- respondents[,c("judge_id",list_var)]
respondents_sub <- respondents_sub[complete.cases(respondents_sub),]


##Keep only the judges for which we have intel in data_judges
matches_all <- matches_all[matches_all$judge_id %in% unique(respondents_sub$judge_id),]

##Merge with the data_judges

data_matches_judge <- merge(matches_all, respondents_sub, by = "judge_id",all.x = T,all.y=F)

##Generic : run the the Elo and compare two groups

eloruns <- 50
mc.cores=12

allgroups <- list(c("18_24","25_59"),c("18_24","60_100"),
                  c("25_59","60_100"),c("Female","Male"),c("Middle","Low"),c("Middle","Upper"),
                  c("Upper","Low"),c("Metropolis","Small city"),c("Metropolis","Median city"),
                  c("Metropolis","Small city"),c("Metropolis","Median city"),c("PhD","High school"),c("PhD","Bachelor"),
                  c("PhD","Master"),c("Master","High school"),c("White", "Non-white"))
whats <- c("Age_class","Age_class","Age_class","Gender","Social_class","Social_class","Social_class","Grew_up",
           "Grew_up","Live_in","Live_in","Education","Education","Education","Education","Ethnicity")



library(imager)

for (i in 1:length(allgroups)){
  
  #i=5
  groups <- allgroups[[i]]
  what <- whats[i]
  cat("Group ",what,": ",paste(groups,collapse = " vs. "),"----","\n")
  cat(" Computing Elos ...\n")
  
  group_elos <- do.call(merge,lapply(groups, function(id){
    #id <- groups[1]
    matches <- data_matches_judge[data_matches_judge[,what]%in%id,]
    matches$Loser <- NA
    matches$Loser[matches$outcome==1]=matches$challenger_2[matches$outcome==1]
    matches$Loser[matches$outcome==0]=matches$challenger_1[matches$outcome==0]
    
    res_elo    <- EloChoice::elochoice(winner = matches$Winner, loser = matches$Loser,
                                       startvalue = 1500, runs = eloruns)
    scores <- cbind.data.frame(Idmages=names(EloChoice::ratings(res_elo, show="mean",drawplot = F)),
                               mean=EloChoice::ratings(res_elo, show="mean",drawplot = F),
                               var=EloChoice::ratings(res_elo, show="var",drawplot = T))
    rm(res_elo)
    scores[,3] <- sqrt(scores[,3])
    colnames(scores) <- c("Id_images",paste0("Elo_",id),paste0("Elo_sd_",id)) 
    scores
  }))
  
  # ggplot(group_elos, aes(x=group_elos[,paste0("Elo_",groups[1])], y=group_elos[,paste0("Elo_",groups[2])])) +
  #   geom_point()+
  #   geom_abline(slope=1, intercept = 0,linetype=3,color="gray")+
  #   geom_smooth(method=lm , color="gray", fill="#ecebeb", se=TRUE,alpha = 0.5,size=0.5) + xlim(900, 2000) + ylim(900, 2000) +
  #   xlab(paste0("Aesthetic (",groups[1]," years old)"))+ylab(paste0("Aesthetic (",groups[2]," years old)"))+
  #   theme_bw()
  
  cat(" Plotting ... \n")
  
  df <- data.frame(x = group_elos[,paste0("Elo_",groups[1])],
                   y = group_elos[,paste0("Elo_",groups[2])],
                   names=group_elos[,"Id_images"])
  
  tiff(here::here("figures_tables","Respondents",paste0("Resp_",groups[1],"_",groups[2],".tiff")), width = 1500, height = 1500)
  plot.2d.lm_labels(scale=0.05,size = -50L,coord <- df,pathphoto <- here::here("data","BIG_FILES","images","png"),
                    xR=min(df$x),yR=max(df$y),colR="black",labelx=paste0("Aesthetic ",what," (",groups[1],")"),
                    labely=paste0("Aesthetic ",what," (",groups[2],")"),cexlab=3.5,cexaxis=2,
                    colline="#325395",lwline=8,df=df)
  dev.off()
  
  cat("\n")
  
}

#----