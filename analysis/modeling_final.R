################# MODELING AESTHETICS ####################

#Here we going to model the effect of the landscape and of image composition on the Aesthetics values of 420 images of google street view evaluated by 3k volunteers

# we build a full model for the landscape and for the image features and run a dredge separated for each group of variables. 

#on sequence we check from colinearity and clean the model

#we run a Moran's I test to check spatial correlation

#we build a model considering the spatial component 

#finally we build a model considering both types of variables togheter (from the front and from above), and plot all the results

#Additionally we run a model and perform a tukey test to see the effect of Local Climate Zones on the aesthetics values

########################################################

############ PACKAGES #########

library(visreg)
library(DHARMa)
library(Matrix)
library(bbmle)
library(lme4)
library(effects)
library(Matrix)
library(MASS)
library(ggplot2)
library(visreg)
library(lattice)
library(survival)
library(MuMIn)
library(car)
library(visreg)
library(merTools)
library(spaMM)
library(GGally)
library(performance)
library(lmerTest)



getwd()

setwd("G:/Meu Drive/PESQUISA - Ecologia Urbana e Serviços Ecossistêmicos/Doutorado/R_Projects/Aesthetics_Cap1")
    

    #### Load and edit data ####
    dataBase <- read.csv("dataBase_FINAL.csv", sep = ",", row.names = NULL)
    
    # check it
    head(dataBase)
    str(dataBase)

    score <- as.numeric(dataBase$Aesthe) #response variable
    hist(score, breaks = 15)
    
    
    
    ###### Plot histogram of Score Aesthetics - response variable######
    x11()
    ggplot(data.frame("Aesthetics" = score),(aes(x= Aesthetics )))+
      geom_histogram(aes(y=..count..), fill = 'lightgray', color = "darkgray")+
      geom_density(aes(y=..density..*(420*50)), color = "darkgray", linewidth = 1.5)+
      labs(y='Frequency')+
      theme_bw()+
      theme(panel.grid = element_blank(), panel.border = element_blank(), panel.background = element_blank())
    
    
    ######defining landscape variables#####
    
    #compositiom
    prop_edif <- scale(dataBase$prop_edif)
    prop_tree <- scale(dataBase$X1_pland)
    prop_open_veg <- scale(dataBase$X2_pland)
    prop_total_veg <- scale(dataBase$X4_pland)
    
    #configuration
    lpi_veg_tot <- scale(dataBase$X4_lpi)
    ed_veg_tot <- scale(dataBase$X4_ed)
    np_veg_tot <- scale(dataBase$X4_np)
    lpi_tree <- scale(dataBase$X1_lpi)
    ed_tree <- scale(dataBase$X1_ed)
    np_tree <- scale(dataBase$X1_np)
    lpi_edif <- scale(dataBase$X5_lpi)
    ed_edif <- scale(dataBase$X5_ed)
    np_edif <- scale(dataBase$X5_np)
    
    #height and volume
    alt_veg_mean <- scale(dataBase$arv_mean)
    vol_tree <- scale(dataBase$vol_arv_su/dataBase$area)
    alt_edif_mean <- scale(dataBase$edif_mean)
    vol_edif <- scale(dataBase$vol_edif/dataBase$area)
    
    
    #Control/random
    apo <- dataBase$area_pondera
    distrito <- dataBase$distrito
    LCZ <- as.factor(dataBase$LCZ)
    
    
    #building a data frame with the variables
    data_landscape <- data.frame( prop_edif, 
                                  prop_tree ,
                                  prop_open_veg,
                                  prop_total_veg,
                                  lpi_veg_tot,
                                  ed_veg_tot,
                                  np_veg_tot,
                                  lpi_tree,
                                  ed_tree,
                                  np_tree,
                                  lpi_edif,
                                  ed_edif,
                                  np_edif,
                                  alt_veg_mean,
                                  vol_tree,
                                  alt_edif_mean,
                                  vol_edif)
    
    #checking correlation and distribution
    
    x11()
    ggpairs(data_landscape)
    
    #transforming some variables in log scale
    #Composition
    prop_tree <- scale(log1p((dataBase$X1_pland)))
    prop_open_veg <- scale(log1p((dataBase$X2_pland)))
    prop_total_veg <- scale(log1p(dataBase$X4_pland))
    
    #configuration
    lpi_veg_tot <- scale(log1p(dataBase$X4_lpi))
    np_veg_tot <- scale(log1p(dataBase$X4_np))
    lpi_tree <- scale(log1p(dataBase$X1_lpi))
    np_tree <- scale(log1p(dataBase$X1_np))
    
    #height and volume
    vol_tree <- scale(log1p(dataBase$vol_arv_su/dataBase$area))
    alt_edif_mean <- scale(log1p(dataBase$edif_mean))
    vol_edif <- scale(log1p(dataBase$vol_edif/dataBase$area))

    ######DREDGE 1 - ABOVE/LANDSCPE######
    
    library(MuMIn)

    #creates the full model
    full_model_landscape <- lm(score ~ prop_edif+ 
                                       prop_tree +
                                       prop_open_veg+
                                       prop_total_veg+
                                       lpi_veg_tot+
                                       ed_veg_tot+
                                       np_veg_tot+
                                       lpi_tree+
                                       ed_tree+
                                       np_tree+
                                       lpi_edif+
                                       ed_edif+
                                       np_edif+
                                       alt_veg_mean+
                                       vol_tree+
                                       alt_edif_mean+
                                       vol_edif)
    
    #check the full model
    summary(full_model_landscape)
    
    #run_dredge_method
    options(na.action = "na.pass")
    candidate_models <- dredge(full_model_landscape)
    summary(candidate_models)
    
    subset(candidate_models,delta<2)
    
    candidate_models_sub <- subset(candidate_models,delta<2)
    
    ###Plots the dredge with weight
    x11()
    par(mar = c(3,5,6,4))
    plot(candidate_models_sub, labAsExpr = T)
    
    #'Best' model
    summary(get.models(candidate_models, 1)[[1]])
    
    BEST_MODEL <- (get.models(candidate_models, 1)[[1]])
    performance::performance(BEST_MODEL)
    
    
    
    #Verifies colinearity of the best model
    vif(BEST_MODEL)
    
    #there is colinearity, than we decide to exclude the variables by interpretation of the biological value/explanation and correlation
    
    #Checking correlation:
    
    data_best_model <- data.frame(alt_veg_mean,
                                      ed_edif,
                                      ed_tree,
                                      lpi_edif,
                                      lpi_veg_tot,
                                      np_edif,
                                      np_tree,
                                      prop_edif,
                                      vol_edif,
                                      vol_tree)
    
    x11()
    ggpairs(data_best_model)
    
    #eliminate lpi_veg_tot, alt_veg_mean, ed_tree, and lpi_edif
    
    #testing the new model
    
    BEST_MODEL_CLEAN <- lm(score~ ed_edif+
                                  np_edif+
                                  np_tree+
                                  vol_edif+
                                  prop_edif+
                                  vol_tree)
    
    summary(BEST_MODEL_CLEAN)
    
    #verifying colinearity
    vif(BEST_MODEL_CLEAN)
    
    options(na.action = "na.pass")
    
    ######## BEST MODEL LANDSCAPE WITH RANDOM EFFECT ########
    
    BEST_MODEL1 <- lmer(score~ed_edif+
                          np_edif+
                          np_tree+
                          vol_edif+
                          prop_edif+
                          vol_tree+(1|LCZ))

    #verifying parameters of the model
    summary(BEST_MODEL1)
    vif(BEST_MODEL1)
    performance::performance(BEST_MODEL1)
    
    
    ################## MORANS TEST I for BEST_MODEL_1############
    
    ## MORAN's I test
    
    model_residuals <- residuals(BEST_MODEL1)
    
    dd <- dist(dataBase[,c('X', 'Y')])
    testSpatialAutocorrelation(model_residuals, x=dataBase$X, y=dataBase$Y, plot = F) #there is spatial autocorrelation
    
    
    ################ GIVEN THE SPATIAL AUTOCORRELATION, THE MODEL NEEDS TO TAKE IT INTO ACCOUNT ###############
    
    library(spaMM)
    library(mgcv)  # Ensure that mgcv package is loaded
    
    BEST_MODEL_SPACE1 <- fitme(score ~ ed_edif + 
                                        np_edif + 
                                        np_tree + 
                                        vol_edif +  
                                        prop_edif +  
                                        vol_tree +
                                        Matern(1|X+Y) +
                                        (1|LCZ), 
                                        data = dataBase)
    
    
    
    #summary of the model
    summary(BEST_MODEL_SPACE1)
    
    check_p = as.data.frame(summary(BEST_MODEL_SPACE1)$beta_table)
    check_p$p.value = round(2*pt(-abs(check_p$`t-value`), df=length(dataBase)-1),4)
    print(check_p)
    
    
    ####################################################################################################################
    
    
    ### defining some image features variables

    heterogenity <- scale(dataBase$Color_heterogeneity)
    complexity <- scale(dataBase$Complexity)
    saturation <- scale(dataBase$Color_saturation)
    brightness <- scale(dataBase$Brightness)
    PCA_1 <- scale(dataBase$PCA_col_1)
    PCA_2 <- scale(dataBase$PCA_col_2)
    PCA_3 <- scale(dataBase$PCA_col_3)
    contrast <- scale(dataBase$Contrast)
    fractal <- scale(dataBase$Self_similarity)
    green_fr <- scale(dataBase$Green_fraction)
    gray_fr <- scale(dataBase$Gray_fraction)
    blue_fr <- scale(dataBase$Blue_fraction)
    
    
    #checking correlation and distribution of variables
    
    
    data_front_features <- data.frame(  heterogenity ,
                                        complexity ,
                                        saturation ,
                                        brightness ,
                                        PCA_1 ,
                                        PCA_2 ,
                                        PCA_3 ,
                                        contrast ,
                                        fractal ,
                                        green_fr ,
                                        gray_fr ,
                                        blue_fr )
    
    x11()
    ggpairs(data_front_features)
    
    
    ######DREDGE 2 IMAGE FEATURES/FRONT######
    library(MuMIn)
    
    #creates full model
    
    model_full_features <- lm(score~heterogenity+
                                complexity+
                                saturation+
                                brightness+
                                PCA_1+
                                PCA_2+
                                PCA_3+
                                contrast+
                                fractal+
                                green_fr+
                                gray_fr+
                                blue_fr)
    
    summary(model_full_features)
    
    
    #run_dredge_method
    options(na.action = "na.pass")
    candidate_models2 <- dredge(model_full_features)
    summary(candidate_models)
    
    candidate_models2_sub <-  subset(candidate_models2,delta<5)
    
    x11()
    par(mar = c(3,5,6,4))
    plot(candidate_models2_sub, labAsExpr = F)
    
    #'Best' model
    BEST_MODEL_FRONT <- summary(get.models(candidate_models2, 1)[[1]])
    
    options(na.action = "na.omit")
    
    ############## THE BEST MODEL FOR PICTURES FEATURES
    
    BEST_MODEL_FRONT1 <- lm(score~blue_fr+
                                  brightness+
                                  complexity+
                                  green_fr+
                                  heterogenity+
                                  PCA_3)

    # verifies colinearity
    vif(BEST_MODEL_FRONT1)  #there is NO colinearity
    
    #check ccorrelation between variables
    data_best_model_front <- data.frame(blue_fr,
                                          brightness,
                                          complexity,
                                          green_fr,
                                          heterogenity,
                                          PCA_3)
    x11()
    ggpairs(data_best_model_front) 
    
    
    
    ### there is correlation of "green" with other two variables - cut of "green"
    
    ### CREATE THE MODEL COM RANDOM EFFECT
    
    BEST_MODEL_FRONT1 <- lmer(score~blue_fr+
                              brightness+
                              complexity+
                              heterogenity+
                              PCA_3+(1|LCZ))
    
    #check model parameters]
    summary(BEST_MODEL_FRONT1)
    vif(BEST_MODEL_FRONT1)    
    performance::performance(BEST_MODEL_FRONT1) 
    

    ################## MORANS TEST I for BEST_MODEL_FRONT############
    
    ## MORAN's I test
    
    model_residuals <- residuals(BEST_MODEL_FRONT1)
    
    dd <- dist(dataBase[,c('X', 'Y')])
    testSpatialAutocorrelation(model_residuals, x=dataBase$X, y=dataBase$Y, plot = F) #there is spatial autocorrelation
    
    ################ GIVEN THE SPATIAL AUTOCORRELATION, THE MODEL NEEDS TO TAKE IT INTO ACCOUNT ###############
    
    library(spaMM)
    library(mgcv)  # Ensure that mgcv package is loaded
    
    BEST_MODEL_SPACE_FRONT <- fitme(score ~ blue_fr +
                                 brightness+
                                 complexity+
                                 heterogenity+
                                 PCA_3+
                                  Matern(1|X+Y) +
                                  (1|LCZ), 
                                  data = dataBase)
    
    #summary of the model
    summary(BEST_MODEL_SPACE_FRONT)

    check_p = as.data.frame(summary(BEST_MODEL_SPACE_FRONT)$beta_table)
    check_p$p.value = round(2*pt(-abs(check_p$`t-value`), df=length(dataBase)-1),10)
    print(check_p)
   
    ######################################################################################################################
    
    ###### BEST MODEL COMBINED WITH EFFECT OF SPACE ########
    
    COMBINED_MODEL_FINAL <- fitme(score ~ blue_fr +
                           brightness+
                           complexity+
                           heterogenity+
                           PCA_3+
                           ed_edif + 
                           np_edif + 
                           np_tree + 
                           vol_edif +  
                           prop_edif +  
                           vol_tree +
                            Matern(1|X+Y) +
                            (1|LCZ), 
                           data = dataBase )
    
    summary(COMBINED_MODEL_FINAL)
    performance::performance(COMBINED_MODEL_FINAL) 
    
    check_p = as.data.frame(summary(COMBINED_MODEL_FINAL)$beta_table)
    check_p$p.value = round(2*pt(-abs(check_p$`t-value`), df=length(dataBase)-1),10)
    print(check_p)
    
    vif(COMBINED_MODEL_FINAL)
    
    ##############################################################################################
    
    ###### GRAPHS ####
    
    #### LANDSCAPE/ABOVE ####
    summary(BEST_MODEL_SPACE1)
    coef_summary <- summary(BEST_MODEL_SPACE1)
    
    # Extract coefficients and standard errors
    coefficients <- coef_summary$beta_table[, c("Estimate", "Cond. SE")]
    
    # Exclude the intercept row from coefficients dataframe
    coefficients <- coefficients[-1, ]
    
    # Create a data frame for plotting
    data_to_plot <- data.frame(
      Predictor = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      SE = coefficients[, "Cond. SE"]
    )
    
    
    predictor_names1 <- c(vol_edif = 'Buildings volume', 
                          prop_edif = 'Buildings coverage(%)',
                          np_tree = 'N.P. Trees', 
                          np_edif = 'N.P. buildings',
                          ed_edif = 'E.D. buildings', 
                          vol_tree = "Tree volume")
    
    X11()
    # Create the scatterplot
    library(magrittr); library(dplyr)
    graph1 <- data_to_plot %>%
      mutate(Predictor = forcats::fct_reorder(Predictor, Estimate, .desc = F, .fun = mean)) %>%
      mutate(Predictor = plyr::revalue(Predictor, predictor_names1)) %>% 
      ggplot(aes(x = Estimate, y = Predictor)) +
      geom_point(size = 4, color = "#55C667FF") +  # Change point color to green
      geom_errorbarh(aes(xmin = Estimate - 2*SE, xmax = Estimate + 2*SE),
                     height = .1, color = "#55C667FF") +  # Change error bar color to green
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Add a vertical line at 0
      labs(x = "", y = "") +
      ggtitle("A)") + theme_minimal() + theme(plot.title = element_text(hjust = 0))+
      theme(plot.title = element_text(hjust = 0, size = 20)) +
      theme(
        text = element_text(size = 25),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()
      ) ;graph1
        
  
    
    #### FEATURES/FRONT ######
    summary(BEST_MODEL_SPACE_FRONT)
    coef_summary <- summary(BEST_MODEL_SPACE_FRONT)
    
    # Extract coefficients and standard errors
    coefficients <- coef_summary$beta_table[, c("Estimate", "Cond. SE")]
    
    # Exclude the intercept row from coefficients dataframe
    coefficients <- coefficients[-1, ]
    
    # Create a data frame for plotting
    data_to_plot <- data.frame(
      Predictor = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      SE = coefficients[, "Cond. SE"]
    )
    
    
    predictor_names2 <- c( blue_fr = "Blue fraction", 
                           brightness = "Brightness", 
                           complexity = "Complexity", 
                           green = "Green fraction", 
                           heterogenity = "Color heterogenity" , 
                           PCA_3 = "PCA3")
    
    X11()
    
    # Create the scatterplot
    library(magrittr); library(dplyr)
    graph2 <- data_to_plot %>%
      mutate(Predictor = forcats::fct_reorder(Predictor, Estimate, .desc = F, .fun = mean)) %>%
      mutate(Predictor = plyr::revalue(Predictor, predictor_names2)) %>% 
      ggplot(aes(x = Estimate, y = Predictor)) +
      geom_point(size = 4, color = "#440154") +  # Change point color to purple
      geom_errorbarh(aes(xmin = Estimate - 2*SE, xmax = Estimate + 2*SE), height = 0.1, color = "#440154") +  # Change error bar color to green
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Add a vertical line at 0
      labs(x = "Coefficient Estimate", y = "") +
      ggtitle("B)") + theme_minimal() + theme(plot.title = element_text(hjust = 0))+
      theme(plot.title = element_text(hjust = 0, size = 20)) +
      theme(
        text = element_text(size = 25),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank()
      ) ;graph2
    
    
    #### COMBINED FRONT+ABOVE  ######
    summary(COMBINED_MODEL_FINAL)
    coef_summary <- summary(COMBINED_MODEL_FINAL)
    
    # Extract coefficients and standard errors
    coefficients <- coef_summary$beta_table[, c("Estimate", "Cond. SE")]
    
    
    # Exclude the intercept row from coefficients dataframe
    coefficients <- coefficients[-1, ]
    
    # Create a data frame for plotting
    data_to_plot3 <- data.frame(
      Predictor = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      SE = coefficients[, "Cond. SE"]
    )
    
    
    predictor_names3 <- c( blue_fr = "Blue fraction", 
                           brightness = "Brightness", 
                           complexity = "Complexity", 
                           heterogenity = "Color heterogenity" , 
                           PCA_3 = "PCA3",
                           vol_edif = 'Buildings volume', 
                           prop_edif = 'Buildings coverage(%)',
                           np_tree = 'N.P. trees', 
                           np_edif = 'N.P. buildings',
                           ed_edif = 'E.D. buildings', 
                           vol_tree = "Tree volume")
    
    
    colors_cat1 <- c(vol_edif = 'Above', prop_edif = 'Above',
                    np_tree = 'Above', ed_tree = 'Above', np_edif = 'Above',
                    ed_edif = 'Above', vol_tree = 'Above', blue_fr = 'Front', brightness = 'Front', 
                    complexity = 'Front', green = 'Front', heterogenity = 'Front' , PCA_3 = 'Front')

    
    x11()
    graph3 <- data_to_plot3 %>% 
      mutate(Predictor = forcats::fct_relevel(Predictor, 
                                              c())) %>%
      left_join(data.frame(Colors=colors_cat1, Predictor = names(colors_cat1)))%>%
      mutate(Predictor = forcats::fct_reorder(Predictor, (Estimate), .desc = F, .fun = mean)) %>%
      mutate(Predictor = plyr::revalue(Predictor, predictor_names3)) %>% 
      ggplot(aes(x = Estimate, y = Predictor, color = Colors)) +
      geom_point(size =4) +  # Add points
      geom_errorbarh(aes(xmin = Estimate - 2*SE, 
                         xmax = Estimate + 2*SE),
                     height = 0.1) +  # Add error bars (2 * SE)
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Add vertical line at 0
      scale_color_manual("Variables", values = c("#55C667FF", "#440154"))+
      #scale_color_viridis_d('View', end = 0.8, direction = -1)+
      labs(x = "Coefficient Estimate", y = "Predictor") +
      ggtitle("C)") + theme_minimal() +  theme(plot.title = element_text(hjust = 0))+
      theme(plot.title = element_text(hjust = 0, size = 20)) +
      theme(text = element_text(size = 25),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank())
    
    graph3   
    
   
    ###### FINAL IMAGE
    
    x11()
    graph1+graph2+graph3 + theme( axis.title.y = element_blank()) +
      patchwork::plot_layout(design = 'AC\nBC', guides = "collect") &
      theme(text = element_text(size = 18),
            axis.title.x = element_text(size = 16),
            legend.title = element_text(size = 16),
            legend.position = "top")  
    
    
    ##################### MODEL LCZ  
    apo <- dataBase$area_pondera
    distrito <- dataBase$distrito
    LCZ <- as.factor(dataBase$LCZ)
    LCZ
  
    MODEL_LCZ <- lmer(score~LCZ + (1|apo))
    summary(MODEL_LCZ)
    performance::performance(MODEL_LCZ)
    
    x11()
    plot(predictorEffects(MODEL_LCZ))
    
    #encontrando valores de P
    
    # Obtain the summary of the model
    model_summary <- summary(MODEL_LCZ)
    
    # Extract coefficients and standard errors
    coefficients <- coef(model_summary)
    se <- coefficients[, "Std. Error"]
    
    # Calculate Wald test statistics
    wald_test_stats <- coefficients[, "Estimate"] / se
    
    # Calculate two-tailed p-values based on the Wald test statistics
    p_values <- 2 * (1 - pnorm(abs(wald_test_stats)))
    
    # Create a data frame to display results
    results3 <- data.frame(
      Variable = rownames(coefficients),
      Estimate = coefficients[, "Estimate"],
      Std_Error = se,
      Wald_Statistic = wald_test_stats,
      P_Value = p_values
    )
    
    
    # Display the results
    print(results3)
    
    
    ### Fazer plot ordenado dos boxplots de acordo com as categorias de LCZ
    
    # Create a named vector to map original LCZ levels to new names
    level_names <- c("1" = "High-compact (1)",
                     "2" = "Medium-compact (2)",
                     "3" = "Low-compact (3)",
                     "4" = "High-open (4)",
                     "5" = "Medium-open (5)",
                     "6" = "Low-open (6)",
                     "8" = "Low-big (8)")
    
    MODEL_LCZ@frame %>%
      mutate(LCZ = forcats::fct_relevel(LCZ, as.character(rev(c(5,1,6,4,2,3,8))))) %>%
      mutate(LCZ = plyr::revalue(LCZ, level_names)) %>%
      ggplot(aes(y = score, x = LCZ, group = LCZ)) +
      geom_boxplot() +
      geom_point(alpha = .3, position = position_jitter(height = 0, width = 0.02)) +
      theme_bw() + 
      theme(panel.grid = element_blank(), text = element_text(size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(y = "Aesthetic", x = "LCZ")
    
    # Check which apo is the most central
    Ds <- visreg(MODEL_LCZ, data =( MODEL_LCZ@frame %>%  
                                      mutate(LCZ = forcats::fct_relevel(LCZ, as.character(rev(c(5,6,1,4,2,3,8)))))))
    
    Ds$res %>%
      mutate(LCZ = plyr::revalue(LCZ, level_names)) %>%
      ggplot(aes(x = LCZ, y = visregRes)) +
      geom_violin() +
      
      geom_point(alpha = .3, position = position_jitter(height = 0, width = 0.02)) +
      theme_bw() + 
      theme(panel.grid = element_blank(), text = element_text(size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(y = "Aesthetic", x = "LCZ")
    
    Ds$fit$apo
    
    BPplot <- expand.grid(LCZ = unique(MODEL_LCZ@frame$LCZ), apo = 1)
    
    BPplot <- data.frame(BPplot, 
                         merTools::predictInterval(MODEL_LCZ, BPplot, which = "fixed",
                                                   n.sims = 10000, stat = "mean",
                                                   level = .3))
    
    head(BPplot)
    x11()
    BPplot %>%
      mutate(LCZ = forcats::fct_relevel(LCZ, as.character(rev(c(5,6,1,4,2,3,8))))) %>%
      mutate(LCZ = plyr::revalue(LCZ, level_names)) %>%
      ggplot(aes(x = LCZ, y = fit, group = LCZ)) +
      geom_point(size = 3) +
      geom_errorbar(aes(y = fit, ymax = upr, ymin = lwr), width = 0.1) +
      theme_bw() + theme(panel.grid = element_blank(),
                         text = element_text(size = 16),
                         axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(y = "Score")
    
    
    
########################### TUKEY TEST ###################
    
    
    # Perform ANOVA
    model <- aov(score ~ as.factor(LCZ), data = dataBase)
    
    # Display ANOVA results
    summary(model)
    
    # Perform Tukey's HSD test
    tukey_results <- TukeyHSD(model)
    
    # Display Tukey's HSD results
    print(tukey_results)
    
    
    