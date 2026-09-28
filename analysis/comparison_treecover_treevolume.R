# ============================================================
# DIRECT TEST: TREE COVER vs TREE VOLUME
# 
#
# Models:
#   1. Tree cover only
#   2. Tree volume only
#   3. Tree cover + tree volume
#
# Same spatial structure used in the manuscript:
# Matern spatial effect + LCZ random effect
# ============================================================


# ------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------

library(spaMM)


# ------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------

setwd(
  "G:/Meu Drive/PESQUISA - Ecologia Urbana e Serviços Ecossistêmicos/Doutorado/R_Projects/Aesthetics_Cap1"
)

dataBase <- read.csv(
  "dataBase_FINAL.csv",
  sep = ",",
  row.names = NULL
)


# ------------------------------------------------------------
# VARIABLES
# Same transformations used in the original analysis
# ------------------------------------------------------------

score <- as.numeric(dataBase$Aesthe)

# Tree cover
prop_tree <- as.numeric(
  scale(
    log1p(dataBase$X1_pland)
  )
)

# Tree volume
vol_tree <- as.numeric(
  scale(
    log1p(dataBase$vol_arv_su / dataBase$area)
  )
)

# Spatial coordinates and LCZ
X <- dataBase$X
Y <- dataBase$Y
LCZ <- as.factor(dataBase$LCZ)


# ------------------------------------------------------------
# CREATE COMMON DATASET
# All three models use EXACTLY the same observations
# ------------------------------------------------------------

comparison_data <- data.frame(
  score = score,
  prop_tree = prop_tree,
  vol_tree = vol_tree,
  X = X,
  Y = Y,
  LCZ = LCZ
)

comparison_data <- comparison_data[
  complete.cases(comparison_data),
]

cat("\nNumber of observations used in all models:",
    nrow(comparison_data), "\n")


# ============================================================
# MODEL 1
# TREE COVER ONLY
# ============================================================

model_cover <- fitme(
  score ~
    prop_tree +
    Matern(1 | X + Y) +
    (1 | LCZ),
  data = comparison_data,
  family = gaussian(),
  method = "ML"
)


# ============================================================
# MODEL 2
# TREE VOLUME ONLY
# ============================================================

model_volume <- fitme(
  score ~
    vol_tree +
    Matern(1 | X + Y) +
    (1 | LCZ),
  data = comparison_data,
  family = gaussian(),
  method = "ML"
)


# ============================================================
# MODEL 3
# TREE COVER + TREE VOLUME
# ============================================================

model_both <- fitme(
  score ~
    prop_tree +
    vol_tree +
    Matern(1 | X + Y) +
    (1 | LCZ),
  data = comparison_data,
  family = gaussian(),
  method = "ML"
)


# ============================================================
# AIC
#
# extractAIC() returns:
# [1] effective df
# [2] marginal AIC
#
# We want ONLY element [2].
# ============================================================

AIC_cover <- unname(
  extractAIC(model_cover)[2]
)

AIC_volume <- unname(
  extractAIC(model_volume)[2]
)

AIC_both <- unname(
  extractAIC(model_both)[2]
)


cat("\n========================================\n")
cat("RAW AIC VALUES\n")
cat("========================================\n")

cat("Tree cover only:", AIC_cover, "\n")
cat("Tree volume only:", AIC_volume, "\n")
cat("Tree cover + volume:", AIC_both, "\n")


# ============================================================
# PSEUDO-R2
#
# R2 = 1 - SSE/SST
#
# Calculated using fitted values from each spatial model.
# ============================================================

pseudo_R2 <- function(model, observed) {
  
  predicted <- as.numeric(
    fitted(model)
  )
  
  SSE <- sum(
    (observed - predicted)^2
  )
  
  SST <- sum(
    (observed - mean(observed))^2
  )
  
  1 - (SSE / SST)
}


R2_cover <- pseudo_R2(
  model_cover,
  comparison_data$score
)

R2_volume <- pseudo_R2(
  model_volume,
  comparison_data$score
)

R2_both <- pseudo_R2(
  model_both,
  comparison_data$score
)


# ============================================================
# FINAL COMPARISON TABLE
# ============================================================

comparison_results <- data.frame(
  
  Model = c(
    "Tree cover only",
    "Tree volume only",
    "Tree cover + tree volume"
  ),
  
  AIC = c(
    AIC_cover,
    AIC_volume,
    AIC_both
  ),
  
  R2 = c(
    R2_cover,
    R2_volume,
    R2_both
  )
)


# Calculate Delta AIC
comparison_results$Delta_AIC <-
  comparison_results$AIC -
  min(comparison_results$AIC)


# Order models from LOWEST to HIGHEST AIC
comparison_results <-
  comparison_results[
    order(comparison_results$AIC),
  ]


# Round only for presentation
comparison_results$AIC <-
  round(comparison_results$AIC, 2)

comparison_results$Delta_AIC <-
  round(comparison_results$Delta_AIC, 2)

comparison_results$R2 <-
  round(comparison_results$R2, 3)

rownames(comparison_results) <- NULL


cat("\n========================================\n")
cat("FINAL MODEL COMPARISON\n")
cat("========================================\n")

print(comparison_results)


# ============================================================
# COEFFICIENTS
# Important particularly for the model containing BOTH
# ============================================================

cat("\n========================================\n")
cat("TREE COVER ONLY - COEFFICIENTS\n")
cat("========================================\n")

print(
  summary(model_cover)$beta_table
)


cat("\n========================================\n")
cat("TREE VOLUME ONLY - COEFFICIENTS\n")
cat("========================================\n")

print(
  summary(model_volume)$beta_table
)


cat("\n========================================\n")
cat("TREE COVER + TREE VOLUME - COEFFICIENTS\n")
cat("========================================\n")

print(
  summary(model_both)$beta_table
)


# ============================================================
# SIMPLE CORRELATION BETWEEN COVER AND VOLUME
# Descriptive only
# ============================================================

cover_volume_correlation <- cor(
  comparison_data$prop_tree,
  comparison_data$vol_tree,
  method = "pearson"
)

cat("\n========================================\n")
cat("TREE COVER vs TREE VOLUME CORRELATION\n")
cat("========================================\n")

cat(
  "Pearson r =",
  round(cover_volume_correlation, 3),
  "\n"
)
########################################

# ============================================================
# TREE COVER vs TREE VOLUME
# Correlation + VIF
# ============================================================

library(car)

# Pearson correlation
cor_test_cover_volume <- cor.test(
  comparison_data$prop_tree,
  comparison_data$vol_tree,
  method = "pearson"
)

cat("\n=============================\n")
cat("TREE COVER vs TREE VOLUME\n")
cat("=============================\n")
cat(
  "Pearson r =",
  round(unname(cor_test_cover_volume$estimate), 3),
  "\n"
)
cat(
  "p-value =",
  format.pval(cor_test_cover_volume$p.value, digits = 3),
  "\n"
)


# VIF for model containing both predictors
vif_model <- lm(
  score ~ prop_tree + vol_tree,
  data = comparison_data
)

cat("\n=============================\n")
cat("VIF: COVER + VOLUME\n")
cat("=============================\n")

print(car::vif(vif_model))

# ============================================================
# SAVE FINAL RESULTS
# ============================================================

write.csv(
  comparison_results,
  "Tree_cover_vs_volume_model_comparison_FINAL.csv",
  row.names = FALSE
)


# ============================================================
# END
# ============================================================

library(SDMTools)
