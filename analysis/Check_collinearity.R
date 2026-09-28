# ============================================================
# MINIMAL CHECK OF MULTICOLLINEARITY
# 
# ============================================================

library(car)

setwd("G:/Meu Drive/PESQUISA - Ecologia Urbana e Serviços Ecossistêmicos/Doutorado/R_Projects/Aesthetics_Cap1")

dataBase <- read.csv("dataBase_FINAL.csv", sep = ",", row.names = NULL)

score <- as.numeric(dataBase$Aesthe)


# ============================================================
# 1. LANDSCAPE VARIABLES
# Variables retained by the original AIC/dredge procedure
# BEFORE collinearity exclusions
# ============================================================

alt_veg_mean <- as.numeric(scale(dataBase$arv_mean))

ed_edif <- as.numeric(scale(dataBase$X5_ed))
ed_tree <- as.numeric(scale(dataBase$X1_ed))

lpi_edif <- as.numeric(scale(dataBase$X5_lpi))
lpi_veg_tot <- as.numeric(scale(log1p(dataBase$X4_lpi)))

np_edif <- as.numeric(scale(dataBase$X5_np))
np_tree <- as.numeric(scale(log1p(dataBase$X1_np)))

prop_edif <- as.numeric(scale(dataBase$prop_edif))

vol_edif <- as.numeric(
  scale(log1p(dataBase$vol_edif / dataBase$area))
)

vol_tree <- as.numeric(
  scale(log1p(dataBase$vol_arv_su / dataBase$area))
)


# Model selected by dredge BEFORE cleaning for collinearity
landscape_before_cleaning <- lm(
  score ~ alt_veg_mean +
    ed_edif +
    ed_tree +
    lpi_edif +
    lpi_veg_tot +
    np_edif +
    np_tree +
    prop_edif +
    vol_edif +
    vol_tree
)

cat("\n========================================\n")
cat("LANDSCAPE MODEL - VIF BEFORE CLEANING\n")
cat("========================================\n")

vif_landscape_before <- car::vif(landscape_before_cleaning)
print(vif_landscape_before)


# Correlation matrix of the AIC-selected predictors
landscape_selected_variables <- data.frame(
  alt_veg_mean,
  ed_edif,
  ed_tree,
  lpi_edif,
  lpi_veg_tot,
  np_edif,
  np_tree,
  prop_edif,
  vol_edif,
  vol_tree
)

cor_landscape <- cor(
  landscape_selected_variables,
  use = "pairwise.complete.obs",
  method = "pearson"
)

cat("\n========================================\n")
cat("LANDSCAPE CORRELATIONS |r| > 0.60\n")
cat("========================================\n")

cor_pairs_landscape <- which(
  abs(cor_landscape) > 0.60 &
    abs(cor_landscape) < 1,
  arr.ind = TRUE
)

cor_pairs_landscape <- data.frame(
  variable_1 = rownames(cor_landscape)[cor_pairs_landscape[, 1]],
  variable_2 = colnames(cor_landscape)[cor_pairs_landscape[, 2]],
  r = cor_landscape[cor_pairs_landscape]
)

# Remove duplicated A-B / B-A pairs
cor_pairs_landscape <- cor_pairs_landscape[
  !duplicated(
    apply(
      cor_pairs_landscape[, c("variable_1", "variable_2")],
      1,
      function(x) paste(sort(x), collapse = "_")
    )
  ),
]

print(cor_pairs_landscape)


# ============================================================
# 2. FINAL CLEAN LANDSCAPE MODEL
# Variables actually retained in the manuscript
# ============================================================

landscape_after_cleaning <- lm(
  score ~ ed_edif +
    np_edif +
    np_tree +
    vol_edif +
    prop_edif +
    vol_tree
)

cat("\n========================================\n")
cat("LANDSCAPE MODEL - VIF AFTER CLEANING\n")
cat("========================================\n")

vif_landscape_after <- car::vif(landscape_after_cleaning)
print(vif_landscape_after)


# ============================================================
# 3. FRONT VARIABLES
# AIC-selected model BEFORE removal of green fraction
# ============================================================

blue_fr <- as.numeric(scale(dataBase$Blue_fraction))
brightness <- as.numeric(scale(dataBase$Brightness))
complexity <- as.numeric(scale(dataBase$Complexity))
green_fr <- as.numeric(scale(dataBase$Green_fraction))
heterogenity <- as.numeric(scale(dataBase$Color_heterogeneity))
PCA_3 <- as.numeric(scale(dataBase$PCA_col_3))


front_before_cleaning <- lm(
  score ~ blue_fr +
    brightness +
    complexity +
    green_fr +
    heterogenity +
    PCA_3
)

cat("\n========================================\n")
cat("FRONT MODEL - VIF BEFORE CLEANING\n")
cat("========================================\n")

vif_front_before <- car::vif(front_before_cleaning)
print(vif_front_before)


front_selected_variables <- data.frame(
  blue_fr,
  brightness,
  complexity,
  green_fr,
  heterogenity,
  PCA_3
)

cor_front <- cor(
  front_selected_variables,
  use = "pairwise.complete.obs",
  method = "pearson"
)

cat("\n========================================\n")
cat("FRONT CORRELATIONS |r| > 0.60\n")
cat("========================================\n")

cor_pairs_front <- which(
  abs(cor_front) > 0.60 &
    abs(cor_front) < 1,
  arr.ind = TRUE
)

cor_pairs_front <- data.frame(
  variable_1 = rownames(cor_front)[cor_pairs_front[, 1]],
  variable_2 = colnames(cor_front)[cor_pairs_front[, 2]],
  r = cor_front[cor_pairs_front]
)

cor_pairs_front <- cor_pairs_front[
  !duplicated(
    apply(
      cor_pairs_front[, c("variable_1", "variable_2")],
      1,
      function(x) paste(sort(x), collapse = "_")
    )
  ),
]

print(cor_pairs_front)


# ============================================================
# 4. SAVE MATRICES FOR SUPPLEMENT
# ============================================================

write.csv(
  round(cor_landscape, 3),
  "Correlation_matrix_landscape_selected.csv"
)

write.csv(
  round(cor_front, 3),
  "Correlation_matrix_front_selected.csv"
)

write.csv(
  data.frame(
    variable = names(vif_landscape_before),
    VIF = as.numeric(vif_landscape_before)
  ),
  "VIF_landscape_before_cleaning.csv",
  row.names = FALSE
)

write.csv(
  data.frame(
    variable = names(vif_landscape_after),
    VIF = as.numeric(vif_landscape_after)
  ),
  "VIF_landscape_after_cleaning.csv",
  row.names = FALSE
)