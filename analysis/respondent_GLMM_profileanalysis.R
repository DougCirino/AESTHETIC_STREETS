# =============================================================================
# GLMM - RESPONDENT SOCIOECONOMIC EFFECTS
# Script only for recovering statistics requested by the reviewer
# =============================================================================

library(dplyr)
library(lme4)
library(car)


# -----------------------------------------------------------------------------
# 1. FILE PATHS
# -----------------------------------------------------------------------------

data_folder <- "G:/Meu Drive/PESQUISA - Ecologia Urbana e Serviços Ecossistêmicos/Doutorado/Dados/Extrapolation/data"

respondents_file <- file.path(
  data_folder,
  "data_respondents_sub.csv"
)

matches_file <- file.path(
  data_folder,
  "matches_all.csv"
)


# -----------------------------------------------------------------------------
# 2. READ DATA
# -----------------------------------------------------------------------------

respondents <- read.csv2(respondents_file)
matches_all <- read.csv2(matches_file)

respondents <- respondents %>%
  rename(judge_id = ID_Judge)


# -----------------------------------------------------------------------------
# 3. RESPONDENT VARIABLES
# -----------------------------------------------------------------------------

respondent_variables <- c(
  "Gender",
  "Age_class",
  "Education",
  "Social_class",
  "Grew_up",
  "Live_in",
  "Ethnicity"
)

respondents_sub <- respondents %>%
  select(judge_id, all_of(respondent_variables)) %>%
  na.omit()


# Keep only judgments from respondents with complete socioeconomic information
matches_all <- matches_all[
  matches_all$judge_id %in% respondents_sub$judge_id,
]


# Merge judgments + respondent characteristics
data_matches_judge <- merge(
  matches_all,
  respondents_sub,
  by = "judge_id",
  all.x = TRUE
)


# -----------------------------------------------------------------------------
# 4. CONVERT VARIABLES TO FACTORS
# -----------------------------------------------------------------------------

data_matches_judge$Gender       <- as.factor(data_matches_judge$Gender)
data_matches_judge$Age_class    <- as.factor(data_matches_judge$Age_class)
data_matches_judge$Education    <- as.factor(data_matches_judge$Education)
data_matches_judge$Social_class <- as.factor(data_matches_judge$Social_class)
data_matches_judge$Grew_up      <- as.factor(data_matches_judge$Grew_up)
data_matches_judge$Live_in      <- as.factor(data_matches_judge$Live_in)
data_matches_judge$Ethnicity    <- as.factor(data_matches_judge$Ethnicity)

data_matches_judge <- droplevels(data_matches_judge)


# Quick sample information
cat("\nNumber of respondents included:",
    length(unique(data_matches_judge$judge_id)), "\n")

cat("Number of pairwise judgments included:",
    nrow(data_matches_judge), "\n\n")


# -----------------------------------------------------------------------------
# 5. ORIGINAL BINOMIAL GLMM
# -----------------------------------------------------------------------------

test_model <- glmer(
  outcome ~ Gender +
    Age_class +
    Education +
    Social_class +
    Grew_up +
    Live_in +
    Ethnicity +
    (1 | challenger_1),
  family = binomial,
  data = data_matches_judge,
  na.action = na.fail
)

summary(test_model)


# =============================================================================
# TABLE 1 - OMNIBUS TEST FOR EACH RESPONDENT CHARACTERISTIC
# =============================================================================

omnibus <- car::Anova(
  test_model,
  type = 2
)

omnibus_table <- data.frame(
  variable = rownames(omnibus),
  omnibus,
  row.names = NULL
)

print(omnibus_table)

write.csv2(
  omnibus_table,
  file.path(
    data_folder,
    "Table_S_GLMM_omnibus_tests.csv"
  ),
  row.names = FALSE
)


# =============================================================================
# TABLE 2 - COEFFICIENTS + 95% CI + ODDS RATIOS + MDE
# =============================================================================

# Coefficients
coef_table <- as.data.frame(
  coef(summary(test_model))
)

coef_table$term <- rownames(coef_table)
rownames(coef_table) <- NULL

names(coef_table)[1:4] <- c(
  "estimate_log_odds",
  "std_error",
  "z_value",
  "p_value"
)


# -----------------------------------------------------------------------------
# 95% Wald confidence intervals
# -----------------------------------------------------------------------------

ci_table <- as.data.frame(
  confint(
    test_model,
    parm = "beta_",
    method = "Wald"
  )
)

ci_table$term <- rownames(ci_table)
rownames(ci_table) <- NULL

names(ci_table)[1:2] <- c(
  "CI95_lower",
  "CI95_upper"
)


# Join coefficient estimates and confidence intervals
coef_table <- merge(
  coef_table,
  ci_table,
  by = "term",
  all.x = TRUE,
  sort = FALSE
)


# -----------------------------------------------------------------------------
# Odds ratios
# -----------------------------------------------------------------------------

coef_table$odds_ratio <- exp(
  coef_table$estimate_log_odds
)

coef_table$OR_CI95_lower <- exp(
  coef_table$CI95_lower
)

coef_table$OR_CI95_upper <- exp(
  coef_table$CI95_upper
)


# -----------------------------------------------------------------------------
# Approximate minimum detectable effect
# alpha = 0.05
# power = 80%
# Wald approximation based on observed SE
# -----------------------------------------------------------------------------

z_alpha <- qnorm(0.975)
z_power <- qnorm(0.80)

coef_table$MDE_log_odds_80power <-
  (z_alpha + z_power) * coef_table$std_error

coef_table$MDE_OR_lower_80power <-
  exp(-coef_table$MDE_log_odds_80power)

coef_table$MDE_OR_upper_80power <-
  exp(coef_table$MDE_log_odds_80power)


# Remove intercept
coef_table <- coef_table[
  coef_table$term != "(Intercept)",
]


# Organize table
coef_table <- coef_table[, c(
  "term",
  "estimate_log_odds",
  "std_error",
  "CI95_lower",
  "CI95_upper",
  "z_value",
  "p_value",
  "odds_ratio",
  "OR_CI95_lower",
  "OR_CI95_upper",
  "MDE_log_odds_80power",
  "MDE_OR_lower_80power",
  "MDE_OR_upper_80power"
)]


# Round for supplementary material
numeric_columns <- sapply(coef_table, is.numeric)

coef_table[numeric_columns] <-
  lapply(
    coef_table[numeric_columns],
    round,
    digits = 4
  )


print(coef_table)


# Save
write.csv2(
  coef_table,
  file.path(
    data_folder,
    "Table_S_GLMM_coefficients_CI_MDE.csv"
  ),
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# DONE
# -----------------------------------------------------------------------------

cat("\nDONE.\n")
cat("Saved:\n")
cat("1. Table_S_GLMM_omnibus_tests.csv\n")
cat("2. Table_S_GLMM_coefficients_CI_MDE.csv\n")