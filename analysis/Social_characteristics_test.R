###################################################################################################
# RESPONDENTS PROFILES & EFFECTS
#
# This script:
#   - formats the raw data for respondents
#   - produces plots of respondent profiles
#   - tests the effects of respondent profiles using a binomial GLMM
#   - produces illustrative plots of respondent-group Elo scores
#   - exports supplementary tables requested during peer review
#
# Outputs:
#   - data/data_matches_judge.csv
#   - figures_tables/Fig_time.tiff
#   - figures_tables/FigS1.1.tiff
#   - figures_tables/FigS1_sub2.tiff
#   - figures_tables/Table_S_GLMM_omnibus_tests.csv
#   - figures_tables/Table_S_GLMM_coefficients_CI_MDE.csv
#   - figures_tables/Respondents/*.tiff
#
# Original author: Nicolas Mouquet
# Editing: Douglas Cirino
###################################################################################################


# ================================================================================================
# 0. SETUP
# ================================================================================================

library(here)
library(dplyr)
library(ggplot2)
library(forcats)
library(gridExtra)
library(lme4)
library(car)


# Create output directories if they do not exist
dir.create(
  here::here("figures_tables"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here::here("figures_tables", "Respondents"),
  recursive = TRUE,
  showWarnings = FALSE
)


# Significance-code helper
signi <- function(p) {
  ifelse(
    is.na(p), "",
    ifelse(
      p < 0.001, "***",
      ifelse(
        p < 0.01, "**",
        ifelse(p < 0.05, "*", "ns")
      )
    )
  )
}


# ================================================================================================
# 1. PLOT RESPONDENT PROFILES - RAW DATA
# ================================================================================================

respondents <- read.csv2(
  here::here("data", "data_respondents_sub.csv")
)

textsize <- 3
axissize <- 12
ticks <- 10
hjust <- -0.2
perct <- 0.2


# ------------------------------------------------------------------------------------------------
# Time series
# ------------------------------------------------------------------------------------------------

times <- as.data.frame(table(respondents$Time))
colnames(times) <- c("date", "respondents")
times$date <- as.Date(times$date)

ts <- ggplot(data = times, aes(x = date, y = respondents)) +
  geom_col(fill = "#74BDD6") +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  )

ggsave(
  file = here::here("figures_tables", "Fig_time.tiff"),
  ts,
  width = 12,
  height = 8,
  dpi = 200,
  units = "cm",
  device = "tiff"
)


# ------------------------------------------------------------------------------------------------
# Gender
# ------------------------------------------------------------------------------------------------

data_sub <- table(respondents$Gender)
data_sub <- data.frame(
  Gender = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub$Gender <- factor(
  data_sub$Gender,
  levels = c("Male", "Female", "Other")
)

Gender <- ggplot(data_sub, aes(value, Gender)) +
  geom_col(aes(fill = Gender), show.legend = FALSE) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlim(0, max(data_sub[, 2]) + perct * max(data_sub[, 2])) +
  xlab("# individuals") +
  ylab("Gender") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# ------------------------------------------------------------------------------------------------
# Ethnicity
# ------------------------------------------------------------------------------------------------

data_sub <- table(respondents$Ethnicity)
data_sub <- data.frame(
  Ethnicity = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")
data_sub$Ethnicity <- as.factor(data_sub$Ethnicity)

Ethnicity_plot <- ggplot(data_sub, aes(value, Ethnicity)) +
  geom_col(aes(fill = Ethnicity), show.legend = FALSE) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlim(0, max(data_sub[, 2]) + perct * max(data_sub[, 2])) +
  xlab("# individuals") +
  ylab("Ethnicity") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# ------------------------------------------------------------------------------------------------
# Color blindness
# ------------------------------------------------------------------------------------------------

data_sub <- table(respondents$Color_blind)
data_sub <- data.frame(
  Color_blind = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub$Color_blind <- factor(
  data_sub$Color_blind,
  levels = c("Yes", "No")
)

Color_blind <- ggplot(data_sub, aes(value, Color_blind)) +
  geom_col(aes(fill = Color_blind), show.legend = FALSE) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlim(0, max(data_sub[, 2]) + perct * max(data_sub[, 2])) +
  xlab("# individuals") +
  ylab("Color_blind") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# ------------------------------------------------------------------------------------------------
# Age
# ------------------------------------------------------------------------------------------------

data_sub <- respondents

data_sub$Gender <- factor(
  data_sub$Gender,
  levels = c("Male", "Female", "Other")
)

xmax <- max(
  table(cut(
    data_sub$Age,
    breaks = seq(0, 100, 5),
    right = FALSE
  ))
) + 10

Age <- ggplot(
  data_sub,
  aes(y = Age, color = Gender, fill = Gender)
) +
  geom_histogram(alpha = 0.6, binwidth = 5) +
  scale_y_continuous(
    breaks = seq(0, 100, 10),
    labels = seq(0, 100, 10)
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize),
    legend.position = c(0.8, 0.75),
    legend.title = element_text(size = axissize),
    legend.text = element_text(size = ticks)
  ) +
  xlim(0, xmax) +
  xlab("# individuals")


# ------------------------------------------------------------------------------------------------
# Age class
# ------------------------------------------------------------------------------------------------

data_sub <- table(respondents$Age_class)

data_sub <- data.frame(
  Age_class = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub$Age_class <- factor(
  data_sub$Age_class,
  levels = c("18_29", "30_59", "60_100")
)

Age_class <- ggplot(data_sub, aes(value, Age_class)) +
  geom_col(aes(fill = Age_class), show.legend = FALSE) +
  xlim(
    0,
    max(data_sub[, 2]) +
      (perct + 0.02) * max(data_sub[, 2])
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlab("# individuals") +
  ylab("Age_class") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# ------------------------------------------------------------------------------------------------
# Education
# ------------------------------------------------------------------------------------------------

data_sub <- table(respondents$Education)

data_sub <- data.frame(
  Education = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub$Education <- factor(
  data_sub$Education,
  levels = c(
    "Elementary school",
    "High school",
    "Bachelor",
    "Master",
    "PhD"
  )
)

Education <- ggplot(data_sub, aes(value, Education)) +
  geom_col(aes(fill = Education), show.legend = FALSE) +
  xlim(
    0,
    max(data_sub[, 2]) +
      (perct + 0.02) * max(data_sub[, 2])
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlab("# individuals") +
  ylab("Education") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# ------------------------------------------------------------------------------------------------
# Grew up
# ------------------------------------------------------------------------------------------------

data_sub <- table(respondents$Grew_up)

data_sub <- data.frame(
  Grew_up = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub$Grew_up <- factor(
  data_sub$Grew_up,
  levels = c(
    "Rural place",
    "Village",
    "Small city",
    "Median city",
    "Metropolis"
  )
)

Grew_up <- ggplot(data_sub, aes(value, Grew_up)) +
  geom_col(aes(fill = Grew_up), show.legend = FALSE) +
  xlim(
    0,
    max(data_sub[, 2]) +
      (perct + 0.02) * max(data_sub[, 2])
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlab("# individuals") +
  ylab("Grew_up") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# ------------------------------------------------------------------------------------------------
# Live in
# ------------------------------------------------------------------------------------------------

data_sub <- table(respondents$Live_in)

data_sub <- data.frame(
  Live_in = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub$Live_in <- factor(
  data_sub$Live_in,
  levels = c(
    "Rural place",
    "Village",
    "Small city",
    "Median city",
    "Metropolis"
  )
)

Live_in <- ggplot(data_sub, aes(value, Live_in)) +
  geom_col(aes(fill = Live_in), show.legend = FALSE) +
  xlim(
    0,
    max(data_sub[, 2]) +
      (perct + 0.02) * max(data_sub[, 2])
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlab("# individuals") +
  ylab("Live_in") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# ------------------------------------------------------------------------------------------------
# Social class
# ------------------------------------------------------------------------------------------------

data_sub <- table(respondents$Social_class)

data_sub <- data.frame(
  Social_class = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub$Social_class <- factor(
  data_sub$Social_class,
  levels = c(
    "Low",
    "Low-middle",
    "Middle",
    "Upper-middle",
    "Upper"
  )
)

Social_class <- ggplot(data_sub, aes(value, Social_class)) +
  geom_col(aes(fill = Social_class), show.legend = FALSE) +
  xlim(
    0,
    max(data_sub[, 2]) +
      (perct + 0.02) * max(data_sub[, 2])
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlab("# individuals") +
  ylab("Social_class") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# ------------------------------------------------------------------------------------------------
# Countries
# ------------------------------------------------------------------------------------------------

data_sub <- table(respondents$Country)
data_sub <- data_sub[order(data_sub, decreasing = TRUE)]

data_sub <- data.frame(
  Country = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub <- data_sub[1:min(8, nrow(data_sub)), ]

data_sub$Country <- factor(
  data_sub$Country,
  levels = rev(as.character(data_sub$Country))
)

Country <- ggplot(data_sub, aes(value, Country)) +
  geom_col(aes(fill = Country), show.legend = FALSE) +
  xlim(
    0,
    max(data_sub[, 2]) +
      (perct + 0.02) * max(data_sub[, 2])
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlab("# individuals") +
  ylab("Country") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# ------------------------------------------------------------------------------------------------
# Survey
# ------------------------------------------------------------------------------------------------

data_sub <- table(respondents$Survey)

data_sub <- data.frame(
  Survey = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub$Survey <- factor(
  data_sub$Survey,
  levels = c("ES", "EN", "BR")
)

Survey <- ggplot(data_sub, aes(value, Survey)) +
  geom_col(aes(fill = Survey), show.legend = FALSE) +
  xlim(
    0,
    max(data_sub[, 2]) +
      (perct + 0.02) * max(data_sub[, 2])
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlab("# individuals") +
  ylab("Survey") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# ------------------------------------------------------------------------------------------------
# Save raw respondent profile figure
# ------------------------------------------------------------------------------------------------

g_raw <- arrangeGrob(
  Gender,
  Age,
  Education,
  Social_class,
  Ethnicity_plot,
  Grew_up,
  Live_in,
  ncol = 3
)

ggsave(
  file = here::here("figures_tables", "FigS1.1.tiff"),
  g_raw,
  width = 35,
  height = 25,
  dpi = 200,
  units = "cm",
  device = "tiff"
)


# ================================================================================================
# 2. PLOT RESPONDENT PROFILES - SUBSET USED IN ANALYSIS
# ================================================================================================

respondents <- read.csv2(
  here::here("data", "data_respondents_sub.csv")
)

textsize <- 3
axissize <- 12
ticks <- 10
hjust <- -0.2
perct <- 0.2


# Gender
data_sub <- table(respondents$Gender)

data_sub <- data.frame(
  Gender = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub$Gender <- factor(
  data_sub$Gender,
  levels = c("Male", "Female", "Other")
)

Gender <- ggplot(data_sub, aes(value, Gender)) +
  geom_col(aes(fill = Gender), show.legend = FALSE) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlim(0, max(data_sub[, 2]) + perct * max(data_sub[, 2])) +
  xlab("# individuals") +
  ylab("Gender") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# Age class
data_sub <- table(respondents$Age_class)

data_sub <- data.frame(
  Age_class = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub$Age_class <- factor(
  data_sub$Age_class,
  levels = c("18_24", "25_59", "60_100")
)

Age_class <- ggplot(data_sub, aes(value, Age_class)) +
  geom_col(aes(fill = Age_class), show.legend = FALSE) +
  xlim(
    0,
    max(data_sub[, 2]) +
      (perct + 0.02) * max(data_sub[, 2])
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlab("# individuals") +
  ylab("Age_class") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# Education
data_sub <- table(respondents$Education)

data_sub <- data.frame(
  Education = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub$Education <- factor(
  data_sub$Education,
  levels = c("High school", "Bachelor", "Master", "PhD")
)

Education <- ggplot(data_sub, aes(value, Education)) +
  geom_col(aes(fill = Education), show.legend = FALSE) +
  xlim(
    0,
    max(data_sub[, 2]) +
      (perct + 0.02) * max(data_sub[, 2])
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlab("# individuals") +
  ylab("Education") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# Grew up
data_sub <- table(respondents$Grew_up)

data_sub <- data.frame(
  Grew_up = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub$Grew_up <- factor(
  data_sub$Grew_up,
  levels = c("Small city", "Median city", "Metropolis")
)

Grew_up <- ggplot(data_sub, aes(value, Grew_up)) +
  geom_col(aes(fill = Grew_up), show.legend = FALSE) +
  xlim(
    0,
    max(data_sub[, 2]) +
      (perct + 0.02) * max(data_sub[, 2])
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlab("# individuals") +
  ylab("Grew_up") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# Live in
data_sub <- table(respondents$Live_in)

data_sub <- data.frame(
  Live_in = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub$Live_in <- factor(
  data_sub$Live_in,
  levels = c("Small city", "Median city", "Metropolis")
)

Live_in <- ggplot(data_sub, aes(value, Live_in)) +
  geom_col(aes(fill = Live_in), show.legend = FALSE) +
  xlim(
    0,
    max(data_sub[, 2]) +
      (perct + 0.02) * max(data_sub[, 2])
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlab("# individuals") +
  ylab("Live_in") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# Social class
data_sub <- table(respondents$Social_class)

data_sub <- data.frame(
  Social_class = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub$Social_class <- factor(
  data_sub$Social_class,
  levels = c(
    "Low",
    "Low-middle",
    "Middle",
    "Upper-middle",
    "Upper"
  )
)

Social_class <- ggplot(data_sub, aes(value, Social_class)) +
  geom_col(aes(fill = Social_class), show.legend = FALSE) +
  xlim(
    0,
    max(data_sub[, 2]) +
      (perct + 0.02) * max(data_sub[, 2])
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlab("# individuals") +
  ylab("Social_class") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# Ethnicity
data_sub <- table(respondents$Ethnicity)

data_sub <- data.frame(
  Ethnicity = names(data_sub),
  value = as.vector(data_sub)
)

data_sub$per <- round(
  (data_sub$value / sum(data_sub$value)) * 100,
  digits = 1
)

data_sub$per <- paste(data_sub$per, "%")

data_sub$Ethnicity <- factor(
  data_sub$Ethnicity,
  levels = c("White", "Non-white", "NA")
)

Ethnicity_plot <- ggplot(data_sub, aes(value, Ethnicity)) +
  geom_col(aes(fill = Ethnicity), show.legend = FALSE) +
  theme_bw() +
  theme(
    axis.text = element_text(size = ticks),
    axis.title = element_text(size = axissize)
  ) +
  xlim(0, max(data_sub[, 2]) + perct * max(data_sub[, 2])) +
  xlab("# individuals") +
  ylab("Ethnicity") +
  geom_text(
    aes(label = per),
    position = position_dodge(0.9),
    hjust = hjust,
    size = textsize
  )


# Save subset plot
g_subset <- arrangeGrob(
  Gender,
  Age_class,
  Education,
  Grew_up,
  Live_in,
  Social_class,
  Ethnicity_plot,
  ncol = 3
)

ggsave(
  file = here::here("figures_tables", "FigS1_sub2.tiff"),
  g_subset,
  width = 35,
  height = 25,
  dpi = 200,
  units = "cm",
  device = "tiff"
)


# ================================================================================================
# 3. TEST RESPONDENT EFFECTS ON PAIRWISE CHOICES - BINOMIAL GLMM
# ================================================================================================

matches_all <- read.csv2(
  here::here("data", "matches_all.csv")
)

respondents <- read.csv2(
  here::here("data", "data_respondents_sub.csv")
)


# Rename respondent ID
respondents <- respondents %>%
  rename(judge_id = ID_Judge)


# Variables included in the GLMM
list_var <- c(
  "Gender",
  "Age_class",
  "Education",
  "Social_class",
  "Grew_up",
  "Live_in",
  "Ethnicity"
)


# Keep respondents with complete information
respondents_sub <- respondents %>%
  select(judge_id, all_of(list_var)) %>%
  na.omit()


# Keep only pairwise comparisons from those respondents
matches_all <- matches_all[
  matches_all$judge_id %in% unique(respondents_sub$judge_id),
]


# Merge matches and respondent characteristics
data_matches_judge <- merge(
  matches_all,
  respondents_sub,
  by = "judge_id",
  all.x = TRUE,
  all.y = FALSE
)


# Save merged dataset
write.csv2(
  data_matches_judge,
  here::here("data", "data_matches_judge.csv"),
  row.names = FALSE
)


# Convert respondent variables to factors
data_matches_judge$Gender <- as.factor(data_matches_judge$Gender)
data_matches_judge$Age_class <- as.factor(data_matches_judge$Age_class)
data_matches_judge$Education <- as.factor(data_matches_judge$Education)
data_matches_judge$Social_class <- as.factor(data_matches_judge$Social_class)
data_matches_judge$Grew_up <- as.factor(data_matches_judge$Grew_up)
data_matches_judge$Live_in <- as.factor(data_matches_judge$Live_in)
data_matches_judge$Ethnicity <- as.factor(data_matches_judge$Ethnicity)

data_matches_judge <- droplevels(data_matches_judge)


# Optional BLAS parallelization
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  
  available_cores <- parallel::detectCores()
  
  n_threads <- max(
    1,
    min(10, available_cores - 1)
  )
  
  RhpcBLASctl::blas_set_num_threads(n_threads)
}


# Fit original binomial GLMM
test_model <- lme4::glmer(
  outcome ~
    Gender +
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


# Model summary
print(summary(test_model))


# ================================================================================================
# 4. SUPPLEMENTARY TABLE 1 - OMNIBUS TESTS OF RESPONDENT CHARACTERISTICS
# ================================================================================================

restest <- car::Anova(
  test_model,
  type = 2
)

output_omnibus <- data.frame(restest)

output_omnibus <- cbind.data.frame(
  variable = rownames(output_omnibus),
  output_omnibus
)

rownames(output_omnibus) <- NULL


# Add significance codes
p_column <- grep(
  "Pr",
  colnames(output_omnibus),
  value = TRUE
)[1]

output_omnibus$significance <- signi(
  output_omnibus[[p_column]]
)


print(output_omnibus)


# Save omnibus table
write.csv2(
  output_omnibus,
  here::here(
    "figures_tables",
    "Table_S_GLMM_omnibus_tests.csv"
  ),
  row.names = FALSE
)


# ================================================================================================
# 5. SUPPLEMENTARY TABLE 2 - COEFFICIENTS, 95% CI, OR AND MDE
# ================================================================================================

# Fixed effects
coef_glmm <- as.data.frame(
  coef(summary(test_model))
)

coef_glmm$term <- rownames(coef_glmm)
rownames(coef_glmm) <- NULL


# Rename coefficient columns
colnames(coef_glmm)[1:4] <- c(
  "estimate_log_odds",
  "std_error",
  "z_value",
  "p_value"
)


# Wald 95% confidence intervals for fixed effects
ci_glmm <- confint(
  test_model,
  parm = "beta_",
  method = "Wald"
)

ci_glmm <- as.data.frame(ci_glmm)
ci_glmm$term <- rownames(ci_glmm)
rownames(ci_glmm) <- NULL

colnames(ci_glmm)[1:2] <- c(
  "CI95_lower",
  "CI95_upper"
)


# Merge coefficients and confidence intervals by term
coef_glmm <- merge(
  coef_glmm,
  ci_glmm,
  by = "term",
  all.x = TRUE,
  sort = FALSE
)


# Odds ratios
coef_glmm$odds_ratio <- exp(
  coef_glmm$estimate_log_odds
)

coef_glmm$OR_CI95_lower <- exp(
  coef_glmm$CI95_lower
)

coef_glmm$OR_CI95_upper <- exp(
  coef_glmm$CI95_upper
)


# ------------------------------------------------------------------------------------------------
# Approximate Minimum Detectable Effect
#
# Alpha = 0.05, two-sided
# Statistical power = 80%
#
# Approximation based on the observed standard error of each fixed-effect
# coefficient:
#
# MDE = (Z_alpha/2 + Z_power) * SE
# ------------------------------------------------------------------------------------------------

z_alpha <- qnorm(0.975)
z_power <- qnorm(0.80)

coef_glmm$MDE_log_odds_80power <- (
  z_alpha + z_power
) * coef_glmm$std_error


# MDE expressed as odds-ratio magnitude
coef_glmm$MDE_OR_upper_80power <- exp(
  coef_glmm$MDE_log_odds_80power
)

coef_glmm$MDE_OR_lower_80power <- exp(
  -coef_glmm$MDE_log_odds_80power
)


# Remove intercept because reviewer asked about respondent characteristics
coef_glmm <- coef_glmm[
  coef_glmm$term != "(Intercept)",
]


# Reorder columns
coef_glmm <- coef_glmm[, c(
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


# Round for publication
numeric_columns <- sapply(
  coef_glmm,
  is.numeric
)

coef_glmm[numeric_columns] <- lapply(
  coef_glmm[numeric_columns],
  round,
  digits = 4
)


print(coef_glmm)


# Save complete coefficient/MDE table
write.csv2(
  coef_glmm,
  here::here(
    "figures_tables",
    "Table_S_GLMM_coefficients_CI_MDE.csv"
  ),
  row.names = FALSE
)


# ================================================================================================
# 6. OPTIONAL: ILLUSTRATIVE ELO COMPARISONS BETWEEN RESPONDENT GROUPS
# ================================================================================================
#
# This section reproduces the original illustrative Elo comparisons.
# It only runs if the custom function plot.2d.lm_labels() is available.
#
# ================================================================================================

if (exists("plot.2d.lm_labels")) {
  
  if (!requireNamespace("EloChoice", quietly = TRUE)) {
    stop("Package 'EloChoice' is required for respondent-group Elo plots.")
  }
  
  if (!requireNamespace("imager", quietly = TRUE)) {
    stop("Package 'imager' is required for respondent-group Elo plots.")
  }
  
  
  matches_all <- read.csv2(
    here::here("data", "matches_all.csv")
  )
  
  respondents <- read.csv2(
    here::here("data", "data_respondents_sub.csv")
  )
  
  names(respondents)[
    names(respondents) == "ID_Judge"
  ] <- "judge_id"
  
  
  list_var <- c(
    "Gender",
    "Age_class",
    "Education",
    "Social_class",
    "Grew_up",
    "Live_in",
    "Ethnicity"
  )
  
  
  respondents_sub <- respondents[
    ,
    c("judge_id", list_var)
  ]
  
  respondents_sub <- respondents_sub[
    complete.cases(respondents_sub),
  ]
  
  
  matches_all <- matches_all[
    matches_all$judge_id %in%
      unique(respondents_sub$judge_id),
  ]
  
  
  data_matches_judge <- merge(
    matches_all,
    respondents_sub,
    by = "judge_id",
    all.x = TRUE,
    all.y = FALSE
  )
  
  
  eloruns <- 50
  
  
  allgroups <- list(
    c("18_24", "25_59"),
    c("18_24", "60_100"),
    c("25_59", "60_100"),
    c("Female", "Male"),
    c("Middle", "Low"),
    c("Middle", "Upper"),
    c("Upper", "Low"),
    c("Metropolis", "Small city"),
    c("Metropolis", "Median city"),
    c("Metropolis", "Small city"),
    c("Metropolis", "Median city"),
    c("PhD", "High school"),
    c("PhD", "Bachelor"),
    c("PhD", "Master"),
    c("Master", "High school"),
    c("White", "Non-white")
  )
  
  
  whats <- c(
    "Age_class",
    "Age_class",
    "Age_class",
    "Gender",
    "Social_class",
    "Social_class",
    "Social_class",
    "Grew_up",
    "Grew_up",
    "Live_in",
    "Live_in",
    "Education",
    "Education",
    "Education",
    "Education",
    "Ethnicity"
  )
  
  
  for (i in seq_along(allgroups)) {
    
    groups <- allgroups[[i]]
    what <- whats[i]
    
    cat(
      "\nGroup ",
      what,
      ": ",
      paste(groups, collapse = " vs. "),
      " ----\n"
    )
    
    cat("Computing Elos ...\n")
    
    
    group_elos <- do.call(
      merge,
      lapply(
        groups,
        function(id) {
          
          matches <- data_matches_judge[
            data_matches_judge[, what] %in% id,
          ]
          
          matches$Loser <- NA
          
          matches$Loser[
            matches$outcome == 1
          ] <- matches$challenger_2[
            matches$outcome == 1
          ]
          
          matches$Loser[
            matches$outcome == 0
          ] <- matches$challenger_1[
            matches$outcome == 0
          ]
          
          
          res_elo <- EloChoice::elochoice(
            winner = matches$Winner,
            loser = matches$Loser,
            startvalue = 1500,
            runs = eloruns
          )
          
          
          scores <- cbind.data.frame(
            Id_images = names(
              EloChoice::ratings(
                res_elo,
                show = "mean",
                drawplot = FALSE
              )
            ),
            mean = EloChoice::ratings(
              res_elo,
              show = "mean",
              drawplot = FALSE
            ),
            var = EloChoice::ratings(
              res_elo,
              show = "var",
              drawplot = FALSE
            )
          )
          
          
          scores[, 3] <- sqrt(scores[, 3])
          
          
          colnames(scores) <- c(
            "Id_images",
            paste0("Elo_", id),
            paste0("Elo_sd_", id)
          )
          
          
          scores
        }
      )
    )
    
    
    cat("Plotting ...\n")
    
    
    df <- data.frame(
      x = group_elos[
        ,
        paste0("Elo_", groups[1])
      ],
      y = group_elos[
        ,
        paste0("Elo_", groups[2])
      ],
      names = group_elos[, "Id_images"]
    )
    
    
    tiff(
      here::here(
        "figures_tables",
        "Respondents",
        paste0(
          "Resp_",
          groups[1],
          "_",
          groups[2],
          ".tiff"
        )
      ),
      width = 1500,
      height = 1500
    )
    
    
    plot.2d.lm_labels(
      scale = 0.05,
      size = -50L,
      coord = df,
      pathphoto = here::here(
        "data",
        "BIG_FILES",
        "images",
        "png"
      ),
      xR = min(df$x),
      yR = max(df$y),
      colR = "black",
      labelx = paste0(
        "Aesthetic ",
        what,
        " (",
        groups[1],
        ")"
      ),
      labely = paste0(
        "Aesthetic ",
        what,
        " (",
        groups[2],
        ")"
      ),
      cexlab = 3.5,
      cexaxis = 2,
      colline = "#325395",
      lwline = 8,
      df = df
    )
    
    
    dev.off()
  }
  
} else {
  
  message(
    "plot.2d.lm_labels() was not found. ",
    "The optional respondent-group Elo figures were skipped. ",
    "The GLMM and supplementary tables were completed normally."
  )
}


# ================================================================================================
# 7. FINAL CHECK
# ================================================================================================

cat("\n------------------------------------------------------------\n")
cat("ANALYSIS COMPLETED\n")
cat("------------------------------------------------------------\n")

cat(
  "\nOmnibus GLMM table saved at:\n",
  here::here(
    "figures_tables",
    "Table_S_GLMM_omnibus_tests.csv"
  ),
  "\n"
)

cat(
  "\nCoefficient / 95% CI / MDE table saved at:\n",
  here::here(
    "figures_tables",
    "Table_S_GLMM_coefficients_CI_MDE.csv"
  ),
  "\n"
)

cat("------------------------------------------------------------\n")