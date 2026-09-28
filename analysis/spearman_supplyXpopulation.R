# ============================================================
# SPEARMAN CORRELATION
# Aesthetic service supply x Population density
# 100 x 100 m grid
# ============================================================

setwd("C:/Users/dougl/OneDrive/Documentos/GitHub/AESTHETIC_STREETS/data")

# Read data
sdb <- read.csv2(
  "Aesthetic+GVI+Demand.csv",
  sep = ",",
  row.names = NULL
)

# Convert variables to numeric
sdb$aesthetic_ <- as.numeric(sdb$aesthetic_)
sdb$pop_17_0_1 <- as.numeric(sdb$pop17_0_1)

# Keep only complete cells
spearman_data <- sdb[
  complete.cases(sdb$aesthetic_, sdb$pop_17_0_1),
  c("aesthetic_", "pop_17_0_1")
]

# Spearman correlation
spearman_result <- cor.test(
  spearman_data$aesthetic_,
  spearman_data$pop_17_0_1,
  method = "spearman",
  exact = FALSE
)

# Output
cat("\n=============================\n")
cat("SPEARMAN CORRELATION\n")
cat("=============================\n")
cat("n =", nrow(spearman_data), "\n")
cat("Spearman rho =", round(unname(spearman_result$estimate), 3), "\n")
cat("p-value =", format.pval(spearman_result$p.value, digits = 3), "\n")
cat("=============================\n")