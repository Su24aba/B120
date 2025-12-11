# Analysis.R
# Final version for assignment – saves graphs and results automatically

library(ggplot2)
library(dplyr)
library(effsize)

# ===============================
# 1. LOAD DATA
# ===============================
data <- read.csv("est16us.csv", stringsAsFactors = FALSE)

# Check names
names(data)

income_col <- "Median.Household.Income"
poverty_col <- "Poverty.Percent..Age.0.4"

# Convert to numeric
data[[income_col]] <- as.numeric(gsub("[^0-9.-]", "", data[[income_col]]))
data[[poverty_col]] <- as.numeric(gsub("[^0-9.-]", "", data[[poverty_col]]))

# Clean data
data_clean <- data %>%
  filter(!is.na(.data[[income_col]]), !is.na(.data[[poverty_col]]))

# ===============================
# 2. CREATE GROUPS (HIGH / LOW CHILD POVERTY)
# ===============================
poverty_median <- median(data_clean[[poverty_col]], na.rm = TRUE)

data_clean <- data_clean %>%
  mutate(ChildPovGroup = ifelse(.data[[poverty_col]] > poverty_median,
                                "High child poverty",
                                "Low child poverty"))


# ===============================
# 3. SUMMARY TABLE
# ===============================
group_summary <- data_clean %>%
  group_by(ChildPovGroup) %>%
  summarise(
    n = n(),
    mean_income = mean(.data[[income_col]]),
    sd_income = sd(.data[[income_col]]),
    median_income = median(.data[[income_col]])
  )

write.csv(group_summary, "group_summary.csv", row.names = FALSE)
print(group_summary)


# ===============================
# 4. MAIN PLOT – BOXPLOT
# ===============================
p1 <- ggplot(data_clean, aes(x = ChildPovGroup, y = .data[[income_col]])) +
  geom_boxplot() +
  labs(
    title = "Median Household Income by Child Poverty Group (Age 0–4)",
    x = "Child Poverty Category",
    y = "Median Household Income"
  ) +
  theme_minimal()

ggsave("boxplot_income_by_group.png", p1, width = 7, height = 5, dpi = 300)


# ===============================
# 5. SUPPLEMENTARY PLOT – HISTOGRAM
# ===============================
p2 <- ggplot(data_clean, aes(x = .data[[income_col]])) +
  geom_histogram(bins = 15) +
  labs(
    title = "Histogram of Median Household Income",
    x = "Median Household Income",
    y = "Number of States"
  ) +
  theme_minimal()

ggsave("hist_income.png", p2, width = 7, height = 5, dpi = 300)


# ===============================
# 6. NORMALITY CHECKS
# ===============================
shapiro_results <- data_clean %>%
  group_by(ChildPovGroup) %>%
  summarise(
    shapiro_p = ifelse(n() >= 3,
                       shapiro.test(.data[[income_col]])$p.value,
                       NA)
  )

print(shapiro_results)


# ===============================
# 7. STATISTICAL TESTS
# ===============================
group_high <- data_clean %>%
  filter(ChildPovGroup == "High child poverty") %>%
  pull(.data[[income_col]])

group_low <- data_clean %>%
  filter(ChildPovGroup == "Low child poverty") %>%
  pull(.data[[income_col]])

t_res <- t.test(group_high, group_low)
mw_res <- wilcox.test(group_high, group_low)
cohen_res <- cohen.d(group_high, group_low, hedges.correction = TRUE)

print(t_res)
print(mw_res)
print(cohen_res)


# ===============================
# 8. SAVE ALL OUTPUT TO TEXT FILE
# ===============================
sink("analysis_output.txt")

cat("===== CHILD POVERTY THRESHOLD =====\n")
cat("Median Poverty.Percent..Age.0.4:", poverty_median, "\n\n")

cat("===== GROUP SUMMARY =====\n")
print(group_summary)

cat("\n===== SHAPIRO-WILK TESTS =====\n")
print(shapiro_results)

cat("\n===== T-TEST RESULT =====\n")
print(t_res)

cat("\n===== MANN-WHITNEY (ROBUST CHECK) =====\n")
print(mw_res)

cat("\n===== COHEN'S D EFFECT SIZE =====\n")
print(cohen_res)

sink()

# Finished
