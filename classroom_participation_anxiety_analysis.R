
# ============================================================
# Classroom Participation Anxiety Scale — R Analysis
# Original course project: 18 anxiety items + 5 participation items
# Input:  data analyze.xlsx (columns ID, Q1 ... Q23)
# Output: outputs/ (summary tables, plots and analysis summary)
# ============================================================

# 1. Packages -------------------------------------------------
required_packages <- c("readxl", "psych")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Install missing packages first: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

# 2. Portable project paths -----------------------------------
# Works with Rscript, with an open saved script in RStudio,
# or with the current working directory as a fallback.
get_project_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  script_arg <- grep("^--file=", args, value = TRUE)
  if (length(script_arg) > 0) {
    script_path <- sub("^--file=", "", script_arg[1])
    if (file.exists(script_path)) {
      return(dirname(normalizePath(script_path, winslash = "/")))
    }
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    editor_path <- tryCatch(
      rstudioapi::getActiveDocumentContext()$path,
      error = function(e) ""
    )
    if (length(editor_path) == 1 && nzchar(editor_path) &&
        file.exists(editor_path)) {
      return(dirname(normalizePath(editor_path, winslash = "/")))
    }
  }
  return(getwd())
}

project_dir <- get_project_dir()
input_path <- file.path(project_dir, "data analyze.xlsx")
output_dir <- file.path(project_dir, "outputs")
if (!file.exists(input_path)) {
  stop(
    "Cannot find 'data analyze.xlsx' in: ", project_dir,
    "\nPut the Excel file next to this R script and run again.",
    call. = FALSE
  )
}
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 3. Read and validate the questionnaire ----------------------
raw_data <- readxl::read_excel(input_path)
question_cols <- paste0("Q", 1:23)
missing_cols <- setdiff(question_cols, names(raw_data))
if (length(missing_cols) > 0) {
  stop("Missing questionnaire columns: ",
       paste(missing_cols, collapse = ", "), call. = FALSE)
}
# Do not carry participant IDs into any exported analysis table.
data <- as.data.frame(raw_data[, question_cols, drop = FALSE])
if (nrow(data) < 3) {
  stop("At least three responses are needed.", call. = FALSE)
}
for (item in question_cols) {
  values <- data[[item]]
  if (!is.numeric(values) || anyNA(values) ||
      any(!is.finite(values)) || any(!(values %in% 1:5))) {
    stop("Invalid values in ", item,
         ": responses must be complete numeric Likert ratings (1–5).",
         call. = FALSE)
  }
}
cat("Valid survey responses:", nrow(data), "\n")

# 4. Reverse-code positively keyed anxiety items and Q21 -----
# For a 1–5 Likert scale, reverse score = 6 - original score.
reverse_items <- c("Q5", "Q6", "Q11", "Q12", "Q17", "Q18", "Q21")
for (item in reverse_items) {
  data[[paste0(item, "_R")]] <- 6 - data[[item]]
}

# 5. Define the original item groups --------------------------
item_groups <- list(
  Fear_Speaking = c("Q1", "Q2", "Q3", "Q4", "Q5_R", "Q6_R"),
  Fear_Negative_Evaluation = c("Q7", "Q8", "Q9", "Q10", "Q11_R", "Q12_R"),
  Avoidance_Behavior = c("Q13", "Q14", "Q15", "Q16", "Q17_R", "Q18_R"),
  Participation_Frequency = c("Q19", "Q20", "Q21_R", "Q22", "Q23")
)
anxiety_items <- unlist(item_groups[1:3], use.names = FALSE)

for (scale in names(item_groups)) {
  data[[scale]] <- rowMeans(data[, item_groups[[scale]], drop = FALSE])
}
data$Overall_Anxiety <- rowMeans(data[, anxiety_items, drop = FALSE])

# 6. Descriptive statistics -----------------------------------
scale_names <- c("Fear of Speaking", "Fear of Negative Evaluation",
                 "Avoidance Behavior", "Overall Anxiety",
                 "Participation Frequency")
score_cols <- c("Fear_Speaking", "Fear_Negative_Evaluation",
                "Avoidance_Behavior", "Overall_Anxiety",
                "Participation_Frequency")
Table1_Descriptive <- data.frame(
  Variable = scale_names,
  Mean = round(vapply(score_cols, function(x) mean(data[[x]]), numeric(1)), 2),
  SD = round(vapply(score_cols, function(x) sd(data[[x]]), numeric(1)), 2),
  row.names = NULL
)

# 7. Internal consistency (Cronbach's alpha) ------------------
get_alpha <- function(items) {
  result <- psych::alpha(data[, items, drop = FALSE],
                         check.keys = FALSE, warnings = FALSE)
  as.numeric(result$total$raw_alpha)
}
alpha_values <- c(
  get_alpha(item_groups$Fear_Speaking),
  get_alpha(item_groups$Fear_Negative_Evaluation),
  get_alpha(item_groups$Avoidance_Behavior),
  get_alpha(anxiety_items),
  get_alpha(item_groups$Participation_Frequency)
)
Table2_Reliability <- data.frame(
  Scale = scale_names,
  Cronbach_Alpha = round(alpha_values, 2)
)

# 8. Pearson correlation: anxiety and engagement --------------
correlation <- cor.test(data$Overall_Anxiety,
                        data$Participation_Frequency,
                        method = "pearson", conf.level = 0.95)
format_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< .001")
  sprintf("%.3f", p)
}
Table3_Correlation <- data.frame(
  Relationship = "Overall Anxiety and Participation Frequency",
  r = round(unname(correlation$estimate), 2),
  t = round(unname(correlation$statistic), 2),
  df = unname(correlation$parameter),
  p_value = format_p(correlation$p.value),
  CI_Lower = round(correlation$conf.int[1], 2),
  CI_Upper = round(correlation$conf.int[2], 2)
)

# 9. Exploratory factor analysis: original 3-factor method ----
efa_data <- data[, anxiety_items, drop = FALSE]
KMO_result <- psych::KMO(efa_data)
bartlett_result <- psych::cortest.bartlett(
  cor(efa_data), n = nrow(efa_data)
)
# Same estimation method as the original course assignment:
# three factors, maximum likelihood, orthogonal varimax rotation.
efa_result <- psych::fa(efa_data, nfactors = 3,
                        rotate = "varimax", fm = "ml")
variance_explained_pct <-
  100 * sum(efa_result$Vaccounted["Proportion Var", ])

Table4_EFA_Summary <- data.frame(
  Statistic = c("KMO", "Bartlett Chi-square", "Bartlett df",
                "Bartlett p-value", "Variance Explained by 3 Factors"),
  Value = c(
    sprintf("%.2f", KMO_result$MSA),
    sprintf("%.2f", bartlett_result$chisq),
    as.character(bartlett_result$df),
    format_p(bartlett_result$p.value),
    sprintf("%.1f%%", variance_explained_pct)
  )
)

# Retain the full loadings internally; blank only small displayed values.
loadings_matrix <- as.matrix(unclass(efa_result$loadings))
loadings_display <- round(loadings_matrix, 2)
loadings_display[abs(loadings_matrix) < 0.30] <- NA_real_
Table5_Factor_Loadings <- data.frame(
  Item = rownames(loadings_display),
  loadings_display,
  row.names = NULL,
  check.names = FALSE
)

# 10. Export only aggregate results (no individual responses) -
write.csv(Table1_Descriptive,
          file.path(output_dir, "Table1_Descriptive.csv"), row.names = FALSE)
write.csv(Table2_Reliability,
          file.path(output_dir, "Table2_Reliability.csv"), row.names = FALSE)
write.csv(Table3_Correlation,
          file.path(output_dir, "Table3_Correlation.csv"), row.names = FALSE)
write.csv(Table4_EFA_Summary,
          file.path(output_dir, "Table4_EFA_Summary.csv"), row.names = FALSE)
write.csv(Table5_Factor_Loadings,
          file.path(output_dir, "Table5_Factor_Loadings.csv"),
          row.names = FALSE, na = "")

# 11. Save figures automatically ------------------------------
save_parallel_plot <- function(path) {
  png(path, width = 1400, height = 950, res = 160)
  on.exit(dev.off(), add = TRUE)
  set.seed(2026)  # Reproducible parallel-analysis reference draws.
  psych::fa.parallel(efa_data, fa = "fa", fm = "ml", n.iter = 50,
                     main = "Parallel Analysis: Anxiety Items")
}

save_correlation_plot <- function(path) {
  png(path, width = 1400, height = 950, res = 160)
  on.exit(dev.off(), add = TRUE)
  plot(data$Overall_Anxiety, data$Participation_Frequency,
       xlab = "Overall Anxiety (1–5)",
       ylab = "Participation Frequency (1–5)",
       main = "Classroom Anxiety and Participation",
       pch = 19, col = grDevices::adjustcolor("steelblue", alpha.f = 0.65))
  abline(lm(Participation_Frequency ~ Overall_Anxiety, data = data),
         col = "firebrick", lwd = 2)
  grid(col = "gray85")
  legend("topright",
         legend = sprintf("Pearson r = %.2f", unname(correlation$estimate)),
         bty = "n")
}

save_parallel_plot(file.path(output_dir, "Figure1_Parallel_Analysis.png"))
save_correlation_plot(file.path(output_dir, "Figure2_Anxiety_Participation.png"))

# 12. Plain-text summary and console output -------------------
summary_lines <- c(
  "CLASSROOM PARTICIPATION ANXIETY SCALE — ANALYSIS SUMMARY",
  "=======================================================",
  sprintf("Completed student questionnaires: %d", nrow(data)),
  "Anxiety items: 18 (three predefined 6-item dimensions)",
  "Participation frequency items: 5 (analyzed separately)",
  sprintf("Overall Cronbach's alpha: %.4f", alpha_values[4]),
  sprintf("Pearson correlation (anxiety vs participation): r = %.4f, p %s",
          unname(correlation$estimate), format_p(correlation$p.value)),
  sprintf("KMO: %.2f", KMO_result$MSA),
  sprintf("Bartlett: chi-square = %.2f, df = %d, p %s",
          bartlett_result$chisq, bartlett_result$df,
          format_p(bartlett_result$p.value)),
  sprintf("Three-factor EFA (ML, varimax) variance explained: %.1f%%",
          variance_explained_pct),
  "",
  "Interpretation: KMO is low; this 3-factor solution is preliminary.",
  "These associations are not evidence of causal effects.",
  "Student-level records and respondent IDs are not exported."
)
writeLines(summary_lines, file.path(output_dir, "Analysis_Summary.txt"))
cat(paste(summary_lines, collapse = "\n"), "\n\n")
cat("Finished. All outputs saved in:\n", output_dir, "\n", sep = "")
