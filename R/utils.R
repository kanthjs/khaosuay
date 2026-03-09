# ══════════════════════════════════════════════════════════════════════
# utils.R — Internal helpers ที่ใช้ร่วมกันทั้ง package
# ฟังก์ชันทั้งหมดขึ้นต้นด้วย . (dot) = internal, ไม่ export
# ══════════════════════════════════════════════════════════════════════


# --- ตรวจสอบและโหลด agricolae ---
.check_agricolae <- function() {
  if (!requireNamespace("agricolae", quietly = TRUE)) {
    stop(
      "\U0001F4E6 ต้องติดตั้ง agricolae ก่อน:\n",
      '  install.packages("agricolae")'
    )
  }
}


# --- ดึง data.frame จาก washed_rice หรือ data.frame ธรรมดา ---
.extract_df <- function(data) {
  if (inherits(data, "washed_rice")) data$data
  else as.data.frame(data)
}


# --- ดึง classification จาก tasted_rice object ---
.get_classification <- function(tasted, resp) {
  if (is.null(tasted)) return(NULL)
  if (!inherits(tasted, "tasted_rice")) {
    warning("\u26A0\uFE0F tasted ไม่ใช่ tasted_rice object")
    return(NULL)
  }
  if (resp %in% names(tasted$details)) {
    return(tasted$details[[resp]]$final_class)
  }
  idx <- which(tasted$results$response == resp)
  if (length(idx) > 0) return(tasted$results$final_class[idx[1]])
  warning("\u26A0\uFE0F ไม่พบ '", resp, "' ใน tasted object")
  NULL
}


# --- แปลง classification → analysis path ---
.class_to_path <- function(classification) {
  switch(classification,
    "Normal + Equal Variance"   = "parametric",
    "Normal + Unequal Variance" = "welch",
    "Non-normal"                = "non-parametric",
    "parametric"
  )
}


# --- Quick assumption check (fallback เมื่อไม่มี tasted) ---
.quick_assumption <- function(residuals, groups, alpha = 0.05) {
  x <- residuals[!is.na(residuals)]
  if (length(x) < 3 || sd(x) == 0) {
    return(list(class = "N/A", path = "parametric",
                shapiro_p = NA, levene_p = NA))
  }
  if (length(x) > 5000) x <- sample(x, 5000)
  shap <- tryCatch(shapiro.test(x),
                   error = function(e) list(p.value = NA))
  norm_pass <- !is.na(shap$p.value) && shap$p.value >= alpha

  if (!norm_pass) {
    return(list(class = "Non-normal", path = "non-parametric",
                shapiro_p = shap$p.value, levene_p = NA))
  }

  # Levene's (Brown-Forsythe)
  ok <- complete.cases(residuals, groups)
  y <- residuals[ok]
  g <- factor(groups[ok])
  if (length(unique(g)) < 2) {
    return(list(class = "Normal + Equal Variance", path = "parametric",
                shapiro_p = shap$p.value, levene_p = NA))
  }
  med <- tapply(y, g, median, na.rm = TRUE)
  lev_result <- anova(lm(abs(y - med[g]) ~ g))
  lev_p <- lev_result$`Pr(>F)`[1]
  var_pass <- !is.na(lev_p) && lev_p >= alpha

  list(
    class     = if (var_pass) "Normal + Equal Variance" else "Normal + Unequal Variance",
    path      = if (var_pass) "parametric" else "welch",
    shapiro_p = shap$p.value,
    levene_p  = lev_p
  )
}


# --- Summary table พร้อมตีพิมพ์ ---
.make_summary_table <- function(data, response, group_col,
                                posthoc_groups = NULL) {
  groups <- split(data[[response]], data[[group_col]])
  tbl <- data.frame(
    treatment = names(groups),
    n    = vapply(groups, function(x) sum(!is.na(x)), integer(1L)),
    mean = round(vapply(groups, mean, double(1L), na.rm = TRUE), 2),
    sd   = round(vapply(groups, sd, double(1L), na.rm = TRUE), 2),
    se   = round(vapply(groups, function(x) {
             sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
           }, double(1L)), 2),
    min  = round(vapply(groups, min, double(1L), na.rm = TRUE), 2),
    max  = round(vapply(groups, max, double(1L), na.rm = TRUE), 2),
    row.names = NULL, stringsAsFactors = FALSE
  )
  if (!is.null(posthoc_groups)) {
    tbl$group <- posthoc_groups[match(tbl$treatment, names(posthoc_groups))]
    tbl$mean_group <- paste0(
      formatC(tbl$mean, format = "f", digits = 2), " ",
      ifelse(is.na(tbl$group), "", tbl$group)
    )
  }
  tbl
}


# --- Post-hoc parametric (agricolae) ---
.run_posthoc_parametric <- function(model, ph_col, df_error, ms_error,
                                    method = "tukey", alpha = 0.05) {
  if (method == "tukey") {
    ph <- agricolae::HSD.test(
      model, trt = ph_col,
      DFerror = df_error, MSerror = ms_error,
      alpha = alpha, group = TRUE, console = FALSE
    )
    name <- "Tukey's HSD"

  } else if (method == "lsd") {
    ph <- agricolae::LSD.test(
      model, trt = ph_col,
      DFerror = df_error, MSerror = ms_error,
      alpha = alpha, group = TRUE, p.adj = "bonferroni",
      console = FALSE
    )
    name <- "Fisher's LSD (Bonferroni)"

  } else if (method == "duncan") {
    ph <- agricolae::duncan.test(
      model, trt = ph_col,
      DFerror = df_error, MSerror = ms_error,
      alpha = alpha, group = TRUE, console = FALSE
    )
    name <- "Duncan's MRT"
  }

  letters <- setNames(
    trimws(as.character(ph$groups$groups)),
    rownames(ph$groups)
  )

  list(name = name, result = ph, groups = ph$groups, letters = letters)
}


# --- Post-hoc non-parametric (agricolae::kruskal) ---
.run_posthoc_nonparametric <- function(y, group, alpha = 0.05) {
  ph <- agricolae::kruskal(
    y, group,
    alpha = alpha, group = TRUE, p.adj = "bonferroni",
    console = FALSE
  )

  letters <- setNames(
    trimws(as.character(ph$groups$groups)),
    rownames(ph$groups)
  )

  list(name = "Kruskal + Dunn's (Bonferroni)",
       result = ph, groups = ph$groups, letters = letters)
}


# --- Verbose print สำหรับ cooked_rice ---
.print_cook_results <- function(all_results, design, alpha) {

  message("\n", strrep("\U0001F33E", 25))
  message("\U0001F35A  cook_", tolower(design),
          " v1.1 — ผลการหุงข้าว (", design, ")")
  message(strrep("\u2500", 60))

  for (resp_name in names(all_results)) {
    res <- all_results[[resp_name]]
    message(strrep("\u2500", 60))
    message("\U0001F33E ", resp_name, " (n=", res$n, ")")
    message("  Formula:     ", res$formula)

    src <- res$assumption$source
    src_label <- if (src == "taste_rice") {
      "\u2705 จาก taste_rice (ไม่เช็คซ้ำ)"
    } else {
      "\U0001F50D auto_check"
    }
    message("  Assumption:  ", res$assumption$class, "  [", src_label, "]")
    message("  Test:        ", res$test_name)

    # p-values
    .print_pvalues <- function(p, name = NULL) {
      sig <- if (is.na(p)) "" else if (p < 0.001) " ***"
             else if (p < 0.01) " **" else if (p < 0.05) " *" else " ns"
      label <- if (!is.null(name)) paste0("  p-value [", name, "]: ")
               else "  p-value:     "
      message(label, formatC(p, format = "g", digits = 4), sig)
    }

    if (!is.null(res$p_values)) {
      for (i in seq_along(res$p_values))
        .print_pvalues(res$p_values[i], names(res$p_values)[i])
    } else {
      .print_pvalues(res$p_value)
    }

    if (!is.null(res$cv_percent) && !is.na(res$cv_percent))
      message("  CV%:         ", res$cv_percent, "%")

    # Post-hoc
    if (res$posthoc_needed) {
      message("\n  \U0001F4CA Post-Hoc: ", res$posthoc_name)
      message("  ", strrep("-", 45))
      tbl <- res$summary_table
      if ("group" %in% names(tbl)) {
        max_w <- max(nchar(as.character(tbl$treatment)))
        for (i in seq_len(nrow(tbl))) {
          message(
            "    ",
            formatC(tbl$treatment[i], width = max_w, flag = "-"),
            "  mean=",
            formatC(tbl$mean[i], width = 8, format = "f", digits = 2),
            " \u00B1",
            formatC(tbl$sd[i], width = 6, format = "f", digits = 2),
            "  [", tbl$group[i], "]"
          )
        }
      }
    } else {
      message("\n  \U0001F44D ไม่มีความแตกต่าง (p >= ", alpha,
              ") — ไม่จำเป็นต้องเปรียบเทียบรายคู่")
    }
  }

  message(strrep("\u2500", 60))
  message("\U0001F4CB $results$<var>$summary_table → ตารางพร้อมตีพิมพ์")
  message("   $results$<var>$group_letters → กลุ่มอักษร (a, b, c)")
  message("   $results$<var>$anova_table   → ANOVA table")
  message("   $results$<var>$model         → aov object")
  message(strrep("\U0001F33E", 25))
}


# --- Print method สำหรับ cooked_rice ---
#' @export
print.cooked_rice <- function(x, ...) {
  cat("\u2500\u2500 cooked_rice v1.1 (", x$design, ") \u2500\u2500\n")
  cat("  tasted:",
      if (x$settings$tasted_provided) "provided" else "auto_check",
      "\n\n")

  for (rn in names(x$results)) {
    res <- x$results[[rn]]
    cat(" ", rn, "\n")
    cat("    ", res$assumption$class, "\n")
    cat("    ", res$test_name, " | p=",
        formatC(res$p_value, format = "g", digits = 4), "\n")
    if (res$posthoc_needed && "mean_group" %in% names(res$summary_table)) {
      cat("    Post-hoc:", res$posthoc_name, "\n")
      for (i in seq_len(nrow(res$summary_table)))
        cat("      ", res$summary_table$treatment[i], ": ",
            res$summary_table$mean_group[i], "\n")
    }
    cat("\n")
  }
  invisible(x)
}