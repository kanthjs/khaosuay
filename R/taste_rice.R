#' ชิมข้าว — ตรวจสอบ Assumptions ก่อนวิเคราะห์สถิติ (taste_rice) v2.0
#'
#' รองรับ CRD, RCBD และ Factorial Design
#'
#' @param data washed_rice object หรือ data.frame
#' @param response character ชื่อคอลัมน์ response (ระบุหลายตัวได้)
#' @param treatment character ชื่อคอลัมน์ treatment (ใส่ vector ได้ เช่น c("A", "B") สำหรับ Factorial)
#' @param block character ชื่อคอลัมน์ block สำหรับ RCBD (default = NULL = CRD)
#' @param mode "quick" = เช็คจาก raw data, "model" = เช็คจาก residuals, "both" = ทำทั้งสองแบบ (default)
#' @param alpha ระดับนัยสำคัญ (default = 0.05)
#' @param plot logical แสดง diagnostic plots (default = TRUE)
#' @param verbose logical แสดงรายงาน (default = TRUE)
#'
#' @return object class "tasted_rice"
#' @export
taste_rice <- function(data,
                       response,
                       treatment = "treatment",
                       block     = NULL,
                       mode      = c("both", "quick", "model"),
                       alpha     = 0.05,
                       plot      = TRUE,
                       verbose   = TRUE) {

  # ══════════════════════════════════════════════════════════════════
  # SETUP & VALIDATION
  # ══════════════════════════════════════════════════════════════════
  mode <- match.arg(mode)

  if (inherits(data, "washed_rice")) {
    df <- data$data
  } else {
    df <- as.data.frame(data)
  }

  is_factorial <- length(treatment) > 1

  # ตรวจคอลัมน์
  all_needed <- c(response, treatment)
  if (!is.null(block)) all_needed <- c(all_needed, block)
  missing_cols <- setdiff(all_needed, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "\U0001F6AB \u0e44\u0e21\u0e48\u0e1e\u0e1a\u0e04\u0e2d\u0e25\u0e31\u0e21\u0e19\u0e4c: ", paste(missing_cols, collapse = ", "),
      "\n  \u0e04\u0e2d\u0e25\u0e31\u0e21\u0e19\u0e4c\u0e17\u0e35\u0e48\u0e21\u0e35: ", paste(names(df), collapse = ", ")
    )
  }

  # แปลงเป็น factor
  for (trt in treatment) {
    if (!is.factor(df[[trt]])) df[[trt]] <- as.factor(df[[trt]])
  }
  if (!is.null(block) && !is.factor(df[[block]])) df[[block]] <- as.factor(df[[block]])

  # containers
  result_rows  <- vector("list", length(response))
  all_details  <- list()
  all_models   <- list()
  all_verdicts <- character(0)

  # ══════════════════════════════════════════════════════════════════
  # HELPERS
  # ══════════════════════════════════════════════════════════════════
  safe_shapiro <- function(x) {
    x <- x[!is.na(x)]
    n <- length(x)
    if (n < 3) return(list(statistic = NA, p.value = NA, note = paste0("n=", n, " (\u0e15\u0e49\u0e2d\u0e07 >= 3)")))
    if (n > 5000) x <- sample(x, 5000)
    if (sd(x) == 0) return(list(statistic = NA, p.value = NA, note = "sd=0 \u0e04\u0e48\u0e32\u0e17\u0e38\u0e01\u0e15\u0e31\u0e27\u0e40\u0e17\u0e48\u0e32\u0e01\u0e31\u0e19"))
    tryCatch(shapiro.test(x), error = function(e) list(statistic = NA, p.value = NA, note = e$message))
  }

  levene_test <- function(y, group) {
    ok <- complete.cases(y, group)
    y <- y[ok]; group <- factor(group[ok])
    if (length(unique(group)) < 2) return(list(statistic = NA, p.value = NA, note = "\u0e15\u0e49\u0e2d\u0e07\u0e21\u0e35 >= 2 \u0e01\u0e25\u0e38\u0e48\u0e21"))
    med <- tapply(y, group, median, na.rm = TRUE)
    abs_dev <- abs(y - med[group])
    result <- anova(lm(abs_dev ~ group))
    list(statistic = result$`F value`[1], p.value = result$`Pr(>F)`[1], method = "Brown-Forsythe")
  }

  classify <- function(norm_pass, var_pass) {
    if (is.na(norm_pass)) return("N/A")
    if (!norm_pass)       return("Non-normal")
    if (is.na(var_pass))  return("Normal (variance \u0e17\u0e14\u0e2a\u0e2d\u0e1a\u0e44\u0e21\u0e48\u0e44\u0e14\u0e49)")
    if (var_pass)         return("Normal + Equal Variance")
    return("Normal + Unequal Variance")
  }

  recommend <- function(resp, classification, is_factorial, n_groups) {
    design_type <- if (is_factorial) "Factorial" else if (n_groups == 2) "2 \u0e01\u0e25\u0e38\u0e48\u0e21" else paste0(n_groups, " \u0e01\u0e25\u0e38\u0e48\u0e21")
    
    rec <- switch(classification,
      "Normal + Equal Variance" = {
        if (is_factorial) "\u2705 \u0e43\u0e0a\u0e49 Factorial ANOVA (aov(y ~ A*B)) \u2192 Tukey HSD"
        else if (n_groups == 2) "\u2705 \u0e43\u0e0a\u0e49 Student's t-test (independent)"
        else "\u2705 \u0e43\u0e0a\u0e49 Fisher's ANOVA \u2192 Tukey HSD"
      },
      "Normal + Unequal Variance" = {
        if (is_factorial) "\u26A0\uFE0F \u0e44\u0e21\u0e48\u0e23\u0e2d\u0e07\u0e23\u0e31\u0e1a\u0e43\u0e19 Base R \u2192 \u0e22\u0e38\u0e1a\u0e01\u0e25\u0e38\u0e48\u0e21\u0e43\u0e0a\u0e49 Welch's ANOVA \u0e2b\u0e23\u0e37\u0e2d\u0e43\u0e0a\u0e49 car::Anova(white.adjust=TRUE)"
        else if (n_groups == 2) "\u26A0\uFE0F \u0e43\u0e0a\u0e49 Welch's t-test"
        else "\u26A0\uFE0F \u0e43\u0e0a\u0e49 Welch's ANOVA (oneway.test) \u2192 Games-Howell"
      },
      "Non-normal" = {
        if (is_factorial) "\U0001F6A8 \u0e44\u0e21\u0e48\u0e23\u0e2d\u0e07\u0e23\u0e31\u0e1a\u0e43\u0e19 Base R \u2192 \u0e41\u0e19\u0e30\u0e19\u0e33 Aligned Rank Transform (ARTool::art) \u0e2b\u0e23\u0e37\u0e2d\u0e22\u0e38\u0e1a\u0e01\u0e25\u0e38\u0e48\u0e21\u0e43\u0e0a\u0e49 Kruskal-Wallis"
        else if (n_groups == 2) "\U0001F6A8 \u0e43\u0e0a\u0e49 Mann-Whitney U test (wilcox.test)"
        else "\U0001F6A8 \u0e43\u0e0a\u0e49 Kruskal-Wallis test \u2192 Dunn's test"
      },
      "\u2753 \u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25\u0e44\u0e21\u0e48\u0e40\u0e1e\u0e35\u0e22\u0e07\u0e1e\u0e2d\u0e2a\u0e33\u0e2b\u0e23\u0e31\u0e1a\u0e01\u0e32\u0e23\u0e08\u0e33\u0e41\u0e19\u0e01"
    )
    paste0(resp, " [", design_type, "]: ", rec)
  }

  # ══════════════════════════════════════════════════════════════════
  # MAIN LOOP
  # ══════════════════════════════════════════════════════════════════
  for (.i in seq_along(response)) {
    resp <- response[[.i]]
    if (!is.numeric(df[[resp]])) {
      warning("\u26A0\uFE0F '", resp, "' \u0e44\u0e21\u0e48\u0e43\u0e0a\u0e48\u0e15\u0e31\u0e27\u0e40\u0e25\u0e02 \u2014 \u0e02\u0e49\u0e32\u0e21")
      next
    }

    detail     <- list(response = resp)
    result_row <- data.frame(response = resp, stringsAsFactors = FALSE)

    # กรอง NA ออก
    working_idx <- complete.cases(df[[resp]], df[, treatment, drop = FALSE])
    if (!is.null(block)) working_idx <- working_idx & complete.cases(df[[block]])
    working_df <- df[working_idx, , drop = FALSE]
    n_used <- nrow(working_df)
    result_row$n <- n_used

    if (n_used < 5) {
      warning("\u26A0\uFE0F '", resp, "': n=", n_used, " \u0e19\u0e49\u0e2d\u0e22\u0e40\u0e01\u0e34\u0e19\u0e44\u0e1b \u2014 \u0e02\u0e49\u0e32\u0e21")
      next
    }

    # สร้าง Interaction Group สำหรับเช็ค Assumptions
    if (is_factorial) {
      working_df$.group <- interaction(working_df[, treatment], drop = TRUE)
    } else {
      working_df$.group <- working_df[[treatment]]
    }
    n_groups_active <- length(levels(working_df$.group))

    quick_normal <- NA; quick_equal_var <- NA
    model_normal <- NA; model_equal_var <- NA; model <- NULL

    # ────────────────────────────────────────────────────────────────
    # QUICK MODE (Raw Data per Group Combination)
    # ────────────────────────────────────────────────────────────────
    if (mode %in% c("quick", "both")) {
      groups    <- split(working_df[[resp]], working_df$.group)
      shap_list <- lapply(groups, safe_shapiro)

      shap_df <- data.frame(
        group   = names(shap_list),
        n       = vapply(groups, function(x) sum(!is.na(x)), integer(1L)),
        p_value = vapply(shap_list, function(x) if (is.null(x$p.value)) NA_real_ else as.double(x$p.value), double(1L)),
        stringsAsFactors = FALSE
      )
      
      testable <- shap_df$p_value[!is.na(shap_df$p_value)]
      quick_normal <- if (length(testable) > 0) all(testable >= alpha) else NA
      
      result_row$quick_normal <- if (is.na(quick_normal)) "N/A" else if (quick_normal) "Normal" else "Non-normal"

      if (!is.na(quick_normal) && quick_normal) {
        lev <- levene_test(working_df[[resp]], working_df$.group)
        quick_equal_var <- !is.na(lev$p.value) && lev$p.value >= alpha
        result_row$quick_variance <- if (quick_equal_var) "Equal" else "Unequal"
      } else {
        quick_equal_var <- NA
        result_row$quick_variance <- "-- (\u0e02\u0e49\u0e32\u0e21)"
      }
      result_row$quick_class <- classify(quick_normal, quick_equal_var)
    }

    # ────────────────────────────────────────────────────────────────
    # MODEL MODE (Residuals)
    # ────────────────────────────────────────────────────────────────
    if (mode %in% c("model", "both")) {
      
      # สร้าง Formula แบบฉลาด: ถ้า Factorial ให้คูณกัน
      main_eff <- if (is_factorial) paste(treatment, collapse = " * ") else treatment
      rhs <- if (!is.null(block)) paste(main_eff, "+", block) else main_eff
      fml <- as.formula(paste(resp, "~", rhs))
      detail$formula <- deparse(fml)

      model <- tryCatch(aov(fml, data = working_df), error = function(e) NULL)

      if (!is.null(model)) {
        all_models[[resp]] <- model
        resid_vals    <- residuals(model)

        shap_resid <- safe_shapiro(resid_vals)
        model_normal <- !is.na(shap_resid$p.value) && shap_resid$p.value >= alpha
        result_row$model_normal <- if (is.na(model_normal)) "N/A" else if (model_normal) "Normal" else "Non-normal"

        if (model_normal) {
          lev_resid <- levene_test(resid_vals, working_df$.group)
          model_equal_var <- !is.na(lev_resid$p.value) && lev_resid$p.value >= alpha
          result_row$model_variance <- if (model_equal_var) "Equal" else "Unequal"
        } else {
          model_equal_var <- NA
          result_row$model_variance <- "-- (\u0e02\u0e49\u0e32\u0e21)"
        }
        result_row$model_class <- classify(model_normal, model_equal_var)
      }
    }

    # ────────────────────────────────────────────────────────────────
    # FINAL CLASSIFICATION
    # ────────────────────────────────────────────────────────────────
    if (mode %in% c("model", "both") && !is.null(model)) {
      final_normal <- model_normal; final_equal_var <- model_equal_var
    } else {
      final_normal <- quick_normal; final_equal_var <- quick_equal_var
    }

    final_class  <- classify(final_normal, final_equal_var)
    verdict_text <- recommend(resp, final_class, is_factorial, n_groups_active)

    result_row$final_class    <- final_class
    result_row$recommendation <- sub("^.*: ", "", verdict_text)

    all_verdicts        <- c(all_verdicts, verdict_text)
    detail$final_class    <- final_class
    detail$recommendation <- result_row$recommendation
    all_details[[resp]]   <- detail
    result_rows[[.i]]     <- result_row
  }

  all_results <- do.call(rbind, Filter(Negate(is.null), result_rows))

  # ══════════════════════════════════════════════════════════════════
  # DIAGNOSTIC PLOTS (Updated for Factorial)
  # ══════════════════════════════════════════════════════════════════
  if (plot && length(all_details) > 0) {
    for (resp in names(all_details)) {
      working_idx <- complete.cases(df[[resp]], df[, treatment, drop = FALSE])
      if (!is.null(block)) working_idx <- working_idx & complete.cases(df[[block]])
      working_df <- df[working_idx, ]
      
      # Grouping สำหรับ Plot
      if (is_factorial) working_df$.group <- interaction(working_df[, treatment], drop = TRUE)
      else working_df$.group <- factor(working_df[[treatment]])
      
      colors <- grDevices::hcl.colors(length(levels(working_df$.group)), palette = "Set2")
      fc <- all_details[[resp]]$final_class

      old_par <- par(no.readonly = TRUE)
      if (mode == "both") par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
      else par(mfrow = c(1, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))

      if (mode %in% c("quick", "both")) {
        qqnorm(working_df[[resp]], main = "QQ Plot (raw data)", pch = 16, col = "grey60")
        qqline(working_df[[resp]], col = "red", lwd = 2)
        boxplot(as.formula(paste(resp, "~ .group")), data = working_df, main = "Boxplot by Groups", col = colors, las = 2)
      }

      if (mode %in% c("model", "both") && !is.null(all_models[[resp]])) {
        mdl <- all_models[[resp]]
        qqnorm(residuals(mdl), main = "QQ Plot (residuals)", pch = 16, col = "steelblue")
        qqline(residuals(mdl), col = "red", lwd = 2)
        plot(fitted(mdl), residuals(mdl), main = "Residuals vs Fitted", pch = 16, col = "steelblue")
        abline(h = 0, col = "red", lty = 2)
      }

      title(main = paste0("\U0001F33E ", resp, "  \u2014  ", fc), outer = TRUE, cex.main = 1.3)
      par(old_par)
    }
  }

  structure(
    list(
      results  = all_results,
      details  = if (length(all_details) > 0) all_details else NULL,
      verdict  = all_verdicts,
      models   = if (length(all_models) > 0) all_models else NULL,
      settings = list(mode = mode, is_factorial = is_factorial)
    ),
    class = c("tasted_rice", "list")
  )
}

#' @export
print.tasted_rice <- function(x, ...) {
  cat("\u2500\u2500 tasted_rice v2.0 \u2500\u2500\n")
  cat("  Design:", ifelse(x$settings$is_factorial, "Factorial", "Single Factor"), "\n")
  cat("  Mode:", x$settings$mode, "\n\n")

  if (nrow(x$results) > 0) {
    print(x$results[, c("response", "final_class", "recommendation")], row.names = FALSE, right = FALSE)
  }
  cat("\n")
  for (v in x$verdict) cat(" ", v, "\n")
  invisible(x)
}
