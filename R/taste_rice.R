#' ชิมข้าว — ตรวจสอบ Assumptions ก่อนวิเคราะห์สถิติ (taste_rice) v1.1
#'
#' ตรวจสอบตาม Decision Tree:
#'   Step 1: Normality (Shapiro-Wilk) → ถ้าไม่ผ่าน → Non-parametric ทันที (จบ)
#'   Step 2: Homogeneity of Variance (Levene's) → ทำเฉพาะเมื่อ Normal เท่านั้น
#'
#' ผลลัพธ์จะจำแนกแต่ละตัวแปรเป็น:
#'   "Normal + Equal Variance"   → ใช้ Parametric ได้เลย (Fisher ANOVA, Student's t)
#'   "Normal + Unequal Variance" → ใช้ Welch's ANOVA / Welch's t-test
#'   "Non-normal"                → ใช้ Non-parametric (Kruskal-Wallis, Mann-Whitney)
#'
#' @param data washed_rice object หรือ data.frame
#' @param response character ชื่อคอลัมน์ response (ระบุหลายตัวได้)
#' @param treatment character ชื่อคอลัมน์ treatment (default = "treatment")
#' @param block character ชื่อคอลัมน์ block สำหรับ RCBD (default = NULL = CRD)
#' @param mode "quick" = เช็คจาก raw data, "model" = เช็คจาก residuals,
#'   "both" = ทำทั้งสองแบบ (default)
#' @param alpha ระดับนัยสำคัญ (default = 0.05)
#' @param plot logical แสดง diagnostic plots (default = TRUE)
#' @param verbose logical แสดงรายงาน (default = TRUE)
#'
#' @return object class "tasted_rice" (list):
#'   \item{results}{data.frame สรุป classification ทุกตัวแปร}
#'   \item{details}{list ผลละเอียดรายตัวแปร}
#'   \item{verdict}{คำแนะนำสรุป}
#'   \item{models}{list ของ aov objects}
#'
#' @examples
#' washed <- wash_rice(my_data, design_check = TRUE)
#' result <- taste_rice(washed,
#'   response = c("yield", "severity"),
#'   block    = "rep"
#' )
#' result$results   # ดูตาราง classification
#' result$verdict   # ดูคำแนะนำ
#'
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
  # SETUP
  # ══════════════════════════════════════════════════════════════════
  mode <- match.arg(mode)

  if (inherits(data, "washed_rice")) {
    df <- data$data
  } else {
    df <- as.data.frame(data)
  }

  # ตรวจคอลัมน์
  all_needed <- c(response, treatment)
  if (!is.null(block)) all_needed <- c(all_needed, block)
  missing_cols <- setdiff(all_needed, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "\U0001F6AB ไม่พบคอลัมน์: ", paste(missing_cols, collapse = ", "),
      "\n  คอลัมน์ที่มี: ", paste(names(df), collapse = ", ")
    )
  }

  if (!is.factor(df[[treatment]])) df[[treatment]] <- as.factor(df[[treatment]])
  if (!is.null(block) && !is.factor(df[[block]])) df[[block]] <- as.factor(df[[block]])

  n_groups <- length(levels(df[[treatment]]))

  # containers
  result_rows  <- vector("list", length(response))
  all_details  <- list()
  all_models   <- list()
  all_verdicts <- character(0)


  # ══════════════════════════════════════════════════════════════════
  # HELPERS
  # ══════════════════════════════════════════════════════════════════

  # --- Shapiro-Wilk (safe) ---
  safe_shapiro <- function(x) {
    x <- x[!is.na(x)]
    n <- length(x)
    if (n < 3) {
      return(list(statistic = NA, p.value = NA,
                  note = paste0("n=", n, " (ต้อง >= 3)")))
    }
    if (n > 5000) x <- sample(x, 5000)
    if (sd(x) == 0) {
      return(list(statistic = NA, p.value = NA,
                  note = "sd=0 ค่าทุกตัวเท่ากัน"))
    }
    tryCatch(shapiro.test(x),
             error = function(e) list(statistic = NA, p.value = NA, note = e$message))
  }

  # --- Levene's Test (Brown-Forsythe, base R) ---
  levene_test <- function(y, group) {
    ok <- complete.cases(y, group)
    y <- y[ok]; group <- factor(group[ok])
    if (length(unique(group)) < 2) {
      return(list(statistic = NA, p.value = NA, df = c(NA, NA),
                  note = "ต้องมี >= 2 กลุ่ม"))
    }
    med <- tapply(y, group, median, na.rm = TRUE)
    abs_dev <- abs(y - med[group])
    result <- anova(lm(abs_dev ~ group))
    list(statistic = result$`F value`[1],
         p.value   = result$`Pr(>F)`[1],
         df        = c(result$Df[1], result$Df[2]),
         method    = "Brown-Forsythe (Levene's with median)")
  }

  # --- Bartlett (เสริม) ---
  safe_bartlett <- function(y, group) {
    tryCatch({
      bt <- bartlett.test(y ~ group)
      list(statistic = bt$statistic, p.value = bt$p.value, df = bt$parameter)
    }, error = function(e) list(statistic = NA, p.value = NA, note = e$message))
  }

  # --- Classification label ---
  classify <- function(norm_pass, var_pass) {
    if (is.na(norm_pass)) return("N/A")
    if (!norm_pass)        return("Non-normal")
    if (is.na(var_pass))   return("Normal (variance ทดสอบไม่ได้)")
    if (var_pass)          return("Normal + Equal Variance")
    return("Normal + Unequal Variance")
  }

  # --- Recommendation ---
  recommend <- function(resp, classification, n_groups) {
    test_type <- if (n_groups == 2) "2 กลุ่ม" else paste0(n_groups, " กลุ่ม")

    rec <- switch(classification,
      "Normal + Equal Variance" = {
        if (n_groups == 2)
          "\u2705 ใช้ Student's t-test (independent) ได้เลย"
        else
          paste0("\u2705 ใช้ Fisher's ANOVA (one-way) ได้เลย",
                 " \u2192 ตามด้วย Tukey HSD / LSD")
      },
      "Normal + Unequal Variance" = {
        if (n_groups == 2)
          "\u26A0\uFE0F ใช้ Welch's t-test (ไม่สมมติ equal variance)"
        else
          paste0("\u26A0\uFE0F ใช้ Welch's ANOVA (oneway.test)",
                 " \u2192 ตามด้วย Games-Howell")
      },
      "Non-normal" = {
        if (n_groups == 2)
          paste0("\U0001F6A8 ใช้ Mann-Whitney U test (wilcox.test)")
        else
          paste0("\U0001F6A8 ใช้ Kruskal-Wallis test",
                 " \u2192 ตามด้วย Dunn's test")
      },
      "\u2753 ข้อมูลไม่เพียงพอสำหรับการจำแนก"
    )

    paste0(resp, " [", test_type, "]: ", rec)
  }


  # ══════════════════════════════════════════════════════════════════
  # MAIN LOOP
  # ══════════════════════════════════════════════════════════════════
  for (.i in seq_along(response)) {
    resp <- response[[.i]]

    if (!is.numeric(df[[resp]])) {
      warning("\u26A0\uFE0F '", resp, "' ไม่ใช่ตัวเลข — ข้าม")
      next
    }

    detail     <- list(response = resp)
    result_row <- data.frame(response = resp, stringsAsFactors = FALSE)

    # working data
    working_idx <- complete.cases(df[[resp]], df[[treatment]])
    if (!is.null(block)) working_idx <- working_idx & complete.cases(df[[block]])
    working_df <- df[working_idx, ]
    n_used <- nrow(working_df)
    result_row$n <- n_used

    if (n_used < 5) {
      warning("\u26A0\uFE0F '", resp, "': n=", n_used, " น้อยเกินไป — ข้าม")
      next
    }

    # initialize ตัวแปรสำหรับ scope ด้านนอก
    quick_normal    <- NA
    quick_equal_var <- NA
    model_normal    <- NA
    model_equal_var <- NA
    model           <- NULL


    # ────────────────────────────────────────────────────────────────
    # QUICK MODE
    # ────────────────────────────────────────────────────────────────
    if (mode %in% c("quick", "both")) {

      # ── Step 1: Normality (Shapiro-Wilk per group) ──
      groups    <- split(working_df[[resp]], working_df[[treatment]])
      shap_list <- lapply(groups, safe_shapiro)

      shap_df <- data.frame(
        group   = names(shap_list),
        n       = vapply(groups, function(x) sum(!is.na(x)), integer(1L)),
        W       = round(vapply(shap_list, function(x)
                    if (is.null(x$statistic)) NA_real_ else as.double(x$statistic), double(1L)), 4),
        p_value = round(vapply(shap_list, function(x)
                    if (is.null(x$p.value)) NA_real_ else as.double(x$p.value), double(1L)), 4),
        normal  = vapply(shap_list, function(x) {
          if (is.na(x$p.value)) "N/A"
          else if (x$p.value >= alpha) "Yes" else "No"
        }, character(1L)),
        row.names = NULL, stringsAsFactors = FALSE
      )
      detail$quick_shapiro_per_group <- shap_df

      testable <- shap_df$p_value[!is.na(shap_df$p_value)]
      if (length(testable) > 0) {
        quick_normal     <- all(testable >= alpha)
        quick_shap_min_p <- min(testable)
      } else {
        quick_normal     <- NA
        quick_shap_min_p <- NA
      }

      result_row$quick_shapiro_min_p <- round(quick_shap_min_p, 4)
      result_row$quick_normal <- if (is.na(quick_normal)) "N/A" else if (quick_normal) "Normal" else "Non-normal"

      # ── Step 2: Levene's — เฉพาะเมื่อ Normal ──
      if (!is.na(quick_normal) && quick_normal) {
        lev  <- levene_test(working_df[[resp]], working_df[[treatment]])
        bart <- safe_bartlett(working_df[[resp]], working_df[[treatment]])
        detail$quick_levene   <- lev
        detail$quick_bartlett <- bart

        quick_equal_var <- !is.na(lev$p.value) && lev$p.value >= alpha

        result_row$quick_levene_F <- round(lev$statistic, 4)
        result_row$quick_levene_p <- round(lev$p.value, 4)
        result_row$quick_variance <- if (quick_equal_var) "Equal" else "Unequal"
      } else {
        # Non-normal → ข้าม Levene's
        quick_equal_var <- NA
        result_row$quick_levene_F <- NA
        result_row$quick_levene_p <- NA
        result_row$quick_variance <- "-- (ข้าม: ไม่ปกติ)"
        detail$quick_levene <- list(
          note = "ข้ามการทดสอบ: ข้อมูลไม่ผ่าน Normality → ไปสาย Non-parametric ทันที"
        )
      }

      result_row$quick_class <- classify(quick_normal, quick_equal_var)
    }


    # ────────────────────────────────────────────────────────────────
    # MODEL MODE
    # ────────────────────────────────────────────────────────────────
    if (mode %in% c("model", "both")) {

      rhs <- if (!is.null(block)) c(treatment, block) else treatment
      fml <- reformulate(rhs, response = resp)
      detail$formula <- deparse(fml)

      model <- tryCatch(
        aov(fml, data = working_df),
        error = function(e) {
          warning("\u26A0\uFE0F Fit model ไม่สำเร็จ '", resp, "': ", e$message)
          NULL
        }
      )

      if (!is.null(model)) {
        all_models[[resp]] <- model
        resid_vals    <- residuals(model)
        fitted_groups <- working_df[[treatment]]

        # ── Step 1: Normality of residuals ──
        shap_resid <- safe_shapiro(resid_vals)
        detail$model_shapiro <- shap_resid

        model_shap_p <- if (is.null(shap_resid$p.value)) NA_real_ else shap_resid$p.value
        model_normal <- !is.na(model_shap_p) && model_shap_p >= alpha

        result_row$model_shapiro_W <- round(
          if (is.null(shap_resid$statistic)) NA_real_ else as.double(shap_resid$statistic), 4)
        result_row$model_shapiro_p <- round(model_shap_p, 4)
        result_row$model_normal    <- if (is.na(model_shap_p)) "N/A" else if (model_normal) "Normal" else "Non-normal"

        # ── Step 2: Levene's — เฉพาะเมื่อ Normal ──
        if (model_normal) {
          lev_resid <- levene_test(resid_vals, fitted_groups)
          detail$model_levene <- lev_resid

          model_equal_var <- !is.na(lev_resid$p.value) && lev_resid$p.value >= alpha

          result_row$model_levene_F <- round(lev_resid$statistic, 4)
          result_row$model_levene_p <- round(lev_resid$p.value, 4)
          result_row$model_variance <- if (model_equal_var) "Equal" else "Unequal"
        } else {
          # Non-normal → ข้าม Levene's
          model_equal_var <- NA
          result_row$model_levene_F <- NA
          result_row$model_levene_p <- NA
          result_row$model_variance <- "-- (ข้าม: ไม่ปกติ)"
          detail$model_levene <- list(
            note = "ข้ามการทดสอบ: residuals ไม่ผ่าน Normality → ไปสาย Non-parametric ทันที"
          )
        }

        result_row$model_class <- classify(model_normal, model_equal_var)
        detail$anova_table     <- summary(model)

      } else {
        result_row$model_shapiro_W <- NA
        result_row$model_shapiro_p <- NA
        result_row$model_normal    <- "N/A"
        result_row$model_levene_F  <- NA
        result_row$model_levene_p  <- NA
        result_row$model_variance  <- "N/A"
        result_row$model_class     <- "N/A"
      }
    }


    # ────────────────────────────────────────────────────────────────
    # FINAL CLASSIFICATION
    # ────────────────────────────────────────────────────────────────

    # ใช้ผล model เป็นหลัก (ถูกสถิติกว่า) ถ้าไม่มีค่อยใช้ quick
    if (mode %in% c("model", "both") && !is.null(model)) {
      final_normal    <- model_normal
      final_equal_var <- model_equal_var
      final_source    <- "model (residuals)"
    } else {
      final_normal    <- quick_normal
      final_equal_var <- quick_equal_var
      final_source    <- "quick (raw data)"
    }

    final_class  <- classify(final_normal, final_equal_var)
    verdict_text <- recommend(resp, final_class, n_groups)

    result_row$final_class     <- final_class
    result_row$recommendation  <- sub("^.*: ", "", verdict_text)

    detail$final_class  <- final_class
    detail$final_source <- final_source
    detail$verdict      <- verdict_text

    all_verdicts        <- c(all_verdicts, verdict_text)
    all_details[[resp]] <- detail
    result_rows[[.i]]   <- result_row
  }

  all_results <- do.call(rbind, Filter(Negate(is.null), result_rows))
  if (is.null(all_results)) all_results <- data.frame()


  # ══════════════════════════════════════════════════════════════════
  # DIAGNOSTIC PLOTS
  # ══════════════════════════════════════════════════════════════════
  if (plot && length(all_details) > 0) {

    for (resp in names(all_details)) {

      det <- all_details[[resp]]
      working_idx <- complete.cases(df[[resp]], df[[treatment]])
      if (!is.null(block)) working_idx <- working_idx & complete.cases(df[[block]])
      working_df <- df[working_idx, ]
      colors <- grDevices::hcl.colors(n_groups, palette = "Set2")
      fc <- det$final_class

      if (mode == "both") {
        old_par <- par(no.readonly = TRUE)
        par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))

        # Row 1: Quick
        qqnorm(working_df[[resp]],
               main = "QQ Plot (raw data)",
               pch = 16, col = "grey60", cex = 0.8)
        qqline(working_df[[resp]], col = "red", lwd = 2)

        boxplot(as.formula(paste(resp, "~", treatment)),
                data = working_df, main = "Boxplot by group (raw)",
                col = colors, las = 1, ylab = resp)

        # Row 2: Model
        if (!is.null(all_models[[resp]])) {
          mdl <- all_models[[resp]]
          res <- residuals(mdl)
          fit <- fitted(mdl)

          qqnorm(res, main = "QQ Plot (residuals)",
                 pch = 16, col = "steelblue", cex = 0.8)
          qqline(res, col = "red", lwd = 2)

          plot(fit, res, main = "Residuals vs Fitted",
               xlab = "Fitted", ylab = "Residuals",
               pch = 16, col = "steelblue", cex = 0.8)
          abline(h = 0, col = "red", lty = 2, lwd = 2)
          lines(lowess(fit, res), col = "orange", lwd = 2)
        }

        title(main = paste0("\U0001F33E ", resp, "  \u2014  ", fc),
              outer = TRUE, cex.main = 1.3)
        par(old_par)

      } else if (mode == "quick") {
        old_par <- par(no.readonly = TRUE)
        par(mfrow = c(1, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))

        qqnorm(working_df[[resp]], main = "QQ Plot (raw data)",
               pch = 16, col = "grey60", cex = 0.8)
        qqline(working_df[[resp]], col = "red", lwd = 2)

        boxplot(as.formula(paste(resp, "~", treatment)),
                data = working_df, main = "Boxplot by group",
                col = colors, las = 1, ylab = resp)

        title(main = paste0("\U0001F33E ", resp, "  \u2014  ", fc),
              outer = TRUE, cex.main = 1.3)
        par(old_par)

      } else if (mode == "model" && !is.null(all_models[[resp]])) {
        old_par <- par(no.readonly = TRUE)
        par(mfrow = c(1, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))

        mdl <- all_models[[resp]]
        res <- residuals(mdl)
        fit <- fitted(mdl)

        qqnorm(res, main = "QQ Plot (residuals)",
               pch = 16, col = "steelblue", cex = 0.8)
        qqline(res, col = "red", lwd = 2)

        plot(fit, res, main = "Residuals vs Fitted",
             xlab = "Fitted", ylab = "Residuals",
             pch = 16, col = "steelblue", cex = 0.8)
        abline(h = 0, col = "red", lty = 2, lwd = 2)
        lines(lowess(fit, res), col = "orange", lwd = 2)

        title(main = paste0("\U0001F33E ", resp, "  \u2014  ", fc),
              outer = TRUE, cex.main = 1.3)
        par(old_par)
      }
    }
  }


  # ══════════════════════════════════════════════════════════════════
  # VERBOSE SUMMARY
  # ══════════════════════════════════════════════════════════════════
  if (verbose) {
    message("\n", strrep("\U0001F33E", 25))
    message("\U0001F37A  taste_rice v1.1 — ผลการชิมข้าว")
    message(strrep("\u2500", 60))
    message("  Mode:      ", mode, "  |  Alpha: ", alpha)
    message("  Treatment: ", treatment, " (", n_groups, " กลุ่ม)")
    if (!is.null(block)) message("  Block:     ", block)
    message(strrep("\u2500", 60))

    # Decision flow
    message("\n  \U0001F4CB Decision Flow:")
    message("  \u250C\u2500 Step 1: Shapiro-Wilk (Normality)")
    message("  \u2502   \u251C\u2500 p > ", alpha,
            " \u2192 \u2705 Normal \u2192 ไป Step 2")
    message("  \u2502   \u2514\u2500 p < ", alpha,
            " \u2192 \U0001F6A8 Non-normal \u2192 Non-parametric (จบ)")
    message("  \u2502")
    message("  \u2514\u2500 Step 2: Levene's Test (Variance) [เฉพาะ Normal]")
    message("      \u251C\u2500 p > ", alpha,
            " \u2192 \u2705 Equal Variance \u2192 Parametric")
    message("      \u2514\u2500 p < ", alpha,
            " \u2192 \u26A0\uFE0F Unequal Variance \u2192 Welch's")
    message(strrep("\u2500", 60))

    # ผลแต่ละตัวแปร
    message("\n  \U0001F4CA ผลการจำแนก:")
    for (i in seq_len(nrow(all_results))) {
      row <- all_results[i, ]
      message(strrep("\u2500", 60))
      message("  \U0001F33E ", row$response, " (n=", row$n, ")")

      # Normality
      if ("model_normal" %in% names(row) && !is.na(row$model_shapiro_p)) {
        message("     [Model] Shapiro-Wilk: W=", row$model_shapiro_W,
                ", p=", row$model_shapiro_p,
                " \u2192 ", row$model_normal)
      }
      if ("quick_normal" %in% names(row) && !is.na(row$quick_shapiro_min_p)) {
        message("     [Quick] Shapiro-Wilk min p=", row$quick_shapiro_min_p,
                " \u2192 ", row$quick_normal)
      }

      # Variance (ถ้าทำ)
      if ("model_variance" %in% names(row) &&
          !is.na(row$model_levene_p) && !grepl("\u0e02\u0e49\u0e32\u0e21", row$model_variance)) {
        message("     [Model] Levene's: F=", row$model_levene_F,
                ", p=", row$model_levene_p,
                " \u2192 ", row$model_variance, " Variance")
      } else if ("model_variance" %in% names(row) &&
                 grepl("\u0e02\u0e49\u0e32\u0e21", row$model_variance)) {
        message("     [Model] Levene's: ข้ามการทดสอบ (ไม่ผ่าน Normality)")
      }

      if ("quick_variance" %in% names(row) &&
          !is.na(row$quick_levene_p) && !grepl("\u0e02\u0e49\u0e32\u0e21", row$quick_variance)) {
        message("     [Quick] Levene's: F=", row$quick_levene_F,
                ", p=", row$quick_levene_p,
                " \u2192 ", row$quick_variance, " Variance")
      } else if ("quick_variance" %in% names(row) &&
                 grepl("\u0e02\u0e49\u0e32\u0e21", row$quick_variance)) {
        message("     [Quick] Levene's: ข้ามการทดสอบ (ไม่ผ่าน Normality)")
      }

      # สรุป
      message("     ", strrep("\u2500", 40))
      message("     \U0001F3F7\uFE0F  Classification: ", row$final_class)
      message("     ", row$recommendation)
    }

    message(strrep("\u2500", 60))
    message("\U0001F4CB ดูผลละเอียด: $results | $details | $models")
    message(strrep("\U0001F33E", 25))
  }


  # ══════════════════════════════════════════════════════════════════
  # RETURN
  # ══════════════════════════════════════════════════════════════════
  structure(
    list(
      results  = all_results,
      details  = all_details,
      verdict  = all_verdicts,
      models   = if (length(all_models) > 0) all_models else NULL,
      settings = list(
        mode = mode, alpha = alpha,
        treatment = treatment, block = block,
        response = response, n_groups = n_groups
      )
    ),
    class = c("tasted_rice", "list")
  )
}


# ══════════════════════════════════════════════════════════════════════
# Print method
# ══════════════════════════════════════════════════════════════════════

#' @export
print.tasted_rice <- function(x, ...) {
  cat("\u2500\u2500 tasted_rice v1.1 \u2500\u2500\n")
  cat("  Mode:", x$settings$mode, " | Alpha:", x$settings$alpha, "\n")
  cat("  Groups:", x$settings$n_groups, "\n\n")

  if (nrow(x$results) > 0) {
    display <- x$results[, c("response", "n", "final_class", "recommendation"),
                          drop = FALSE]
    print(display, row.names = FALSE, right = FALSE)
  }

  cat("\n")
  for (v in x$verdict) cat(" ", v, "\n")

  cat("\n  $results  \u2192 full table\n")
  cat("  $details  \u2192 per-variable stats\n")
  cat("  $models   \u2192 aov objects\n")
  invisible(x)
}