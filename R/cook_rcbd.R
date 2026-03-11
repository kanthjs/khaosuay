#' หุงข้าว RCBD — Randomized Complete Block Design
#'
#' วิเคราะห์สถิติแปลงทดลองแบบ RCBD รองรับ Single Factor และ Factorial in RCBD
#' รับผลจาก taste_rice() เพื่อเลือก Parametric / Non-parametric อัตโนมัติ
#'
#' @param data washed_rice object หรือ data.frame
#' @param response character ชื่อ response variable (ระบุหลายตัวได้)
#' @param treatment character ชื่อคอลัมน์ treatment (single factor)
#' @param block character ชื่อคอลัมน์ block/rep (default = "rep")
#' @param factors character vector ชื่อปัจจัย (สำหรับ factorial in RCBD)
#' @param tasted tasted_rice object จาก taste_rice() — ถ้ามีจะไม่เช็คซ้ำ
#' @param posthoc character วิธี post-hoc: "tukey" (default), "lsd", "duncan"
#' @param alpha numeric ระดับนัยสำคัญ (default = 0.05)
#' @param verbose logical แสดงรายงาน (default = TRUE)
#'
#' @return object class "cooked_rice" (list) ประกอบด้วย:
#'   \item{results}{list ผลวิเคราะห์แยกรายตัวแปร}
#'   \item{design}{"RCBD"}
#'   \item{data}{data.frame ที่ใช้วิเคราะห์}
#'   \item{settings}{list ค่าที่ตั้ง}
#'
#' @examples
#' \dontrun{
#' # Single factor RCBD
#' washed <- wash_rice(my_data, design_check = TRUE)
#' tasted <- taste_rice(washed, response = "yield", block = "rep")
#' cooked <- cook_rcbd(washed, response = "yield", block = "rep",
#'                     tasted = tasted)
#'
#' # Factorial in RCBD
#' cooked <- cook_rcbd(washed, response = "yield",
#'                     factors = c("variety", "chemical"),
#'                     block = "rep", tasted = tasted)
#'
#' # ดูผล
#' cooked$results$yield$summary_table
#' cooked$results$yield$cv_percent
#' }
#' @export
cook_rcbd <- function(data,
                      response,
                      treatment = "treatment",
                      block     = "rep",
                      factors   = NULL,
                      tasted    = NULL,
                      posthoc   = c("tukey", "lsd", "duncan"),
                      alpha     = 0.05,
                      verbose   = TRUE) {

  .check_agricolae()
  posthoc <- match.arg(posthoc)
  df <- .extract_df(data)

  # Block ต้องเป็น factor
  if (!is.factor(df[[block]])) df[[block]] <- as.factor(df[[block]])

  # กำหนด factors
  is_factorial <- !is.null(factors) && length(factors) > 1
  if (is_factorial) {
    for (f in factors) if (!is.factor(df[[f]])) df[[f]] <- as.factor(df[[f]])
    df$.interaction <- interaction(df[, factors], sep = ":")
  } else {
    if (!is.null(factors) && length(factors) == 1) treatment <- factors[1]
    if (!is.factor(df[[treatment]])) df[[treatment]] <- as.factor(df[[treatment]])
  }

  all_results <- list()

  for (resp in response) {
    if (!is.numeric(df[[resp]])) {
      warning("'", resp, "' \u0e44\u0e21\u0e48\u0e43\u0e0a\u0e48\u0e15\u0e31\u0e27\u0e40\u0e25\u0e02 \u2014 \u0e02\u0e49\u0e32\u0e21")
      next
    }

    res <- list(response = resp, design = "RCBD")

    # ── Formula (+ block) ──
    if (is_factorial) {
      fml <- reformulate(
        c(paste(factors, collapse = " * "), block), response = resp
      )
      group_col <- ".interaction"
    } else {
      fml <- reformulate(c(treatment, block), response = resp)
      group_col <- treatment
    }
    res$formula <- deparse(fml)

    # ── Working data ──
    needed <- c(resp, block, if (is_factorial) factors else treatment)
    working_df <- df[complete.cases(df[, needed]), ]
    res$n <- nrow(working_df)

    # ── เลือก analysis path ──
    tasted_class <- .get_classification(tasted, resp)

    if (!is.null(tasted_class)) {
      analysis_path <- .class_to_path(tasted_class)
      res$assumption <- list(class = tasted_class, path = analysis_path,
                             source = "taste_rice")
    } else {
      model_tmp <- aov(fml, data = working_df)
      res$assumption <- .quick_assumption(
        residuals(model_tmp), working_df[[group_col]], alpha
      )
      res$assumption$source <- "auto_check"
      analysis_path <- res$assumption$path
    }

    # ══════════════════════════════════════════════════════════════
    # PARAMETRIC PATH
    # ══════════════════════════════════════════════════════════════
    if (analysis_path %in% c("parametric", "welch")) {

      model <- aov(fml, data = working_df)
      res$test_name <- "Fisher's ANOVA (RCBD)"

      if (analysis_path == "welch") {
        res$test_name <- paste0(res$test_name,
          " [\u26A0\uFE0F variance \u0e44\u0e21\u0e48\u0e40\u0e17\u0e48\u0e32\u0e01\u0e31\u0e19 \u2014 \u0e04\u0e27\u0e23 transform \u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25]")
      }

      res$anova_table <- summary(model)
      res$model <- model

      # ดึง p-value
      aov_s <- summary(model)[[1]]
      if (is_factorial) {
        all_terms <- trimws(rownames(aov_s))
        keep <- all_terms != "Residuals" & all_terms != trimws(block)
        res$p_values <- setNames(aov_s$`Pr(>F)`[keep], all_terms[keep])
        res$p_value  <- min(res$p_values, na.rm = TRUE)
      } else {
        res$p_value <- aov_s$`Pr(>F)`[1]
      }

      # CV%
      df_error <- aov_s$Df[nrow(aov_s)]
      ms_error <- aov_s$`Mean Sq`[nrow(aov_s)]
      res$cv_percent <- round(
        sqrt(ms_error) / mean(working_df[[resp]], na.rm = TRUE) * 100, 2
      )
      res$mse <- ms_error
      res$df_error <- df_error

      # POST-HOC
      if (!is.na(res$p_value) && res$p_value < alpha) {
        res$posthoc_needed <- TRUE
        ph_col <- if (is_factorial) ".interaction" else treatment

        ph <- .run_posthoc_parametric(model, ph_col, df_error, ms_error,
                                      posthoc, alpha)
        res$posthoc_name   <- ph$name
        res$posthoc_result <- ph$result
        res$posthoc_groups <- ph$groups
        res$group_letters  <- ph$letters

        # Factorial: post-hoc ตาม main effect
        if (is_factorial) {
          res$posthoc_by_factor <- list()
          for (f in factors) {
            p_f <- res$p_values[trimws(f)]
            if (!is.na(p_f) && p_f < alpha) {
              ph_f <- .run_posthoc_parametric(model, f, df_error, ms_error,
                                              "tukey", alpha)
              res$posthoc_by_factor[[f]] <- ph_f$groups
            }
          }
        }
      } else {
        res$posthoc_needed <- FALSE
        res$group_letters  <- NULL
      }

    # ══════════════════════════════════════════════════════════════
    # NON-PARAMETRIC PATH
    # ══════════════════════════════════════════════════════════════
    } else {
      kw_col <- if (is_factorial) ".interaction" else treatment

      # Friedman (ถูกหลัก RCBD) → fallback Kruskal-Wallis
      friedman_ok <- tryCatch({
        ft <- friedman.test(
          as.formula(paste(resp, "~", kw_col, "|", block)),
          data = working_df
        )
        TRUE
      }, error = function(e) FALSE)

      if (friedman_ok) {
        res$test_name   <- "Friedman's Test (non-parametric RCBD)"
        res$test_result <- ft
        res$p_value     <- ft$p.value
      } else {
        kw <- kruskal.test(reformulate(kw_col, response = resp),
                           data = working_df)
        res$test_name   <- "Kruskal-Wallis (fallback)"
        res$test_result <- kw
        res$p_value     <- kw$p.value
      }
      res$cv_percent <- NA

      # POST-HOC
      if (!is.na(res$p_value) && res$p_value < alpha) {
        res$posthoc_needed <- TRUE
        ph <- .run_posthoc_nonparametric(working_df[[resp]],
                                         working_df[[kw_col]], alpha)
        res$posthoc_name   <- ph$name
        res$posthoc_result <- ph$result
        res$posthoc_groups <- ph$groups
        res$group_letters  <- ph$letters
      } else {
        res$posthoc_needed <- FALSE
        res$group_letters  <- NULL
      }
    }

    # Summary table
    res$summary_table <- .make_summary_table(
      working_df, resp, group_col, res$group_letters
    )
    all_results[[resp]] <- res
  }

  if (verbose) .print_cook_results(all_results, "RCBD", alpha)

  structure(
    list(results = all_results, design = "RCBD", data = df,
         settings = list(response = response, treatment = treatment,
                         block = block, factors = factors,
                         posthoc = posthoc, alpha = alpha,
                         is_factorial = is_factorial,
                         tasted_provided = !is.null(tasted))),
    class = c("cooked_rice", "list")
  )
}
