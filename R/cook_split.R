#' หุงข้าว Split-plot — Split-plot Design
#'
#' วิเคราะห์สถิติแปลงทดลองแบบ Split-plot ที่มี error term สองระดับ:
#'   Main-plot error: ใช้ทดสอบ main-plot factor
#'   Sub-plot error:  ใช้ทดสอบ sub-plot factor และ interaction
#'
#' Formula ที่สร้างอัตโนมัติ:
#'   y ~ main_plot * sub_plot + Error(block/main_plot)
#'
#' @param data washed_rice object หรือ data.frame
#' @param response character ชื่อ response variable (ระบุหลายตัวได้)
#' @param main_plot character ชื่อ main-plot factor
#'   (ปัจจัยที่สุ่มในระดับแปลงใหญ่ เช่น พันธุ์ข้าว, วิธีไถ)
#' @param sub_plot character ชื่อ sub-plot factor
#'   (ปัจจัยที่สุ่มในระดับแปลงย่อย เช่น อัตราปุ๋ย, สารเคมี)
#' @param block character ชื่อ block/rep (default = "rep")
#' @param tasted tasted_rice object จาก taste_rice() (optional)
#' @param posthoc character วิธี post-hoc: "tukey" (default), "lsd", "duncan"
#' @param alpha numeric ระดับนัยสำคัญ (default = 0.05)
#' @param verbose logical แสดงรายงาน (default = TRUE)
#'
#' @return object class "cooked_rice" (list)
#'
#' @details
#' Split-plot design ใช้เมื่อปัจจัยหนึ่งยากต่อการสุ่มในระดับเล็ก
#' เช่น พันธุ์ข้าว (ต้องปลูกเป็นแปลงใหญ่) x อัตราปุ๋ย (สุ่มในแปลงย่อย)
#'
#' ANOVA table แยกเป็น 2 ส่วน:
#'   Main-plot stratum: block, main_plot, Error(block:main_plot)
#'   Sub-plot stratum:  sub_plot, main_plot:sub_plot, Residuals
#'
#' Post-hoc ใช้ error term ที่ถูกต้องตามระดับ:
#'   main_plot effect   -> main-plot error (block:main_plot)
#'   sub_plot effect    -> sub-plot error (Residuals)
#'   interaction        -> sub-plot error (Residuals)
#'
#' @examples
#' washed <- wash_rice(my_data)
#' cooked <- cook_split(washed,
#'   response  = "yield",
#'   main_plot = "variety",
#'   sub_plot  = "fertilizer",
#'   block     = "rep"
#' )
#' cooked$results$yield$anova_table
#' cooked$results$yield$posthoc_main
#' cooked$results$yield$posthoc_sub
#'
#' @export
cook_split <- function(data,
                       response,
                       main_plot,
                       sub_plot,
                       block     = "rep",
                       tasted    = NULL,
                       posthoc   = c("tukey", "lsd", "duncan"),
                       alpha     = 0.05,
                       verbose   = TRUE) {

  .check_agricolae()
  posthoc <- match.arg(posthoc)
  df <- .extract_df(data)

  # Factor conversion
  for (col in c(main_plot, sub_plot, block)) {
    if (!col %in% names(df))
      stop("\U0001F6AB ไม่พบคอลัมน์ '", col, "'")
    if (!is.factor(df[[col]])) df[[col]] <- as.factor(df[[col]])
  }

  df$.interaction <- interaction(df[[main_plot]], df[[sub_plot]], sep = ":")

  all_results <- list()

  for (resp in response) {
    if (!is.numeric(df[[resp]])) {
      warning("'", resp, "' ไม่ใช่ตัวเลข — ข้าม")
      next
    }

    res <- list(response = resp, design = "Split-plot")

    # Working data
    needed <- c(resp, main_plot, sub_plot, block)
    working_df <- df[complete.cases(df[, needed]), ]
    res$n <- nrow(working_df)
    res$n_levels <- list(
      main_plot = nlevels(working_df[[main_plot]]),
      sub_plot  = nlevels(working_df[[sub_plot]]),
      block     = nlevels(working_df[[block]])
    )

    # Analysis path
    tasted_class <- .get_classification(tasted, resp)
    if (!is.null(tasted_class)) {
      analysis_path <- .class_to_path(tasted_class)
      res$assumption <- list(class = tasted_class, path = analysis_path,
                             source = "taste_rice")
    } else {
      fml_flat <- reformulate(
        c(paste(main_plot, sub_plot, sep = " * "), block), response = resp
      )
      model_tmp <- aov(fml_flat, data = working_df)
      res$assumption <- .quick_assumption(
        residuals(model_tmp), working_df$.interaction, alpha
      )
      res$assumption$source <- "auto_check"
      analysis_path <- res$assumption$path
    }

    # ══════════════════════════════════════════════════════════════
    # PARAMETRIC PATH
    # ══════════════════════════════════════════════════════════════
    if (analysis_path %in% c("parametric", "welch")) {

      # Split-plot formula
      fml_str <- paste0(resp, " ~ ", main_plot, " * ", sub_plot,
                        " + Error(", block, "/", main_plot, ")")
      fml <- as.formula(fml_str)
      res$formula <- fml_str

      res$test_name <- "Split-plot ANOVA"
      if (analysis_path == "welch") {
        res$test_name <- paste0(res$test_name,
          " [\u26A0\uFE0F variance ไม่เท่ากัน — ควร transform]")
      }

      # Fit split-plot
      model_split <- aov(fml, data = working_df)
      res$model <- model_split
      res$anova_table <- summary(model_split)

      # Flat model สำหรับ agricolae (ต้องระบุ DFerror/MSerror เอง)
      fml_flat <- reformulate(
        c(block, main_plot, paste0(block, ":", main_plot),
          sub_plot, paste0(main_plot, ":", sub_plot)),
        response = resp
      )
      model_flat <- aov(fml_flat, data = working_df)
      res$model_flat <- model_flat
      aov_flat <- summary(model_flat)[[1]]
      flat_terms <- trimws(rownames(aov_flat))

      # ── Error terms ──
      # Main-plot error = block:main_plot
      mp_err_name <- paste0(block, ":", main_plot)
      mp_idx <- which(flat_terms == mp_err_name)
      if (length(mp_idx) == 0) {
        mp_err_name <- paste0(main_plot, ":", block)
        mp_idx <- which(flat_terms == mp_err_name)
      }

      if (length(mp_idx) > 0) {
        df_mp <- aov_flat$Df[mp_idx]
        ms_mp <- aov_flat$`Mean Sq`[mp_idx]
      } else {
        warning("\u26A0\uFE0F ไม่พบ main-plot error — ใช้ Residuals แทน")
        df_mp <- aov_flat$Df[nrow(aov_flat)]
        ms_mp <- aov_flat$`Mean Sq`[nrow(aov_flat)]
      }

      # Sub-plot error = Residuals
      df_sp <- aov_flat$Df[nrow(aov_flat)]
      ms_sp <- aov_flat$`Mean Sq`[nrow(aov_flat)]

      res$error_terms <- list(
        main_plot = list(df = df_mp, ms = ms_mp),
        sub_plot  = list(df = df_sp, ms = ms_sp)
      )

      # ── p-values จาก split-plot ANOVA ──
      p_values <- c()
      for (stratum_name in names(res$anova_table)) {
        st <- res$anova_table[[stratum_name]]
        if (!is.null(st) && is.data.frame(st[[1]])) {
          st_df <- st[[1]]
          for (j in seq_len(nrow(st_df))) {
            term <- trimws(rownames(st_df)[j])
            if (term != "Residuals" && !is.na(st_df$`Pr(>F)`[j])) {
              p_values[term] <- st_df$`Pr(>F)`[j]
            }
          }
        }
      }
      res$p_values <- p_values
      res$p_value  <- if (length(p_values) > 0) min(p_values, na.rm = TRUE) else NA

      # CV%
      grand_mean <- mean(working_df[[resp]], na.rm = TRUE)
      res$cv_percent_mainplot <- round(sqrt(ms_mp) / grand_mean * 100, 2)
      res$cv_percent_subplot  <- round(sqrt(ms_sp) / grand_mean * 100, 2)

      # ── POST-HOC (แยกตาม error term) ──
      res$posthoc_main <- NULL
      res$posthoc_sub  <- NULL
      res$posthoc_interaction <- NULL

      # Main-plot → main-plot error
      p_main <- p_values[main_plot]
      if (!is.null(p_main) && !is.na(p_main) && p_main < alpha) {
        res$posthoc_main <- .run_posthoc_parametric(
          model_flat, main_plot, df_mp, ms_mp, posthoc, alpha
        )
      }

      # Sub-plot → sub-plot error
      p_sub <- p_values[sub_plot]
      if (!is.null(p_sub) && !is.na(p_sub) && p_sub < alpha) {
        res$posthoc_sub <- .run_posthoc_parametric(
          model_flat, sub_plot, df_sp, ms_sp, posthoc, alpha
        )
      }

      # Interaction → sub-plot error
      int_term <- paste0(main_plot, ":", sub_plot)
      p_int <- p_values[int_term]
      if (is.null(p_int) || is.na(p_int)) {
        int_term <- paste0(sub_plot, ":", main_plot)
        p_int <- p_values[int_term]
      }
      if (!is.null(p_int) && !is.na(p_int) && p_int < alpha) {
        res$posthoc_interaction <- .run_posthoc_parametric(
          model_flat, c(main_plot, sub_plot), df_sp, ms_sp, posthoc, alpha
        )
        res$interaction_significant <- TRUE
      } else {
        res$interaction_significant <- FALSE
      }

      res$posthoc_needed <- !is.null(res$posthoc_main) ||
                            !is.null(res$posthoc_sub) ||
                            !is.null(res$posthoc_interaction)
      if (res$posthoc_needed) {
        res$posthoc_name <- paste0(
          switch(posthoc,
            tukey  = "Tukey's HSD",
            lsd    = "Fisher's LSD (Bonferroni)",
            duncan = "Duncan's MRT"
          ), " (แยกตาม error term)")
      }

      # group_letters
      if (!is.null(res$posthoc_interaction)) {
        res$group_letters <- res$posthoc_interaction$letters
      } else if (!is.null(res$posthoc_main)) {
        res$group_letters <- res$posthoc_main$letters
      } else if (!is.null(res$posthoc_sub)) {
        res$group_letters <- res$posthoc_sub$letters
      } else {
        res$group_letters <- NULL
      }

    # ══════════════════════════════════════════════════════════════
    # NON-PARAMETRIC PATH
    # ══════════════════════════════════════════════════════════════
    } else {
      res$formula   <- paste(resp, "~", main_plot, "*", sub_plot,
                             "(non-parametric)")
      res$test_name <- "Kruskal-Wallis (Split-plot non-parametric)"

      kw_main <- kruskal.test(
        reformulate(main_plot, response = resp), data = working_df)
      kw_sub <- kruskal.test(
        reformulate(sub_plot, response = resp), data = working_df)
      kw_int <- kruskal.test(
        reformulate(".interaction", response = resp), data = working_df)

      res$p_values <- c(
        setNames(kw_main$p.value, main_plot),
        setNames(kw_sub$p.value, sub_plot),
        setNames(kw_int$p.value, "interaction")
      )
      res$p_value <- min(res$p_values, na.rm = TRUE)
      res$cv_percent_subplot  <- NA
      res$cv_percent_mainplot <- NA

      res$posthoc_main <- NULL
      res$posthoc_sub  <- NULL
      res$posthoc_interaction <- NULL

      if (kw_main$p.value < alpha) {
        res$posthoc_main <- .run_posthoc_nonparametric(
          working_df[[resp]], working_df[[main_plot]], alpha)
      }
      if (kw_sub$p.value < alpha) {
        res$posthoc_sub <- .run_posthoc_nonparametric(
          working_df[[resp]], working_df[[sub_plot]], alpha)
      }
      if (kw_int$p.value < alpha) {
        res$posthoc_interaction <- .run_posthoc_nonparametric(
          working_df[[resp]], working_df$.interaction, alpha)
        res$interaction_significant <- TRUE
      } else {
        res$interaction_significant <- FALSE
      }

      res$posthoc_needed <- !is.null(res$posthoc_main) ||
                            !is.null(res$posthoc_sub) ||
                            !is.null(res$posthoc_interaction)
      if (res$posthoc_needed) res$posthoc_name <- "Kruskal + Dunn's (Bonferroni)"

      if (!is.null(res$posthoc_interaction)) {
        res$group_letters <- res$posthoc_interaction$letters
      } else {
        res$group_letters <- NULL
      }
    }

    # Summary tables
    res$summary_table <- .make_summary_table(
      working_df, resp, ".interaction", res$group_letters
    )
    res$summary_main <- .make_summary_table(
      working_df, resp, main_plot,
      if (!is.null(res$posthoc_main)) res$posthoc_main$letters else NULL
    )
    res$summary_sub <- .make_summary_table(
      working_df, resp, sub_plot,
      if (!is.null(res$posthoc_sub)) res$posthoc_sub$letters else NULL
    )

    all_results[[resp]] <- res
  }

  if (verbose) .print_split_results(all_results, main_plot, sub_plot, block, alpha)

  structure(
    list(results = all_results, design = "Split-plot", data = df,
         settings = list(response = response, main_plot = main_plot,
                         sub_plot = sub_plot, block = block,
                         posthoc = posthoc, alpha = alpha,
                         tasted_provided = !is.null(tasted))),
    class = c("cooked_rice", "list")
  )
}


# ══════════════════════════════════════════════════════════════════════
# Verbose print
# ══════════════════════════════════════════════════════════════════════

.print_split_results <- function(all_results, main_plot, sub_plot,
                                  block, alpha) {

  message("\n", strrep("\U0001F33E", 25))
  message("\U0001F35A  cook_split v1.0 — ผลการหุงข้าว (Split-plot)")
  message(strrep("\u2500", 60))
  message("  Main-plot:  ", main_plot)
  message("  Sub-plot:   ", sub_plot)
  message("  Block:      ", block)
  message(strrep("\u2500", 60))

  for (resp_name in names(all_results)) {
    res <- all_results[[resp_name]]
    message(strrep("\u2500", 60))
    message("\U0001F33E ", resp_name, " (n=", res$n, ")")
    message("  Formula:     ", res$formula)

    src <- res$assumption$source
    src_label <- if (src == "taste_rice") {
      "\u2705 จาก taste_rice"
    } else {
      "\U0001F50D auto_check"
    }
    message("  Assumption:  ", res$assumption$class, "  [", src_label, "]")
    message("  Test:        ", res$test_name)

    # p-values ตาม term
    if (length(res$p_values) > 0) {
      message("\n  p-values:")
      for (i in seq_along(res$p_values)) {
        p <- res$p_values[i]
        nm <- names(res$p_values)[i]
        sig <- if (is.na(p)) "" else if (p < 0.001) " ***"
               else if (p < 0.01) " **" else if (p < 0.05) " *" else " ns"
        err <- if (nm == main_plot) " [main-plot error]" else " [sub-plot error]"
        message("    ", formatC(nm, width = 25, flag = "-"),
                formatC(p, format = "g", digits = 4, width = 10), sig, err)
      }
    }

    if (!is.na(res$cv_percent_mainplot)) {
      message("\n  CV% (main-plot): ", res$cv_percent_mainplot, "%")
      message("  CV% (sub-plot):  ", res$cv_percent_subplot, "%")
    }

    # Post-hoc
    .ph_section <- function(ph, label, dimmed = FALSE) {
      if (is.null(ph)) return()
      prefix <- if (dimmed) "  \U0001F4CC" else "  \U0001F4CA"
      message("\n", prefix, " Post-Hoc [", label, "]: ", ph$name)
      if (dimmed) {
        message("     \u26A0\uFE0F (อ้างอิงเท่านั้น — Interaction มีนัยสำคัญ ดูตารางจับคู่ผสมเป็นหลัก)")
      }
      message("  ", strrep("-", 45))
      grp <- ph$groups
      if (!is.null(grp)) {
        nms <- rownames(grp)
        w <- max(nchar(nms))
        for (i in seq_len(nrow(grp))) {
          message("    ", formatC(nms[i], width = w, flag = "-"),
                  "  mean=", formatC(grp[i, 1], width = 8, format = "f", digits = 2),
                  "  [", trimws(grp[i, 2]), "]")
        }
      }
    }

    int_sig <- isTRUE(res$interaction_significant)

    if (int_sig) {
      # Interaction มีนัยสำคัญ → แสดง interaction เป็นหลัก
      message("\n  \u2757 Interaction มีนัยสำคัญ (p < ", alpha, ")")
      message("     ผลของ ", main_plot, " ขึ้นอยู่กับระดับของ ", sub_plot,
              " (และในทางกลับกัน)")
      message("     \u2192 แนะนำให้อ่านผลจากตารางจับคู่ผสม (interaction) เป็นหลัก")

      .ph_section(res$posthoc_interaction, "interaction * (sub-plot error)")
      .ph_section(res$posthoc_main, paste(main_plot, "(main-plot error)"), dimmed = TRUE)
      .ph_section(res$posthoc_sub, paste(sub_plot, "(sub-plot error)"), dimmed = TRUE)
    } else {
      # Interaction ไม่มีนัยสำคัญ → แสดง main/sub ตามปกติ
      .ph_section(res$posthoc_main, paste(main_plot, "(main-plot error)"))
      .ph_section(res$posthoc_sub, paste(sub_plot, "(sub-plot error)"))
    }

    if (!res$posthoc_needed) {
      message("\n  \U0001F44D ไม่มีความแตกต่าง (p >= ", alpha, ")")
    }
  }

  message(strrep("\u2500", 60))
  message("\U0001F4CB $results$<var>$anova_table      → Split-plot ANOVA")
  message("   $results$<var>$summary_table   → ตาราง interaction")
  message("   $results$<var>$summary_main    → ตาราง main-plot")
  message("   $results$<var>$summary_sub     → ตาราง sub-plot")
  message("   $results$<var>$posthoc_main    → post-hoc (main-plot error)")
  message("   $results$<var>$posthoc_sub     → post-hoc (sub-plot error)")
  message("   $results$<var>$posthoc_interaction → post-hoc interaction")
  message("   $results$<var>$error_terms     → df/MS ของแต่ละ error term")
  message(strrep("\U0001F33E", 25))
}