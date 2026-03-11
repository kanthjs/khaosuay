#' ทำความสะอาดและเตรียมข้อมูลเกษตร (ล้างข้าวสาร) v3.1 — Smart Mapper + Full Pipeline
#'
#' ฟังก์ชันนี้ออกแบบสำหรับ cleaning ข้อมูลแปลงทดลองและข้อมูลสำรวจทางการเกษตร
#' รองรับ dataframe ที่ตั้งชื่อคอลัมน์หลากหลาย ทั้งภาษาไทยและอังกฤษ
#' ผ่านระบบ Smart Alias Dictionary ที่จับคู่ชื่อคอลัมน์อัตโนมัติ
#'
#' @param data data.frame หรือ tibble
#' @param treatment_col ระบุชื่อคอลัมน์ Treatment ชัดเจน (ถ้าไม่ระบุจะเดาจาก dictionary)
#' @param rep_col ระบุชื่อคอลัมน์ Rep/Block ชัดเจน (ถ้าไม่ระบุจะเดาจาก dictionary)
#' @param factor_cols คอลัมน์เพิ่มเติมที่อยากบังคับเป็น factor
#' @param numeric_cols คอลัมน์ที่ต้องเป็นตัวเลข (ถ้าไม่ระบุจะ auto-detect)
#' @param valid_ranges named list กำหนดพิสัยที่ยอมรับ เช่น list(severity = c(0, 100))
#' @param design_check logical ตรวจสอบความสมบูรณ์ของแผนการทดลอง
#' @param outlier_method วิธีตรวจ outlier: "iqr" (default), "zscore", หรือ "none"
#' @param outlier_threshold ค่า threshold (default: 1.5 สำหรับ IQR, 3 สำหรับ z-score)
#' @param remove_duplicates logical ลบแถวซ้ำอัตโนมัติ (default = FALSE แค่เตือน)
#' @param check_gps logical ตรวจสอบพิกัด GPS (default = TRUE ถ้าพบคอลัมน์พิกัด)
#' @param thailand_only logical เช็คว่าพิกัดอยู่ในประเทศไทย (default = TRUE)
#' @param custom_aliases named list เพิ่ม alias เอง เช่น list(treatment = c("formula"), rep = c("set"))
#' @param verbose logical แสดงรายงานสรุป (default = TRUE)
#'
#' @return object class "washed_rice" (list) ประกอบด้วย:
#'   \item{data}{data.frame ที่สะอาดแล้ว}
#'   \item{report}{list สรุปทุกขั้นตอน}
#'   \item{flagged}{data.frame แถวที่ถูก flag (outlier, out-of-range)}
#'   \item{column_map}{data.frame แสดงการจับคู่ชื่อคอลัมน์}
#'
#' @examples
#' \dontrun{
#' # ใช้งานแบบด่วน — ปล่อยให้ฟังก์ชันเดาเอง
#' result <- wash_rice(my_data)
#' clean <- result$data
#'
#' # งานแปลงทดลอง RCBD
#' result <- wash_rice(my_data,
#'   treatment_col = "\u0e2a\u0e32\u0e22\u0e1e\u0e31\u0e19\u0e18\u0e38\u0e4c",
#'   rep_col       = "\u0e0b\u0e49\u0e33",
#'   valid_ranges  = list(yield = c(0, 1500), severity = c(0, 100)),
#'   design_check  = TRUE
#' )
#'
#' # งานสำรวจระบาดวิทยา
#' result <- wash_rice(survey_data,
#'   valid_ranges = list(severity = c(0, 100), incidence = c(0, 100)),
#'   outlier_method = "zscore"
#' )
#' }
#' @export
wash_rice <- function(data,
                      treatment_col = NULL,
                      rep_col = NULL,
                      factor_cols = NULL,
                      numeric_cols = NULL,
                      valid_ranges = NULL,
                      design_check = FALSE,
                      outlier_method = c("iqr", "zscore", "none"),
                      outlier_threshold = NULL,
                      remove_duplicates = FALSE,
                      check_gps = TRUE,
                      thailand_only = TRUE,
                      custom_aliases = NULL,
                      verbose = TRUE) {
  # ════════════════════════════════════════════════════════════════════

  # SETUP
  # ════════════════════════════════════════════════════════════════════
  outlier_method <- match.arg(outlier_method)
  if (is.null(outlier_threshold)) {
    outlier_threshold <- switch(outlier_method,
      iqr    = 1.5,
      zscore = 3,
      none   = NA_real_
    )
  }

  clean_df <- as.data.frame(data)
  n_original <- nrow(clean_df)
  original_names <- names(clean_df)
  report <- list()
  flagged_rows <- data.frame()

  .log <- function(..., type = "info") {
    if (!verbose) {
      return(invisible(NULL))
    }
    prefix <- switch(type,
      info  = "[INFO]  ",
      ok    = "[OK]    ",
      warn  = "[WARN]  ",
      map   = "[MAP]   ",
      flag  = "[FLAG]  "
    )
    message(prefix, ...)
  }

  # helper สำหรับ flag แถวที่มีปัญหา (สะสมไว้ใน flagged_rows)
  .flag <- function(df, idx, reason, col_name = NA, values = NA) {
    if (length(idx) == 0) {
      return(flagged_rows)
    }
    fl <- df[idx, , drop = FALSE]
    fl$.row_number <- idx
    fl$.flag_reason <- reason
    fl$.flag_column <- col_name
    fl$.flag_value <- if (length(values) == length(idx)) values else NA
    rbind(flagged_rows, fl)
  }


  # ════════════════════════════════════════════════════════════════════
  # STEP 1: STANDARDIZE COLUMN NAMES
  # ════════════════════════════════════════════════════════════════════
  names(clean_df) <- tolower(gsub("[[:space:]]+|\\.", "_", names(clean_df)))
  names(clean_df) <- gsub("_+", "_", names(clean_df))
  names(clean_df) <- gsub("^_|_$", "", names(clean_df))

  renamed <- data.frame(
    original = original_names,
    cleaned = names(clean_df),
    stringsAsFactors = FALSE
  )
  renamed_changed <- renamed[renamed$original != renamed$cleaned, ]
  report$renamed_columns <- renamed_changed

  if (nrow(renamed_changed) > 0) {
    .log("\u0e1b\u0e23\u0e31\u0e1a\u0e0a\u0e37\u0e48\u0e2d\u0e04\u0e2d\u0e25\u0e31\u0e21\u0e19\u0e4c ", nrow(renamed_changed), " \u0e15\u0e31\u0e27")
  }


  # ════════════════════════════════════════════════════════════════════
  # STEP 2: SMART ALIAS DICTIONARY (พจนานุกรมเดาใจ)
  # ════════════════════════════════════════════════════════════════════

  # --- Built-in dictionaries ---
  dict <- list(
    treatment = c(
      "treatment", "trt", "variety", "var", "genotype", "gen",
      "entry", "line", "cultivar", "cv",
      # ภาษาไทย (ชื่อที่พบบ่อยจากศูนย์วิจัย)
      "\u0e2a\u0e32\u0e22\u0e1e\u0e31\u0e19\u0e18\u0e38\u0e4c",
      "\u0e01\u0e23\u0e23\u0e21\u0e27\u0e34\u0e18\u0e35",
      "\u0e1e\u0e31\u0e19\u0e18\u0e38\u0e4c",
      "\u0e2a\u0e39\u0e15\u0e23",
      "\u0e17\u0e23\u0e35\u0e17\u0e40\u0e21\u0e19\u0e15\u0e4c"
    ),
    rep = c(
      "rep", "block", "blk", "replication", "replicate", "r",
      "\u0e0b\u0e49\u0e33",
      "\u0e1a\u0e25\u0e47\u0e2d\u0e01",
      "\u0e01\u0e25\u0e38\u0e48\u0e21"
    )
  )

  # เพิ่ม custom aliases จากผู้ใช้
  if (!is.null(custom_aliases)) {
    for (key in names(custom_aliases)) {
      if (key %in% names(dict)) {
        dict[[key]] <- unique(c(dict[[key]], tolower(custom_aliases[[key]])))
      }
    }
  }

  # --- Smart detect function ---
smart_detect <- function(df_names, aliases, user_input) {
  if (!is.null(user_input)) {
    target <- tolower(gsub("[[:space:]]+|\\.", "_", user_input))
    target <- gsub("_+", "_", target)
    target <- gsub("^_|_$", "", target)
    idx <- which(df_names == target)
  } else {
    idx <- which(df_names %in% aliases)
  }
  if (length(idx) > 0) {
    return(list(found = TRUE, col = df_names[idx[1]]))
  }
  return(list(found = FALSE, col = NA_character_))
}

  # --- Execute smart detect ---
trt_result <- smart_detect(names(clean_df), dict$treatment, treatment_col)
rep_result <- smart_detect(names(clean_df), dict$rep, rep_col)

# เก็บชื่อคอลัมน์จริงที่ detect ได้ไว้ใช้ภายใน
trt_col <- if (trt_result$found) trt_result$col else NA_character_
rep_col_detected <- if (rep_result$found) rep_result$col else NA_character_

column_map <- data.frame(
  original     = original_names,
  standardized = names(clean_df),   # \u0e44\u0e21\u0e48\u0e21\u0e35\u0e01\u0e32\u0e23\u0e40\u0e1b\u0e25\u0e35\u0e48\u0e22\u0e19\u0e41\u0e1b\u0e25\u0e07
  stringsAsFactors = FALSE
)
report$column_map   <- column_map
report$trt_col_used <- trt_col
report$rep_col_used <- rep_col_detected

if (trt_result$found) {
  .log("\u0e04\u0e2d\u0e25\u0e31\u0e21\u0e19\u0e4c\u0e01\u0e23\u0e23\u0e21\u0e27\u0e34\u0e18\u0e35: '", trt_col, "' (\u0e44\u0e21\u0e48\u0e40\u0e1b\u0e25\u0e35\u0e48\u0e22\u0e19\u0e0a\u0e37\u0e48\u0e2d)", type = "map")
} else if (verbose) {
  .log("\u0e44\u0e21\u0e48\u0e1e\u0e1a\u0e04\u0e2d\u0e25\u0e31\u0e21\u0e19\u0e4c\u0e01\u0e23\u0e23\u0e21\u0e27\u0e34\u0e18\u0e35 (treatment) \u0e43\u0e19\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25", type = "warn")
}

if (rep_result$found) {
  .log("\u0e04\u0e2d\u0e25\u0e31\u0e21\u0e19\u0e4c\u0e0b\u0e49\u0e33: '", rep_col_detected, "' (\u0e44\u0e21\u0e48\u0e40\u0e1b\u0e25\u0e35\u0e48\u0e22\u0e19\u0e0a\u0e37\u0e48\u0e2d)", type = "map")
} else if (verbose) {
  .log("\u0e44\u0e21\u0e48\u0e1e\u0e1a\u0e04\u0e2d\u0e25\u0e31\u0e21\u0e19\u0e4c\u0e0b\u0e49\u0e33 (rep/block) \u0e43\u0e19\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25", type = "warn")
}


  # ════════════════════════════════════════════════════════════════════
  # STEP 3: TRIM WHITESPACE + EMPTY STRING → NA
  # ════════════════════════════════════════════════════════════════════
  char_cols <- names(clean_df)[vapply(clean_df, is.character, logical(1))]

  # นับ empty strings ก่อน replace (vectorized ด้วย vapply)
  n_empty_converted <- sum(vapply(
    lapply(clean_df[char_cols], trimws),
    function(x) sum(!is.na(x) & x == "", na.rm = TRUE),
    integer(1)
  ))

  # Vectorized: trim whitespace + แปลง empty string -> NA ใน one pass ด้วย lapply
  clean_df[char_cols] <- lapply(clean_df[char_cols], function(x) {
    x <- trimws(x)
    replace(x, !is.na(x) & x == "", NA_character_)
  })

  report$whitespace_trimmed <- length(char_cols)
  report$empty_to_na <- n_empty_converted


  if (n_empty_converted > 0) {
    .log("\u0e41\u0e1b\u0e25\u0e07\u0e04\u0e48\u0e32\u0e27\u0e48\u0e32\u0e07 (empty string) \u0e40\u0e1b\u0e47\u0e19 NA \u0e08\u0e33\u0e19\u0e27\u0e19 ", n_empty_converted, " \u0e08\u0e38\u0e14")
  }


  # ════════════════════════════════════════════════════════════════════
  # STEP 4: NUMERIC COERCION
  # ════════════════════════════════════════════════════════════════════
  if (is.null(numeric_cols)) {
    # Auto-detect ด้วย vapply — vectorized ไม่ต้องใช้ for loop
    is_suspect_numeric <- vapply(names(clean_df), function(col) {
      x <- clean_df[[col]]
      if (!is.character(x)) {
        return(FALSE)
      }
      non_na <- gsub("[, ]", "", x[!is.na(x)])
      if (length(non_na) == 0) {
        return(FALSE)
      }
      mean(!is.na(suppressWarnings(as.numeric(non_na)))) >= 0.8
    }, logical(1))
    suspect_numeric <- names(clean_df)[is_suspect_numeric]
  } else {
    suspect_numeric <- numeric_cols
  }

  coercion_log <- list()
  for (col in suspect_numeric) {
    if (col %in% names(clean_df) && !is.numeric(clean_df[[col]])) {
      original_vals <- clean_df[[col]]
      stripped <- gsub("[, ]", "", original_vals)
      converted <- suppressWarnings(as.numeric(stripped))
      n_failed <- sum(!is.na(original_vals) & is.na(converted))
      if (n_failed > 0) {
        coercion_log[[col]] <- n_failed
        warning(
          "[WARN] \u0e04\u0e2d\u0e25\u0e31\u0e21\u0e19\u0e4c '", col, "': \u0e41\u0e1b\u0e25\u0e07\u0e40\u0e1b\u0e47\u0e19\u0e15\u0e31\u0e27\u0e40\u0e25\u0e02\u0e44\u0e21\u0e48\u0e44\u0e14\u0e49 ",
          n_failed, " \u0e04\u0e48\u0e32 (\u0e16\u0e39\u0e01\u0e41\u0e17\u0e19\u0e14\u0e49\u0e27\u0e22 NA)"
        )
      }
      clean_df[[col]] <- converted
      .log("\u0e41\u0e1b\u0e25\u0e07 '", col, "' (character -> numeric)", type = "info")
    }
  }
  report$coercion_issues <- coercion_log


  # ════════════════════════════════════════════════════════════════════
  # STEP 5: CONVERT FACTOR COLUMNS
  # ════════════════════════════════════════════════════════════════════
  auto_factors <- c("treatment", "rep")
  if (!is.null(factor_cols)) {
    # standardize ชื่อที่ผู้ใช้ส่งมา
    user_factors <- tolower(gsub("[[:space:]]+|\\.", "_", factor_cols))
    auto_factors <- unique(c(auto_factors, user_factors))
  }
  # เพิ่ม common factor columns ที่ไม่น่าเป็นตัวเลข
  common_factors <- c(
    "location", "loc", "season", "year", "site",
    "province", "district", "amphoe"
  )
  auto_factors <- unique(c(auto_factors, common_factors))
  matched_factors <- intersect(auto_factors, names(clean_df))

  # Vectorized factor conversion ด้วย lapply
  clean_df[matched_factors] <- lapply(clean_df[matched_factors], function(x) {
    if (is.factor(x)) {
      levels(x) <- trimws(levels(x))
      x
    } else {
      as.factor(trimws(as.character(x)))
    }
  })
  report$factor_columns <- matched_factors

  if (length(matched_factors) > 0) {
    .log("\u0e41\u0e1b\u0e25\u0e07\u0e40\u0e1b\u0e47\u0e19 Factor: ", paste(matched_factors, collapse = ", "))
  }


  # ════════════════════════════════════════════════════════════════════
  # STEP 6: DUPLICATE DETECTION
  # ════════════════════════════════════════════════════════════════════
  dup_idx <- duplicated(clean_df)
  n_dup <- sum(dup_idx)
  report$duplicates_found <- n_dup

  if (n_dup > 0) {
    if (remove_duplicates) {
      clean_df <- clean_df[!dup_idx, ]
      .log("\u0e25\u0e1a\u0e41\u0e16\u0e27\u0e0b\u0e49\u0e33 ", n_dup, " \u0e41\u0e16\u0e27", type = "flag")
    } else {
      warning(
        "[WARN] \u0e1e\u0e1a\u0e41\u0e16\u0e27\u0e0b\u0e49\u0e33 ", n_dup, " \u0e41\u0e16\u0e27",
        " (\u0e43\u0e0a\u0e49 remove_duplicates = TRUE \u0e40\u0e1e\u0e37\u0e48\u0e2d\u0e25\u0e1a\u0e2d\u0e31\u0e15\u0e42\u0e19\u0e21\u0e31\u0e15\u0e34)"
      )
    }
  }


  # ════════════════════════════════════════════════════════════════════
  # STEP 7: MISSING VALUE SUMMARY
  # ════════════════════════════════════════════════════════════════════
  na_by_col <- colSums(is.na(clean_df))
  na_by_col <- na_by_col[na_by_col > 0]
  report$na_summary <- na_by_col
  report$na_total <- sum(na_by_col)

  if (length(na_by_col) > 0) {
    detail <- paste0(names(na_by_col), "(", na_by_col, ")")
    warning(
      "[WARN] \u0e1e\u0e1a NA \u0e23\u0e27\u0e21 ", sum(na_by_col), " \u0e08\u0e38\u0e14 \u0e43\u0e19 ",
      length(na_by_col), " \u0e04\u0e2d\u0e25\u0e31\u0e21\u0e19\u0e4c: ", paste(detail, collapse = ", ")
    )
  }


  # ════════════════════════════════════════════════════════════════════
  # STEP 8: NEGATIVE VALUE CHECK
  # ════════════════════════════════════════════════════════════════════
  num_cols <- names(clean_df)[vapply(clean_df, is.numeric, logical(1))]
  neg_cols <- character(0)

  # Vectorized negative check ด้วย lapply
  neg_results <- lapply(num_cols, function(col) {
    idx <- which(!is.na(clean_df[[col]]) & clean_df[[col]] < 0)
    if (length(idx) > 0) list(col = col, idx = idx) else NULL
  })
  neg_results <- Filter(Negate(is.null), neg_results)

  for (r in neg_results) {
    neg_cols <- c(neg_cols, r$col)
    flagged_rows <- .flag(
      clean_df, r$idx,
      reason = paste0("\u0e04\u0e48\u0e32\u0e15\u0e34\u0e14\u0e25\u0e1a\u0e43\u0e19 '", r$col, "'"),
      col_name = r$col,
      values = clean_df[[r$col]][r$idx]
    )
  }
  report$negative_value_columns <- neg_cols

  if (length(neg_cols) > 0) {
    warning(
      "[WARN] \u0e1e\u0e1a\u0e04\u0e48\u0e32\u0e15\u0e34\u0e14\u0e25\u0e1a\u0e43\u0e19\u0e04\u0e2d\u0e25\u0e31\u0e21\u0e19\u0e4c: ", paste(neg_cols, collapse = ", ")
    )
  }


  # ════════════════════════════════════════════════════════════════════
  # STEP 9: DOMAIN-SPECIFIC RANGE VALIDATION
  # ════════════════════════════════════════════════════════════════════
  range_violations <- list()

  if (!is.null(valid_ranges)) {
    for (col in names(valid_ranges)) {
      target_col <- tolower(gsub("[[:space:]]+|\\.", "_", col))
      if (target_col %in% names(clean_df) && is.numeric(clean_df[[target_col]])) {
        lo <- valid_ranges[[col]][1]
        hi <- valid_ranges[[col]][2]
        oor_idx <- which(
          !is.na(clean_df[[target_col]]) &
            (clean_df[[target_col]] < lo | clean_df[[target_col]] > hi)
        )
        if (length(oor_idx) > 0) {
          range_violations[[target_col]] <- list(
            rows        = oor_idx,
            values      = clean_df[[target_col]][oor_idx],
            valid_range = c(lo, hi)
          )
          flagged_rows <- .flag(
            clean_df, oor_idx,
            reason = paste0(target_col, " \u0e19\u0e2d\u0e01\u0e1e\u0e34\u0e2a\u0e31\u0e22 [", lo, ", ", hi, "]"),
            col_name = target_col,
            values = clean_df[[target_col]][oor_idx]
          )
          warning(
            "[WARN] '", target_col, "': \u0e1e\u0e1a ", length(oor_idx),
            " \u0e04\u0e48\u0e32\u0e19\u0e2d\u0e01\u0e1e\u0e34\u0e2a\u0e31\u0e22 [", lo, "-", hi, "]"
          )
        }
      }
    }
  }
  report$range_violations <- range_violations


  # ════════════════════════════════════════════════════════════════════
  # STEP 10: OUTLIER DETECTION
  # ════════════════════════════════════════════════════════════════════
  outlier_info <- list()

  if (outlier_method != "none" && length(num_cols) > 0) {
    # Vectorized outlier detection ด้วย lapply
    .detect_outliers <- function(col) {
      vals <- clean_df[[col]]
      if (sum(!is.na(vals)) < 5) {
        return(NULL)
      }

      outlier_idx <- if (outlier_method == "iqr") {
        q1 <- quantile(vals, 0.25, na.rm = TRUE)
        q3 <- quantile(vals, 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        which(!is.na(vals) & (vals < q1 - outlier_threshold * iqr |
          vals > q3 + outlier_threshold * iqr))
      } else if (outlier_method == "zscore") {
        z <- as.numeric(scale(vals))
        which(!is.na(z) & abs(z) > outlier_threshold)
      } else {
        integer(0)
      }

      if (length(outlier_idx) == 0) {
        return(NULL)
      }
      list(rows = outlier_idx, values = vals[outlier_idx], n = length(outlier_idx))
    }

    ol_list <- lapply(stats::setNames(num_cols, num_cols), .detect_outliers)
    ol_list <- Filter(Negate(is.null), ol_list)
    outlier_info <- ol_list

    for (col in names(ol_list)) {
      flagged_rows <- .flag(
        clean_df, ol_list[[col]]$rows,
        reason = paste0("outlier (", outlier_method, ") \u0e43\u0e19 '", col, "'"),
        col_name = col,
        values = ol_list[[col]]$values
      )
    }
  }
  report$outliers <- outlier_info

  if (length(outlier_info) > 0) {
    total_ol <- sum(vapply(outlier_info, function(x) x$n, integer(1)))
    .log(
      "\u0e1e\u0e1a\u0e04\u0e48\u0e32\u0e1c\u0e34\u0e14\u0e1b\u0e01\u0e15\u0e34 (outlier) \u0e23\u0e27\u0e21 ", total_ol, " \u0e08\u0e38\u0e14 \u0e43\u0e19\u0e04\u0e2d\u0e25\u0e31\u0e21\u0e19\u0e4c: ",
      paste(names(outlier_info), collapse = ", "),
      " \u2192 \u0e14\u0e39 $flagged",
      type = "flag"
    )
  }


  # ════════════════════════════════════════════════════════════════════
  # STEP 11: EXPERIMENTAL DESIGN BALANCE CHECK
  # ════════════════════════════════════════════════════════════════════
  if (design_check) {
    has_trt <- "treatment" %in% names(clean_df)
    has_rep <- "rep" %in% names(clean_df)

    if (has_trt && has_rep) {
      dt <- table(clean_df$treatment, clean_df$rep)
      is_balanced <- length(unique(as.vector(dt))) == 1
      report$design_balanced <- is_balanced
      report$design_table <- dt

      if (!is_balanced) {
        # หา missing combinations (base R ไม่พึ่ง dplyr)
        expected <- expand.grid(
          treatment = levels(factor(clean_df$treatment)),
          rep = levels(factor(clean_df$rep)),
          stringsAsFactors = FALSE
        )
        observed <- unique(clean_df[, c("treatment", "rep")])
        observed$treatment <- as.character(observed$treatment)
        observed$rep <- as.character(observed$rep)

        # tag ตาม observed
        observed$.present <- TRUE
        missing_check <- merge(expected, observed, all.x = TRUE)
        missing_combos <- missing_check[
          is.na(missing_check$.present),
          c("treatment", "rep")
        ]

        report$missing_combinations <- missing_combos

        warning(
          "[WARN] \u0e41\u0e1c\u0e19\u0e01\u0e32\u0e23\u0e17\u0e14\u0e25\u0e2d\u0e07\u0e44\u0e21\u0e48\u0e2a\u0e21\u0e14\u0e38\u0e25! ",
          if (nrow(missing_combos) > 0) {
            paste0("\u0e02\u0e32\u0e14 ", nrow(missing_combos), " combination")
          } else {
            "\u0e08\u0e33\u0e19\u0e27\u0e19\u0e0b\u0e49\u0e33\u0e43\u0e19\u0e41\u0e15\u0e48\u0e25\u0e30\u0e01\u0e23\u0e23\u0e21\u0e27\u0e34\u0e18\u0e35\u0e44\u0e21\u0e48\u0e40\u0e17\u0e48\u0e32\u0e01\u0e31\u0e19"
          }
        )
      } else {
        .log("\u0e41\u0e1c\u0e19\u0e01\u0e32\u0e23\u0e17\u0e14\u0e25\u0e2d\u0e07\u0e2a\u0e21\u0e14\u0e38\u0e25\u0e04\u0e23\u0e1a\u0e17\u0e38\u0e01 combination", type = "ok")
      }
    } else {
      missing_parts <- c(
        if (!has_trt) "treatment" else NULL,
        if (!has_rep) "rep" else NULL
      )
      warning(
        "[WARN] \u0e44\u0e21\u0e48\u0e2a\u0e32\u0e21\u0e32\u0e23\u0e16\u0e40\u0e0a\u0e47\u0e04\u0e41\u0e1c\u0e19\u0e01\u0e32\u0e23\u0e17\u0e14\u0e25\u0e2d\u0e07\u0e44\u0e14\u0e49 \u2014 \u0e44\u0e21\u0e48\u0e1e\u0e1a\u0e04\u0e2d\u0e25\u0e31\u0e21\u0e19\u0e4c: ",
        paste(missing_parts, collapse = ", "),
        "\n  \u0e25\u0e2d\u0e07\u0e23\u0e30\u0e1a\u0e38\u0e1c\u0e48\u0e32\u0e19 treatment_col / rep_col",
        "\n  \u0e2b\u0e23\u0e37\u0e2d\u0e40\u0e1e\u0e34\u0e48\u0e21\u0e43\u0e19 custom_aliases"
      )
    }
  }


  # ════════════════════════════════════════════════════════════════════
  # STEP 12: GPS COORDINATE CHECK
  # ════════════════════════════════════════════════════════════════════
  if (check_gps) {
    lat_candidates <- c("lat", "latitude", "y")
    lon_candidates <- c("lon", "lng", "long", "longitude", "x")

    lat_col <- intersect(lat_candidates, names(clean_df))
    lon_col <- intersect(lon_candidates, names(clean_df))

    if (length(lat_col) > 0 && length(lon_col) > 0) {
      lat_col <- lat_col[1]
      lon_col <- lon_col[1]

      gps_issues <- list()

      if (is.numeric(clean_df[[lat_col]]) && is.numeric(clean_df[[lon_col]])) {
        # เช็คพิสัยโลก
        bad_lat <- which(
          !is.na(clean_df[[lat_col]]) &
            (clean_df[[lat_col]] < -90 | clean_df[[lat_col]] > 90)
        )
        bad_lon <- which(
          !is.na(clean_df[[lon_col]]) &
            (clean_df[[lon_col]] < -180 | clean_df[[lon_col]] > 180)
        )

        if (length(bad_lat) > 0) {
          gps_issues$invalid_lat <- bad_lat
          flagged_rows <- .flag(
            clean_df, bad_lat,
            reason = "latitude \u0e19\u0e2d\u0e01 [-90, 90]", col_name = lat_col,
            values = clean_df[[lat_col]][bad_lat]
          )
        }
        if (length(bad_lon) > 0) {
          gps_issues$invalid_lon <- bad_lon
          flagged_rows <- .flag(
            clean_df, bad_lon,
            reason = "longitude \u0e19\u0e2d\u0e01 [-180, 180]", col_name = lon_col,
            values = clean_df[[lon_col]][bad_lon]
          )
        }

        if (length(bad_lat) + length(bad_lon) > 0) {
          warning(
            "[WARN] \u0e1e\u0e1a\u0e1e\u0e34\u0e01\u0e31\u0e14 GPS \u0e17\u0e35\u0e48\u0e44\u0e21\u0e48\u0e16\u0e39\u0e01\u0e15\u0e49\u0e2d\u0e07 ",
            length(bad_lat) + length(bad_lon), " \u0e08\u0e38\u0e14"
          )
        }

        # เช็คขอบเขตประเทศไทย
        if (thailand_only) {
          thai_lat <- which(
            !is.na(clean_df[[lat_col]]) &
              (clean_df[[lat_col]] < 5 | clean_df[[lat_col]] > 21)
          )
          thai_lon <- which(
            !is.na(clean_df[[lon_col]]) &
              (clean_df[[lon_col]] < 97 | clean_df[[lon_col]] > 106)
          )
          suspect_outside <- unique(c(thai_lat, thai_lon))
          # ลบพวกที่ invalid ไปแล้ว ไม่ต้อง flag ซ้ำ
          suspect_outside <- setdiff(suspect_outside, c(bad_lat, bad_lon))

          gps_issues$outside_thailand <- suspect_outside

          if (length(suspect_outside) > 0) {
            .log(
              "\u0e1e\u0e34\u0e01\u0e31\u0e14\u0e1a\u0e32\u0e07\u0e08\u0e38\u0e14\u0e2d\u0e22\u0e39\u0e48\u0e19\u0e2d\u0e01\u0e02\u0e2d\u0e1a\u0e40\u0e02\u0e15\u0e1b\u0e23\u0e30\u0e40\u0e17\u0e28\u0e44\u0e17\u0e22 (lat 5-21, lon 97-106) ",
              length(suspect_outside), " \u0e41\u0e16\u0e27",
              " \u2014 \u0e1b\u0e01\u0e15\u0e34\u0e16\u0e49\u0e32\u0e40\u0e1b\u0e47\u0e19\u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25\u0e15\u0e48\u0e32\u0e07\u0e1b\u0e23\u0e30\u0e40\u0e17\u0e28 \u0e1b\u0e34\u0e14\u0e44\u0e14\u0e49\u0e14\u0e49\u0e27\u0e22 thailand_only = FALSE",
              type = "warn"
            )
          }
        }

        report$gps_issues <- gps_issues
        report$gps_columns <- c(lat = lat_col, lon = lon_col)
      }
    }
  }


  # ════════════════════════════════════════════════════════════════════
  # STEP 13: FINAL SUMMARY REPORT
  # ════════════════════════════════════════════════════════════════════
  report$n_original <- n_original
  report$n_final <- nrow(clean_df)
  report$n_columns <- ncol(clean_df)
  report$numeric_columns <- num_cols
  report$factor_levels <- lapply(
    clean_df[matched_factors],
    function(x) if (is.factor(x)) levels(x) else NULL
  )

  # Remove duplicate flags (same row flagged for multiple reasons)
  if (nrow(flagged_rows) > 0) {
    flagged_rows <- flagged_rows[order(flagged_rows$.row_number), ]
  }

  n_flagged <- nrow(flagged_rows)

  if (verbose) {
    message("\n", strrep("-", 50))
    message("wash_rice v3.1 \u2014 \u0e2a\u0e23\u0e38\u0e1b\u0e1c\u0e25\u0e01\u0e32\u0e23\u0e25\u0e49\u0e32\u0e07\u0e02\u0e49\u0e32\u0e27")
    message(strrep("-", 50))
    message(
      "  \u2022 \u0e41\u0e16\u0e27:        ", n_original, " -> ", nrow(clean_df),
      if (n_original != nrow(clean_df)) {
        paste0("  (\u0e25\u0e1a ", n_original - nrow(clean_df), " \u0e41\u0e16\u0e27)")
      } else {
        ""
      }
    )
    message("  \u2022 \u0e04\u0e2d\u0e25\u0e31\u0e21\u0e19\u0e4c:    ", ncol(clean_df))
    if (length(matched_factors) > 0) {
      message("  \u2022 Factor:     ", paste(matched_factors, collapse = ", "))
    }
    if (length(num_cols) > 0) {
      message("  \u2022 Numeric:    ", paste(num_cols, collapse = ", "))
    }
    if (report$na_total > 0) {
      message("  \u2022 NA:         ", report$na_total, " \u0e08\u0e38\u0e14")
    }
    if (n_dup > 0) {
      message(
        "  \u2022 \u0e41\u0e16\u0e27\u0e0b\u0e49\u0e33:     ", n_dup,
        if (remove_duplicates) " (\u0e25\u0e1a\u0e41\u0e25\u0e49\u0e27)" else " (\u0e22\u0e31\u0e07\u0e2d\u0e22\u0e39\u0e48)"
      )
    }
    if (length(outlier_info) > 0) {
      message(
        "  \u2022 Outliers:   ",
        sum(sapply(outlier_info, function(x) x$n)), " \u0e08\u0e38\u0e14"
      )
    }
    if (n_flagged > 0) {
      message("  \u2022 Flagged:    ", n_flagged, " \u0e41\u0e16\u0e27 -> \u0e14\u0e39\u0e43\u0e19 $flagged")
    }
    message(strrep("-", 50))
    message("\u0e25\u0e49\u0e32\u0e07\u0e02\u0e49\u0e32\u0e27\u0e40\u0e2a\u0e23\u0e47\u0e08\u0e40\u0e23\u0e35\u0e22\u0e1a\u0e23\u0e49\u0e2d\u0e22! \u0e02\u0e49\u0e2d\u0e21\u0e39\u0e25\u0e1e\u0e23\u0e49\u0e2d\u0e21\u0e2b\u0e38\u0e07\u0e41\u0e25\u0e49\u0e27")
    message(strrep("-", 50))
  }


  # ════════════════════════════════════════════════════════════════════
  # RETURN
  # ════════════════════════════════════════════════════════════════════
  structure(
    list(
      data       = clean_df,
      report     = report,
      flagged    = flagged_rows,
      column_map = column_map
    ),
    class = c("washed_rice", "list")
  )
}


# ══════════════════════════════════════════════════════════════════════
# Print method
# ══════════════════════════════════════════════════════════════════════

#' @export
print.washed_rice <- function(x, ...) {
  cat("\u2500\u2500 washed_rice v3.1 \u2500\u2500\n")
  cat("  Rows:", nrow(x$data), "| Cols:", ncol(x$data), "\n")
  if (!is.null(x$report$na_total) && x$report$na_total > 0) {
    cat("  NA:", x$report$na_total, "\n")
  }
  if (nrow(x$flagged) > 0) {
    cat("  Flagged:", nrow(x$flagged), "rows\n")
  }
  cat("\n  $data       \u2192 clean data.frame\n")
  cat("  $report     \u2192 cleaning details\n")
  cat("  $flagged    \u2192 problematic rows\n")
  cat("  $column_map \u2192 name mapping\n")
  invisible(x)
}


# ══════════════════════════════════════════════════════════════════════
# ตัวอย่างการใช้งาน
# ══════════════════════════════════════════════════════════════════════

if (FALSE) {
  # ─── ตัวอย่าง 1: ข้อมูลที่คอลัมน์ตั้งชื่อเป็นภาษาไทย ───
  # สมมติ dataframe มีคอลัมน์: "สายพันธุ์", "ซ้ำ", "ผลผลิต", "ความรุนแรง"
  result <- wash_rice(
    data = thai_data,
    valid_ranges = list(
      "\u0e1c\u0e25\u0e1c\u0e25\u0e34\u0e15" = c(0, 1500),
      "\u0e04\u0e27\u0e32\u0e21\u0e23\u0e38\u0e19\u0e41\u0e23\u0e07" = c(0, 100)
    ),
    design_check = TRUE
  )
  # ฟังก์ชันจะเดาว่า "สายพันธุ์" = treatment, "ซ้ำ" = rep


  # ─── ตัวอย่าง 2: ข้อมูลที่ใช้ชื่อแปลก ───
  # คอลัมน์: "Formula", "Set", "DI", "DS"
  result <- wash_rice(
    data = weird_data,
    treatment_col = "Formula", # \u0e0a\u0e35\u0e49\u0e40\u0e1b\u0e49\u0e32\u0e15\u0e23\u0e07
    rep_col = "Set", # \u0e0a\u0e35\u0e49\u0e40\u0e1b\u0e49\u0e32\u0e15\u0e23\u0e07
    valid_ranges = list(DI = c(0, 100), DS = c(0, 100)),
    design_check = TRUE
  )


  # ─── ตัวอย่าง 3: ข้อมูลสำรวจระบาดวิทยา (มี GPS) ───
  result <- wash_rice(
    data = survey_data,
    factor_cols = c("province", "season"),
    valid_ranges = list(severity = c(0, 100), incidence = c(0, 100)),
    outlier_method = "zscore",
    outlier_threshold = 3,
    remove_duplicates = TRUE
  )
  # เช็ค GPS issues
  result$report$gps_issues


  # ─── ตัวอย่าง 4: เพิ่ม alias เฉพาะหน่วยงาน ───
  result <- wash_rice(
    data = center_data,
    custom_aliases = list(
      treatment = c("formula", "fungicide", "dose"),
      rep       = c("set", "round")
    ),
    design_check = TRUE
  )


  # ─── ตัวอย่าง 5: ใช้แบบด่วน ───
  clean <- wash_rice(raw_data)$data
}
