#' วาดกราฟผลการวิเคราะห์ — Publication-Ready Plots
#'
#' สร้างกราฟจาก cooked_rice object โดยอัตโนมัติ
#' เลือกรูปแบบตามจำนวนซ้ำ:
#'   n ≤ 4 → Bar Graph + Error Bar + Jitter Points + Letters
#'   n > 4 → Boxplot + Jitter Points + Mean dot + Letters
#'
#' กราฟออกแบบตามมาตรฐานวารสารวิชาการ:
#'   - พื้นขาว ไม่มี gridlines
#'   - เส้นแกนเฉพาะ X ล่าง + Y ซ้าย
#'   - ฟอนต์อ่านง่าย ขนาดเหมาะสม
#'   - Colorblind-friendly palette (viridis)
#'   - รองรับ grayscale
#'   - ตัวอักษร a,b,c ลอยเหนือ error bar
#'
#' @param cooked cooked_rice object จาก cook_crd / cook_rcbd / cook_split
#' @param response character ชื่อ response ที่จะ plot (ถ้าไม่ระบุ plot ทุกตัว)
#' @param plot_type "auto" (เลือกตาม n), "bar", "box" (บังคับ)
#' @param show_points logical แสดงจุดข้อมูลดิบ (default = TRUE)
#' @param show_letters logical แสดงกลุ่มอักษร a,b,c (default = TRUE)
#' @param error_type "se" (standard error, default) หรือ "sd"
#' @param palette character ชื่อ palette: "viridis" (default), "grey", "set2"
#' @param y_label character ชื่อแกน Y (ถ้าไม่ระบุ ใช้ชื่อ response)
#' @param x_label character ชื่อแกน X (ถ้าไม่ระบุ ใช้ชื่อ treatment)
#' @param title character ชื่อกราฟ (ถ้าไม่ระบุ ไม่ใส่)
#' @param font_family character ฟอนต์ (default = "sans" = Arial)
#' @param font_size numeric ขนาดฟอนต์พื้นฐาน (default = 12)
#' @param bar_width numeric ความกว้างแท่ง (default = 0.6)
#' @param point_size numeric ขนาดจุด jitter (default = 2.5)
#' @param jitter_width numeric ความกว้างการกระจายจุด (default = 0.15)
#' @param letter_size numeric ขนาดตัวอักษร a,b,c (default = 4.5)
#' @param letter_nudge numeric ระยะยกตัวอักษรเหนือ error bar (default = auto)
#' @param save_path character path สำหรับบันทึกไฟล์ (NULL = ไม่บันทึก)
#' @param save_width numeric ความกว้างภาพ (inch, default = 7)
#' @param save_height numeric ความสูงภาพ (inch, default = 5)
#' @param save_dpi numeric resolution (default = 300)
#'
#' @return list ของ ggplot objects (invisible) แต่ละ response 1 plot
#'
#' @examples
#' \dontrun{
#' cooked <- cook_rcbd(washed, response = "yield", block = "rep",
#'                     tasted = tasted)
#' plot_cooked(cooked)
#' plot_cooked(cooked, palette = "grey", error_type = "sd")
#' plot_cooked(cooked, save_path = "figures/yield.png")
#' }
#' @export
plot_cooked <- function(cooked,
                        response     = NULL,
                        plot_type    = c("auto", "bar", "box"),
                        show_points  = TRUE,
                        show_letters = TRUE,
                        error_type   = c("se", "sd"),
                        palette      = c("viridis", "grey", "set2"),
                        y_label      = NULL,
                        x_label      = NULL,
                        title        = NULL,
                        font_family  = "sans",
                        font_size    = 12,
                        bar_width    = 0.6,
                        point_size   = 2.5,
                        jitter_width = 0.15,
                        letter_size  = 4.5,
                        letter_nudge = NULL,
                        save_path    = NULL,
                        save_width   = 7,
                        save_height  = 5,
                        save_dpi     = 300) {

  # ── Check ggplot2 ──
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("\U0001F4E6 \u0e15\u0e49\u0e2d\u0e07\u0e15\u0e34\u0e14\u0e15\u0e31\u0e49\u0e07 ggplot2:\n  install.packages(\"ggplot2\")")
  }

  if (!inherits(cooked, "cooked_rice")) {
    stop("\U0001F6AB \u0e15\u0e49\u0e2d\u0e07\u0e2a\u0e48\u0e07 cooked_rice object \u0e08\u0e32\u0e01 cook_crd / cook_rcbd / cook_split")
  }

  plot_type  <- match.arg(plot_type)
  error_type <- match.arg(error_type)
  palette    <- match.arg(palette)

  # response ที่จะ plot
  all_resp <- names(cooked$results)
  if (!is.null(response)) {
    all_resp <- intersect(response, all_resp)
    if (length(all_resp) == 0) {
      stop("\u0e44\u0e21\u0e48\u0e1e\u0e1a response '", paste(response, collapse = ", "),
           "' \u0e43\u0e19 cooked object")
    }
  }

  design <- cooked$design
  settings <- cooked$settings
  df <- cooked$data
  all_plots <- list()

  for (resp in all_resp) {

    res <- cooked$results[[resp]]

    # ═══════════════════════════════════════════════════════════════
    # เตรียมข้อมูลสำหรับ plot
    # ═══════════════════════════════════════════════════════════════

    # กำหนด group column ตาม design
    if (design == "Split-plot") {
      # Factorial / Split-plot: ดูว่า interaction significant ไหม
      if (isTRUE(res$interaction_significant)) {
        # interaction significant → plot แบบ factorial (x = main, fill = sub)
        group_col  <- settings$main_plot
        fill_col   <- settings$sub_plot
        is_grouped <- TRUE
      } else {
        # ไม่ significant → plot main effect อย่างเดียว
        # แต่ยังแสดง sub เป็น fill ไว้ให้เห็น
        group_col  <- settings$main_plot
        fill_col   <- settings$sub_plot
        is_grouped <- TRUE
      }
    } else if (isTRUE(settings$is_factorial)) {
      # Factorial CRD/RCBD
      group_col  <- settings$factors[1]
      fill_col   <- settings$factors[2]
      is_grouped <- TRUE
    } else {
      # Single factor
      group_col  <- settings$treatment
      fill_col   <- NULL
      is_grouped <- FALSE
    }

    # Working data
    needed <- c(resp, group_col)
    if (is_grouped) needed <- c(needed, fill_col)
    working_df <- df[complete.cases(df[, needed]), ]

    # คำนวณ n per group
    if (is_grouped) {
      n_per_group <- min(table(interaction(working_df[[group_col]],
                                            working_df[[fill_col]])))
    } else {
      n_per_group <- min(table(working_df[[group_col]]))
    }

    # ═══════════════════════════════════════════════════════════════
    # เลือก plot_type อัตโนมัติ
    # ═══════════════════════════════════════════════════════════════
    use_type <- plot_type
    if (use_type == "auto") {
      use_type <- if (n_per_group <= 4) "bar" else "box"
    }

    # ═══════════════════════════════════════════════════════════════
    # คำนวณ summary statistics
    # ═══════════════════════════════════════════════════════════════
    if (is_grouped) {
      sum_df <- do.call(rbind, lapply(
        split(working_df, list(working_df[[group_col]], working_df[[fill_col]])),
        function(d) {
          vals <- d[[resp]]
          data.frame(
            x_group  = d[[group_col]][1],
            fill_group = d[[fill_col]][1],
            mean     = mean(vals, na.rm = TRUE),
            sd       = sd(vals, na.rm = TRUE),
            se       = sd(vals, na.rm = TRUE) / sqrt(sum(!is.na(vals))),
            n        = sum(!is.na(vals)),
            stringsAsFactors = FALSE
          )
        }
      ))
      rownames(sum_df) <- NULL
      sum_df$label_id <- paste0(sum_df$x_group, ":", sum_df$fill_group)
    } else {
      sum_df <- do.call(rbind, lapply(
        split(working_df[[resp]], working_df[[group_col]]),
        function(vals) {
          data.frame(
            x_group = NA,
            mean    = mean(vals, na.rm = TRUE),
            sd      = sd(vals, na.rm = TRUE),
            se      = sd(vals, na.rm = TRUE) / sqrt(sum(!is.na(vals))),
            n       = sum(!is.na(vals)),
            stringsAsFactors = FALSE
          )
        }
      ))
      sum_df$x_group <- rownames(sum_df)
      rownames(sum_df) <- NULL
      sum_df$label_id <- sum_df$x_group
    }

    # error bar value
    sum_df$error <- if (error_type == "se") sum_df$se else sum_df$sd
    sum_df$ymax  <- sum_df$mean + sum_df$error
    sum_df$ymin  <- sum_df$mean - sum_df$error

    # ═══════════════════════════════════════════════════════════════
    # จับคู่ group letters
    # ═══════════════════════════════════════════════════════════════
    if (show_letters && !is.null(res$group_letters)) {
      letters_vec <- res$group_letters
      sum_df$letter <- letters_vec[match(sum_df$label_id, names(letters_vec))]

      # ถ้า match ไม่ได้ ลองจาก interaction post-hoc
      if (all(is.na(sum_df$letter)) && is_grouped) {
        # ลองจาก posthoc_interaction (split-plot)
        if (!is.null(res$posthoc_interaction)) {
          int_letters <- res$posthoc_interaction$letters
          sum_df$letter <- int_letters[match(sum_df$label_id, names(int_letters))]
        }
      }

      # ถ้ายังไม่ได้ ลองจาก main/sub แยก
      if (all(is.na(sum_df$letter)) && !is_grouped) {
        if (!is.null(res$posthoc_main)) {
          main_letters <- res$posthoc_main$letters
          sum_df$letter <- main_letters[match(sum_df$x_group, names(main_letters))]
        }
      }
    } else {
      sum_df$letter <- NA
    }

    # ═══════════════════════════════════════════════════════════════
    # letter nudge (ระยะยกตัวอักษร)
    # ═══════════════════════════════════════════════════════════════
    if (is.null(letter_nudge)) {
      y_range <- max(sum_df$ymax, na.rm = TRUE) - min(sum_df$ymin, na.rm = TRUE)
      auto_nudge <- y_range * 0.05
    } else {
      auto_nudge <- letter_nudge
    }
    sum_df$letter_y <- sum_df$ymax + auto_nudge

    # ═══════════════════════════════════════════════════════════════
    # สร้าง ggplot
    # ═══════════════════════════════════════════════════════════════

    p <- ggplot2::ggplot()

    # --- Palette ---
    if (is_grouped) {
      n_fill <- length(unique(sum_df$fill_group))
    } else {
      n_fill <- length(unique(sum_df$x_group))
    }

    if (palette == "viridis") {
      scale_fill <- ggplot2::scale_fill_viridis_d(
        option = "D", begin = 0.2, end = 0.85
      )
    } else if (palette == "grey") {
      scale_fill <- ggplot2::scale_fill_grey(start = 0.3, end = 0.85)
    } else if (palette == "set2") {
      scale_fill <- ggplot2::scale_fill_brewer(palette = "Set2")
    }

    # ═══════════════════════════════════════════════════════════════
    # BAR GRAPH
    # ═══════════════════════════════════════════════════════════════
    if (use_type == "bar") {

      if (is_grouped) {
        # Grouped bar
        p <- p +
          ggplot2::geom_col(
            data = sum_df,
            ggplot2::aes(x = x_group, y = mean, fill = fill_group),
            position = ggplot2::position_dodge(width = 0.8),
            width = bar_width, color = "black", linewidth = 0.3
          ) +
          ggplot2::geom_errorbar(
            data = sum_df,
            ggplot2::aes(x = x_group, ymin = ymin, ymax = ymax,
                         group = fill_group),
            position = ggplot2::position_dodge(width = 0.8),
            width = 0.2, linewidth = 0.5
          )

        # Jitter points
        if (show_points) {
          p <- p +
            ggplot2::geom_point(
              data = working_df,
              ggplot2::aes(x = .data[[group_col]], y = .data[[resp]],
                           group = .data[[fill_col]]),
              position = ggplot2::position_jitterdodge(
                jitter.width = jitter_width, dodge.width = 0.8
              ),
              size = point_size, shape = 21, fill = "white",
              color = "grey30", stroke = 0.5, alpha = 0.8
            )
        }

        # Letters
        if (show_letters && any(!is.na(sum_df$letter))) {
          p <- p +
            ggplot2::geom_label(
              data = sum_df[!is.na(sum_df$letter), ],
              ggplot2::aes(x = x_group, y = letter_y,
                           label = letter, group = fill_group),
              position = ggplot2::position_dodge(width = 0.8),
              size = letter_size, fontface = "plain",
              family = font_family, vjust = 0,
              label.padding = ggplot2::unit(0.15, "lines"),
              label.size = 0,
              fill = ggplot2::alpha("white", 0.75)
            )
        }

        p <- p + scale_fill +
          ggplot2::labs(fill = fill_col)

      } else {
        # Single factor bar
        p <- p +
          ggplot2::geom_col(
            data = sum_df,
            ggplot2::aes(x = x_group, y = mean, fill = x_group),
            width = bar_width, color = "black", linewidth = 0.3
          ) +
          ggplot2::geom_errorbar(
            data = sum_df,
            ggplot2::aes(x = x_group, ymin = ymin, ymax = ymax),
            width = 0.2, linewidth = 0.5
          )

        if (show_points) {
          p <- p +
            ggplot2::geom_jitter(
              data = working_df,
              ggplot2::aes(x = .data[[group_col]], y = .data[[resp]]),
              width = jitter_width,
              size = point_size, shape = 21, fill = "white",
              color = "grey30", stroke = 0.5, alpha = 0.8
            )
        }

        if (show_letters && any(!is.na(sum_df$letter))) {
          p <- p +
            ggplot2::geom_label(
              data = sum_df[!is.na(sum_df$letter), ],
              ggplot2::aes(x = x_group, y = letter_y, label = letter),
              size = letter_size, fontface = "plain",
              family = font_family, vjust = 0,
              label.padding = ggplot2::unit(0.15, "lines"),
              label.size = 0,
              fill = ggplot2::alpha("white", 0.75)
            )
        }

        p <- p + scale_fill +
          ggplot2::guides(fill = "none")
      }

    # ═══════════════════════════════════════════════════════════════
    # BOXPLOT
    # ═══════════════════════════════════════════════════════════════
    } else {

      if (is_grouped) {
        p <- p +
          ggplot2::geom_boxplot(
            data = working_df,
            ggplot2::aes(x = .data[[group_col]], y = .data[[resp]],
                         fill = .data[[fill_col]]),
            position = ggplot2::position_dodge(width = 0.8),
            width = bar_width, outlier.shape = NA,
            color = "black", linewidth = 0.3
          )

        if (show_points) {
          p <- p +
            ggplot2::geom_point(
              data = working_df,
              ggplot2::aes(x = .data[[group_col]], y = .data[[resp]],
                           group = .data[[fill_col]]),
              position = ggplot2::position_jitterdodge(
                jitter.width = jitter_width, dodge.width = 0.8
              ),
              size = point_size, shape = 21, fill = "white",
              color = "grey30", stroke = 0.5, alpha = 0.8
            )
        }

        # Mean dot (จุดแดง)
        p <- p +
          ggplot2::geom_point(
            data = sum_df,
            ggplot2::aes(x = x_group, y = mean, group = fill_group),
            position = ggplot2::position_dodge(width = 0.8),
            shape = 18, size = 3, color = "#D32F2F"
          )

        if (show_letters && any(!is.na(sum_df$letter))) {
          # สำหรับ boxplot ใช้ ymax จาก boxplot whisker แทน
          box_ymax <- do.call(rbind, lapply(
            split(working_df, list(working_df[[group_col]],
                                   working_df[[fill_col]])),
            function(d) {
              vals <- d[[resp]][!is.na(d[[resp]])]
              q3 <- quantile(vals, 0.75)
              iqr <- IQR(vals)
              whisker_top <- max(vals[vals <= q3 + 1.5 * iqr])
              data.frame(
                x_group = d[[group_col]][1],
                fill_group = d[[fill_col]][1],
                whisker_top = whisker_top,
                stringsAsFactors = FALSE
              )
            }
          ))
          rownames(box_ymax) <- NULL
          box_ymax$label_id <- paste0(box_ymax$x_group, ":", box_ymax$fill_group)

          sum_df$letter_y_box <- box_ymax$whisker_top[
            match(sum_df$label_id, box_ymax$label_id)
          ] + auto_nudge

          p <- p +
            ggplot2::geom_label(
              data = sum_df[!is.na(sum_df$letter), ],
              ggplot2::aes(x = x_group,
                           y = letter_y_box,
                           label = letter, group = fill_group),
              position = ggplot2::position_dodge(width = 0.8),
              size = letter_size, fontface = "plain",
              family = font_family, vjust = 0,
              label.padding = ggplot2::unit(0.15, "lines"),
              label.size = 0,
              fill = ggplot2::alpha("white", 0.75)
            )
        }

        p <- p + scale_fill +
          ggplot2::labs(fill = fill_col)

      } else {
        # Single factor boxplot
        p <- p +
          ggplot2::geom_boxplot(
            data = working_df,
            ggplot2::aes(x = .data[[group_col]], y = .data[[resp]],
                         fill = .data[[group_col]]),
            width = bar_width, outlier.shape = NA,
            color = "black", linewidth = 0.3
          )

        if (show_points) {
          p <- p +
            ggplot2::geom_jitter(
              data = working_df,
              ggplot2::aes(x = .data[[group_col]], y = .data[[resp]]),
              width = jitter_width,
              size = point_size, shape = 21, fill = "white",
              color = "grey30", stroke = 0.5, alpha = 0.8
            )
        }

        # Mean dot
        p <- p +
          ggplot2::geom_point(
            data = sum_df,
            ggplot2::aes(x = x_group, y = mean),
            shape = 18, size = 3, color = "#D32F2F"
          )

        if (show_letters && any(!is.na(sum_df$letter))) {
          box_whisker <- sapply(
            split(working_df[[resp]], working_df[[group_col]]),
            function(vals) {
              vals <- vals[!is.na(vals)]
              q3 <- quantile(vals, 0.75)
              iqr <- IQR(vals)
              max(vals[vals <= q3 + 1.5 * iqr])
            }
          )
          sum_df$letter_y_box <- box_whisker[match(sum_df$x_group,
                                                    names(box_whisker))] + auto_nudge

          p <- p +
            ggplot2::geom_label(
              data = sum_df[!is.na(sum_df$letter), ],
              ggplot2::aes(x = x_group, y = letter_y_box, label = letter),
              size = letter_size, fontface = "plain",
              family = font_family, vjust = 0,
              label.padding = ggplot2::unit(0.15, "lines"),
              label.size = 0,
              fill = ggplot2::alpha("white", 0.75)
            )
        }

        p <- p + scale_fill +
          ggplot2::guides(fill = "none")
      }
    }

    # ═══════════════════════════════════════════════════════════════
    # PUBLICATION THEME
    # ═══════════════════════════════════════════════════════════════

    # Labels
    x_lab <- if (!is.null(x_label)) x_label else group_col
    y_lab <- if (!is.null(y_label)) y_label else resp
    plot_title <- if (!is.null(title)) title else NULL

    # Y axis: เริ่มจาก 0 สำหรับ bar, ปล่อยอัตโนมัติสำหรับ boxplot
    if (use_type == "bar") {
      y_top <- max(sum_df$letter_y, sum_df$ymax, na.rm = TRUE) * 1.08
      p <- p + ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.05)),
        limits = c(0, y_top)
      )
    } else {
      # Boxplot: ให้มีที่ว่างด้านบนสำหรับ letters
      p <- p + ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0.05, 0.12))
      )
    }

    # clip = "off" ป้องกันตัวอักษรถูกตัดที่ขอบกราฟ
    p <- p + ggplot2::coord_cartesian(clip = "off")

    p <- p +
      ggplot2::labs(x = x_lab, y = y_lab, title = plot_title) +
      ggplot2::theme_classic(base_size = font_size, base_family = font_family) +
      ggplot2::theme(
        # พื้นหลังขาวล้วน
        plot.background  = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),

        # เส้นแกน: เฉพาะ X ล่าง + Y ซ้าย
        axis.line.x.bottom = ggplot2::element_line(color = "black", linewidth = 0.5),
        axis.line.y.left   = ggplot2::element_line(color = "black", linewidth = 0.5),
        axis.line.x.top    = ggplot2::element_blank(),
        axis.line.y.right  = ggplot2::element_blank(),

        # ไม่มี gridlines
        panel.grid = ggplot2::element_blank(),

        # แกน
        axis.text  = ggplot2::element_text(color = "black", size = font_size),
        axis.title = ggplot2::element_text(color = "black", size = font_size + 1,
                                            face = "bold"),
        axis.ticks = ggplot2::element_line(color = "black", linewidth = 0.3),

        # Legend
        legend.position    = "top",
        legend.title       = ggplot2::element_text(size = font_size, face = "bold"),
        legend.text        = ggplot2::element_text(size = font_size - 1),
        legend.background  = ggplot2::element_rect(fill = "white", color = NA),
        legend.key         = ggplot2::element_rect(fill = "white", color = NA),

        # Title
        plot.title = ggplot2::element_text(size = font_size + 2, face = "bold",
                                            hjust = 0.5),

        # Margin
        plot.margin = ggplot2::margin(10, 15, 10, 10)
      )

    # ═══════════════════════════════════════════════════════════════
    # CV% annotation (สำหรับ RCBD / Split-plot)
    # ═══════════════════════════════════════════════════════════════
    cv_text <- NULL
    if (!is.null(res$cv_percent) && !is.na(res$cv_percent)) {
      cv_text <- paste0("CV = ", res$cv_percent, "%")
    } else if (!is.null(res$cv_percent_subplot) && !is.na(res$cv_percent_subplot)) {
      cv_text <- paste0("CV(a) = ", res$cv_percent_mainplot,
                        "%  CV(b) = ", res$cv_percent_subplot, "%")
    }

    if (!is.null(cv_text)) {
      p <- p + ggplot2::annotate(
        "text", x = Inf, y = Inf, label = cv_text,
        hjust = 1.1, vjust = 1.5, size = 3.5,
        fontface = "italic", color = "grey40",
        family = font_family
      )
    }

    # ═══════════════════════════════════════════════════════════════
    # Print + Save
    # ═══════════════════════════════════════════════════════════════
    print(p)

    if (!is.null(save_path)) {
      # สร้าง filename ต่อ response ถ้ามีหลายตัว
      if (length(all_resp) > 1) {
        ext <- tools::file_ext(save_path)
        base <- tools::file_path_sans_ext(save_path)
        fpath <- paste0(base, "_", resp, ".", ext)
      } else {
        fpath <- save_path
      }

      dir.create(dirname(fpath), showWarnings = FALSE, recursive = TRUE)
      ggplot2::ggsave(fpath, plot = p,
                      width = save_width, height = save_height,
                      dpi = save_dpi, bg = "white")
      message("\U0001F4BE \u0e1a\u0e31\u0e19\u0e17\u0e36\u0e01\u0e20\u0e32\u0e1e: ", fpath)
    }

    all_plots[[resp]] <- p
  }

  invisible(all_plots)
}
