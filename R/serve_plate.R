#' สร้างกราฟรายงาน (จัดจานข้าวสวย)
#'
#' สร้าง bar chart สำหรับแสดงผลการทดลองเกษตร
#' รองรับการแสดงข้อความภาษาไทยผ่านการตั้งค่าฟอนต์ด้วย [setup_thai_font()]
#'
#' @param data data.frame ที่มีข้อมูลสำหรับสร้างกราฟ
#' @param x ชื่อคอลัมน์แกน x (กรรมวิธี)
#' @param y ชื่อคอลัมน์แกน y (ผลผลิต)
#' @param title หัวข้อกราฟ (default: `"รายงานผลการทดลอง"`)
#' @param font_family ชื่อฟอนต์ที่จะใช้ในกราฟ ควรตรงกับที่โหลดไว้ใน
#'   [setup_thai_font()] (default: `"Sarabun"`)
#'
#' @return ggplot object
#'
#' @examples
#' \dontrun{
#' setup_thai_font()
#' df <- data.frame(trt = factor(c("A", "B", "C")), yield = c(10, 15, 12))
#' serve_plate(df, "trt", "yield", title = "ผลผลิตข้าวเฉลี่ยตามกรรมวิธี")
#' }
#'
#' @importFrom rlang .data
#' @export
serve_plate <- function(data, x, y,
                        title = "รายงานผลการทดลอง",
                        font_family = "Sarabun") {
  ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[x]], y = .data[[y]], fill = .data[[x]])
  ) +
    ggplot2::geom_bar(stat = "identity", color = "black", width = 0.7) +
    ggplot2::scale_fill_brewer(palette = "Greens") +
    ggplot2::labs(title = title, x = "กรรมวิธี", y = "ผลผลิตเฉลี่ย") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = font_family),
      plot.title = ggplot2::element_text(
        family = font_family,
        face = "bold",
        size = 14
      ),
      axis.title = ggplot2::element_text(family = font_family),
      axis.text = ggplot2::element_text(family = font_family),
      legend.text = ggplot2::element_text(family = font_family)
    )
}
