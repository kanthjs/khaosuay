#' ตั้งค่าฟอนต์ภาษาไทยสำหรับกราฟ
#'
#' โหลดฟอนต์ภาษาไทยจาก Google Fonts และเปิดใช้งาน showtext
#' เพื่อให้ ggplot2 แสดงข้อความภาษาไทยได้ถูกต้อง
#' ต้องเรียกฟังก์ชันนี้ก่อนสร้างกราฟด้วย [serve_plate()]
#'
#' @param font_name ชื่อ Google Font ที่รองรับภาษาไทย (default: `"Sarabun"`)
#'   ตัวเลือกยอดนิยม: `"Sarabun"`, `"Prompt"`, `"Kanit"`, `"Noto Sans Thai"`
#'
#' @return ชื่อฟอนต์ (invisible) เพื่อใช้ส่งต่อไปยัง [serve_plate()]
#'
#' @examples
#' \dontrun{
#' setup_thai_font()
#' setup_thai_font("Prompt")
#' }
#'
#' @export
setup_thai_font <- function(font_name = "Sarabun") {
  if (!requireNamespace("showtext", quietly = TRUE)) {
    stop(
      "ต้องการแพ็กเกจ 'showtext' \n",
      "ติดตั้งด้วย: install.packages('showtext')"
    )
  }
  if (!requireNamespace("sysfonts", quietly = TRUE)) {
    stop(
      "ต้องการแพ็กเกจ 'sysfonts' \n",
      "ติดตั้งด้วย: install.packages('sysfonts')"
    )
  }

  sysfonts::font_add_google(font_name, font_name)
  showtext::showtext_auto()

  message("โหลดฟอนต์ '", font_name, "' เรียบร้อย พร้อมใช้งานกับ serve_plate()")
  invisible(font_name)
}
