#' ทำความสะอาดข้อมูลเบื้องต้น (ล้างข้าว)
#'
#' ลบแถวที่มีค่า NA ออกจากข้อมูลก่อนนำไปวิเคราะห์ด้วย [cook_rcbd()]
#'
#' @param data data.frame ที่ต้องการทำความสะอาด
#'
#' @return data.frame ที่ลบแถวที่มี NA ออกแล้ว
#'
#' @examples
#' df <- data.frame(x = c(1, 2, NA, 4), y = c("a", "b", "c", NA))
#' wash_rice(df)
#'
#' @export
wash_rice <- function(data) {
  clean_data <- stats::na.omit(data)
  message("ทำความสะอาดเมล็ดข้าวเรียบร้อย: ลบข้อมูลที่ไม่สมบูรณ์ออกแล้ว")
  return(clean_data)
}
