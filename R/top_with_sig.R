#' ตรวจสอบความแตกต่างและใส่ตัวอักษร (โรยหน้าด้วยสถิติ)
#' 
#' @param model ผลลัพธ์จาก cook_rcbd
#' @param treatment ชื่อคอลัมน์ของกรรมวิธี
#' @export
top_with_sig <- function(model, treatment) {
  # ต้องโหลด library agricolae มาช่วย (อย่าลืมใส่ใน DESCRIPTION)
  # ในที่นี้ใช้คำสั่งพื้นฐานเพื่อให้เห็นภาพ
  res <- agricolae::duncan.test(model, treatment, main = "Yield Comparison")
  
  message("--- โรยหน้าเรียบร้อย! กลุ่มที่ตัวอักษรเหมือนกันแปลว่าไม่ต่างกันทางสถิติ ---")
  return(res$groups)
}