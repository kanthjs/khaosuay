#' วิเคราะห์แผนการทดลองแบบ RCBD (หุงข้าว RCBD)
#'
#' @param data data.frame ที่มีข้อมูล
#' @param outcome ชื่อคอลัมน์ของผลผลิต (เช่น yield)
#' @param treatment ชื่อคอลัมน์ของกรรมวิธี (เช่น variety)
#' @param block ชื่อคอลัมน์ของ Block (เช่น replication)
#' @return รายการผลลัพธ์ ANOVA
#' @export
cook_rcbd <- function(data, outcome, treatment, block) {
  
  # สร้างสูตรโมเดล: outcome ~ treatment + block
  formula_str <- paste(outcome, "~", treatment, "+", block)
  formula_obj <- stats::as.formula(formula_str)
  
  # วิเคราะห์ ANOVA
  model <- stats::aov(formula_obj, data = data)
  
  message("--- หุงข้าว RCBD เสร็จแล้ว! กำลังตรวจสอบความนุ่มของข้อมูล ---")
  return(model)
}

#' @examples
#' library(agridat)
#' data(gomez.nitrogen)
#' model <- cook_rcbd(gomez.nitrogen, "nitro","trt", "rep")
#' summary(model)
