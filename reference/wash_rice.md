# ทำความสะอาดและเตรียมข้อมูลเกษตร (ล้างข้าวสาร) v3.1 — Smart Mapper + Full Pipeline

ฟังก์ชันนี้ออกแบบสำหรับ cleaning ข้อมูลแปลงทดลองและข้อมูลสำรวจทางการเกษตร รองรับ
dataframe ที่ตั้งชื่อคอลัมน์หลากหลาย ทั้งภาษาไทยและอังกฤษ ผ่านระบบ Smart Alias
Dictionary ที่จับคู่ชื่อคอลัมน์อัตโนมัติ

## Usage

``` r
wash_rice(
  data,
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
  verbose = TRUE
)
```

## Arguments

- data:

  data.frame หรือ tibble

- treatment_col:

  ระบุชื่อคอลัมน์ Treatment ชัดเจน (ถ้าไม่ระบุจะเดาจาก dictionary)

- rep_col:

  ระบุชื่อคอลัมน์ Rep/Block ชัดเจน (ถ้าไม่ระบุจะเดาจาก dictionary)

- factor_cols:

  คอลัมน์เพิ่มเติมที่อยากบังคับเป็น factor

- numeric_cols:

  คอลัมน์ที่ต้องเป็นตัวเลข (ถ้าไม่ระบุจะ auto-detect)

- valid_ranges:

  named list กำหนดพิสัยที่ยอมรับ เช่น list(severity = c(0, 100))

- design_check:

  logical ตรวจสอบความสมบูรณ์ของแผนการทดลอง

- outlier_method:

  วิธีตรวจ outlier: "iqr" (default), "zscore", หรือ "none"

- outlier_threshold:

  ค่า threshold (default: 1.5 สำหรับ IQR, 3 สำหรับ z-score)

- remove_duplicates:

  logical ลบแถวซ้ำอัตโนมัติ (default = FALSE แค่เตือน)

- check_gps:

  logical ตรวจสอบพิกัด GPS (default = TRUE ถ้าพบคอลัมน์พิกัด)

- thailand_only:

  logical เช็คว่าพิกัดอยู่ในประเทศไทย (default = TRUE)

- custom_aliases:

  named list เพิ่ม alias เอง เช่น list(treatment = c("formula"), rep =
  c("set"))

- verbose:

  logical แสดงรายงานสรุป (default = TRUE)

## Value

object class "washed_rice" (list) ประกอบด้วย:

- data:

  data.frame ที่สะอาดแล้ว

- report:

  list สรุปทุกขั้นตอน

- flagged:

  data.frame แถวที่ถูก flag (outlier, out-of-range)

- column_map:

  data.frame แสดงการจับคู่ชื่อคอลัมน์

## Examples

``` r
if (FALSE) { # \dontrun{
# ใช้งานแบบด่วน — ปล่อยให้ฟังก์ชันเดาเอง
result <- wash_rice(my_data)
clean <- result$data

# งานแปลงทดลอง RCBD
result <- wash_rice(my_data,
  treatment_col = "\u0e2a\u0e32\u0e22\u0e1e\u0e31\u0e19\u0e18\u0e38\u0e4c",
  rep_col       = "\u0e0b\u0e49\u0e33",
  valid_ranges  = list(yield = c(0, 1500), severity = c(0, 100)),
  design_check  = TRUE
)

# งานสำรวจระบาดวิทยา
result <- wash_rice(survey_data,
  valid_ranges = list(severity = c(0, 100), incidence = c(0, 100)),
  outlier_method = "zscore"
)
} # }
```
