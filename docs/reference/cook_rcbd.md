# หุงข้าว RCBD — Randomized Complete Block Design

วิเคราะห์สถิติแปลงทดลองแบบ RCBD รองรับ Single Factor และ Factorial in RCBD
รับผลจาก taste_rice() เพื่อเลือก Parametric / Non-parametric อัตโนมัติ

## Usage

``` r
cook_rcbd(
  data,
  response,
  treatment = "treatment",
  block = "rep",
  factors = NULL,
  tasted = NULL,
  posthoc = c("tukey", "lsd", "duncan"),
  alpha = 0.05,
  verbose = TRUE
)
```

## Arguments

- data:

  washed_rice object หรือ data.frame

- response:

  character ชื่อ response variable (ระบุหลายตัวได้)

- treatment:

  character ชื่อคอลัมน์ treatment (single factor)

- block:

  character ชื่อคอลัมน์ block/rep (default = "rep")

- factors:

  character vector ชื่อปัจจัย (สำหรับ factorial in RCBD)

- tasted:

  tasted_rice object จาก taste_rice() — ถ้ามีจะไม่เช็คซ้ำ

- posthoc:

  character วิธี post-hoc: "tukey" (default), "lsd", "duncan"

- alpha:

  numeric ระดับนัยสำคัญ (default = 0.05)

- verbose:

  logical แสดงรายงาน (default = TRUE)

## Value

object class "cooked_rice" (list) ประกอบด้วย:

- results:

  list ผลวิเคราะห์แยกรายตัวแปร

- design:

  "RCBD"

- data:

  data.frame ที่ใช้วิเคราะห์

- settings:

  list ค่าที่ตั้ง

## Examples

``` r
if (FALSE) { # \dontrun{
# Single factor RCBD
washed <- wash_rice(my_data, design_check = TRUE)
tasted <- taste_rice(washed, response = "yield", block = "rep")
cooked <- cook_rcbd(washed, response = "yield", block = "rep",
                    tasted = tasted)

# Factorial in RCBD
cooked <- cook_rcbd(washed, response = "yield",
                    factors = c("variety", "chemical"),
                    block = "rep", tasted = tasted)

# ดูผล
cooked$results$yield$summary_table
cooked$results$yield$cv_percent
} # }
```
