# หุงข้าว CRD — Completely Randomized Design

วิเคราะห์สถิติแปลงทดลองแบบ CRD รองรับทั้ง Single Factor และ Factorial รับผลจาก
taste_rice() เพื่อเลือก Parametric / Non-parametric อัตโนมัติ

## Usage

``` r
cook_crd(
  data,
  response,
  treatment = "treatment",
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

- factors:

  character vector ชื่อปัจจัย (สำหรับ factorial เช่น c("variety",
  "fertilizer"))

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

  "CRD"

- data:

  data.frame ที่ใช้วิเคราะห์

- settings:

  list ค่าที่ตั้ง

## Examples

``` r
if (FALSE) { # \dontrun{
# Single factor
washed <- wash_rice(my_data)
tasted <- taste_rice(washed, response = "yield", mode = "model")
cooked <- cook_crd(washed, response = "yield", tasted = tasted)

# Factorial
cooked <- cook_crd(washed, response = "yield",
                   factors = c("variety", "fertilizer"),
                   tasted = tasted)

# ดูผล
cooked$results$yield$summary_table
cooked$results$yield$group_letters
} # }
```
