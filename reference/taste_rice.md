# ชิมข้าว — ตรวจสอบ Assumptions ก่อนวิเคราะห์สถิติ (taste_rice) v2.0

รองรับ CRD, RCBD และ Factorial Design

## Usage

``` r
taste_rice(
  data,
  response,
  treatment = "treatment",
  block = NULL,
  mode = c("both", "quick", "model"),
  alpha = 0.05,
  plot = TRUE,
  verbose = TRUE
)
```

## Arguments

- data:

  washed_rice object หรือ data.frame

- response:

  character ชื่อคอลัมน์ response (ระบุหลายตัวได้)

- treatment:

  character ชื่อคอลัมน์ treatment (ใส่ vector ได้ เช่น c("A", "B") สำหรับ
  Factorial)

- block:

  character ชื่อคอลัมน์ block สำหรับ RCBD (default = NULL = CRD)

- mode:

  "quick" = เช็คจาก raw data, "model" = เช็คจาก residuals, "both" =
  ทำทั้งสองแบบ (default)

- alpha:

  ระดับนัยสำคัญ (default = 0.05)

- plot:

  logical แสดง diagnostic plots (default = TRUE)

- verbose:

  logical แสดงรายงาน (default = TRUE)

## Value

object class "tasted_rice"
