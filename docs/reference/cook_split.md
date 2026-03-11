# หุงข้าว Split-plot — Split-plot Design

วิเคราะห์สถิติแปลงทดลองแบบ Split-plot ที่มี error term สองระดับ: Main-plot
error: ใช้ทดสอบ main-plot factor Sub-plot error: ใช้ทดสอบ sub-plot factor
และ interaction

## Usage

``` r
cook_split(
  data,
  response,
  main_plot,
  sub_plot,
  block = "rep",
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

- main_plot:

  character ชื่อ main-plot factor (ปัจจัยที่สุ่มในระดับแปลงใหญ่ เช่น พันธุ์ข้าว, วิธีไถ)

- sub_plot:

  character ชื่อ sub-plot factor (ปัจจัยที่สุ่มในระดับแปลงย่อย เช่น อัตราปุ๋ย, สารเคมี)

- block:

  character ชื่อ block/rep (default = "rep")

- tasted:

  tasted_rice object จาก taste_rice() (optional)

- posthoc:

  character วิธี post-hoc: "tukey" (default), "lsd", "duncan"

- alpha:

  numeric ระดับนัยสำคัญ (default = 0.05)

- verbose:

  logical แสดงรายงาน (default = TRUE)

## Value

object class "cooked_rice" (list)

## Details

Formula ที่สร้างอัตโนมัติ: y ~ main_plot \* sub_plot + Error(block/main_plot)

Split-plot design ใช้เมื่อปัจจัยหนึ่งยากต่อการสุ่มในระดับเล็ก เช่น พันธุ์ข้าว
(ต้องปลูกเป็นแปลงใหญ่) x อัตราปุ๋ย (สุ่มในแปลงย่อย)

ANOVA table แยกเป็น 2 ส่วน: Main-plot stratum: block, main_plot,
Error(block:main_plot) Sub-plot stratum: sub_plot, main_plot:sub_plot,
Residuals

Post-hoc ใช้ error term ที่ถูกต้องตามระดับ: main_plot effect -\> main-plot
error (block:main_plot) sub_plot effect -\> sub-plot error (Residuals)
interaction -\> sub-plot error (Residuals)

## Examples

``` r
if (FALSE) { # \dontrun{
washed <- wash_rice(my_data)
cooked <- cook_split(washed,
  response  = "yield",
  main_plot = "variety",
  sub_plot  = "fertilizer",
  block     = "rep"
)
cooked$results$yield$anova_table
cooked$results$yield$posthoc_main
cooked$results$yield$posthoc_sub
} # }
```
