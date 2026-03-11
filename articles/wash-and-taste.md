# ซาวข้าวและชิมข้าว: ทำความสะอาดและตรวจสอบข้อมูล

``` r
library(khaosuay)
#> khaosuay: สำหรับกราฟภาษาไทย เรียก setup_thai_font() ก่อนใช้ serve_plate()
```

## ทำไมต้องซาวข้าว? (Why wash_rice?)

ข้อมูลจากแปลงทดลองทางการเกษตรมักมีปัญหาหลายอย่าง:

- ชื่อคอลัมน์ไม่สม่ำเสมอ (ภาษาไทย/อังกฤษ ตัวพิมพ์ใหญ่/เล็ก)
- ค่าว่าง (empty string) ที่ไม่ใช่ NA
- ตัวเลขถูกเก็บเป็น character (เช่น มีเครื่องหมาย comma)
- ค่าผิดปกติ (outlier) จากการคีย์ข้อมูลผิด
- แผนการทดลองไม่สมดุล (missing combinations)

[`wash_rice()`](https://kanthjs.github.io/KhaoSuay/reference/wash_rice.md)
จัดการปัญหาเหล่านี้อัตโนมัติ แต่ **ไม่ลบหรือแก้ไขข้อมูลดิบ** โดยตรง — เพียงแต่
**แจ้งเตือนและ flag** ให้คุณพิจารณาก่อนตัดสินใจ

------------------------------------------------------------------------

## ตัวอย่าง: ข้อมูลที่มีปัญหาหลายจุด

``` r
# สร้างข้อมูลที่มี "ปัญหา" หลายจุด
messy_data <- data.frame(
  Variety  = c("KDML105", "KDML105", "RD41", "RD41", "RD57", "RD57",
               "KDML105", "KDML105", "RD41", "RD41", "RD57", "RD57"),
  Rep      = c(1, 2, 1, 2, 1, 2, 3, 4, 3, 4, 3, 4),
  Yield    = c(650, 680, 590, 610, 720, 700, 660, 640, 600, 585, 710, 695),
  Height   = c(120, 125, 110, 115, 130, 128, 122, 118, 112, 108, 132, -5),
  stringsAsFactors = FALSE
)
messy_data
#>    Variety Rep Yield Height
#> 1  KDML105   1   650    120
#> 2  KDML105   2   680    125
#> 3     RD41   1   590    110
#> 4     RD41   2   610    115
#> 5     RD57   1   720    130
#> 6     RD57   2   700    128
#> 7  KDML105   3   660    122
#> 8  KDML105   4   640    118
#> 9     RD41   3   600    112
#> 10    RD41   4   585    108
#> 11    RD57   3   710    132
#> 12    RD57   4   695     -5
```

### การซาวข้าวแบบพื้นฐาน

``` r
washed <- wash_rice(messy_data, treatment_col = "Variety", rep_col = "Rep")
#> [INFO]  ปรับชื่อคอลัมน์ 4 ตัว
#> [MAP]   คอลัมน์กรรมวิธี: 'variety' (ไม่เปลี่ยนชื่อ)
#> [MAP]   คอลัมน์ซ้ำ: 'rep' (ไม่เปลี่ยนชื่อ)
#> [INFO]  แปลงเป็น Factor: rep
#> Warning in wash_rice(messy_data, treatment_col = "Variety", rep_col = "Rep"):
#> [WARN] พบค่าติดลบในคอลัมน์: height
#> [FLAG]  พบค่าผิดปกติ (outlier) รวม 1 จุด ในคอลัมน์: height → ดู $flagged
#> 
#> --------------------------------------------------
#> wash_rice v3.1 — สรุปผลการล้างข้าว
#> --------------------------------------------------
#>   • แถว:        12 -> 12
#>   • คอลัมน์:    4
#>   • Factor:     rep
#>   • Numeric:    yield, height
#>   • Outliers:   1 จุด
#>   • Flagged:    2 แถว -> ดูใน $flagged
#> --------------------------------------------------
#> ล้างข้าวเสร็จเรียบร้อย! ข้อมูลพร้อมหุงแล้ว
#> --------------------------------------------------
```

### ดูผลลัพธ์

``` r
# ข้อมูลที่สะอาดแล้ว
washed$data
#>    variety rep yield height
#> 1  KDML105   1   650    120
#> 2  KDML105   2   680    125
#> 3     RD41   1   590    110
#> 4     RD41   2   610    115
#> 5     RD57   1   720    130
#> 6     RD57   2   700    128
#> 7  KDML105   3   660    122
#> 8  KDML105   4   640    118
#> 9     RD41   3   600    112
#> 10    RD41   4   585    108
#> 11    RD57   3   710    132
#> 12    RD57   4   695     -5

# ดูว่าคอลัมน์ถูก map อย่างไร
washed$column_map
#>   original standardized
#> 1  Variety      variety
#> 2      Rep          rep
#> 3    Yield        yield
#> 4   Height       height

# ดูแถวที่ถูก flag
washed$flagged
#>     variety rep yield height .row_number              .flag_reason .flag_column
#> 12     RD57   4   695     -5          12         ค่าติดลบใน 'height'       height
#> 121    RD57   4   695     -5          12 outlier (iqr) ใน 'height'       height
#>     .flag_value
#> 12           -5
#> 121          -5
```

------------------------------------------------------------------------

## พารามิเตอร์สำคัญของ wash_rice()

### กำหนดพิสัยที่ยอมรับ (valid_ranges)

``` r
washed2 <- wash_rice(
  messy_data,
  treatment_col = "Variety",
  rep_col       = "Rep",
  valid_ranges  = list(yield = c(0, 1500), height = c(0, 200)),
  design_check  = TRUE
)
#> [INFO]  ปรับชื่อคอลัมน์ 4 ตัว
#> [MAP]   คอลัมน์กรรมวิธี: 'variety' (ไม่เปลี่ยนชื่อ)
#> [MAP]   คอลัมน์ซ้ำ: 'rep' (ไม่เปลี่ยนชื่อ)
#> [INFO]  แปลงเป็น Factor: rep
#> Warning in wash_rice(messy_data, treatment_col = "Variety", rep_col = "Rep", :
#> [WARN] พบค่าติดลบในคอลัมน์: height
#> Warning in wash_rice(messy_data, treatment_col = "Variety", rep_col = "Rep", :
#> [WARN] 'height': พบ 1 ค่านอกพิสัย [0-200]
#> [FLAG]  พบค่าผิดปกติ (outlier) รวม 1 จุด ในคอลัมน์: height → ดู $flagged
#> Warning in wash_rice(messy_data, treatment_col = "Variety", rep_col = "Rep", : [WARN] ไม่สามารถเช็คแผนการทดลองได้ — ไม่พบคอลัมน์: treatment
#>   ลองระบุผ่าน treatment_col / rep_col
#>   หรือเพิ่มใน custom_aliases
#> 
#> --------------------------------------------------
#> wash_rice v3.1 — สรุปผลการล้างข้าว
#> --------------------------------------------------
#>   • แถว:        12 -> 12
#>   • คอลัมน์:    4
#>   • Factor:     rep
#>   • Numeric:    yield, height
#>   • Outliers:   1 จุด
#>   • Flagged:    3 แถว -> ดูใน $flagged
#> --------------------------------------------------
#> ล้างข้าวเสร็จเรียบร้อย! ข้อมูลพร้อมหุงแล้ว
#> --------------------------------------------------
```

เมื่อกำหนด `valid_ranges` ค่าที่อยู่นอกพิสัยจะถูก flag แต่ไม่ถูกลบออก
ให้คุณกลับไปตรวจสอบข้อมูลดิบเอง

### เปลี่ยนวิธีตรวจ Outlier

``` r
# ใช้ z-score แทน IQR
washed3 <- wash_rice(messy_data,
  outlier_method    = "zscore",
  outlier_threshold = 3
)

# ปิดการตรวจ outlier
washed4 <- wash_rice(messy_data, outlier_method = "none")
```

### Smart Alias Dictionary

[`wash_rice()`](https://kanthjs.github.io/KhaoSuay/reference/wash_rice.md)
มี dictionary ที่จับคู่ชื่อคอลัมน์ภาษาไทย/อังกฤษอัตโนมัติ:

| Role      | ชื่อที่รองรับ                                                                                                  |
|-----------|-----------------------------------------------------------------------------------------------------------|
| Treatment | treatment, trt, variety, var, genotype, gen, entry, line, cultivar, cv, สายพันธุ์, กรรมวิธี, พันธุ์, สูตร, ทรีทเมนต์ |
| Rep/Block | rep, block, blk, replication, replicate, r, ซ้ำ, บล็อก, กลุ่ม                                                 |

สามารถเพิ่มเองผ่าน `custom_aliases`:

``` r
washed5 <- wash_rice(
  data = my_data,
  custom_aliases = list(
    treatment = c("formula", "fungicide"),
    rep       = c("set", "round")
  )
)
```

------------------------------------------------------------------------

## ทำไมต้องชิมข้าว? (Why taste_rice?)

ก่อนจะ “หุงข้าว” (วิเคราะห์สถิติ) เราต้องรู้ก่อนว่าข้อมูลมีลักษณะเป็นอย่างไร
เพื่อเลือกสถิติที่เหมาะสม:

| ลักษณะข้อมูล                 | ผลการชิม        | สถิติที่ควรใช้                    |
|---------------------------|----------------|------------------------------|
| Normal + Equal Variance   | Parametric     | ANOVA + Tukey HSD            |
| Normal + Unequal Variance | Welch          | Welch’s ANOVA + Games-Howell |
| Non-normal                | Non-parametric | Kruskal-Wallis + Dunn’s test |

[`taste_rice()`](https://kanthjs.github.io/KhaoSuay/reference/taste_rice.md)
ทดสอบให้อัตโนมัติ และส่งคำตอบนี้ให้ `cook_*()` เลือกสถิติที่ถูกต้อง

------------------------------------------------------------------------

## ตัวอย่าง: ชิมข้าวแบบต่าง ๆ

### Quick mode: ตรวจจาก raw data

``` r
set.seed(42)
trial_data <- data.frame(
  variety = rep(c("A", "B", "C", "D"), each = 5),
  rep     = rep(1:5, times = 4),
  yield   = c(
    rnorm(5, 600, 30), rnorm(5, 550, 25),
    rnorm(5, 680, 35), rnorm(5, 590, 20)
  )
)

washed <- wash_rice(trial_data, treatment_col = "variety", rep_col = "rep",
                    verbose = FALSE)

tasted_quick <- taste_rice(washed, response = "yield", treatment = "variety",
                           mode = "quick", plot = FALSE, verbose = FALSE)
print(tasted_quick)
#> ── tasted_rice v2.0 ──
#>   Design: Single Factor 
#>   Mode: quick 
#> 
#>  response final_class recommendation                         
#>  yield    Non-normal  🚨 ใช้ Kruskal-Wallis test → Dunn's test
#> 
#>   yield [4 กลุ่ม]: 🚨 ใช้ Kruskal-Wallis test → Dunn's test
```

### Model mode: ตรวจจาก residuals ของ ANOVA model

``` r
tasted_model <- taste_rice(washed, response = "yield", treatment = "variety",
                           mode = "model", plot = FALSE, verbose = FALSE)
print(tasted_model)
#> ── tasted_rice v2.0 ──
#>   Design: Single Factor 
#>   Mode: model 
#> 
#>  response final_class             recommendation                  
#>  yield    Normal + Equal Variance ✅ ใช้ Fisher's ANOVA → Tukey HSD
#> 
#>   yield [4 กลุ่ม]: ✅ ใช้ Fisher's ANOVA → Tukey HSD
```

### Both mode (แนะนำ): ตรวจทั้ง raw data และ residuals

``` r
tasted_both <- taste_rice(washed, response = "yield", treatment = "variety",
                          mode = "both", plot = FALSE, verbose = FALSE)
print(tasted_both)
#> ── tasted_rice v2.0 ──
#>   Design: Single Factor 
#>   Mode: both 
#> 
#>  response final_class             recommendation                  
#>  yield    Normal + Equal Variance ✅ ใช้ Fisher's ANOVA → Tukey HSD
#> 
#>   yield [4 กลุ่ม]: ✅ ใช้ Fisher's ANOVA → Tukey HSD
```

**หมายเหตุ:** เมื่อใช้ `mode = "both"` ผลจาก residuals (model)
จะถูกใช้เป็นผลสุดท้าย เพราะเป็นวิธีที่ถูกต้องกว่าทางสถิติ

------------------------------------------------------------------------

## ชิมข้าวหลายตัวแปรพร้อมกัน

``` r
trial_data$height <- c(
  rnorm(5, 120, 5), rnorm(5, 115, 8),
  rnorm(5, 130, 6), rnorm(5, 118, 4)
)

washed <- wash_rice(trial_data, treatment_col = "variety", rep_col = "rep",
                    verbose = FALSE)

tasted_multi <- taste_rice(
  washed,
  response  = c("yield", "height"),
  treatment = "variety",
  mode      = "both",
  plot      = FALSE,
  verbose   = FALSE
)
print(tasted_multi)
#> ── tasted_rice v2.0 ──
#>   Design: Single Factor 
#>   Mode: both 
#> 
#>  response final_class             recommendation                  
#>  yield    Normal + Equal Variance ✅ ใช้ Fisher's ANOVA → Tukey HSD
#>  height   Normal + Equal Variance ✅ ใช้ Fisher's ANOVA → Tukey HSD
#> 
#>   yield [4 กลุ่ม]: ✅ ใช้ Fisher's ANOVA → Tukey HSD 
#>   height [4 กลุ่ม]: ✅ ใช้ Fisher's ANOVA → Tukey HSD
```

ฟังก์ชันจะตรวจทุกตัวแปรที่ระบุ และบอกคำแนะนำแยกรายตัว ผลนี้ส่งต่อไปยัง `cook_*()`
ได้ทันที เพื่อให้แต่ละตัวแปรถูกวิเคราะห์ด้วยสถิติที่ถูกต้อง

------------------------------------------------------------------------

## ชิมข้าวสำหรับ RCBD (มี Block)

``` r
tasted_rcbd <- taste_rice(
  washed,
  response  = "yield",
  treatment = "variety",
  block     = "rep",
  mode      = "both",
  plot      = FALSE,
  verbose   = FALSE
)
print(tasted_rcbd)
#> ── tasted_rice v2.0 ──
#>   Design: Single Factor 
#>   Mode: both 
#> 
#>  response final_class             recommendation                  
#>  yield    Normal + Equal Variance ✅ ใช้ Fisher's ANOVA → Tukey HSD
#> 
#>   yield [4 กลุ่ม]: ✅ ใช้ Fisher's ANOVA → Tukey HSD
```

เมื่อระบุ `block` ฟังก์ชันจะรวม block เข้าไปใน model สำหรับการตรวจ residuals

------------------------------------------------------------------------

## สรุป Workflow ขั้นตอนที่ 1-2

``` r
# ขั้นตอนที่ 1: ซาวข้าว
washed <- wash_rice(
  data          = raw_data,
  treatment_col = "variety",
  rep_col       = "rep",
  valid_ranges  = list(yield = c(0, 1500)),
  design_check  = TRUE
)

# ตรวจสอบ flagged rows -> กลับไปแก้ไขข้อมูลดิบถ้าจำเป็น
washed$flagged

# ขั้นตอนที่ 2: ชิมข้าว
tasted <- taste_rice(
  data      = washed,
  response  = c("yield", "height"),
  treatment = "variety",
  mode      = "both"
)

# ดูคำแนะนำ
tasted$verdict

# พร้อมส่งต่อไป cook_crd() / cook_rcbd() / cook_split()
```
