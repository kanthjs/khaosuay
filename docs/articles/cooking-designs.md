# การหุงข้าว: วิเคราะห์ CRD, RCBD และ Split-plot

``` r
library(khaosuay)
#> khaosuay: สำหรับกราฟภาษาไทย เรียก setup_thai_font() ก่อนใช้ serve_plate()
```

## เลือกหม้อหุงข้าวให้ตรงกับแผนการทดลอง

khaosuay มี “หม้อหุงข้าว” 3 ใบ ให้เลือกตามแผนการทดลอง:

| แผนการทดลอง | ฟังก์ชัน | เมื่อไรใช้ |
|----|----|----|
| **CRD** (Completely Randomized Design) | [`cook_crd()`](https://kanthjs.github.io/KhaoSuay/reference/cook_crd.md) | สิ่งทดลองถูกสุ่มเสร็จสมบูรณ์ ไม่มี block |
| **RCBD** (Randomized Complete Block Design) | [`cook_rcbd()`](https://kanthjs.github.io/KhaoSuay/reference/cook_rcbd.md) | มี block/ซ้ำ เพื่อควบคุมความแปรปรวน |
| **Split-plot** | [`cook_split()`](https://kanthjs.github.io/KhaoSuay/reference/cook_split.md) | มีปัจจัย 2 ระดับ (main-plot + sub-plot) |

ทุกฟังก์ชันทำงานเหมือนกัน:

1.  รับผลจาก
    [`taste_rice()`](https://kanthjs.github.io/KhaoSuay/reference/taste_rice.md)
    เพื่อเลือกสถิติอัตโนมัติ
2.  ถ้าข้อมูล **Normal + Equal Variance** → ANOVA + Post-hoc
    (Tukey/LSD/Duncan)
3.  ถ้าข้อมูล **Non-normal** → Kruskal-Wallis + Dunn’s test
4.  คืนผลเป็น `cooked_rice` object พร้อมใช้ใน
    [`plot_cooked()`](https://kanthjs.github.io/KhaoSuay/reference/plot_cooked.md)

------------------------------------------------------------------------

## 1. CRD — Completely Randomized Design

### Single factor

``` r
set.seed(123)
crd_data <- data.frame(
  treatment = rep(c("Control", "Fert_A", "Fert_B", "Fert_C"), each = 4),
  rep       = rep(1:4, times = 4),
  yield     = c(
    rnorm(4, 500, 30), rnorm(4, 580, 25),
    rnorm(4, 620, 35), rnorm(4, 560, 28)
  ),
  height    = c(
    rnorm(4, 100, 5), rnorm(4, 110, 6),
    rnorm(4, 115, 4), rnorm(4, 108, 7)
  )
)

# ซาวข้าว
washed <- wash_rice(crd_data, verbose = FALSE)

# ชิมข้าว
tasted <- taste_rice(washed, response = c("yield", "height"),
                     treatment = "treatment",
                     mode = "both", plot = FALSE, verbose = FALSE)

# หุงข้าว CRD
cooked_crd <- cook_crd(
  data      = washed,
  response  = c("yield", "height"),
  treatment = "treatment",
  tasted    = tasted,
  posthoc   = "tukey"
)
#> 
#> 🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾
#> 🍚  cook_crd v1.1 — ผลการหุงข้าว (CRD)
#> ────────────────────────────────────────────────────────────
#> ────────────────────────────────────────────────────────────
#> 🌾 yield (n=16)
#>   Formula:     yield ~ treatment
#>   Assumption:  Normal + Equal Variance  [✅ จาก taste_rice (ไม่เช็คซ้ำ)]
#>   Test:        Fisher's ANOVA (Type I)
#>   p-value:     0.0008185 ***
#>   CV%:         5.1%
#> 
#>   📊 Post-Hoc: Tukey's HSD
#>   ---------------------------------------------
#>     Control  mean=  506.29 ± 28.07  [b]
#>     Fert_A   mean=  586.50 ± 30.62  [a]
#>     Fert_B   mean=  623.95 ± 30.29  [a]
#>     Fert_C   mean=  572.20 ± 27.60  [a]
#> ────────────────────────────────────────────────────────────
#> 🌾 height (n=16)
#>   Formula:     height ~ treatment
#>   Assumption:  Normal + Equal Variance  [✅ จาก taste_rice (ไม่เช็คซ้ำ)]
#>   Test:        Fisher's ANOVA (Type I)
#>   p-value:     0.01123 *
#>   CV%:         4.98%
#> 
#>   📊 Post-Hoc: Tukey's HSD
#>   ---------------------------------------------
#>     Control  mean=   98.45 ±  6.09  [b]
#>     Fert_A   mean=  105.44 ±  2.35  [ab]
#>     Fert_B   mean=  113.68 ±  4.34  [a]
#>     Fert_C   mean=  108.43 ±  7.14  [ab]
#> ────────────────────────────────────────────────────────────
#> 📋 $results$<var>$summary_table → ตารางพร้อมตีพิมพ์
#>    $results$<var>$group_letters → กลุ่มอักษร (a, b, c)
#>    $results$<var>$anova_table   → ANOVA table
#>    $results$<var>$model         → aov object
#> 🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾
```

### ดูผลลัพธ์

``` r
# ตาราง summary พร้อม group letters
cooked_crd$results$yield$summary_table
#>   treatment n   mean    sd    se    min    max group mean_group
#> 1   Control 4 506.29 28.07 14.03 483.19 546.76     b   506.29 b
#> 2    Fert_A 4 586.50 30.62 15.31 548.37 622.88     a   586.50 a
#> 3    Fert_B 4 623.95 30.29 15.15 595.96 662.84     a   623.95 a
#> 4    Fert_C 4 572.20 27.60 13.80 544.44 610.03     a   572.20 a

# ANOVA table
cooked_crd$results$yield$anova_table
#>             Df Sum Sq Mean Sq F value   Pr(>F)    
#> treatment    3  28907    9636   11.32 0.000819 ***
#> Residuals   12  10215     851                     
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# CV%
cooked_crd$results$yield$cv_percent
#> [1] 5.1

# Group letters (สำหรับ plot)
cooked_crd$results$yield$group_letters
#>  Fert_B  Fert_A  Fert_C Control 
#>     "a"     "a"     "a"     "b"
```

### Factorial CRD

สำหรับ Factorial design ใช้ `factors` แทน `treatment`:

``` r
set.seed(456)
fact_data <- data.frame(
  variety    = rep(rep(c("V1", "V2"), each = 3), 4),
  fertilizer = rep(rep(c("Low", "Medium", "High"), times = 2), 4),
  rep        = rep(1:4, each = 6),
  yield      = rnorm(24, mean = rep(c(500, 550, 580, 520, 600, 650), 4), sd = 25)
)

washed_f <- wash_rice(fact_data, verbose = FALSE)
tasted_f <- taste_rice(washed_f, response = "yield",
                       treatment = c("variety", "fertilizer"),
                       mode = "both", plot = FALSE, verbose = FALSE)

cooked_fact <- cook_crd(
  data     = washed_f,
  response = "yield",
  factors  = c("variety", "fertilizer"),
  tasted   = tasted_f
)
#> 
#> 🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾
#> 🍚  cook_crd v1.1 — ผลการหุงข้าว (CRD)
#> ────────────────────────────────────────────────────────────
#> ────────────────────────────────────────────────────────────
#> 🌾 yield (n=24)
#>   Formula:     yield ~ variety * fertilizer
#>   Assumption:  Normal + Equal Variance  [✅ จาก taste_rice (ไม่เช็คซ้ำ)]
#>   Test:        Fisher's ANOVA (Type I)
#>   p-value [variety]: 0.02141 *
#>   p-value [fertilizer]: 1.347e-05 ***
#>   p-value [variety:fertilizer]: 0.04574 *
#>   CV%:         5.48%
#> 
#>   📊 Post-Hoc: Tukey's HSD
#>   ---------------------------------------------
#>     V1:High    mean=  579.33 ± 28.70  [NA]
#>     V2:High    mean=  659.89 ± 17.02  [NA]
#>     V1:Low     mean=  516.35 ± 37.49  [NA]
#>     V2:Low     mean=  516.34 ± 43.09  [NA]
#>     V1:Medium  mean=  575.40 ± 17.20  [NA]
#>     V2:Medium  mean=  591.75 ± 35.26  [NA]
#> ────────────────────────────────────────────────────────────
#> 📋 $results$<var>$summary_table → ตารางพร้อมตีพิมพ์
#>    $results$<var>$group_letters → กลุ่มอักษร (a, b, c)
#>    $results$<var>$anova_table   → ANOVA table
#>    $results$<var>$model         → aov object
#> 🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾
```

ผลวิเคราะห์จะแสดง p-value ของ main effects และ interaction:

``` r
cooked_fact$results$yield$summary_table
#>   treatment n   mean    sd    se    min    max group mean_group
#> 1   V1:High 4 579.33 28.70 14.35 543.98 605.18  <NA>    579.33 
#> 2   V2:High 4 659.89 17.02  8.51 641.90 682.78  <NA>    659.89 
#> 3    V1:Low 4 516.35 37.49 18.75 466.41 557.00  <NA>    516.35 
#> 4    V2:Low 4 516.34 43.09 21.55 477.07 568.68  <NA>    516.34 
#> 5 V1:Medium 4 575.40 17.20  8.60 556.26 591.35  <NA>    575.40 
#> 6 V2:Medium 4 591.75 35.26 17.63 564.33 643.42  <NA>    591.75
```

------------------------------------------------------------------------

## 2. RCBD — Randomized Complete Block Design

RCBD เหมือน CRD แต่เพิ่ม `block` เข้าไปใน model เพื่อควบคุมความแปรปรวนจาก block
(ซ้ำ):

``` r
set.seed(789)
rcbd_data <- data.frame(
  treatment = rep(c("T1", "T2", "T3", "T4"), times = 4),
  rep       = rep(1:4, each = 4),
  yield     = c(
    rnorm(4, c(600, 550, 700, 620), 20),
    rnorm(4, c(610, 560, 710, 630), 20),
    rnorm(4, c(590, 540, 690, 610), 20),
    rnorm(4, c(605, 555, 705, 625), 20)
  )
)

washed_r <- wash_rice(rcbd_data, verbose = FALSE)
tasted_r <- taste_rice(washed_r, response = "yield",
                       treatment = "treatment", block = "rep",
                       mode = "both", plot = FALSE, verbose = FALSE)

cooked_rcbd <- cook_rcbd(
  data      = washed_r,
  response  = "yield",
  treatment = "treatment",
  block     = "rep",
  tasted    = tasted_r,
  posthoc   = "tukey"
)
#> 
#> 🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾
#> 🍚  cook_rcbd v1.1 — ผลการหุงข้าว (RCBD)
#> ────────────────────────────────────────────────────────────
#> ────────────────────────────────────────────────────────────
#> 🌾 yield (n=16)
#>   Formula:     yield ~ treatment + rep
#>   Assumption:  Normal + Equal Variance  [✅ จาก taste_rice (ไม่เช็คซ้ำ)]
#>   Test:        Fisher's ANOVA (RCBD)
#>   p-value:     5.597e-06 ***
#>   CV%:         3.06%
#> 
#>   📊 Post-Hoc: Tukey's HSD
#>   ---------------------------------------------
#>     T1  mean=  596.12 ± 18.01  [b]
#>     T2  mean=  538.78 ± 23.00  [c]
#>     T3  mean=  700.45 ± 17.23  [a]
#>     T4  mean=  612.41 ± 16.72  [b]
#> ────────────────────────────────────────────────────────────
#> 📋 $results$<var>$summary_table → ตารางพร้อมตีพิมพ์
#>    $results$<var>$group_letters → กลุ่มอักษร (a, b, c)
#>    $results$<var>$anova_table   → ANOVA table
#>    $results$<var>$model         → aov object
#> 🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾
```

``` r
cooked_rcbd$results$yield$summary_table
#>   treatment n   mean    sd    se    min    max group mean_group
#> 1        T1 4 596.12 18.01  9.00 569.78 610.48     b   596.12 b
#> 2        T2 4 538.78 23.00 11.50 504.78 554.79     c   538.78 c
#> 3        T3 4 700.45 17.23  8.62 681.95 723.56     a   700.45 a
#> 4        T4 4 612.41 16.72  8.36 589.94 626.51     b   612.41 b
cooked_rcbd$results$yield$cv_percent
#> [1] 3.06
```

### ความแตกต่างจาก CRD

- ANOVA table จะมี source of variation เพิ่มจาก block
- CV% จะต่ำกว่า CRD (เพราะ block ดูดซับความแปรปรวนไปส่วนหนึ่ง)
- ถ้าข้อมูล Non-normal → ใช้ Friedman’s test (Non-parametric RCBD) แทน
  Kruskal-Wallis

### Factorial in RCBD

``` r
set.seed(123)
  df <- expand.grid(
    variety = c("RD6", "KDML105"),
    chemical = c("Control", "Fung_A", "Fung_B"),
    rep = 1:4,
    stringsAsFactors = FALSE
  )
  df$yield <- with(df, {
    v <- ifelse(variety == "KDML105", 180, 0)
    ch <- ifelse(chemical == "Fung_A", 50, ifelse(chemical == "Fung_B", 80, 0))
    500 + v + ch + rnorm(nrow(df), 0, 20)
  })

washed_r <- wash_rice(df, verbose = FALSE)

cooked_fact_rcbd <- cook_rcbd(
  data     = washed_r,
  response = "yield",
  factors  = c("variety", "chemical"),
  block    = "rep",
  tasted   = tasted
)
```

------------------------------------------------------------------------

## 3. Split-plot Design

Split-plot ใช้เมื่อปัจจัยหนึ่งยากต่อการสุ่มในระดับเล็ก:

- **Main-plot factor**: ปัจจัยที่สุ่มในระดับแปลงใหญ่ (เช่น พันธุ์ข้าว วิธีไถ)
- **Sub-plot factor**: ปัจจัยที่สุ่มในระดับแปลงย่อย (เช่น อัตราปุ๋ย สารเคมี)

``` r
set.seed(321)
split_data <- data.frame(
  variety    = rep(rep(c("V1", "V2", "V3"), each = 3), 3),
  fertilizer = rep(c("N0", "N50", "N100"), times = 9),
  rep        = rep(1:3, each = 9),
  yield      = c(
    rnorm(9, c(400,450,480, 420,500,550, 380,430,470), 20),
    rnorm(9, c(410,460,490, 430,510,560, 390,440,480), 20),
    rnorm(9, c(405,455,485, 425,505,555, 385,435,475), 20)
  )
)

washed_s <- wash_rice(split_data, verbose = FALSE)
tasted_s <- taste_rice(washed_s, response = "yield",
                       treatment = c("variety", "fertilizer"),
                       block = "rep",
                       mode = "both", plot = FALSE, verbose = FALSE)

cooked_split <- cook_split(
  data      = washed_s,
  response  = "yield",
  main_plot = "variety",
  sub_plot  = "fertilizer",
  block     = "rep",
  tasted    = tasted_s
)
#> 
#> 🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾
#> 🍚  cook_split v1.0 — ผลการหุงข้าว (Split-plot)
#> ────────────────────────────────────────────────────────────
#>   Main-plot:  variety
#>   Sub-plot:   fertilizer
#>   Block:      rep
#> ────────────────────────────────────────────────────────────
#> ────────────────────────────────────────────────────────────
#> 🌾 yield (n=27)
#>   Formula:     yield ~ variety * fertilizer + Error(rep/variety)
#>   Assumption:  Normal + Equal Variance  [✅ จาก taste_rice]
#>   Test:        Split-plot ANOVA
#> 
#>   p-values:
#>     variety                    0.002676 ** [main-plot error]
#>     fertilizer                1.061e-06 *** [sub-plot error]
#>     variety:fertilizer           0.1564 ns [sub-plot error]
#> 
#>   CV% (main-plot): 3.77%
#>   CV% (sub-plot):  4.35%
#> 
#>   📊 Post-Hoc [variety (main-plot error)]: Tukey's HSD
#>   ---------------------------------------------
#>     V2  mean=  497.92  [a]
#>     V1  mean=  457.08  [b]
#>     V3  mean=  428.06  [c]
#> 
#>   📊 Post-Hoc [fertilizer (sub-plot error)]: Tukey's HSD
#>   ---------------------------------------------
#>     N100  mean=  501.88  [a]
#>     N50   mean=  474.28  [b]
#>     N0    mean=  406.90  [c]
#> ────────────────────────────────────────────────────────────
#> 📋 $results$<var>$anova_table      → Split-plot ANOVA
#>    $results$<var>$summary_table   → ตาราง interaction
#>    $results$<var>$summary_main    → ตาราง main-plot
#>    $results$<var>$summary_sub     → ตาราง sub-plot
#>    $results$<var>$posthoc_main    → post-hoc (main-plot error)
#>    $results$<var>$posthoc_sub     → post-hoc (sub-plot error)
#>    $results$<var>$posthoc_interaction → post-hoc interaction
#>    $results$<var>$error_terms     → df/MS ของแต่ละ error term
#> 🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾🌾
```

### ผลลัพธ์ Split-plot

Split-plot มี error term 2 ระดับ:

- **Main-plot error** (`block:main_plot`): ใช้ทดสอบ main-plot factor
- **Sub-plot error** (`Residuals`): ใช้ทดสอบ sub-plot factor และ
  interaction

``` r
# ตาราง interaction
cooked_split$results$yield$summary_table
#>   treatment n   mean    sd    se    min    max group mean_group
#> 1     V1:N0 3 416.54 17.57 10.14 398.96 434.10  <NA>    416.54 
#> 2     V2:N0 3 424.74  8.24  4.76 417.61 433.77  <NA>    424.74 
#> 3     V3:N0 3 379.43 13.24  7.65 369.84 394.54  <NA>    379.43 
#> 4   V1:N100 3 499.16 22.91 13.23 474.44 519.69  <NA>    499.16 
#> 5   V2:N100 3 541.95 11.74  6.78 533.56 555.36  <NA>    541.95 
#> 6   V3:N100 3 464.52 31.84 18.38 428.38 488.41  <NA>    464.52 
#> 7    V1:N50 3 455.55 17.20  9.93 435.76 466.95  <NA>    455.55 
#> 8    V2:N50 3 527.05 30.74 17.75 497.52 558.87  <NA>    527.05 
#> 9    V3:N50 3 440.23  7.78  4.49 434.66 449.12  <NA>    440.23

# ตาราง main-plot
cooked_split$results$yield$summary_main
#>   treatment n   mean    sd    se    min    max group mean_group
#> 1        V1 9 457.08 39.54 13.18 398.96 519.69     b   457.08 b
#> 2        V2 9 497.92 57.80 19.27 417.61 558.87     a   497.92 a
#> 3        V3 9 428.06 41.87 13.96 369.84 488.41     c   428.06 c

# ตาราง sub-plot
cooked_split$results$yield$summary_sub
#>   treatment n   mean    sd    se    min    max group mean_group
#> 1        N0 9 406.90 23.98  7.99 369.84 434.10     c   406.90 c
#> 2      N100 9 501.88 39.34 13.11 428.38 555.36     a   501.88 a
#> 3       N50 9 474.28 44.00 14.67 434.66 558.87     b   474.28 b

# Error terms (df, MS)
cooked_split$results$yield$error_terms
#> $main_plot
#> $main_plot$df
#> [1] 4
#> 
#> $main_plot$ms
#> [1] 302.3123
#> 
#> 
#> $sub_plot
#> $sub_plot$df
#> [1] 12
#> 
#> $sub_plot$ms
#> [1] 402.2172

# CV% แยกตาม error term
cooked_split$results$yield$cv_percent_mainplot
#> [1] 3.77
cooked_split$results$yield$cv_percent_subplot
#> [1] 4.35
```

### การอ่านผล Interaction

- ถ้า **Interaction มีนัยสำคัญ**: ผลของ main-plot ขึ้นอยู่กับระดับของ sub-plot
  (และในทางกลับกัน) ให้อ่านผลจากตารางจับคู่ผสม (interaction)
- ถ้า **Interaction ไม่มีนัยสำคัญ**: อ่านผล main effects แยกกันได้

``` r
# ตรวจว่า interaction significant ไหม
cooked_split$results$yield$interaction_significant
#> [1] FALSE
```

------------------------------------------------------------------------

## เลือก Post-hoc method

ทุก `cook_*()` รองรับ 3 วิธี post-hoc:

| Method | ลักษณะ | เมื่อไรใช้ |
|----|----|----|
| `"tukey"` (default) | Conservative ที่สุด ควบคุม familywise error rate | ทั่วไป แนะนำเป็นค่าเริ่มต้น |
| `"duncan"` | Liberal กว่า Tukey ใช้ multiple range | เมื่อต้องการแยกกลุ่มละเอียดกว่า |
| `"lsd"` | Least Significant Difference + Bonferroni | เมื่อต้องการวิธีที่เข้าใจง่าย |

``` r
# เปลี่ยน post-hoc เป็น Duncan
cooked_duncan <- cook_crd(washed, response = "yield",
                          treatment = "treatment",
                          tasted = tasted, posthoc = "duncan")
```

------------------------------------------------------------------------

## ไม่มี taste_rice ก็หุงได้

ถ้าไม่มีผลจาก
[`taste_rice()`](https://kanthjs.github.io/KhaoSuay/reference/taste_rice.md)
ฟังก์ชัน `cook_*()` จะตรวจ assumptions เองอัตโนมัติ (auto_check) แต่
**แนะนำให้ใช้
[`taste_rice()`](https://kanthjs.github.io/KhaoSuay/reference/taste_rice.md)
ก่อน** เพราะ:

1.  ได้เห็น diagnostic plots สำหรับตรวจสอบด้วยตาเอง
2.  ได้รับคำแนะนำที่ละเอียดกว่า
3.  ผลจาก
    [`taste_rice()`](https://kanthjs.github.io/KhaoSuay/reference/taste_rice.md)
    ถูกส่งต่อไปยัง `cook_*()` อย่างชัดเจน ไม่ต้องเดา

``` r
# หุงได้โดยไม่ต้อง taste_rice (จะ auto_check)
cooked_auto <- cook_crd(washed, response = "yield", treatment = "treatment")
# แต่จะมีเครื่องหมาย [auto_check] แทน [taste_rice]
```

------------------------------------------------------------------------

## สรุปผลลัพธ์จาก cooked_rice

ทุก `cook_*()` คืน object class `cooked_rice` ที่มีโครงสร้าง:

    cooked_rice
    ├── $results           # list ผลวิเคราะห์แยกรายตัวแปร
    │   └── $yield
    │       ├── $summary_table   # ตาราง mean ± sd พร้อม group letters
    │       ├── $anova_table     # ANOVA table
    │       ├── $model           # aov object (สำหรับวิเคราะห์เพิ่ม)
    │       ├── $group_letters   # named vector สำหรับ plot
    │       ├── $p_value         # p-value ของ overall test
    │       ├── $cv_percent      # CV%
    │       ├── $posthoc_name    # ชื่อ post-hoc ที่ใช้
    │       └── $assumption      # ข้อมูล classification
    ├── $design            # "CRD" / "RCBD" / "Split-plot"
    ├── $data              # data.frame ที่ใช้วิเคราะห์
    └── $settings          # ค่าที่ตั้งไว้ทั้งหมด

Object นี้ส่งต่อไปยัง
[`plot_cooked()`](https://kanthjs.github.io/KhaoSuay/reference/plot_cooked.md)
ได้ทันทีเพื่อสร้างกราฟ publication-ready
