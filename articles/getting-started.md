# เริ่มต้นใช้งาน khaosuay

## แนะนำแพ็กเกจ

**khaosuay** เป็นแพ็กเกจ R สำหรับวิเคราะห์ข้อมูลเกษตรกรรม
โดยใช้คำอุปมาจากการหุงข้าว ทำให้ workflow
การวิเคราะห์แผนการทดลองเข้าใจง่ายและสนุกยิ่งขึ้น

ขั้นตอนการวิเคราะห์ประกอบด้วย:

| ฟังก์ชัน                                                                            | ความหมาย     | หน้าที่                             |
|----------------------------------------------------------------------------------|--------------|----------------------------------|
| [`wash_rice()`](https://kanthjs.github.io/KhaoSuay/reference/wash_rice.md)       | ล้างข้าว       | ทำความสะอาดข้อมูล                  |
| [`cook_rcbd()`](https://kanthjs.github.io/KhaoSuay/reference/cook_rcbd.md)       | หุงข้าว RCBD   | วิเคราะห์ ANOVA แบบ RCBD           |
| [`top_with_sig()`](https://kanthjs.github.io/KhaoSuay/reference/top_with_sig.md) | โรยหน้าด้วยสถิติ | ทดสอบความแตกต่างด้วย Duncan’s test |
| [`serve_plate()`](https://kanthjs.github.io/KhaoSuay/reference/serve_plate.md)   | จัดจานข้าวสวย  | สร้างกราฟรายงาน                   |

## ติดตั้ง

``` r
# ติดตั้งจาก GitHub
# install.packages("pak")
pak::pak("khaosuay")
```

## โหลดแพ็กเกจ

``` r
library(khaosuay)
#> khaosuay: สำหรับกราฟภาษาไทย เรียก setup_thai_font() ก่อนใช้ serve_plate()
```

------------------------------------------------------------------------

## ตัวอย่างการใช้งาน: ข้อมูลข้าว nitrogen

ใช้ข้อมูล `gomez.nitrogen` จากแพ็กเกจ `agridat`
ซึ่งเป็นการทดลองผลของปุ๋ยไนโตรเจนต่อผลผลิตข้าว

### ขั้นที่ 1: เตรียมข้อมูล (ล้างข้าว)

``` r
library(agridat)
data(gomez.nitrogen)

# ดูข้อมูลเบื้องต้น
head(gomez.nitrogen)
```

    #>   rep  trt nitro
    #> 1   1   N0  4.51
    #> 2   1  N30  5.02
    #> 3   1  N60  5.88
    #> 4   1  N90  5.98
    #> 5   1 N120  5.71
    #> 6   1 N150  5.74

ทำความสะอาดข้อมูลด้วย
[`wash_rice()`](https://kanthjs.github.io/KhaoSuay/reference/wash_rice.md):

``` r
clean_data <- wash_rice(gomez.nitrogen)
```

    #> ทำความสะอาดเมล็ดข้าวเรียบร้อย: ลบข้อมูลที่ไม่สมบูรณ์ออกแล้ว

### ขั้นที่ 2: วิเคราะห์ RCBD (หุงข้าว)

``` r
model <- cook_rcbd(
  data      = clean_data,
  outcome   = "nitro",
  treatment = "trt",
  block     = "rep"
)
summary(model)
```

    #> --- หุงข้าว RCBD เสร็จแล้ว! กำลังตรวจสอบความนุ่มของข้อมูล ---
    #>             Df Sum Sq Mean Sq F value   Pr(>F)    
    #> trt          5  6.843  1.3686 120.643 1.45e-11 ***
    #> rep          3  0.232  0.0775   6.831  0.00402 ** 
    #> Residuals   15  0.170  0.0113                     
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### ขั้นที่ 3: ทดสอบความแตกต่าง (โรยหน้า)

``` r
sig_groups <- top_with_sig(model, "trt")
print(sig_groups)
```

    #> --- โรยหน้าเรียบร้อย! กลุ่มที่ตัวอักษรเหมือนกันแปลว่าไม่ต่างกันทางสถิติ ---
    #>       nitro groups
    #> N90  6.0200      a
    #> N60  5.8700     ab
    #> N120 5.7600      b
    #> N150 5.7075      b
    #> N30  5.1825      c
    #> N0   4.4550      d

ตัวอักษรที่เหมือนกันหมายความว่าไม่แตกต่างกันทางสถิติที่ระดับนัยสำคัญ 0.05

### ขั้นที่ 4: สร้างกราฟ (จัดจาน)

``` r
setup_thai_font()

mean_data <- aggregate(nitro ~ trt, data = clean_data, FUN = mean)

serve_plate(
  data  = mean_data,
  x     = "trt",
  y     = "nitro",
  title = "ผลผลิตข้าวเฉลี่ยตามระดับปุ๋ยไนโตรเจน"
)
```

------------------------------------------------------------------------

## สรุป Workflow

``` r
library(khaosuay)
library(agridat)

data(gomez.nitrogen)

gomez.nitrogen |>
  wash_rice() |>
  (\(d) cook_rcbd(d, "nitro", "trt", "rep"))() |>
  top_with_sig("trt")
#> ทำความสะอาดเมล็ดข้าวเรียบร้อย: ลบข้อมูลที่ไม่สมบูรณ์ออกแล้ว
#> --- หุงข้าว RCBD เสร็จแล้ว! กำลังตรวจสอบความนุ่มของข้อมูล ---
#> --- โรยหน้าเรียบร้อย! กลุ่มที่ตัวอักษรเหมือนกันแปลว่าไม่ต่างกันทางสถิติ ---
#>       nitro groups
#> T7 2.400833      a
#> T3 2.379167      a
#> T8 2.275000      a
#> T4 2.272500      a
#> T2 2.253333      a
#> T6 2.190833      a
#> T5 2.113333      a
#> T1 2.041667      a
```

workflow นี้ประกอบด้วย 3 ขั้นตอนหลักต่อเนื่องกัน: ล้างข้อมูล → วิเคราะห์ RCBD →
ทดสอบความแตกต่าง

------------------------------------------------------------------------

## ดูเพิ่มเติม

- [`?wash_rice`](https://kanthjs.github.io/KhaoSuay/reference/wash_rice.md)
  — รายละเอียดการทำความสะอาดข้อมูล
- [`?cook_rcbd`](https://kanthjs.github.io/KhaoSuay/reference/cook_rcbd.md)
  — รายละเอียดการวิเคราะห์ RCBD
- [`?top_with_sig`](https://kanthjs.github.io/KhaoSuay/reference/top_with_sig.md)
  — รายละเอียด Duncan’s multiple range test
- [`?serve_plate`](https://kanthjs.github.io/KhaoSuay/reference/serve_plate.md)
  — รายละเอียดการสร้างกราฟ
- [`?setup_thai_font`](https://kanthjs.github.io/KhaoSuay/reference/setup_thai_font.md)
  — การตั้งค่าฟอนต์ภาษาไทย
