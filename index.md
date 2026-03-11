# khaosuay ![khaosuay hex logo](reference/figures/khaosuay-hexlogo-nobg.png)

> **ข้าวสวย** — R Package
> สำหรับวิเคราะห์ข้อมูลการทดลองทางเกษตรกรรมในบริบทของไทย
> เปลี่ยนข้อมูลดิบให้กลายเป็นรายงานสถิติที่พร้อมใช้งาน เหมือนข้าวสวยหุงสุกใหม่ที่พร้อมเสิร์ฟ

------------------------------------------------------------------------

## ทำไมต้อง khaosuay (ข้าวสวย)?

การวิเคราะห์สถิติทางการเกษตร (เช่น RCBD, Split-plot)
มักมีความยุ่งยากในการเตรียมข้อมูล ตรวจสอบข้อตกลงเบื้องต้นทางสถิติ (Assumptions)
และการสร้างกราฟเพื่อเขียนรายงาน `khaosuay` จึงเกิดขึ้นมาเพื่อ:

- **ลดขั้นตอนที่ซับซ้อน** — เปลี่ยน syntax ยาก ๆ ให้เป็นฟังก์ชันที่เข้าใจง่ายในคอนเซปต์การ
  “หุงข้าว”
- **ทำงานต่อเนื่อง (Pipeline)** — ส่งต่อข้อมูลจากขั้นตอนทำความสะอาด ไปทดสอบสถิติ
  ไปวิเคราะห์ เเละไปวาดกราฟ ได้อย่างลื่นไหล
- **รองรับภาษาไทย** — แก้ไขปัญหาการอ่านชื่อคอลัมน์ภาษาไทยแบบอัตโนมัติ

------------------------------------------------------------------------

## การติดตั้ง

ติดตั้งเวอร์ชันล่าสุดจาก GitHub โดยใช้แพ็กเกจ `devtools` หรือ `pak`:

``` r
# ผ่าน devtools
# install.packages("devtools")
devtools::install_github("kanthjs/KhaoSuay")

# หรือผ่าน pak
# install.packages("pak")
pak::pak("kanthjs/KhaoSuay")
```

------------------------------------------------------------------------

## Workflow การหุงข้าว 4 ขั้นตอน

การใช้ `khaosuay` ง่ายเหมือนการหุงข้าวหนึ่งหม้อ โดยแบ่งเป็น 4 ขั้นตอนหลัก:

| ขั้นตอน | ฟังก์ชัน                                                                                                                                                                                                                                | คำอุปมา | หน้าที่                                                               |
|-------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------|--------------------------------------------------------------------|
| 1     | [`wash_rice()`](https://kanthjs.github.io/KhaoSuay/reference/wash_rice.md)                                                                                                                                                           | ซาวข้าว | ทำความสะอาดข้อมูล ตรวจ outlier และแปลงชนิดตัวแปร                       |
| 2     | [`taste_rice()`](https://kanthjs.github.io/KhaoSuay/reference/taste_rice.md)                                                                                                                                                         | ชิมข้าว  | ตรวจ Assumptions (Normality, Equal Variance)                       |
| 3     | [`cook_crd()`](https://kanthjs.github.io/KhaoSuay/reference/cook_crd.md) / [`cook_rcbd()`](https://kanthjs.github.io/KhaoSuay/reference/cook_rcbd.md) / [`cook_split()`](https://kanthjs.github.io/KhaoSuay/reference/cook_split.md) | หุงข้าว  | วิเคราะห์สถิติตามแผนการทดลอง เลือกระบบ Parametric/Non-parametric อัตโนมัติ |
| 4     | [`plot_cooked()`](https://kanthjs.github.io/KhaoSuay/reference/plot_cooked.md)                                                                                                                                                       | จัดจาน  | สร้างกราฟสรุปผลพร้อมตัวอักษรทางสถิติ (a, b, c) แบบ publication-ready      |

------------------------------------------------------------------------

## ตัวอย่างการใช้งานด่วน (Quick Start)

สมมติว่าคุณมีข้อมูลผลผลิตข้าว (`yield`) จากการทดลองแบบ CRD ที่มี 4 สิ่งทดลอง
(`variety`):

``` r
library(khaosuay)

# 1. ซาวข้าว (ทำความสะอาดข้อมูล)
washed <- wash_rice(
  data = my_data, 
  treatment_col = "variety", 
  rep_col = "rep",
  design_check = TRUE
)

# 2. ชิมข้าว (ตรวจสอบสถิติพื้นฐาน Normality & Variance)
tasted <- taste_rice(
  data = washed,
  response = "yield",
  treatment = "variety",
  mode = "both"
)

# 3. หุงข้าว (วิเคราะห์ ANOVA หรือ Kruskal-Wallis อัตโนมัติจากผลการชิม)
cooked <- cook_crd(
  data = washed,
  response = "yield",
  treatment = "variety",
  tasted = tasted
)

# 4. จัดจาน (วาดกราฟแท่งหรือ Boxplot พร้อมตัวอักษร)
plot <- plot_cooked(
  cooked = cooked,
  response = "yield",
  y_label = "Yield (kg/rai)",
  x_label = "Rice Variety"
)
```

------------------------------------------------------------------------

## สรุปความสามารถหลักของฟังก์ชัน

### 1. `wash_rice()` — การซาวข้าว

- **Smart Alias:** ค้นหาคอลัมน์ “ซ้ำ”, “สายพันธุ์”, “Treatment”
  ภาษาไทยและอังกฤษอัตโนมัติ
- **Outlier Detection:** ตรวจหาค่าที่ผิดปกติด้วย IQR หรือ Z-score
- **Domain Range:** เตือนเมื่อพบค่าที่อยู่นอกเหนือช่วงที่เป็นไปได้จริง (เช่น
  เปอร์เซ็นต์ความรุนแรงโรคไม่ควรเกิน 100)
- **Design Check:** ตรวจสอบความสมดุล (Balance) ของแผนการทดลอง

### 2. `taste_rice()` — การชิมข้าว

- ตรวจสอบ Normality ด้วย **Shapiro-Wilk test**
- ตรวจสอบ Equal Variance ด้วย **Levene’s test (Brown-Forsythe)**
- สรุปผลเป็นคำแนะนำว่าควรใช้ Parametric (เช่น ANOVA) หรือ Non-parametric (เช่น
  Kruskal-Wallis)

### 3. `cook_*()` — การหุงข้าว

มีหม้อหุงข้าว 3 แบบรองรับงานวิจัย ได้แก่
[`cook_crd()`](https://kanthjs.github.io/KhaoSuay/reference/cook_crd.md),
[`cook_rcbd()`](https://kanthjs.github.io/KhaoSuay/reference/cook_rcbd.md),
และ
[`cook_split()`](https://kanthjs.github.io/KhaoSuay/reference/cook_split.md) -
เปลี่ยนผ่านพฤติกรรมระหว่าง Parametric และ Non-parametric
ได้อัตโนมัติจากคำแนะนำของ
[`taste_rice()`](https://kanthjs.github.io/KhaoSuay/reference/taste_rice.md) -
มีผลลัพธ์เป็นตาราง Summary สถิติที่หาค่า Mean, SD, SE, CV% พร้อมจัดกลุ่มตัวอักษร
Post-hoc (เช่น Tukey HSD หรือ Dunn’s test) เสร็จสรรพ

### 4. `plot_cooked()` — การจัดจาน

- ประเมินจำนวนซ้ำ (n) ของคุณอัตโนมัติ: ดึง Bar graph มาใช้หาก n น้อย และใช้
  Boxplot หาก n เริ่มกว้าง
- จัดวางตัวอักษรทางสถิติ (Statistical Letters) ให้อัตโนมัติเหนือกราฟ
- โทนสีกราฟรองรับตามมาตรฐาน Colorblind-friendly (viridis, grey, set2)

------------------------------------------------------------------------

## เอกสารอ้างอิงและคู่มือ (Documentation)

สามารถอ่านคู่มือฉบับเต็มและรายละเอียดเพิ่มเติมของการวิเคราะห์แต่ละแผนการทดลองได้ที่หน้าเว็บไซต์
Pkgdown (อยู่ระหว่างการพัฒนา) หรือพิมพ์
[`?wash_rice`](https://kanthjs.github.io/KhaoSuay/reference/wash_rice.md),
[`?taste_rice`](https://kanthjs.github.io/KhaoSuay/reference/taste_rice.md),
[`?cook_rcbd`](https://kanthjs.github.io/KhaoSuay/reference/cook_rcbd.md)
ใน R Console.

------------------------------------------------------------------------

## การมีส่วนร่วม

หากพบ bug หรือมีไอเดียอยากเพิ่ม “เมนู” ใหม่ๆ สามารถเปิด Issue หรือส่ง Pull Request
มาได้เลย!

## สัญญาอนุญาต

Distributed under the MIT License. See `LICENSE` for more information.
