# khaosuay <img src="man/figures/khaosuay-hexlogo-nobg.png" align="right" height="139" alt="khaosuay hex logo"/>

> **ข้าวสวย** — R Package สำหรับวิเคราะห์ข้อมูลการทดลองทางเกษตรกรรมในบริบทของไทย
> เปลี่ยนข้อมูลดิบให้กลายเป็นรายงานสถิติที่พร้อมใช้งาน เหมือนข้าวสวยหุงสุกใหม่ที่พร้อมเสิร์ฟ

---

## ทำไมต้อง khaosuay?

การวิเคราะห์สถิติทางการเกษตร (เช่น RCBD, Split-plot) มักมีความยุ่งยากในการเตรียมข้อมูลและการสร้างกราฟเพื่อเขียนรายงาน `khaosuay` จึงเกิดขึ้นมาเพื่อ:

- **ลดขั้นตอนที่ซับซ้อน** — เปลี่ยน syntax ยากๆ ให้เป็นฟังก์ชันที่เข้าใจง่ายในคอนเซปต์การ "หุงข้าว"
- **รองรับภาษาไทย** — แก้ไขปัญหา font ภาษาไทยในกราฟที่นักวิจัยไทยมักเจอ
- **พร้อมทำรายงาน** — สร้างตาราง ANOVA และกราฟเปรียบเทียบค่าเฉลี่ยที่ใส่ตัวอักษร (a, b, c) โดยอัตโนมัติ

---

## การติดตั้ง

```r
# install.packages("pak")
pak::pak("kanthjs/khaosuay")
```

---

## ขั้นตอนการใช้งาน (The Cooking Workflow)

การใช้ `khaosuay` ง่ายเหมือนการหุงข้าวหนึ่งหม้อ แบ่งเป็น 4 ขั้นตอน:

### 1. ล้างข้าว — เตรียมข้อมูล

```r
library(khaosuay)

clean_data <- wash_rice(my_agri_data)
```

### 2. หุงข้าว — วิเคราะห์แผนการทดลอง RCBD

```r
model <- cook_rcbd(
  data      = clean_data,
  outcome   = "yield",
  treatment = "variety",
  block     = "rep"
)
```

### 3. โรยหน้า — ทดสอบความแตกต่างทางสถิติ

```r
results <- top_with_sig(model, treatment = "variety")
```

ตัวอักษรที่เหมือนกัน (a, b, c) หมายความว่าไม่แตกต่างกันทางสถิติที่ระดับนัยสำคัญ 0.05

### 4. จัดจาน — สร้างกราฟรายงาน

```r
setup_thai_font()

serve_plate(
  data  = results,
  x     = "variety",
  y     = "yield",
  title = "เปรียบเทียบผลผลิตข้าวแต่ละสายพันธุ์"
)
```

---

## ตัวอย่างจากข้อมูลจริง

ใช้ข้อมูล `gomez.nitrogen` จากแพ็กเกจ `agridat` (ผลของปุ๋ยไนโตรเจนต่อผลผลิตข้าว):

```r
library(khaosuay)
library(agridat)

data(gomez.nitrogen)

# ครบ workflow ใน 3 บรรทัด
clean  <- wash_rice(gomez.nitrogen)
model  <- cook_rcbd(clean, "nitro", "trt", "rep")
result <- top_with_sig(model, "trt")
```

ผลลัพธ์ตัวอย่าง:

| Treatment (Nitro) | Mean Yield | Significance |
|:-----------------:|:----------:|:------------:|
| 140               | 5,235      | a            |
| 110               | 4,892      | ab           |
| ...               | ...        | ...          |

---

## ฟังก์ชันทั้งหมด

| ฟังก์ชัน | ความหมาย | หน้าที่ |
|---|---|---|
| `wash_rice()` | ล้างข้าว | ลบแถวที่มี NA ออกจากข้อมูล |
| `cook_rcbd()` | หุงข้าว RCBD | วิเคราะห์ ANOVA แบบ RCBD |
| `top_with_sig()` | โรยหน้าด้วยสถิติ | Duncan's multiple range test |
| `serve_plate()` | จัดจานข้าวสวย | สร้าง bar chart รายงาน |
| `setup_thai_font()` | ตั้งค่าฟอนต์ | โหลด font Sarabun สำหรับกราฟภาษาไทย |

---

## การมีส่วนร่วม

หากพบ bug หรือมีไอเดียอยากเพิ่ม "เมนู" ใหม่ๆ สามารถเปิด Issue หรือส่ง Pull Request มาได้เลย!

## สัญญาอนุญาต

Distributed under the MIT License. See `LICENSE` for more information.
