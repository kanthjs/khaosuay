# วาดกราฟผลการวิเคราะห์ — Publication-Ready Plots

สร้างกราฟจาก cooked_rice object โดยอัตโนมัติ เลือกรูปแบบตามจำนวนซ้ำ: n ≤ 4 →
Bar Graph + Error Bar + Jitter Points + Letters n \> 4 → Boxplot +
Jitter Points + Mean dot + Letters

## Usage

``` r
plot_cooked(
  cooked,
  response = NULL,
  plot_type = c("auto", "bar", "box"),
  show_points = TRUE,
  show_letters = TRUE,
  error_type = c("se", "sd"),
  palette = c("viridis", "grey", "set2"),
  y_label = NULL,
  x_label = NULL,
  title = NULL,
  font_family = "sans",
  font_size = 12,
  bar_width = 0.6,
  point_size = 2.5,
  jitter_width = 0.15,
  letter_size = 4.5,
  letter_nudge = NULL,
  save_path = NULL,
  save_width = 7,
  save_height = 5,
  save_dpi = 300
)
```

## Arguments

- cooked:

  cooked_rice object จาก cook_crd / cook_rcbd / cook_split

- response:

  character ชื่อ response ที่จะ plot (ถ้าไม่ระบุ plot ทุกตัว)

- plot_type:

  "auto" (เลือกตาม n), "bar", "box" (บังคับ)

- show_points:

  logical แสดงจุดข้อมูลดิบ (default = TRUE)

- show_letters:

  logical แสดงกลุ่มอักษร a,b,c (default = TRUE)

- error_type:

  "se" (standard error, default) หรือ "sd"

- palette:

  character ชื่อ palette: "viridis" (default), "grey", "set2"

- y_label:

  character ชื่อแกน Y (ถ้าไม่ระบุ ใช้ชื่อ response)

- x_label:

  character ชื่อแกน X (ถ้าไม่ระบุ ใช้ชื่อ treatment)

- title:

  character ชื่อกราฟ (ถ้าไม่ระบุ ไม่ใส่)

- font_family:

  character ฟอนต์ (default = "sans" = Arial)

- font_size:

  numeric ขนาดฟอนต์พื้นฐาน (default = 12)

- bar_width:

  numeric ความกว้างแท่ง (default = 0.6)

- point_size:

  numeric ขนาดจุด jitter (default = 2.5)

- jitter_width:

  numeric ความกว้างการกระจายจุด (default = 0.15)

- letter_size:

  numeric ขนาดตัวอักษร a,b,c (default = 4.5)

- letter_nudge:

  numeric ระยะยกตัวอักษรเหนือ error bar (default = auto)

- save_path:

  character path สำหรับบันทึกไฟล์ (NULL = ไม่บันทึก)

- save_width:

  numeric ความกว้างภาพ (inch, default = 7)

- save_height:

  numeric ความสูงภาพ (inch, default = 5)

- save_dpi:

  numeric resolution (default = 300)

## Value

list ของ ggplot objects (invisible) แต่ละ response 1 plot

## Details

กราฟออกแบบตามมาตรฐานวารสารวิชาการ: - พื้นขาว ไม่มี gridlines - เส้นแกนเฉพาะ X
ล่าง + Y ซ้าย - ฟอนต์อ่านง่าย ขนาดเหมาะสม - Colorblind-friendly palette
(viridis) - รองรับ grayscale - ตัวอักษร a,b,c ลอยเหนือ error bar

## Examples

``` r
if (FALSE) { # \dontrun{
cooked <- cook_rcbd(washed, response = "yield", block = "rep",
                    tasted = tasted)
plot_cooked(cooked)
plot_cooked(cooked, palette = "grey", error_type = "sd")
plot_cooked(cooked, save_path = "figures/yield.png")
} # }
```
