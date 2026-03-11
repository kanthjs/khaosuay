# สร้างกราฟรายงาน (จัดจานข้าวสวย)

สร้าง bar chart สำหรับแสดงผลการทดลองเกษตร
รองรับการแสดงข้อความภาษาไทยผ่านการตั้งค่าฟอนต์ด้วย \[setup_thai_font()\]

## Usage

``` r
serve_plate(data, x, y, title = "รายงานผลการทดลอง", font_family = "Sarabun")
```

## Arguments

- data:

  data.frame ที่มีข้อมูลสำหรับสร้างกราฟ

- x:

  ชื่อคอลัมน์แกน x (กรรมวิธี)

- y:

  ชื่อคอลัมน์แกน y (ผลผลิต)

- title:

  หัวข้อกราฟ (default: \`"รายงานผลการทดลอง"\`)

- font_family:

  ชื่อฟอนต์ที่จะใช้ในกราฟ ควรตรงกับที่โหลดไว้ใน \[setup_thai_font()\] (default:
  \`"Sarabun"\`)

## Value

ggplot object

## Examples

``` r
if (FALSE) { # \dontrun{
setup_thai_font()
df <- data.frame(trt = factor(c("A", "B", "C")), yield = c(10, 15, 12))
serve_plate(df, "trt", "yield", title = "ผลผลิตข้าวเฉลี่ยตามกรรมวิธี")
} # }
```
