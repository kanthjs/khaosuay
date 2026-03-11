# ตั้งค่าฟอนต์ภาษาไทยสำหรับกราฟ

โหลดฟอนต์ภาษาไทยจาก Google Fonts และเปิดใช้งาน showtext เพื่อให้ ggplot2
แสดงข้อความภาษาไทยได้ถูกต้อง ต้องเรียกฟังก์ชันนี้ก่อนสร้างกราฟด้วย \[serve_plate()\]

## Usage

``` r
setup_thai_font(font_name = "Sarabun")
```

## Arguments

- font_name:

  ชื่อ Google Font ที่รองรับภาษาไทย (default: \`"Sarabun"\`) ตัวเลือกยอดนิยม:
  \`"Sarabun"\`, \`"Prompt"\`, \`"Kanit"\`, \`"Noto Sans Thai"\`

## Value

ชื่อฟอนต์ (invisible) เพื่อใช้ส่งต่อไปยัง \[serve_plate()\]

## Examples

``` r
if (FALSE) { # \dontrun{
setup_thai_font()
setup_thai_font("Prompt")
} # }
```
