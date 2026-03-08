# วิเคราะห์แผนการทดลองแบบ RCBD (หุงข้าว RCBD)

วิเคราะห์แผนการทดลองแบบ RCBD (หุงข้าว RCBD)

## Usage

``` r
cook_rcbd(data, outcome, treatment, block)
```

## Arguments

- data:

  data.frame ที่มีข้อมูล

- outcome:

  ชื่อคอลัมน์ของผลผลิต (เช่น yield)

- treatment:

  ชื่อคอลัมน์ของกรรมวิธี (เช่น variety)

- block:

  ชื่อคอลัมน์ของ Block (เช่น replication)

## Value

รายการผลลัพธ์ ANOVA
