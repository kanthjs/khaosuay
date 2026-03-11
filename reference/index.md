# Package index

## แพ็กเกจ (Package Overview)

ภาพรวมของแพ็กเกจ khaosuay และวิธีใช้งานเบื้องต้น

- [`khaosuay`](https://kanthjs.github.io/KhaoSuay/reference/khaosuay-package.md)
  [`khaosuay-package`](https://kanthjs.github.io/KhaoSuay/reference/khaosuay-package.md)
  : khaosuay: Knowledge-based Harvest & Agricultural Output: Statistical
  Unified Analysis for Yield

## ขั้นตอนที่ 1: ซาวข้าว (Data Cleaning)

ทำความสะอาดข้อมูล ตรวจ outlier ตรวจแผนการทดลอง แนะนำสิ่งที่ต้องพิจารณาก่อนวิเคราะห์

- [`wash_rice()`](https://kanthjs.github.io/KhaoSuay/reference/wash_rice.md)
  : ทำความสะอาดและเตรียมข้อมูลเกษตร (ล้างข้าวสาร) v3.1 — Smart Mapper + Full
  Pipeline

## ขั้นตอนที่ 2: ชิมข้าว (Assumption Check)

ตรวจสอบ Normality และ Equal Variance เพื่อเลือกสถิติที่ถูกต้อง (Parametric /
Non-parametric)

- [`taste_rice()`](https://kanthjs.github.io/KhaoSuay/reference/taste_rice.md)
  : ชิมข้าว — ตรวจสอบ Assumptions ก่อนวิเคราะห์สถิติ (taste_rice) v2.0

## ขั้นตอนที่ 3: หุงข้าว (Statistical Analysis)

วิเคราะห์สถิติตามแผนการทดลอง เลือก ANOVA หรือ Kruskal-Wallis อัตโนมัติ พร้อม
Post-hoc

- [`cook_crd()`](https://kanthjs.github.io/KhaoSuay/reference/cook_crd.md)
  : หุงข้าว CRD — Completely Randomized Design
- [`cook_rcbd()`](https://kanthjs.github.io/KhaoSuay/reference/cook_rcbd.md)
  : หุงข้าว RCBD — Randomized Complete Block Design
- [`cook_split()`](https://kanthjs.github.io/KhaoSuay/reference/cook_split.md)
  : หุงข้าว Split-plot — Split-plot Design

## ขั้นตอนที่ 4: จัดจาน (Visualization)

สร้างกราฟ publication-ready พร้อมตัวอักษรทางสถิติ (a, b, c) รองรับ Bar graph
และ Boxplot

- [`plot_cooked()`](https://kanthjs.github.io/KhaoSuay/reference/plot_cooked.md)
  : วาดกราฟผลการวิเคราะห์ — Publication-Ready Plots
