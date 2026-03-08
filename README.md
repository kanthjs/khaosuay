ข้าวสวย หรือ KhaoSuay (KhaoSuay: Knowledge-based Harvest & Agricultural Output: Statistical Unified Analysis for Yield) package เพื่อ รวบรวม ฟังชั่นที่ใช้วิเคราะห์ ข้อมูลทางการเกาตร ในบริยทของไทย โดย จะทำ document ที่เป็นภาษาไทย และ สนับสนุน ภาษไทยอย่างยิ่งยวด
 
 รวบรวมฟังชั้น ที่ทำทั้ง 3 ขั้นตอน

1. ขั้นตอนการเตรียม (Data Cleaning)

wash_rice(): การทำความสะอาดข้อมูล (Cleaning Data) ลบข้อมูลที่ผิดพลาด หรือจัดการ Missing Values

pick_grain(): การเลือกเฉพาะตัวแปรหรือคอลัมน์ที่ต้องการ (Select variables)

2. ขั้นตอนการหุง (Statistical Modeling)

cook_rcbd(): การวิเคราะห์ ANOVA สำหรับแผน RCBD (หุงข้าวแบบมาตรฐาน)

cook_split(): การวิเคราะห์สำหรับ Split-plot (หุงข้าวแบบมีชั้นเชิง)

check_doneness(): การเช็ค Assumptions ของสถิติ (เช่น Normality, Homogeneity)

3. ขั้นตอนการจัดจาน (Visualization & Reporting)

serve_plate(): การสร้างกราฟที่สวยงาม (ggplot2 wrapper) ให้พร้อมเสิร์ฟในรายงาน

top_with_sig(): การใส่ตัวอักษรแสดงความแตกต่างทางสถิติ (a, b, c) ลงบนหัวกราฟแท่ง

steam_report(): การ Export ผลลัพธ์ออกมาเป็นไฟล์ Word/PDF


## อยากเริ่มต้น ด้วย การใช้ ข้อมูลจาก agridat package