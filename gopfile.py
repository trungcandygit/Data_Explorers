import os
from fpdf import FPDF
from fpdf.enums import XPos, YPos

class AcademicReport(FPDF):
    def __init__(self):
        super().__init__()
        self.toc_data = []
        self.setup_vietnamese_font()

    def setup_vietnamese_font(self):
        # Đường dẫn font Arial mặc định trên macOS
        # Đây là font có sẵn trong máy, không phải tải ngoài
        paths = [
            "/System/Library/Fonts/Supplemental/Arial.ttf",
            "/Library/Fonts/Arial.ttf",
            "/System/Library/Fonts/Supplemental/Arial Bold.ttf"
        ]
        
        # Đăng ký font Thường
        if os.path.exists(paths[0]):
            self.add_font("TiengViet", "", paths[0])
        elif os.path.exists(paths[1]):
            self.add_font("TiengViet", "", paths[1])
            
        # Đăng ký font Đậm (Bold)
        if os.path.exists(paths[2]):
            self.add_font("TiengViet", "B", paths[2])
        else:
            # Nếu không tìm thấy font Bold, dùng tạm font thường cho style B
            self.add_font("TiengViet", "B", paths[0])

    def header(self):
        if self.page_no() > 1:
            self.set_font("TiengViet", "", 8)
            self.set_text_color(128, 128, 128)
            self.cell(0, 10, "Báo cáo Mã nguồn & Dữ liệu Hệ thống", 
                      new_x=XPos.LMARGIN, new_y=YPos.NEXT, align="R")
            self.ln(5)

    def footer(self):
        self.set_y(-15)
        self.set_font("TiengViet", "", 8)
        self.set_text_color(128, 128, 128)
        self.cell(0, 10, f"Trang {self.page_no()}", align="C")

def create_report():
    pdf = AcademicReport()
    pdf.set_auto_page_break(auto=True, margin=20)

    # --- 1. TRANG BÌA ---
    pdf.add_page()
    pdf.set_font("TiengViet", "B", 24)
    pdf.ln(60)
    pdf.cell(0, 15, "BÁO CÁO TỔNG HỢP MÃ NGUỒN", align="C", new_x=XPos.LMARGIN, new_y=YPos.NEXT)
    
    pdf.set_font("TiengViet", "B", 14)
    pdf.ln(5)
    pdf.cell(0, 10, "Phân tích Dữ liệu và Hệ thống Shiny App", align="C", new_x=XPos.LMARGIN, new_y=YPos.NEXT)
    
    pdf.ln(30)
    pdf.set_font("TiengViet", "B", 12)
    pdf.cell(0, 10, "Link triển khai hệ thống:", new_x=XPos.LMARGIN, new_y=YPos.NEXT)
    pdf.set_font("TiengViet", "", 12)
    pdf.set_text_color(0, 0, 255)
    url = "https://nhom10trdevidedby5.shinyapps.io/thi_data/"
    pdf.cell(0, 10, url, new_x=XPos.LMARGIN, new_y=YPos.NEXT, link=url)
    
    pdf.set_text_color(0)
    pdf.ln(20)
    pdf.set_font("TiengViet", "B", 10)
    pdf.set_fill_color(245, 245, 245)
    note = ("LƯU Ý: Do sử dụng máy chủ miễn phí, vui lòng đợi 30-60 giây "
            "để hệ thống khởi tạo và tải biểu đồ trong lần truy cập đầu tiên.")
    pdf.multi_cell(0, 8, note, border=1, align='C', fill=True)

    # --- 2. NỘI DUNG CÁC FILE ---
    files_to_include = []
    if os.path.exists("data_summary.txt"):
        files_to_include.append("data_summary.txt")
    
    files_to_include += ['app.R'] + [f'phase{i}.R' for i in range(1, 11)]

    for file_name in files_to_include:
        if os.path.exists(file_name):
            pdf.add_page()
            # Lưu dữ liệu cho mục lục
            pdf.toc_data.append((file_name, pdf.page_no()))
            
            # Tiêu đề mục
            pdf.set_font("TiengViet", "B", 16)
            pdf.set_fill_color(230, 230, 230)
            pdf.cell(0, 12, f"Tệp tin: {file_name}", fill=True, border='B', 
                     new_x=XPos.LMARGIN, new_y=YPos.NEXT)
            pdf.ln(5)
            
            # Nội dung mã nguồn (Giữ nguyên tiếng Việt trong code)
            pdf.set_font("TiengViet", "", 9) 
            try:
                with open(file_name, "r", encoding="utf-8") as f:
                    content = f.read()
                    pdf.multi_cell(0, 5, content)
            except Exception as e:
                pdf.cell(0, 10, f"Lỗi đọc file: {str(e)}")
        else:
            print(f"Không tìm thấy: {file_name}")

    # --- 3. MỤC LỤC (Để ở cuối để tránh lỗi phiên bản thư viện) ---
    pdf.add_page()
    pdf.set_font("TiengViet", "B", 18)
    pdf.cell(0, 15, "MỤC LỤC BÁO CÁO", new_x=XPos.LMARGIN, new_y=YPos.NEXT, align="L")
    pdf.ln(5)
    
    pdf.set_font("TiengViet", "", 12)
    for title, page in pdf.toc_data:
        pdf.cell(160, 10, title, border='B')
        pdf.cell(0, 10, f"{page}", border='B', align="R", new_x=XPos.LMARGIN, new_y=YPos.NEXT)

    # --- 4. XUẤT FILE ---
    output_name = "Bao_Cao_Ma_Nguon_Final.pdf"
    pdf.output(output_name)
    print(f"\nĐã hoàn thành! Báo cáo: {output_name}")

if __name__ == "__main__":
    create_report()