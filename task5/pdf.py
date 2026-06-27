#!/usr/bin/env python3
from PyPDF2 import PdfReader, PdfWriter

# 修改这里的参数
input_file = "./Project_5.pdf"    # 源文件
output_file = "output.pdf"  # 输出文件
start_page = 92            # 起始页（从1开始）
end_page = 103         # 结束页（包含）

reader = PdfReader(input_file)
writer = PdfWriter()

for page_num in range(start_page - 1, end_page):
    writer.add_page(reader.pages[page_num])

with open(output_file, 'wb') as f:
    writer.write(f)

print(f"成功提取第 {start_page}-{end_page} 页到 {output_file}")
