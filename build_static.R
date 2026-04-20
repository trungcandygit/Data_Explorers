cat <<EOF > build_static.R
# =========================================================================
# SCRIPT CHẾ BIẾN DỮ LIỆU TĨNH (Build Once, Run Everywhere)
# =========================================================================
library(plotly)
library(dplyr)

# 1. Đọc dữ liệu nén xz
all_data <- readRDS("data_final.rds")

# 2. Khởi tạo Snapshot
SNAPSHOT <- list()

# 3. Nấu sẵn các biểu đồ (Copy logic từ các file phase vào đây)
# Ví dụ mẫu cho Phase 2:
SNAPSHOT\$plot_phase2_gwp <- plot_ly(all_data\$master, x = ~THANG, y = ~PHIBH, type = 'bar')
# ... Thêm các phase khác vào đây ...

# 4. Lưu thành file tĩnh cực nhẹ
saveRDS(SNAPSHOT, "snapshot_final.rds", compress = "xz")
message(">>> Đã tạo xong snapshot_final.rds")
EOF