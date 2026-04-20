library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(stats)
library(readxl)
library(scales)

for (i in 1:10) {
  file_name <- paste0("phase", i, ".R")
  if (file.exists(file_name)) {
    source(file_name, local = TRUE)
  } else {
    warning(paste("Không tìm thấy file:", file_name))
  }
}

GLOBAL_DATA_CACHE <- NULL
if (file.exists("data_final.rds")) {
  GLOBAL_DATA_CACHE <- readRDS("data_final.rds")
}

COLORS <- list(
  primary   = "#1F3864",      
  secondary = "#4472C4",      
  accent    = "#1F3864",      
  
  palette = c("#4472C4", "#9467BD", "#2CA02C", "#D62728", "#5B9BD5", "#FF7F0E", "#8C564B"),
  
  heat_low  = "#D9D7F1",      
  heat_high = "#1F3864",      
  
  success = "#2CA02C",        
  danger  = "#D62728",        
  warning = "#FF7F0E",        
  info    = "#5B9BD5"         
)

format_vnd <- function(x) {
  ifelse(is.na(x), "N/A",
    ifelse(abs(x) >= 1e9,
      paste0(formatC(x / 1e9, format = "f", digits = 1, big.mark = ".", decimal.mark = ","), " tỷ"),
    ifelse(abs(x) >= 1e6,
      paste0(formatC(x / 1e6, format = "f", digits = 1, big.mark = ".", decimal.mark = ","), " tr"),
      formatC(x, format = "f", digits = 0, big.mark = ".", decimal.mark = ",")
    ))
  )
}

label_trieu <- function(x) {
  ifelse(abs(x) >= 1e9, paste0(round(x/1e9, 1), " tỷ"),
    ifelse(abs(x) >= 1e6, paste0(round(x/1e6, 1), " tr"),
      formatC(x, format = "f", digits = 0, big.mark = ".", decimal.mark = ",")))
}

theme_academic <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      text = element_text(family = "sans", color = "#1F3864"),
      plot.title = element_text(face = "bold", size = base_size + 2, color = COLORS$primary, hjust = 0),
      plot.subtitle = element_text(size = base_size - 1, color = "#4472C4"),
      axis.title = element_text(face = "bold", size = base_size - 2, color = "#1F3864"),
      axis.text = element_text(size = base_size - 3, color = "#4472C4"),
      legend.title = element_text(face = "bold", size = base_size - 2, color = COLORS$primary),
      legend.text = element_text(size = base_size - 3),
      panel.grid.major.y = element_line(color = "#D9D7F1", linewidth = 0.5),
      panel.grid.major.x = element_line(color = "#D9D7F1", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      strip.text = element_text(face = "bold", color = COLORS$primary),
      plot.margin = margin(0, 0, 0, 0)
    )
}

load_master_data <- function(excel_path) {
  message(">>> Đang đọc file Excel: ", excel_path)
  
  don_hang    <- read_excel(excel_path, sheet = "Đơn hàng")
  hop_dong    <- read_excel(excel_path, sheet = "Hợp đồng")
  khach_hang  <- read_excel(excel_path, sheet = "Khách hàng")
  goi_sp      <- read_excel(excel_path, sheet = "Gói Sản Phẩm")
  nhom_sp     <- read_excel(excel_path, sheet = "Nhóm sản phẩm")
  kenh_ban    <- read_excel(excel_path, sheet = "Kênh Bán")
  kenh_ct     <- read_excel(excel_path, sheet = "Kênh bán chi tiết")
  chi_nhanh   <- read_excel(excel_path, sheet = "Chi nhánh")
  ma_nv       <- read_excel(excel_path, sheet = "Mã nhân viên")
  nhan_vien   <- read_excel(excel_path, sheet = "Nhân viên")
  do_tuoi     <- read_excel(excel_path, sheet = "Độ tuổi")
  xe_cg       <- read_excel(excel_path, sheet = "Xe cơ giới")
  hang_xe     <- read_excel(excel_path, sheet = "Hãng Xe")
  tt_xe       <- read_excel(excel_path, sheet = "Thông tin xe cơ giới")
  kpi_raw     <- read_excel(excel_path, sheet = "KPI")
  phong_ban   <- read_excel(excel_path, sheet = "Phòng ban")
  
  master <- don_hang %>%
    left_join(goi_sp, by = c("MA_GSP" = "MA_GOISANPHAM")) %>%
    left_join(nhom_sp, by = c("MA_NHOMSANPHAM" = "MA_NHOMSANPHAM")) %>%
    left_join(kenh_ct, by = "MA_KENHBANCHITIET") %>%
    left_join(kenh_ban, by = "MA_KENHBAN") %>%
    left_join(ma_nv %>% select(MA_NV, MA_CN, MA_PB, MA_CONGTY), by = "MA_NV") %>%
    left_join(chi_nhanh %>% select(MA_CN, TEN_CN), by = "MA_CN")
  
  kh_full <- khach_hang %>% left_join(do_tuoi, by = "TUOI")
  master <- master %>%
    left_join(kh_full %>% select(MA_KH, TUOI, GIOI_TINH, NGHENGHIEP, XHKH, NHOM_TUOI), by = "MA_KH")
  
  xe_info <- tt_xe %>%
    left_join(xe_cg %>% select(MA_DONG_XE, TEN_DONG_XE, GIA_TIEN_XE = GIA_TIEN, MA_HANG_XE, SO_CHO_NGOI), by = "MA_DONG_XE") %>%
    left_join(hang_xe, by = "MA_HANG_XE")
  master <- master %>%
    left_join(xe_info %>% select(MA_HD, TEN_DONG_XE, GIA_TIEN_XE, TEN_HANG_XE, BIEN_SO_XE, SO_CHO_NGOI), by = "MA_HD")
  
  master <- master %>%
    mutate(
      NGAY_KY_HD    = as.Date(NGAY_KY_HD),
      NGAY_HIEU_LUC = as.Date(NGAY_HIEU_LUC),
      NGAY_HET_HAN  = as.Date(NGAY_HET_HAN),
      PHIBH          = as.numeric(PHIBH),
      GIA_BH         = as.numeric(GIA_BH),
      GIA_TIEN       = as.numeric(GIA_TIEN),
      TUOI           = as.numeric(TUOI),
      THANG          = floor_date(NGAY_KY_HD, "month"),
      THANGNAM       = format(NGAY_KY_HD, "%Y%m")
    ) %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.), "Chưa xác định", .)))
  
  nv_info <- ma_nv %>% left_join(nhan_vien %>% select(MA_GOCNV, HO_TEN, SDT), by = "MA_GOCNV")
  master <- master %>% left_join(nv_info %>% select(MA_NV, HO_TEN_NV = HO_TEN, SDT_NV = SDT), by = "MA_NV")
  
  return(list(master = master, kpi = kpi_raw, hop_dong = hop_dong, phong_ban = phong_ban, chi_nhanh = chi_nhanh))
}

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Bảo Hiểm Quant Dashboard", titleWidth = 320),
  dashboardSidebar(width = 320, sidebarMenu(id = "tabs",
    menuItem("Phase 1: Chất lượng dữ liệu",     tabName = "tab_phase1", icon = NULL),
    menuItem("Phase 2: Khai phá vĩ mô",         tabName = "tab_phase2", icon = NULL),
    menuItem("Phase 3: Phân tích chuyên sâu",   tabName = "tab_phase3", icon = NULL),
    menuItem("Phase 4: Phân tích phân khúc",    tabName = "tab_phase4", icon = NULL),
    menuItem("Phase 5: Mạng lưới và vận Hành",   tabName = "tab_phase5", icon = NULL),
    menuItem("Phase 6: Dự báo & rủi ro",        tabName = "tab_phase6", icon = NULL),
    menuItem("Phase 7: Chiến lược",  tabName = "tab_phase7", icon = NULL),
    menuItem("Phase 8: Quản trị KPI",           tabName = "tab_phase8", icon = NULL),
    menuItem("Phase 9: KH tiềm năng và Tái tục", tabName = "tab_phase9", icon = NULL),
    menuItem("Phase 10: Tổng kết",        tabName = "tab_phase10", icon = NULL)  )),
  
  dashboardBody(
    tags$head(
      tags$script(HTML("
        $(document).on('shiny:visualchange', function(event) {
          setTimeout(function() {
            window.dispatchEvent(new Event('resize'));
            $('.js-plotly-plot').each(function() {
              Plotly.Plots.resize(this);
            });
          }, 350); 
        });
      ")),
      
      tags$style(HTML(sprintf('
        body, .main-header .logo, .sidebar-menu, h1, h2, h3, h4, h5, h6 { 
          font-family: "Helvetica Neue", Helvetica, Arial, sans-serif !important; 
        }

        .plotly.html-widget, .js-plotly-plot {
          width: 100%% !important;
          height: 100%% !important;
          min-height: 350px;
        }
        
        .content { padding: 15px !important; }
        .box-body { padding: 10px !important; }

        .skin-blue .main-header .navbar { background-color: %s !important; }
        .skin-blue .main-header .logo { 
          background-color: #1F3864 !important; 
          color: #FFFFFF !important; 
          font-weight: bold; 
        }
        .skin-blue .main-header .logo:hover { background-color: %s !important; }
        
        .skin-blue .main-sidebar { 
          background-color: #FFFFFF !important; 
          border-right: 1px solid #D9D7F1; 
        }
        .skin-blue .sidebar-menu > li > a { color: #1F3864 !important; font-size: 15px; }
        .skin-blue .sidebar-menu > li:hover > a, 
        .skin-blue .sidebar-menu > li.active > a { 
          background-color: #D9D7F1 !important; 
          color: %s !important; 
          border-left-color: %s !important;
          font-weight: bold;
        }

        .box.box-primary { border-top-color: %s !important; }
        .box.box-success { border-top-color: %s !important; }
        .box.box-danger  { border-top-color: %s !important; }
        .box.box-info    { border-top-color: %s !important; }
        .box.box-warning { border-top-color: %s !important; }
        
        table.dataTable thead th {
          background-color: %s !important;
          color: #FFFFFF !important;
          border-bottom: none !important;
        }
      ', 
      COLORS$primary, COLORS$primary, COLORS$primary, COLORS$primary, 
      COLORS$primary, COLORS$success, COLORS$danger, COLORS$info, COLORS$warning,
      COLORS$primary)))
    ),
    
    tabItems(
      phase1UI("tab_phase1"), phase2UI("tab_phase2"), phase3UI("tab_phase3"),
      phase4UI("tab_phase4"), phase5UI("tab_phase5"), phase6UI("tab_phase6"),
      phase7UI("tab_phase7"), phase8UI("tab_phase8"), phase9UI("tab_phase9"),
      phase10UI("tab_phase10")
    )
  )
)

server <- function(input, output, session) {
  all_data <- reactiveVal(GLOBAL_DATA_CACHE)

  if (is.null(GLOBAL_DATA_CACHE)) {
    showNotification("Lỗi: Dữ liệu chưa nạp vào RAM.", type = "error")
  }
  
  phase1Server("tab_phase1", all_data)
  phase2Server("tab_phase2", all_data)
  phase3Server("tab_phase3", all_data)
  phase4Server("tab_phase4", all_data)
  phase5Server("tab_phase5", all_data)
  phase6Server("tab_phase6", all_data)
  phase7Server("tab_phase7", all_data)
  phase8Server("tab_phase8", all_data)
  phase9Server("tab_phase9", all_data)
  phase10Server("tab_phase10", all_data)
}

shinyApp(ui = ui, server = server)