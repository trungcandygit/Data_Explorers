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


# =============================================================================
# =============================================================================

FONT_FAMILY        <- "Inter, 'Helvetica Neue', Helvetica, Arial, sans-serif"
FONT_FAMILY_PLOTLY <- "Inter, 'Helvetica Neue', Helvetica, Arial, sans-serif"

COLORS <- list(
  # Six core hues (corporate quant style)
  navy   = "#1F3864",   # primary — header, line nhấn, text in đậm
  steel  = "#4472C4",   # secondary — bar/line chính (mặc định)
  teal   = "#2EA395",   # tertiary — positive/series 2
  gold   = "#D4A437",   # quaternary — highlight/series 3
  red    = "#C0392B",   # alert/negative
  gray   = "#7F8C8D",   # neutral/series phụ

  primary    = "#1F3864",
  secondary  = "#4472C4",
  success    = "#2EA395",
  warning    = "#D4A437",
  danger     = "#C0392B",
  info       = "#4472C4",
  muted      = "#7F8C8D",

  text_main  = "#2C3E50",
  text_light = "#6C7A89",

  bg         = "#FFFFFF",
  bg_alt     = "#F8F9FA",
  bg_card    = "#FFFFFF",
  grid       = "#ECEEF1",
  border     = "#E0E4EA",

  heat_low   = "#F0F4F8",
  heat_mid   = "#9DB4D4",
  heat_high  = "#1F3864",

  new        = "#4472C4",   # HĐ Mới  → steel
  renewal    = "#D4A437",   # Tái tục → gold
  up         = "#2EA395",   # tăng    → teal
  down       = "#C0392B"    # giảm    → red
)

# 1.3 ─ Palette cho chart đa series (dùng theo thứ tự, dừng ở 6 màu cốt lõi)
PALETTE_MAIN <- unname(c(
  COLORS$steel, COLORS$teal, COLORS$gold,
  COLORS$navy,  COLORS$red,  COLORS$gray
))

# 1.4 ─ Palette XHKH (Đồng → Kim cương) theo gradient nhạt-đậm
PALETTE_XHKH <- c(
  "Đồng"      = "#A8B5C2",
  "Bạc"       = "#7F8C8D",
  "Vàng"      = "#D4A437",
  "Bạch kim"  = "#4472C4",
  "Kim cương" = "#1F3864"
)

# 1.5 ─ Sizing system
SIZE <- list(
  base       = 12,
  title      = 14,
  subtitle   = 11,
  axis_title = 10,
  axis_text  = 9,
  legend     = 9,
  label      = 3.2,
  line       = 1.1,
  bar_width  = 0.66,
  marker     = 3
)


# =============================================================================
# 2. FORMATTING HELPERS
# =============================================================================
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
  ifelse(abs(x) >= 1e9, paste0(round(x / 1e9, 1), " tỷ"),
    ifelse(abs(x) >= 1e6, paste0(round(x / 1e6, 1), " tr"),
      formatC(x, format = "f", digits = 0, big.mark = ".", decimal.mark = ",")))
}

label_int <- function(x) formatC(x, format = "d", big.mark = ".")


# =============================================================================
# 3. GGPLOT THEME — DUY NHẤT, mọi phase phải dùng theme_quant()
# =============================================================================
theme_quant <- function(base_size = SIZE$base,
                        grid       = c("y", "x", "both", "none"),
                        legend_pos = "top") {
  grid <- match.arg(grid)

  base <- theme_minimal(base_size = base_size, base_family = "sans") %+replace%
    theme(
      text             = element_text(family = "sans", color = COLORS$text_main),

      plot.title       = element_text(face = "bold", color = COLORS$navy,
                                      size = SIZE$title, hjust = 0,
                                      margin = margin(b = 6)),
      plot.subtitle    = element_text(color = COLORS$text_light,
                                      size = SIZE$subtitle,
                                      margin = margin(b = 8)),

      axis.title       = element_text(face = "bold",
                                      size = SIZE$axis_title,
                                      color = COLORS$text_main),
      axis.title.x     = element_text(margin = margin(t = 6)),
      axis.title.y     = element_text(margin = margin(r = 6)),
      axis.text        = element_text(size = SIZE$axis_text,
                                      color = COLORS$text_light),
      axis.line        = element_blank(),
      axis.ticks       = element_blank(),

      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = COLORS$bg, color = NA),
      plot.background  = element_rect(fill = COLORS$bg, color = NA),

      legend.position  = legend_pos,
      legend.title     = element_text(face = "bold",
                                      size = SIZE$legend,
                                      color = COLORS$text_main),
      legend.text      = element_text(size = SIZE$legend,
                                      color = COLORS$text_main),
      legend.key       = element_blank(),
      legend.background = element_blank(),

      strip.text       = element_text(face = "bold",
                                      color = COLORS$navy,
                                      size = SIZE$axis_title),
      strip.background = element_rect(fill = COLORS$bg_alt, color = NA),

      plot.margin      = margin(4, 4, 4, 4)
    )

  # Grid lines tuỳ orientation
  grid_h <- element_line(color = COLORS$grid, linetype = "dashed", linewidth = 0.35)
  grid_v <- element_line(color = COLORS$grid, linetype = "dashed", linewidth = 0.35)
  blank  <- element_blank()

  base <- base + switch(grid,
    "y"    = theme(panel.grid.major.y = grid_h, panel.grid.major.x = blank),
    "x"    = theme(panel.grid.major.x = grid_v, panel.grid.major.y = blank),
    "both" = theme(panel.grid.major.y = grid_h, panel.grid.major.x = grid_v),
    "none" = theme(panel.grid.major   = blank)
  )

  base
}


# =============================================================================
# 4. PLOTLY LAYOUT — DUY NHẤT, mọi phase phải dùng ly_quant()
# =============================================================================
PLOTLY_FONT <- list(family = FONT_FAMILY_PLOTLY,
                    color  = COLORS$text_main,
                    size   = 12)

ly_quant <- function(p,
                     l = 8, r = 8, t = 16, b = 8,
                     legend_top = TRUE,
                     show_modebar = FALSE,
                     hover_mode = "closest") {

  legend_cfg <- if (legend_top) {
    list(orientation = "h", x = 0.5, xanchor = "center",
         y = 1.12, yanchor = "bottom",
         font = list(size = SIZE$legend, family = FONT_FAMILY_PLOTLY,
                     color = COLORS$text_main),
         bgcolor = "rgba(0,0,0,0)")
  } else {
    list(font = list(size = SIZE$legend, family = FONT_FAMILY_PLOTLY,
                     color = COLORS$text_main))
  }

  axis_common <- list(
    automargin  = TRUE,
    gridcolor   = COLORS$grid,
    gridwidth   = 0.4,
    linecolor   = COLORS$border,
    zerolinecolor = COLORS$border,
    tickfont    = list(size = SIZE$axis_text,
                       family = FONT_FAMILY_PLOTLY,
                       color = COLORS$text_light),
    title       = list(font = list(size = SIZE$axis_title,
                                   family = FONT_FAMILY_PLOTLY,
                                   color = COLORS$text_main))
  )

  p %>%
    layout(
      font       = PLOTLY_FONT,
      paper_bgcolor = COLORS$bg,
      plot_bgcolor  = COLORS$bg,
      margin     = list(l = l, r = r, t = t, b = b, pad = 0),
      xaxis      = axis_common,
      yaxis      = axis_common,
      legend     = legend_cfg,
      hovermode  = hover_mode,
      hoverlabel = list(font = list(family = FONT_FAMILY_PLOTLY, size = 11),
                        bgcolor = COLORS$navy,
                        bordercolor = COLORS$navy,
                        font_color = "white")
    ) %>%
    config(displayModeBar = show_modebar, displaylogo = FALSE)
}


# =============================================================================
# 5. DATATABLE THEME — DUY NHẤT, mọi phase phải dùng dt_quant()
# =============================================================================
dt_init_js <- function() {
  JS(sprintf("
    function(settings, json) {
      $(this.api().table().header()).css({
        'background-color': '%s',
        'color': '#FFFFFF',
        'font-family': '%s',
        'font-weight': '600',
        'border-bottom': 'none',
        'font-size': '13px'
      });
      $(this.api().table().body()).css({
        'font-family': '%s',
        'font-size': '12.5px',
        'color': '%s'
      });
    }",
    COLORS$navy, FONT_FAMILY, FONT_FAMILY, COLORS$text_main))
}

dt_quant <- function(df,
                     colnames = NULL,
                     page_length = 10,
                     dom = "ftrip",
                     scrollX = TRUE,
                     filter = "none",
                     ordering = TRUE) {

  args <- list(data = df, rownames = FALSE, filter = filter,
               options = list(
                 dom = dom,
                 pageLength = page_length,
                 scrollX = scrollX,
                 ordering = ordering,
                 initComplete = dt_init_js(),
                 language = list(
                   search = "Tìm:",
                   info = "Hiển thị _START_-_END_ / _TOTAL_",
                   paginate = list(previous = "Trước", `next` = "Sau"),
                   lengthMenu = "Hiển thị _MENU_ dòng",
                   infoEmpty  = "Không có dữ liệu"
                 )
               ))
  if (!is.null(colnames)) args$colnames <- colnames

  do.call(datatable, args)
}


# =============================================================================
# 6. UI BUILDING BLOCKS — box header / page header thống nhất
# =============================================================================

# Header trang (h2 + caption)
page_header <- function(title, subtitle = NULL) {
  tagList(
    tags$h2(title,
      style = sprintf("color:%s; font-weight:700; font-size:22px; margin:0 0 4px 0; font-family:%s; letter-spacing:-0.2px;",
                      COLORS$navy, FONT_FAMILY)),
    if (!is.null(subtitle))
      tags$p(subtitle,
        style = sprintf("color:%s; font-size:13px; margin-bottom:18px; font-family:%s;",
                        COLORS$text_light, FONT_FAMILY))
  )
}

# Section header (chia trang phase 5, 10)
section_header <- function(text) {
  tags$h3(text,
    style = sprintf(
      "color:%s; font-weight:700; font-size:15px; margin:18px 0 12px 0; font-family:%s; border-bottom:2px solid %s; padding-bottom:6px; text-transform:uppercase; letter-spacing:0.5px;",
      COLORS$navy, FONT_FAMILY, COLORS$border))
}

# Tiêu đề trong box (thay h4 dài hardcode)
box_title <- function(title, subtitle = NULL) {
  tagList(
    tags$h4(title,
      style = sprintf("color:%s; font-weight:700; font-size:14px; margin:0 0 2px 0; font-family:%s;",
                      COLORS$navy, FONT_FAMILY)),
    if (!is.null(subtitle) && nzchar(subtitle))
      tags$p(subtitle,
        style = sprintf("color:%s; font-size:11.5px; margin:0 0 10px 0; font-family:%s; line-height:1.4;",
                        COLORS$text_light, FONT_FAMILY))
  )
}

# Wrapper box thống nhất
qbox <- function(title, subtitle = NULL, ..., width = 6, height_px = NULL) {
  box(title = NULL, width = width, status = "primary", solidHeader = FALSE,
      box_title(title, subtitle),
      ...)
}


# =============================================================================
# 7. NẠP DỮ LIỆU CACHE & MODULES
# =============================================================================
GLOBAL_DATA_CACHE <- NULL
if (file.exists("data_final.rds")) {
  GLOBAL_DATA_CACHE <- readRDS("data_final.rds")
}

# Tự động load 10 module — local = TRUE để chia sẻ globals (COLORS, theme_quant…)
for (i in 1:10) {
  fn <- paste0("phase", i, ".R")
  if (file.exists(fn)) source(fn, local = TRUE) else warning("Không tìm thấy: ", fn)
}


# =============================================================================
# 8. (Optional) load_master_data — nạp Excel gốc khi cần build lại data_final.rds
# =============================================================================
load_master_data <- function(excel_path) {
  message(">>> Đọc Excel: ", excel_path)
  don_hang   <- read_excel(excel_path, sheet = "Đơn hàng")
  hop_dong   <- read_excel(excel_path, sheet = "Hợp đồng")
  khach_hang <- read_excel(excel_path, sheet = "Khách hàng")
  goi_sp     <- read_excel(excel_path, sheet = "Gói Sản Phẩm")
  nhom_sp    <- read_excel(excel_path, sheet = "Nhóm sản phẩm")
  kenh_ban   <- read_excel(excel_path, sheet = "Kênh Bán")
  kenh_ct    <- read_excel(excel_path, sheet = "Kênh bán chi tiết")
  chi_nhanh  <- read_excel(excel_path, sheet = "Chi nhánh")
  ma_nv      <- read_excel(excel_path, sheet = "Mã nhân viên")
  nhan_vien  <- read_excel(excel_path, sheet = "Nhân viên")
  do_tuoi    <- read_excel(excel_path, sheet = "Độ tuổi")
  xe_cg      <- read_excel(excel_path, sheet = "Xe cơ giới")
  hang_xe    <- read_excel(excel_path, sheet = "Hãng Xe")
  tt_xe      <- read_excel(excel_path, sheet = "Thông tin xe cơ giới")
  kpi_raw    <- read_excel(excel_path, sheet = "KPI")
  phong_ban  <- read_excel(excel_path, sheet = "Phòng ban")

  master <- don_hang %>%
    left_join(goi_sp,   by = c("MA_GSP" = "MA_GOISANPHAM")) %>%
    left_join(nhom_sp,  by = "MA_NHOMSANPHAM") %>%
    left_join(kenh_ct,  by = "MA_KENHBANCHITIET") %>%
    left_join(kenh_ban, by = "MA_KENHBAN") %>%
    left_join(ma_nv %>% select(MA_NV, MA_CN, MA_PB, MA_CONGTY), by = "MA_NV") %>%
    left_join(chi_nhanh %>% select(MA_CN, TEN_CN), by = "MA_CN")

  kh_full <- khach_hang %>% left_join(do_tuoi, by = "TUOI")
  master <- master %>%
    left_join(kh_full %>% select(MA_KH, TUOI, GIOI_TINH, NGHENGHIEP, XHKH, NHOM_TUOI),
              by = "MA_KH")

  xe_info <- tt_xe %>%
    left_join(xe_cg %>% select(MA_DONG_XE, TEN_DONG_XE, GIA_TIEN_XE = GIA_TIEN,
                               MA_HANG_XE, SO_CHO_NGOI), by = "MA_DONG_XE") %>%
    left_join(hang_xe, by = "MA_HANG_XE")
  master <- master %>%
    left_join(xe_info %>% select(MA_HD, TEN_DONG_XE, GIA_TIEN_XE, TEN_HANG_XE,
                                 BIEN_SO_XE, SO_CHO_NGOI), by = "MA_HD")

  master <- master %>%
    mutate(
      NGAY_KY_HD    = as.Date(NGAY_KY_HD),
      NGAY_HIEU_LUC = as.Date(NGAY_HIEU_LUC),
      NGAY_HET_HAN  = as.Date(NGAY_HET_HAN),
      PHIBH         = as.numeric(PHIBH),
      GIA_BH        = as.numeric(GIA_BH),
      GIA_TIEN      = as.numeric(GIA_TIEN),
      TUOI          = as.numeric(TUOI),
      THANG         = floor_date(NGAY_KY_HD, "month"),
      THANGNAM      = format(NGAY_KY_HD, "%Y%m")
    ) %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.), "Chưa xác định", .)))

  nv_info <- ma_nv %>% left_join(nhan_vien %>% select(MA_GOCNV, HO_TEN, SDT),
                                 by = "MA_GOCNV")
  master <- master %>%
    left_join(nv_info %>% select(MA_NV, HO_TEN_NV = HO_TEN, SDT_NV = SDT),
              by = "MA_NV")

  list(master = master, kpi = kpi_raw, hop_dong = hop_dong,
       phong_ban = phong_ban, chi_nhanh = chi_nhanh)
}


# =============================================================================
# 9. UI — DASHBOARD SHELL
# =============================================================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Bảo Hiểm Quant Dashboard", titleWidth = 320),

  dashboardSidebar(width = 320,
    sidebarMenu(id = "tabs",
      menuItem("Phase 1: Chất lượng dữ liệu",       tabName = "tab_phase1"),
      menuItem("Phase 2: Khai phá vĩ mô",           tabName = "tab_phase2"),
      menuItem("Phase 3: Phân tích chuyên sâu",     tabName = "tab_phase3"),
      menuItem("Phase 4: Phân khúc khách hàng",     tabName = "tab_phase4"),
      menuItem("Phase 5: Mạng lưới & Vận hành",     tabName = "tab_phase5"),
      menuItem("Phase 6: Khách hàng tiềm năng",     tabName = "tab_phase6"),
      menuItem("Phase 7: Chiến lược kinh doanh",    tabName = "tab_phase7"),
      menuItem("Phase 8: Quản trị KPI",             tabName = "tab_phase8"),
      menuItem("Phase 9: Tái tục & Cảnh báo",       tabName = "tab_phase9"),
      menuItem("Phase 10: Tổng kết & Đề xuất",      tabName = "tab_phase10")
    )
  ),

  dashboardBody(

    tags$head(
      # ─── Google Font Inter ───
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
      tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap",
                rel = "stylesheet"),

      # ─── Auto-resize plotly khi đổi tab ───
      tags$script(HTML("
        $(document).on('shiny:visualchange', function() {
          setTimeout(function() {
            window.dispatchEvent(new Event('resize'));
            $('.js-plotly-plot').each(function(){ Plotly.Plots.resize(this); });
          }, 350);
        });
      ")),

      # ─── CSS GLOBAL — quant corporate look ───
      tags$style(HTML(sprintf("
        :root {
          --c-navy:   %s;
          --c-steel:  %s;
          --c-teal:   %s;
          --c-gold:   %s;
          --c-red:    %s;
          --c-gray:   %s;
          --c-text:   %s;
          --c-muted:  %s;
          --c-border: %s;
          --c-bg-alt: %s;
        }

        /* Font global */
        body, .main-header .logo, .sidebar-menu, h1, h2, h3, h4, h5, h6,
        .box, .form-control, .btn, table, .dataTables_wrapper {
          font-family: %s !important;
          -webkit-font-smoothing: antialiased;
        }

        /* Background nhẹ để box nổi */
        .content-wrapper { background-color: var(--c-bg-alt) !important; }
        .content { padding: 16px !important; }

        /* Header */
        .skin-blue .main-header .navbar { background-color: var(--c-navy) !important; }
        .skin-blue .main-header .logo {
          background-color: #15264A !important; color:#FFFFFF !important;
          font-weight:600; letter-spacing:-0.2px;
        }
        .skin-blue .main-header .logo:hover { background-color: var(--c-navy) !important; }
        .skin-blue .main-header .navbar .sidebar-toggle:hover { background-color: #15264A !important; }

        /* Sidebar */
        .skin-blue .main-sidebar {
          background-color: #FFFFFF !important;
          border-right: 1px solid var(--c-border);
        }
        .skin-blue .sidebar-menu > li > a {
          color: var(--c-text) !important;
          font-size: 13.5px;
          padding: 10px 16px;
          border-left: 3px solid transparent;
        }
        .skin-blue .sidebar-menu > li:hover > a {
          background-color: var(--c-bg-alt) !important;
          color: var(--c-navy) !important;
          border-left-color: var(--c-steel) !important;
        }
        .skin-blue .sidebar-menu > li.active > a {
          background-color: var(--c-bg-alt) !important;
          color: var(--c-navy) !important;
          border-left-color: var(--c-navy) !important;
          font-weight: 600;
        }

        /* Box (card) — flat, viền nhẹ thay shadow nặng */
        .box {
          border: 1px solid var(--c-border) !important;
          border-radius: 6px !important;
          box-shadow: 0 1px 2px rgba(31,56,100,0.04) !important;
          margin-bottom: 16px !important;
        }
        .box.box-primary { border-top: 3px solid var(--c-navy) !important; }
        .box.box-success { border-top: 3px solid var(--c-teal) !important; }
        .box.box-danger  { border-top: 3px solid var(--c-red)  !important; }
        .box.box-info    { border-top: 3px solid var(--c-steel) !important; }
        .box.box-warning { border-top: 3px solid var(--c-gold) !important; }
        .box-body { padding: 14px !important; }

        /* ValueBox — flat, đồng phục bo góc */
        .small-box {
          border-radius: 6px !important;
          box-shadow: 0 1px 2px rgba(31,56,100,0.06) !important;
          overflow: hidden;
        }
        .small-box h3 { font-size: 26px !important; font-weight: 700 !important; letter-spacing:-0.5px; }
        .small-box p  { font-size: 12.5px !important; opacity: 0.95; }
        .small-box .icon { font-size: 56px !important; opacity: 0.18 !important; }

        /* Map valueBox color → palette quant */
        .small-box.bg-blue   { background-color: var(--c-navy)  !important; color:#FFF !important; }
        .small-box.bg-aqua,
        .small-box.bg-teal   { background-color: var(--c-steel) !important; color:#FFF !important; }
        .small-box.bg-green  { background-color: var(--c-teal)  !important; color:#FFF !important; }
        .small-box.bg-yellow { background-color: var(--c-gold)  !important; color:#FFF !important; }
        .small-box.bg-red    { background-color: var(--c-red)   !important; color:#FFF !important; }
        .small-box.bg-purple { background-color: var(--c-navy)  !important; color:#FFF !important; }

        /* Plotly fill khung */
        .plotly.html-widget, .js-plotly-plot {
          width: 100%% !important;
          min-height: 300px;
        }

        /* DataTable nhẹ nhàng */
        table.dataTable thead th {
          background-color: var(--c-navy) !important;
          color: #FFFFFF !important;
          border-bottom: none !important;
          font-weight: 600 !important;
          font-size: 13px;
        }
        table.dataTable.stripe tbody tr.odd  { background-color: #FAFBFC; }
        table.dataTable tbody td { border-top: 1px solid var(--c-border); }
        .dataTables_wrapper .dataTables_filter input,
        .dataTables_wrapper .dataTables_length select {
          border: 1px solid var(--c-border); border-radius: 4px; padding: 4px 8px;
        }

        /* Buttons */
        .btn { border-radius: 4px !important; font-weight: 500; }
        .btn-success { background-color: var(--c-teal) !important; border-color: var(--c-teal) !important; }
        .btn-success:hover { background-color: #258576 !important; border-color: #258576 !important; }

        /* Inputs */
        .form-control { border: 1px solid var(--c-border) !important; border-radius: 4px !important; }
        .selectize-input { border: 1px solid var(--c-border) !important; border-radius: 4px !important; }
      ",
      COLORS$navy, COLORS$steel, COLORS$teal, COLORS$gold, COLORS$red, COLORS$gray,
      COLORS$text_main, COLORS$text_light, COLORS$border, COLORS$bg_alt,
      FONT_FAMILY)))
    ),

    tabItems(
      phase1UI("tab_phase1"),  phase2UI("tab_phase2"),  phase3UI("tab_phase3"),
      phase4UI("tab_phase4"),  phase5UI("tab_phase5"),  phase6UI("tab_phase6"),
      phase7UI("tab_phase7"),  phase8UI("tab_phase8"),  phase9UI("tab_phase9"),
      phase10UI("tab_phase10")
    )
  )
)


# =============================================================================
# 10. SERVER
# =============================================================================
server <- function(input, output, session) {
  all_data <- reactiveVal(GLOBAL_DATA_CACHE)

  if (is.null(GLOBAL_DATA_CACHE))
    showNotification("Lỗi: Dữ liệu chưa nạp vào RAM (data_final.rds).",
                     type = "error", duration = NULL)

  phase1Server("tab_phase1",  all_data)
  phase2Server("tab_phase2",  all_data)
  phase3Server("tab_phase3",  all_data)
  phase4Server("tab_phase4",  all_data)
  phase5Server("tab_phase5",  all_data)
  phase6Server("tab_phase6",  all_data)
  phase7Server("tab_phase7",  all_data)
  phase8Server("tab_phase8",  all_data)
  phase9Server("tab_phase9",  all_data)
  phase10Server("tab_phase10", all_data)
}

shinyApp(ui = ui, server = server)