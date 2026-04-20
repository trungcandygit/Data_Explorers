# =============================================================================
# PHASE 10 — TỔNG KẾT DỰ ÁN & ĐỀ XUẤT ĐIỀU HÀNH
# =============================================================================

# --- helper nội bộ phase10: render khối kết luận ---
.p10_block <- function(title, items) {
  css_h <- sprintf("color:%s; font-weight:700; font-size:14px; margin:0 0 10px 0; font-family:%s;",
                   COLORS$navy, FONT_FAMILY)
  css_b <- sprintf("font-family:%s; color:%s; font-size:13px; line-height:1.85;",
                   FONT_FAMILY, COLORS$text_main)
  css_s <- sprintf("color:%s;", COLORS$navy)

  tags$div(style = css_b,
    tags$h4(style = css_h, title),
    tags$ul(style = "padding-left:18px; margin:0;",
      lapply(items, function(li) {
        if (is.null(li$strong))
          tags$li(li$text)
        else
          tags$li(tags$strong(style = css_s, paste0(li$strong, ": ")), li$text)
      })
    )
  )
}

phase10UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    page_header(
      "Phần 10 — Tổng kết dự án & đề xuất điều hành",
      "Tóm lược các phát hiện chính, chỉ số cốt lõi và định hướng chiến lược."
    ),

    fluidRow(
      valueBoxOutput(ns("vb_total_gwp"), width = 3),
      valueBoxOutput(ns("vb_total_hd"),  width = 3),
      valueBoxOutput(ns("vb_total_kh"),  width = 3),
      valueBoxOutput(ns("vb_renewal"),   width = 3)
    ),

    fluidRow(
      qbox("Hiệu suất tổng thể dự án",
           "Mức độ hoàn thành các chỉ số lõi so với kỳ vọng",
           plotlyOutput(ns("plot_radar_summary"), height = "400px"), width = 8),
      qbox("Chỉ số cốt lõi", NULL,
           uiOutput(ns("ui_core_metrics")), width = 4)
    ),

    section_header("Kết luận chính"),
    fluidRow(
      qbox("Hiệu suất kinh doanh & doanh thu", NULL,
           uiOutput(ns("ui_conclusion_sales")), width = 6),
      qbox("Hành vi khách hàng", NULL,
           uiOutput(ns("ui_conclusion_customer")), width = 6)
    ),
    fluidRow(
      qbox("Sản phẩm & danh mục", NULL,
           uiOutput(ns("ui_conclusion_product")), width = 6),
      qbox("Thách thức & rủi ro", NULL,
           uiOutput(ns("ui_conclusion_challenge")), width = 6)
    ),

    section_header("Khuyến nghị chiến lược"),
    fluidRow(
      qbox("Nâng cao tỷ lệ tái tục", NULL,
           uiOutput(ns("ui_recom_retention")), width = 6),
      qbox("Chiến lược sản phẩm", NULL,
           uiOutput(ns("ui_recom_product")), width = 6)
    ),
    fluidRow(
      qbox("Tối ưu kênh phân phối", NULL,
           uiOutput(ns("ui_recom_channel")), width = 6),
      qbox("Công nghệ & dữ liệu", NULL,
           uiOutput(ns("ui_recom_tech")), width = 6)
    ),

    fluidRow(
      column(12,
        tags$div(style = "text-align:center; padding:40px 0 20px 0;",
          tags$h2("Cảm ơn!",
            style = sprintf("color:%s; font-weight:700; font-size:32px; margin-bottom:8px; font-family:%s;",
                            COLORS$navy, FONT_FAMILY)),
          tags$p("Nhóm: ", tags$strong("10tr_devided_by_5"),
            style = sprintf("font-size:15px; color:%s; font-family:%s;",
                            COLORS$text_main, FONT_FAMILY)),
          tags$p("tạm thời để trống mail",
            style = sprintf("color:%s; font-weight:bold; font-size:15px; font-family:%s;",
                            COLORS$steel, FONT_FAMILY))
        )
      )
    )
  )
}

phase10Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {

    get_data <- reactive({ req(all_data()); all_data()$master })
    get_kpi  <- reactive({ req(all_data()); all_data()$kpi })

    # ──────────── KPI BOXES ────────────
    output$vb_total_gwp <- renderValueBox({
      valueBox(format_vnd(sum(get_data()$PHIBH, na.rm = TRUE)),
               "Tổng GWP", icon = icon("coins"), color = "blue")
    })
    output$vb_total_hd <- renderValueBox({
      valueBox(label_int(n_distinct(get_data()$MA_HD)),
               "Hợp đồng", icon = icon("file-contract"), color = "aqua")
    })
    output$vb_total_kh <- renderValueBox({
      valueBox(label_int(n_distinct(get_data()$MA_KH)),
               "Khách hàng", icon = icon("users"), color = "green")
    })
    output$vb_renewal <- renderValueBox({
      df <- get_data()
      pct <- round(sum(df$LOAI_HD == "Tái tục", na.rm = TRUE) / nrow(df) * 100, 1)
      valueBox(paste0(pct, "%"), "Tỷ lệ tái tục",
               icon = icon("sync"), color = "yellow")
    })

    # ──────────── RADAR SUMMARY ────────────
    output$plot_radar_summary <- renderPlotly({
      df  <- get_data(); kpi <- get_kpi()
      kpi_pct     <- min(round(sum(df$PHIBH, na.rm = TRUE) /
                               sum(kpi$DOANH_THU, na.rm = TRUE) * 100, 0), 100)
      renewal_pct <- round(sum(df$LOAI_HD == "Tái tục") / nrow(df) * 100, 0)
      n_cn        <- n_distinct(df$TEN_CN[df$TEN_CN != "Chưa xác định"])
      coverage    <- min(round(n_cn / 22 * 100, 0), 100)
      avg_goi     <- round(nrow(df) / n_distinct(df$MA_KH), 2)
      crosssell   <- min(round(avg_goi / 3 * 100, 0), 100)

      scores <- c(kpi_pct, renewal_pct, coverage, crosssell, 85)
      labels <- c("Hoàn thành KPI", "Tỷ lệ tái tục",
                  "Phủ sóng CN", "Cross-sell index", "Chất lượng DL")

      plot_ly(type = "scatterpolar",
              r = c(scores, scores[1]),
              theta = c(labels, labels[1]),
              fill = "toself",
              fillcolor = "rgba(68, 114, 196, 0.25)",
              line = list(color = COLORS$navy, width = 2.5),
              marker = list(color = COLORS$navy, size = 7)) %>%
        layout(font = PLOTLY_FONT,
               polar = list(
                 radialaxis = list(visible = TRUE, range = c(0, 100),
                                   gridcolor = COLORS$grid,
                                   linecolor = COLORS$border),
                 angularaxis = list(gridcolor = COLORS$grid,
                                    linecolor = COLORS$border,
                                    tickfont = list(size = 11,
                                                    color = COLORS$navy,
                                                    family = FONT_FAMILY_PLOTLY))),
               showlegend = FALSE,
               margin = list(l = 60, r = 60, t = 30, b = 40),
               paper_bgcolor = COLORS$bg) %>%
        config(displayModeBar = FALSE)
    })

    # ──────────── CORE METRICS TABLE ────────────
    output$ui_core_metrics <- renderUI({
      df <- get_data(); kpi <- get_kpi()
      kpi_pct <- round(sum(df$PHIBH, na.rm = TRUE) /
                       sum(kpi$DOANH_THU, na.rm = TRUE) * 100, 1)
      renewal <- round(sum(df$LOAI_HD == "Tái tục") / nrow(df) * 100, 1)
      top_cn <- df %>% filter(TEN_CN != "Chưa xác định") %>%
        group_by(TEN_CN) %>% summarise(v = sum(PHIBH)) %>%
        arrange(desc(v)) %>% slice(1) %>% pull(TEN_CN)
      top_sp <- df %>% filter(TENGOISANPHAM != "Chưa xác định") %>%
        group_by(TENGOISANPHAM) %>% summarise(v = sum(PHIBH)) %>%
        arrange(desc(v)) %>% slice(1) %>% pull(TENGOISANPHAM)

      mr <- function(l, v) tags$tr(
        tags$td(style = sprintf("padding:6px 8px; font-weight:bold; border-bottom:1px solid %s; width:50%%;",
                                COLORS$border), l),
        tags$td(style = sprintf("padding:6px 8px; border-bottom:1px solid %s; color:%s; font-weight:bold;",
                                COLORS$border, COLORS$navy), v))

      tags$div(style = sprintf("font-family:%s; color:%s; font-size:13px;",
                                FONT_FAMILY, COLORS$text_main),
        tags$table(style = "width:100%; border-collapse:collapse;",
          mr("Tổng GWP",          format_vnd(sum(df$PHIBH, na.rm = TRUE))),
          mr("Hoàn thành KPI",    paste0(kpi_pct, "%")),
          mr("Phí BH bình quân",  format_vnd(mean(df$PHIBH, na.rm = TRUE))),
          mr("Tỷ lệ tái tục",     paste0(renewal, "%")),
          mr("CN dẫn đầu",        top_cn),
          mr("SP dẫn đầu",        top_sp),
          mr("Tổng gói BH",       label_int(nrow(df))),
          mr("KH unique",         label_int(n_distinct(df$MA_KH)))
        )
      )
    })

    # ──────────── KẾT LUẬN ────────────
    output$ui_conclusion_sales <- renderUI({
      df <- get_data()
      top_cn <- df %>% filter(TEN_CN != "Chưa xác định") %>%
        group_by(TEN_CN) %>% summarise(v = sum(PHIBH)) %>%
        arrange(desc(v)) %>% slice(1)
      .p10_block("Hiệu suất kinh doanh & doanh thu", list(
        list(strong = "Quy mô",
             text = paste0("Tổng ", label_int(nrow(df)), " gói BH với GWP đạt ",
                           format_vnd(sum(df$PHIBH, na.rm = TRUE)), ".")),
        list(strong = "Chi nhánh dẫn đầu",
             text = paste0(top_cn$TEN_CN, " với ", format_vnd(top_cn$v),
                           " — chiếm tỷ trọng lớn nhất.")),
        list(strong = "Xu hướng",
             text = "Doanh thu tăng trưởng ổn định Q1-2023 → Q2-2025, dấu hiệu bão hoà nhẹ gần đây."),
        list(strong = "Kênh bán",
             text = "Trực tiếp & Đại lý chi phối; E-Commerce và Bancas còn dư địa.")
      ))
    })

    output$ui_conclusion_customer <- renderUI({
      df <- get_data()
      renewal <- round(sum(df$LOAI_HD == "Tái tục") / nrow(df) * 100, 1)
      pct_vip <- round(df %>% filter(XHKH %in% c("Kim cương", "Bạch kim")) %>%
                         pull(MA_KH) %>% n_distinct() /
                       n_distinct(df$MA_KH) * 100, 1)
      .p10_block("Hành vi khách hàng", list(
        list(strong = "Tổng KH",
             text = paste0(label_int(n_distinct(df$MA_KH)),
                           " khách hàng — phân bố đều qua 5 hạng.")),
        list(strong = "Tỷ lệ tái tục",
             text = paste0(renewal, "% — cần cải thiện chiến lược giữ chân.")),
        list(strong = "KH giá trị cao",
             text = paste0(pct_vip, "% Kim cương / Bạch kim — đóng góp phần lớn DT.")),
        list(strong = "Cross-sell",
             text = "Số gói TB/KH còn thấp ở Đồng/Bạc — cơ hội lớn.")
      ))
    })

    output$ui_conclusion_product <- renderUI({
      df <- get_data()
      top_sp <- df %>% filter(TENGOISANPHAM != "Chưa xác định") %>%
        group_by(TENGOISANPHAM) %>% summarise(v = sum(PHIBH)) %>%
        arrange(desc(v)) %>% slice(1)
      .p10_block("Sản phẩm & danh mục", list(
        list(strong = "SP dẫn đầu",
             text = paste0(top_sp$TENGOISANPHAM, " — tỷ trọng DT cao nhất.")),
        list(strong = "BH bắt buộc",
             text = "TNDS xe ô tô / xe máy có volume lớn nhưng phí bình quân thấp (Cash Cow)."),
        list(strong = "BH tự nguyện",
             text = "Vật chất xe có phí bình quân cao nhất — sản phẩm Star, đẩy mạnh."),
        list(strong = "BH con người",
             text = "BHAT, BHSK còn nhỏ — tiềm năng phát triển qua Bancas.")
      ))
    })

    output$ui_conclusion_challenge <- renderUI({
      .p10_block("Thách thức & rủi ro", list(
        list(strong = "Tập trung quá mức",
             text = "DT phụ thuộc vào số ít CN và SP chủ lực — rủi ro nếu thị trường biến động."),
        list(strong = "Tái tục thấp",
             text = "Đặc biệt tại các CN tỉnh — cần hệ thống nhắc tự động."),
        list(strong = "Ngoại lai dữ liệu",
             text = "Tồn tại gói phí < 1 nghìn và tài khoản Hội sở gộp — cần audit logic."),
        list(strong = "Kênh mới",
             text = "E-Commerce & Bancas còn nhỏ, cần đầu tư đa dạng nguồn thu.")
      ))
    })

    # ──────────── KHUYẾN NGHỊ ────────────
    output$ui_recom_retention <- renderUI({
      .p10_block("Nâng cao tỷ lệ tái tục", list(
        list(strong = "Nhắc hạn tự động",
             text = "Triển khai Zalo / SMS trước 30-60 ngày hết hạn."),
        list(strong = "Ưu đãi tái tục sớm",
             text = "Giảm 5-10% nếu gia hạn trước 30 ngày."),
        list(strong = "KPI tái tục riêng",
             text = "Gán cho từng NV và CN — theo dõi hàng tuần."),
        list(strong = "Win-back",
             text = "Chiến dịch cho KH hạng Vàng+ đã hết hạn quá 90 ngày.")
      ))
    })

    output$ui_recom_product <- renderUI({
      .p10_block("Chiến lược sản phẩm", list(
        list(strong = "Combo TNDS + Vật chất",
             text = "Cho kênh Đại lý — tăng phí bình quân/gói."),
        list(strong = "Fleet Shield",
             text = "Gói chuyên biệt cho doanh nghiệp vận tải (cụm VIP/K-Means)."),
        list(strong = "Mở rộng BH con người",
             text = "BHAT, BHSK qua kênh Bancas — phí BQ cao, volume thấp."),
        list(strong = "Up-sell",
             text = "KH có xe > 500 triệu nhưng phí < 3 triệu — hàng nghìn leads tiềm năng.")
      ))
    })

    output$ui_recom_channel <- renderUI({
      .p10_block("Tối ưu kênh phân phối", list(
        list(strong = "E-Commerce",
             text = "Tích hợp mua BH online — tiết kiệm chi phí phân phối."),
        list(strong = "Bancas",
             text = "Đẩy mạnh với ngân hàng đối tác — KH ngân hàng có thu nhập ổn định."),
        list(strong = "Top Sales transfer",
             text = "Chuyển giao mô hình từ CN mạnh sang CN yếu."),
        list(strong = "Mở rộng POS",
             text = "Mạng lưới gara / salon ô tô — tiếp cận trực tiếp KH mua xe mới.")
      ))
    })

    output$ui_recom_tech <- renderUI({
      .p10_block("Công nghệ & dữ liệu", list(
        list(strong = "AI Recommendation Engine",
             text = "Triển khai vào quy trình bán hàng — gợi ý SP realtime."),
        list(strong = "Data Warehouse",
             text = "Tập trung — kết nối dữ liệu bán hàng, KH, claim."),
        list(strong = "Dashboard KPI realtime",
             text = "Cho ban lãnh đạo — theo dõi tiến độ hàng ngày."),
        list(strong = "Làm sạch tài khoản Hội sở",
             text = "Tách riêng giao dịch tổng công ty.")
      ))
    })
  })
}