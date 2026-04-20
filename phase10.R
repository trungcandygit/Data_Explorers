phase10UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    h2("Phần 10: Tổng kết dự án và Đề xuất điều hành",
       style = "color:#303983; font-weight:700; margin-bottom:4px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),
    p("Tóm lược các phát hiện chính, chỉ số cốt lõi và định hướng chiến lược cho giai đoạn tiếp theo.",
      style = "color:#6C7A89; margin-bottom:18px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),

    fluidRow(
      valueBoxOutput(ns("vb_total_gwp"), width = 3), valueBoxOutput(ns("vb_total_hd"), width = 3),
      valueBoxOutput(ns("vb_total_kh"), width = 3), valueBoxOutput(ns("vb_renewal"), width = 3)
    ),

    fluidRow(
      box(title = NULL, width = 8, status = "primary", solidHeader = FALSE,
          tags$h4("Hiệu suất tổng thể dự án", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Đánh giá mức độ hoàn thành các chỉ số lõi so với kỳ vọng.", style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_radar_summary"), height = "400px")),
      box(title = NULL, width = 4, status = "primary", solidHeader = FALSE,
          tags$h4("Chỉ số cốt lõi", style = "color:#303983; font-weight:700; margin:0 0 8px 0; font-family:'Helvetica Neue',sans-serif;"),
          uiOutput(ns("ui_core_metrics")))
    ),

    tags$h3("Kết luận chính",
            style = "color:#303983; font-weight:700; font-size:22px; margin:25px 0 15px 0; font-family:'Helvetica Neue',sans-serif; border-bottom:2px solid #EBEBEB; padding-bottom:8px;"),
    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE, uiOutput(ns("ui_conclusion_sales"))),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE, uiOutput(ns("ui_conclusion_customer")))
    ),
    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE, uiOutput(ns("ui_conclusion_product"))),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE, uiOutput(ns("ui_conclusion_challenge")))
    ),

    tags$h3("Khuyến nghị chiến lược",
            style = "color:#303983; font-weight:700; font-size:22px; margin:25px 0 15px 0; font-family:'Helvetica Neue',sans-serif; border-bottom:2px solid #EBEBEB; padding-bottom:8px;"),
    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE, uiOutput(ns("ui_recom_retention"))),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE, uiOutput(ns("ui_recom_product")))
    ),
    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE, uiOutput(ns("ui_recom_channel"))),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE, uiOutput(ns("ui_recom_tech")))
    ),

    fluidRow(
      column(12, tags$div(style = "text-align:center; padding:40px 0 20px 0;",
        tags$h2("Cảm ơn!", style = "color:#303983; font-weight:700; font-size:32px; margin-bottom:8px; font-family:'Helvetica Neue',sans-serif;"),
        tags$p("Nhóm: ", tags$strong("10tr_devided_by_5"), style = "font-size:15px; color:#4A4A4A; font-family:'Helvetica Neue',sans-serif;"),
        tags$p("tạm thời để trống mail", style = "color:#4A6FA5; font-weight:bold; font-size:15px; font-family:'Helvetica Neue',sans-serif;")
      ))
    )
  )
}

phase10Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    get_data <- reactive({ req(all_data()); all_data()$master })
    get_kpi  <- reactive({ req(all_data()); all_data()$kpi })
    pf <- list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", color = "#4A4A4A")
    css_h <- "color:#303983; font-weight:700; font-size:16px; margin:0 0 10px 0; font-family:'Helvetica Neue',sans-serif;"
    css_b <- "font-family:'Helvetica Neue',sans-serif; color:#4A4A4A; font-size:13px; line-height:1.9;"
    css_s <- "color:#303983;"

    output$vb_total_gwp <- renderValueBox({
      valueBox(format_vnd(sum(get_data()$PHIBH, na.rm = TRUE)), "Tổng GWP", color = "blue")
    })
    output$vb_total_hd <- renderValueBox({
      valueBox(formatC(n_distinct(get_data()$MA_HD), format = "d", big.mark = "."), "Hợp đồng", color = "blue")
    })
    output$vb_total_kh <- renderValueBox({
      valueBox(formatC(n_distinct(get_data()$MA_KH), format = "d", big.mark = "."), "Khách hàng", color = "teal")
    })
    output$vb_renewal <- renderValueBox({
      df <- get_data(); pct <- round(sum(df$LOAI_HD == "Tái tục", na.rm = TRUE) / nrow(df) * 100, 1)
      valueBox(paste0(pct, "%"), "Tỷ lệ tái tục", color = "green")
    })

    output$plot_radar_summary <- renderPlotly({
      df <- get_data(); kpi <- get_kpi()
      kpi_pct <- min(round(sum(df$PHIBH, na.rm = TRUE) / sum(kpi$DOANH_THU, na.rm = TRUE) * 100, 0), 100)
      renewal_pct <- round(sum(df$LOAI_HD == "Tái tục") / nrow(df) * 100, 0)
      n_cn <- n_distinct(df$TEN_CN[df$TEN_CN != "Chưa xác định"])
      coverage <- min(round(n_cn / 22 * 100, 0), 100)
      avg_goi <- round(nrow(df) / n_distinct(df$MA_KH), 2)
      crosssell <- min(round(avg_goi / 3 * 100, 0), 100)
      scores <- c(kpi_pct, renewal_pct, coverage, crosssell, 85)
      labels <- c("Hoàn thành KPI", "Tỷ lệ tái tục", "Phủ sóng CN", "Cross-sell index", "Chất lượng DL")
      plot_ly(type = "scatterpolar", r = c(scores, scores[1]), theta = c(labels, labels[1]), fill = "toself",
              fillcolor = "rgba(74,111,165,0.25)", line = list(color = "#303983", width = 2.5),
              marker = list(color = "#303983", size = 7)) %>%
        layout(font = pf,
               polar = list(radialaxis = list(visible = TRUE, range = c(0, 100), gridcolor = "#EBEBEB", linecolor = "#EBEBEB"),
                            angularaxis = list(gridcolor = "#EBEBEB", linecolor = "#EBEBEB", tickfont = list(size = 11, color = "#303983"))),
               showlegend = FALSE, margin = list(l = 60, r = 60, t = 30, b = 40)) %>% config(displayModeBar = FALSE)
    })

    output$ui_core_metrics <- renderUI({
      df <- get_data(); kpi <- get_kpi()
      kpi_pct <- round(sum(df$PHIBH, na.rm = TRUE) / sum(kpi$DOANH_THU, na.rm = TRUE) * 100, 1)
      renewal <- round(sum(df$LOAI_HD == "Tái tục") / nrow(df) * 100, 1)
      top_cn <- df %>% filter(TEN_CN != "Chưa xác định") %>% group_by(TEN_CN) %>% summarise(v = sum(PHIBH)) %>% arrange(desc(v)) %>% slice(1) %>% pull(TEN_CN)
      top_sp <- df %>% filter(TENGOISANPHAM != "Chưa xác định") %>% group_by(TENGOISANPHAM) %>% summarise(v = sum(PHIBH)) %>% arrange(desc(v)) %>% slice(1) %>% pull(TENGOISANPHAM)
      mr <- function(l, v) tags$tr(
        tags$td(style = "padding:6px 8px; font-weight:bold; border-bottom:1px solid #EBEBEB; width:50%;", l),
        tags$td(style = "padding:6px 8px; border-bottom:1px solid #EBEBEB; color:#303983; font-weight:bold;", v))
      tags$div(style = "font-family:'Helvetica Neue',sans-serif; color:#4A4A4A; font-size:13px;",
        tags$table(style = "width:100%; border-collapse:collapse;",
          mr("Tổng GWP", format_vnd(sum(df$PHIBH, na.rm = TRUE))),
          mr("Hoàn thành KPI", paste0(kpi_pct, "%")),
          mr("Phí BH bình quân", format_vnd(mean(df$PHIBH, na.rm = TRUE))),
          mr("Tỷ lệ tái tục", paste0(renewal, "%")),
          mr("CN dẫn đầu", top_cn), mr("SP dẫn đầu", top_sp),
          mr("Tổng gói BH", formatC(nrow(df), format = "d", big.mark = ".")),
          mr("KH unique", formatC(n_distinct(df$MA_KH), format = "d", big.mark = "."))))
    })

    output$ui_conclusion_sales <- renderUI({
      df <- get_data()
      top_cn <- df %>% filter(TEN_CN != "Chưa xác định") %>% group_by(TEN_CN) %>% summarise(v = sum(PHIBH)) %>% arrange(desc(v)) %>% slice(1)
      tags$div(style = css_b,
        tags$h4(style = css_h, "Hiệu suất kinh doanh & Doanh thu"),
        tags$ul(
          tags$li(tags$strong(style = css_s, "Quy mô: "), paste0("Tổng ", formatC(nrow(df), format = "d", big.mark = "."), " gói BH với tổng GWP đạt ", format_vnd(sum(df$PHIBH, na.rm = TRUE)), ".")),
          tags$li(tags$strong(style = css_s, "Chi nhánh dẫn đầu: "), paste0(top_cn$TEN_CN, " với ", format_vnd(top_cn$v), " — chiếm tỷ trọng lớn nhất.")),
          tags$li(tags$strong(style = css_s, "Xu hướng: "), "Doanh thu tăng trưởng ổn định từ Q1-2023 đến Q2-2025, dấu hiệu bão hòa nhẹ ở các tháng gần đây."),
          tags$li(tags$strong(style = css_s, "Kênh bán: "), "Kênh trực tiếp và Đại lý chiếm tỷ trọng chi phối. Kênh E-Commerce và Bancas còn nhiều dư địa.")))
    })

    output$ui_conclusion_customer <- renderUI({
      df <- get_data()
      renewal <- round(sum(df$LOAI_HD == "Tái tục") / nrow(df) * 100, 1)
      pct_vip <- round(df %>% filter(XHKH %in% c("Kim cương", "Bạch kim")) %>% pull(MA_KH) %>% n_distinct() / n_distinct(df$MA_KH) * 100, 1)
      tags$div(style = css_b,
        tags$h4(style = css_h, "Hành vi khách hàng"),
        tags$ul(
          tags$li(tags$strong(style = css_s, "Tổng KH: "), paste0(formatC(n_distinct(df$MA_KH), format = "d", big.mark = "."), " khách hàng — phân bố đều qua 5 hạng.")),
          tags$li(tags$strong(style = css_s, "Tỷ lệ tái tục: "), paste0(renewal, "% — cần cải thiện chiến lược giữ chân.")),
          tags$li(tags$strong(style = css_s, "KH giá trị cao: "), paste0(pct_vip, "% thuộc Kim cương/Bạch kim — đóng góp phần lớn doanh thu.")),
          tags$li(tags$strong(style = css_s, "Cross-sell: "), "Số gói TB/KH còn thấp, đặc biệt nhóm Đồng/Bạc — cơ hội lớn.")))
    })

    output$ui_conclusion_product <- renderUI({
      df <- get_data()
      top_sp <- df %>% filter(TENGOISANPHAM != "Chưa xác định") %>% group_by(TENGOISANPHAM) %>% summarise(v = sum(PHIBH)) %>% arrange(desc(v)) %>% slice(1)
      tags$div(style = css_b,
        tags$h4(style = css_h, "Sản phẩm & Danh mục"),
        tags$ul(
          tags$li(tags$strong(style = css_s, "SP dẫn đầu: "), paste0(top_sp$TENGOISANPHAM, " — chiếm tỷ trọng doanh thu cao nhất.")),
          tags$li(tags$strong(style = css_s, "BH bắt buộc: "), "Nhóm TNDS xe ô tô/xe máy có volume lớn nhưng phí bình quân thấp (Cash Cow)."),
          tags$li(tags$strong(style = css_s, "BH tự nguyện: "), "Nhóm Vật chất xe có phí bình quân cao nhất — sản phẩm Star, cần đẩy mạnh."),
          tags$li(tags$strong(style = css_s, "BH con người: "), "Các gói BHAT, BHSK chiếm tỷ trọng nhỏ — tiềm năng phát triển qua Bancas.")))
    })

    output$ui_conclusion_challenge <- renderUI({
      tags$div(style = css_b,
        tags$h4(style = css_h, "Thách thức & Rủi ro"),
        tags$ul(
          tags$li(tags$strong(style = css_s, "Tập trung quá mức: "), "Doanh thu phụ thuộc lớn vào một số chi nhánh và sản phẩm chủ lực — rủi ro nếu thị trường thay đổi."),
          tags$li(tags$strong(style = css_s, "Tái tục thấp: "), "Tỷ lệ tái tục chưa tối ưu, đặc biệt ở các chi nhánh tỉnh — cần hệ thống nhắc nhở tự động."),
          tags$li(tags$strong(style = css_s, "Ngoại lai dữ liệu: "), "Tồn tại các gói BH phí cực thấp (< 1 nghìn) và tài khoản Hội sở gộp — cần kiểm tra logic."),
          tags$li(tags$strong(style = css_s, "Kênh mới: "), "E-Commerce và Bancas mới chiếm tỷ trọng nhỏ, cần đầu tư để đa dạng hóa nguồn thu.")))
    })

    output$ui_recom_retention <- renderUI({
      tags$div(style = css_b,
        tags$h4(style = css_h, "Nâng cao tỷ lệ tái tục"),
        tags$ul(
          tags$li("Triển khai hệ thống ", tags$strong("nhắc hạn tự động"), " qua Zalo/SMS trước 30-60 ngày hết hạn."),
          tags$li("Chương trình ", tags$strong("ưu đãi tái tục sớm"), ": giảm 5-10% nếu gia hạn trước 30 ngày."),
          tags$li("Gán KPI tái tục riêng cho từng nhân viên và chi nhánh — theo dõi hàng tuần."),
          tags$li("Chiến dịch ", tags$strong("win-back"), " cho KH hạng Vàng+ đã hết hạn quá 90 ngày.")))
    })

    output$ui_recom_product <- renderUI({
      tags$div(style = css_b,
        tags$h4(style = css_h, "Chiến lược sản phẩm"),
        tags$ul(
          tags$li("Thiết kế gói ", tags$strong("Combo TNDS + Vật chất"), " cho kênh Đại lý — tăng phí bình quân/gói."),
          tags$li("Phát triển gói ", tags$strong("Fleet Shield"), " chuyên biệt cho doanh nghiệp vận tải (cụm VIP/K-Means)."),
          tags$li("Mở rộng danh mục BH con người (BHAT, BHSK) qua kênh Bancas — phí BQ cao, volume còn thấp."),
          tags$li("Up-sell KH có xe > 500 triệu nhưng phí < 3 triệu — ước tính hàng nghìn leads tiềm năng.")))
    })

    output$ui_recom_channel <- renderUI({
      tags$div(style = css_b,
        tags$h4(style = css_h, "Tối ưu kênh phân phối"),
        tags$ul(
          tags$li("Tăng cường ", tags$strong("kênh E-Commerce"), ": tích hợp mua BH online — tiết kiệm chi phí phân phối."),
          tags$li("Đẩy mạnh ", tags$strong("Bancas"), " với ngân hàng đối tác — KH ngân hàng có thu nhập ổn định."),
          tags$li("Chuyển giao mô hình ", tags$strong("Top Sales"), " từ chi nhánh mạnh sang chi nhánh yếu."),
          tags$li("Mở rộng mạng lưới ", tags$strong("gara/salon ô tô"), " — kênh POS tiếp cận trực tiếp KH mua xe mới.")))
    })

    output$ui_recom_tech <- renderUI({
      tags$div(style = css_b,
        tags$h4(style = css_h, "Công nghệ & Dữ liệu"),
        tags$ul(
          tags$li("Triển khai ", tags$strong("Gợi ý sản phẩm"), " vào quy trình bán hàng — gợi ý sản phẩm realtime."),
          tags$li("Xây dựng ", tags$strong("Data Warehouse"), " tập trung — kết nối dữ liệu bán hàng, KH, claim."),
          tags$li("Dashboard ", tags$strong("KPI realtime"), " cho ban lãnh đạo — theo dõi tiến độ hàng ngày."),
          tags$li("Làm sạch dữ liệu ", tags$strong("tài khoản Hội sở"), " — tách riêng giao dịch tổng công ty.")))
    })
  })
}