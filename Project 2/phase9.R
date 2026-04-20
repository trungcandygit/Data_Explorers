# =============================================================================
# PHASE 9 — KH TIỀM NĂNG & TÁI TỤC
# =============================================================================

phase9UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    page_header(
      "Phần 9 — Khách hàng tiềm năng & tái tục",
      "Cảnh báo sớm hợp đồng sắp hết hạn, tệp KH tiềm năng và phễu chuyển đổi."
    ),

    fluidRow(
      valueBoxOutput(ns("vbox_expiring_30d"),     width = 3),
      valueBoxOutput(ns("vbox_expiring_60d"),     width = 3),
      valueBoxOutput(ns("vbox_high_value_leads"), width = 3),
      valueBoxOutput(ns("vbox_renewal_rate"),     width = 3)
    ),

    fluidRow(
      qbox("Gói BH hết hạn trong 60 ngày tới",
           "Danh sách KH cần liên hệ gấp để duy trì tỷ lệ tái tục",
           DTOutput(ns("table_renewal_leads")), width = 12)
    ),

    fluidRow(
      qbox("Tệp KH tiềm năng (Hạng cao)",
           "Kim cương / Bạch kim — chưa đa dạng gói SP",
           DTOutput(ns("table_high_value_leads")), width = 6),
      qbox("Phễu chuyển đổi dự kiến",
           "Tổng KH → hạng cao → sắp hết hạn → đã tái tục",
           plotlyOutput(ns("plot_lead_funnel"), height = "380px"), width = 6)
    ),

    fluidRow(
      qbox("Phân bố ngày hết hạn theo tháng (12 tháng tới)",
           "Bar = số gói, line = xu hướng",
           plotlyOutput(ns("plot_expiry_timeline"), height = "380px"), width = 6),
      qbox("Doanh thu tái tục tiềm năng theo nhóm SP",
           "GWP nếu tái tục thành công toàn bộ gói sắp hết hạn",
           plotlyOutput(ns("plot_renewal_value"), height = "380px"), width = 6)
    ),

    fluidRow(
      qbox("Tỷ lệ tái tục theo chi nhánh (Top 15)",
           "Chi nhánh nào giữ chân khách hàng tốt nhất?",
           plotlyOutput(ns("plot_renewal_by_cn"), height = "380px"), width = 6),
      qbox("Phân bố thời hạn còn lại của HĐ",
           "Đường nét đứt đỏ = mốc 30 / 60 / 90 ngày",
           plotlyOutput(ns("plot_ttl_histogram"), height = "380px"), width = 6)
    )
  )
}

phase9Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {

    get_data   <- reactive({ req(all_data()); all_data()$master })
    today_date <- as.Date("2026-04-16")

    # ──────────── KPI BOXES ────────────
    output$vbox_expiring_30d <- renderValueBox({
      n <- get_data() %>%
        filter(NGAY_HET_HAN >= today_date,
               NGAY_HET_HAN <= today_date + 30) %>% nrow()
      valueBox(label_int(n), "Hết hạn < 30 ngày",
               icon = icon("clock"), color = "red")
    })
    output$vbox_expiring_60d <- renderValueBox({
      n <- get_data() %>%
        filter(NGAY_HET_HAN >= today_date,
               NGAY_HET_HAN <= today_date + 60) %>% nrow()
      valueBox(label_int(n), "Hết hạn < 60 ngày",
               icon = icon("hourglass-half"), color = "yellow")
    })
    output$vbox_high_value_leads <- renderValueBox({
      n <- get_data() %>%
        filter(XHKH %in% c("Kim cương", "Bạch kim")) %>%
        pull(MA_KH) %>% n_distinct()
      valueBox(label_int(n), "KH hạng cao",
               icon = icon("gem"), color = "blue")
    })
    output$vbox_renewal_rate <- renderValueBox({
      df <- get_data()
      pct <- round(sum(df$LOAI_HD == "Tái tục", na.rm = TRUE) / nrow(df) * 100, 1)
      valueBox(paste0(pct, "%"), "Tỷ lệ tái tục",
               icon = icon("sync"), color = "green")
    })

    # ──────────── 1. RENEWAL LEADS TABLE ────────────
    output$table_renewal_leads <- renderDT({
      renewal <- get_data() %>%
        filter(NGAY_HET_HAN >= today_date,
               NGAY_HET_HAN <= today_date + 60) %>%
        mutate(So_ngay = as.numeric(NGAY_HET_HAN - today_date),
               Muc_do = case_when(
                 So_ngay <= 7  ~ "Khẩn cấp",
                 So_ngay <= 30 ~ "Cần xử lý",
                 TRUE          ~ "Theo dõi")) %>%
        select(MA_KH, MA_HD, TENGOISANPHAM, TEN_CN, PHIBH,
               NGAY_HET_HAN, So_ngay, Muc_do, SDT_NV) %>%
        arrange(So_ngay) %>% head(200)

      dt_quant(renewal,
               colnames = c("Mã KH", "Mã HĐ", "Gói SP", "Chi nhánh",
                            "Phí BH", "Ngày hết hạn", "Số ngày còn",
                            "Mức độ", "SĐT NV"),
               page_length = 10) %>%
        formatCurrency("PHIBH", currency = "",
                       interval = 3, mark = ".", digits = 0) %>%
        formatStyle("Muc_do",
          backgroundColor = styleEqual(
            c("Khẩn cấp", "Cần xử lý", "Theo dõi"),
            c(COLORS$red, COLORS$gold, COLORS$steel)),
          color = "white", fontWeight = "bold")
    })

    # ──────────── 2. HIGH VALUE LEADS TABLE ────────────
    output$table_high_value_leads <- renderDT({
      high_val <- get_data() %>%
        filter(XHKH %in% c("Kim cương", "Bạch kim")) %>%
        group_by(MA_KH, XHKH, TEN_CN) %>%
        summarise(So_goi = n(),
                  Tong_Phi = sum(PHIBH, na.rm = TRUE),
                  SP_da_co = paste(unique(NHOMSANPHAM), collapse = ", "),
                  .groups = "drop") %>%
        arrange(desc(Tong_Phi)) %>% head(50)

      dt_quant(high_val,
               colnames = c("Mã KH", "Xếp hạng", "Chi nhánh",
                            "Số gói", "Tổng phí", "SP đã có"),
               page_length = 8) %>%
        formatCurrency("Tong_Phi", currency = "",
                       interval = 3, mark = ".", digits = 0)
    })

    # ──────────── 3. FUNNEL ────────────
    output$plot_lead_funnel <- renderPlotly({
      df <- get_data()
      plot_ly(type = "funnel",
              y = c("Tổng KH unique", "KH hạng Vàng+",
                    "KH sắp hết hạn (90d)", "KH đã tái tục"),
              x = c(n_distinct(df$MA_KH),
                    df %>% filter(XHKH %in% c("Kim cương", "Bạch kim", "Vàng")) %>%
                      pull(MA_KH) %>% n_distinct(),
                    df %>% filter(NGAY_HET_HAN >= today_date,
                                  NGAY_HET_HAN <= today_date + 90) %>%
                      pull(MA_KH) %>% n_distinct(),
                    df %>% filter(LOAI_HD == "Tái tục") %>%
                      pull(MA_KH) %>% n_distinct()),
              textinfo = "value+percent initial",
              marker = list(color = c(COLORS$navy, COLORS$steel,
                                      COLORS$teal, COLORS$gold))) %>%
        layout(font = PLOTLY_FONT,
               margin = list(l = 140, r = 20, t = 20, b = 20),
               paper_bgcolor = COLORS$bg) %>%
        config(displayModeBar = FALSE)
    })

    # ──────────── 4. EXPIRY TIMELINE ────────────
    output$plot_expiry_timeline <- renderPlotly({
      df_exp <- get_data() %>%
        filter(NGAY_HET_HAN >= today_date,
               NGAY_HET_HAN <= today_date + 365) %>%
        mutate(Thang_HH = floor_date(NGAY_HET_HAN, "month")) %>%
        group_by(Thang_HH) %>% summarise(N = n(), .groups = "drop")

      plot_ly(df_exp, x = ~Thang_HH) %>%
        add_bars(y = ~N, name = "Số gói",
                 marker = list(color = COLORS$steel, opacity = 0.6)) %>%
        add_lines(y = ~N, name = "Xu hướng",
                  line = list(color = COLORS$navy, width = 2,
                              shape = "spline", smoothing = 1.3)) %>%
        layout(xaxis = list(tickformat = "%b %Y", dtick = "M2"),
               bargap = 0.3) %>%
        ly_quant(t = 40)
    })

    # ──────────── 5. RENEWAL VALUE ────────────
    output$plot_renewal_value <- renderPlotly({
      df_rv <- get_data() %>%
        filter(NGAY_HET_HAN >= today_date,
               NGAY_HET_HAN <= today_date + 365,
               NHOMSANPHAM != "Chưa xác định") %>%
        group_by(NHOMSANPHAM) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")

      p <- ggplot(df_rv, aes(x = reorder(NHOMSANPHAM, GWP), y = GWP)) +
        geom_col(fill = COLORS$teal, width = SIZE$bar_width) + coord_flip() +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0, 0.08))) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "GWP tiềm năng (VND)")

      ggplotly(p) %>% ly_quant(b = 25)
    })

    # ──────────── 6. RENEWAL BY CN ────────────
    output$plot_renewal_by_cn <- renderPlotly({
      df_rr <- get_data() %>%
        filter(TEN_CN != "Chưa xác định",
               !grepl("Hội sở|TCT", TEN_CN, ignore.case = TRUE)) %>%
        group_by(TEN_CN) %>%
        summarise(Total = n(),
                  Pct = round(sum(LOAI_HD == "Tái tục", na.rm = TRUE) / Total * 100, 1),
                  .groups = "drop") %>%
        top_n(15, Total)

      p <- ggplot(df_rr, aes(x = reorder(TEN_CN, Pct), y = Pct)) +
        geom_col(fill = COLORS$navy, width = SIZE$bar_width) +
        geom_text(aes(label = paste0(Pct, "%")),
                  hjust = -0.08, size = SIZE$label,
                  color = COLORS$navy, fontface = "bold") +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "Tỷ lệ tái tục (%)")

      ggplotly(p, tooltip = "y") %>% ly_quant(b = 25)
    })

    # ──────────── 7. TTL HISTOGRAM ────────────
    output$plot_ttl_histogram <- renderPlotly({
      df_ttl <- get_data() %>%
        filter(NGAY_HET_HAN >= today_date) %>%
        mutate(TTL = as.numeric(NGAY_HET_HAN - today_date)) %>%
        filter(TTL <= 365)

      p <- ggplot(df_ttl, aes(x = TTL)) +
        geom_histogram(binwidth = 15, fill = COLORS$steel,
                       alpha = 0.85, color = "white", linewidth = 0.2) +
        geom_vline(xintercept = c(30, 60, 90),
                   linetype = "dashed", color = COLORS$red, linewidth = 0.6) +
        annotate("text", x = 30, y = Inf, label = "30d", vjust = 2, hjust = -0.2,
                 color = COLORS$red, size = 3, fontface = "bold") +
        annotate("text", x = 60, y = Inf, label = "60d", vjust = 2, hjust = -0.2,
                 color = COLORS$red, size = 3, fontface = "bold") +
        annotate("text", x = 90, y = Inf, label = "90d", vjust = 2, hjust = -0.2,
                 color = COLORS$red, size = 3, fontface = "bold") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                           labels = label_int) +
        theme_quant(grid = "y") +
        labs(x = "Số ngày còn lại", y = "Số gói BH")

      ggplotly(p) %>% ly_quant(b = 25)
    })
  })
}