# =============================================================================
# PHASE 5 — MẠNG LƯỚI CHI NHÁNH & VẬN HÀNH
# =============================================================================

phase5UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    page_header(
      "Phần 5 — Mạng lưới chi nhánh & vận hành",
      "Sức khoẻ mạng lưới, hiệu suất chi nhánh & nhân viên, giám sát KPI."
    ),

    # ===== SECTION 1 =====
    section_header("1. Tổng quan mạng lưới chi nhánh"),
    fluidRow(
      qbox("Top chi nhánh theo doanh thu GWP", NULL,
           plotlyOutput(ns("plot_branch_gwp_bar"), height = "380px"), width = 7),
      qbox("Cơ cấu xếp hạng KH tại Top 3 CN",
           "Tỷ lệ Đồng / Bạc / Vàng / Bạch kim / Kim cương",
           plotlyOutput(ns("plot_branch_xhkh"), height = "380px"), width = 5)
    ),

    # ===== SECTION 2 =====
    section_header("2. Xu hướng doanh thu & cơ cấu sản phẩm"),
    fluidRow(
      qbox("Biến động doanh thu Top 5 CN theo tháng",
           "Loại trừ Hội sở — đường cong mượt giúp nhận diện xu hướng",
           plotlyOutput(ns("plot_branch_trend_multi"), height = "380px"), width = 12)
    ),
    fluidRow(
      qbox("Ma trận sản phẩm × chi nhánh (Top 10 CN)",
           "Heatmap — màu đậm = đóng góp lớn",
           plotlyOutput(ns("plot_matrix_prod_branch"), height = "440px"), width = 12)
    ),

    # ===== SECTION 3 =====
    section_header("3. Quản trị mục tiêu (KPI)"),
    fluidRow(
      qbox("Doanh thu thực tế vs mục tiêu (KPI)",
           "Cột = thực tế, đường nét đứt = mục tiêu",
           plotlyOutput(ns("plot_kpi_combo"), height = "420px"), width = 9),
      qbox("Bộ lọc & tiến độ", NULL, width = 3,
           selectInput(ns("select_kpi_channel"), "Chọn kênh:", choices = "Tất cả"),
           hr(),
           valueBoxOutput(ns("vbox_kpi_pct"), width = 12))
    ),

    # ===== SECTION 4 =====
    section_header("4. Phân tích năng suất nhân viên"),
    fluidRow(
      qbox("Phân phối năng suất NV",
           "Histogram log-scale — phân hoá năng suất giữa các NV",
           plotlyOutput(ns("plot_staff_density"), height = "370px"), width = 6),
      qbox("Tổng GWP theo kênh bán", NULL,
           plotlyOutput(ns("plot_channel_comp"), height = "370px"), width = 6)
    ),
    fluidRow(
      qbox("Năng suất TB/NV theo chi nhánh (Top 15)", NULL,
           plotlyOutput(ns("plot_productivity_cn"), height = "370px"), width = 6),
      qbox("Top 15 nhân viên xuất sắc", NULL,
           DTOutput(ns("table_top_staff")), width = 6)
    )
  )
}

phase5Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {

    get_data <- reactive({ req(all_data()); all_data()$master })
    get_kpi  <- reactive({ req(all_data()); all_data()$kpi })

    observe({
      kb <- get_data() %>%
        filter(TENKENHBAN != "Chưa xác định") %>%
        pull(TENKENHBAN) %>% unique() %>% sort()
      updateSelectInput(session, "select_kpi_channel",
                        choices = c("Tất cả", kb))
    })

    # ──────────── 1. BRANCH GWP BAR ────────────
    output$plot_branch_gwp_bar <- renderPlotly({
      df_cn <- get_data() %>%
        filter(TEN_CN != "Chưa xác định") %>%
        group_by(TEN_CN) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        top_n(15, GWP)

      p <- ggplot(df_cn, aes(x = reorder(TEN_CN, GWP), y = GWP)) +
        geom_col(fill = COLORS$steel, width = SIZE$bar_width) +
        coord_flip() +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0, 0.08))) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "GWP (VND)")

      ggplotly(p) %>% ly_quant(b = 25)
    })

    # ──────────── 2. BRANCH XHKH STACKED ────────────
    output$plot_branch_xhkh <- renderPlotly({
      df <- get_data()
      top3 <- df %>%
        filter(TEN_CN != "Chưa xác định") %>%
        group_by(TEN_CN) %>% summarise(v = sum(PHIBH)) %>%
        top_n(3, v) %>% pull(TEN_CN)

      df_xh <- df %>% filter(TEN_CN %in% top3) %>%
        group_by(TEN_CN, XHKH) %>%
        summarise(N = n_distinct(MA_KH), .groups = "drop")

      p <- ggplot(df_xh, aes(x = TEN_CN, y = N, fill = XHKH)) +
        geom_col(position = "fill", width = 0.6) +
        scale_fill_manual(values = PALETTE_XHKH) +
        scale_y_continuous(labels = scales::percent_format(),
                           expand = expansion(mult = c(0, 0.02))) +
        theme_quant(grid = "y") +
        theme(axis.text.x = element_text(face = "bold"),
              legend.title = element_blank()) +
        labs(x = NULL, y = "Tỷ lệ")

      ggplotly(p) %>% ly_quant(t = 40)
    })

    # ──────────── 3. BRANCH TREND ────────────
    output$plot_branch_trend_multi <- renderPlotly({
      df <- get_data()
      top5 <- df %>%
        filter(TEN_CN != "Chưa xác định",
               !grepl("Hội sở|Hoi so", TEN_CN, ignore.case = TRUE)) %>%
        group_by(TEN_CN) %>% summarise(v = sum(PHIBH)) %>%
        top_n(5, v) %>% pull(TEN_CN)

      df_trend <- df %>% filter(TEN_CN %in% top5) %>%
        group_by(THANG, TEN_CN) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")

      p <- ggplot(df_trend, aes(x = THANG, y = GWP, color = TEN_CN)) +
        geom_line(linewidth = SIZE$line) +
        scale_color_manual(values = PALETTE_MAIN) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0.02, 0.08))) +
        theme_quant(grid = "y") +
        theme(legend.title = element_blank()) +
        labs(x = NULL, y = "GWP (VND)")

      ggplotly(p) %>%
        style(line = list(shape = "spline", smoothing = 1.3)) %>%
        ly_quant(t = 40)
    })

    # ──────────── 4. PRODUCT × BRANCH HEATMAP ────────────
    output$plot_matrix_prod_branch <- renderPlotly({
      df <- get_data()
      top10 <- df %>%
        filter(TEN_CN != "Chưa xác định") %>%
        group_by(TEN_CN) %>% summarise(v = sum(PHIBH)) %>%
        top_n(10, v) %>% pull(TEN_CN)

      df_mat <- df %>%
        filter(TEN_CN %in% top10, NHOMSANPHAM != "Chưa xác định") %>%
        group_by(TEN_CN, NHOMSANPHAM) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")

      p <- ggplot(df_mat, aes(x = TEN_CN, y = NHOMSANPHAM, fill = GWP)) +
        geom_tile(color = "white", linewidth = 0.6) +
        scale_fill_gradient(low = COLORS$heat_low, high = COLORS$heat_high,
                            labels = label_trieu) +
        theme_quant(grid = "none") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
              axis.text.y = element_text(face = "bold")) +
        labs(x = NULL, y = NULL)

      ggplotly(p) %>% ly_quant(b = 65)
    })

    # ──────────── 5. KPI COMBO ────────────
    output$plot_kpi_combo <- renderPlotly({
      df_act <- get_data(); df_kpi <- get_kpi()

      if (input$select_kpi_channel != "Tất cả") {
        kb_code <- df_act %>% filter(TENKENHBAN == input$select_kpi_channel) %>%
          pull(MA_KENHBAN) %>% unique() %>% first()
        df_act <- df_act %>% filter(TENKENHBAN == input$select_kpi_channel)
        df_kpi <- df_kpi %>% filter(MA_KENHBAN == kb_code)
      }

      act_sum <- df_act %>%
        mutate(YM = format(NGAY_KY_HD, "%Y%m")) %>%
        group_by(YM) %>%
        summarise(Actual = sum(PHIBH, na.rm = TRUE), .groups = "drop")
      kpi_sum <- df_kpi %>%
        group_by(YM = as.character(THANGNAM)) %>%
        summarise(Target = sum(DOANH_THU, na.rm = TRUE), .groups = "drop")

      df_combo <- full_join(act_sum, kpi_sum, by = "YM") %>%
        arrange(YM) %>%
        mutate(Label = paste0(substr(YM, 5, 6), "/", substr(YM, 1, 4)))

      plot_ly(df_combo, x = ~Label) %>%
        add_bars(y = ~Actual, name = "Thực tế",
                 marker = list(color = COLORS$steel)) %>%
        add_lines(y = ~Target, name = "Mục tiêu (KPI)",
                  line = list(color = COLORS$red, width = 3,
                              dash = "dash", shape = "spline")) %>%
        layout(barmode = "group", bargap = 0.3,
               xaxis = list(tickangle = -45)) %>%
        ly_quant(t = 40, b = 50)
    })

    output$vbox_kpi_pct <- renderValueBox({
      total_act    <- sum(get_data()$PHIBH, na.rm = TRUE)
      total_target <- sum(get_kpi()$DOANH_THU, na.rm = TRUE)
      pct <- ifelse(total_target > 0, round(total_act / total_target * 100, 1), 0)
      valueBox(paste0(pct, "%"), "Tiến độ KPI",
               icon = icon("check-double"),
               color = ifelse(pct >= 100, "green",
                              ifelse(pct >= 80, "yellow", "red")))
    })

    # ──────────── 6. STAFF HISTOGRAM ────────────
    output$plot_staff_density <- renderPlotly({
      df_staff <- get_data() %>%
        group_by(MA_NV) %>%
        summarise(Total_GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        filter(Total_GWP > 0)

      p <- ggplot(df_staff, aes(x = Total_GWP)) +
        geom_histogram(bins = 45, fill = COLORS$teal, alpha = 0.85,
                       color = "white", linewidth = 0.2) +
        scale_x_log10(labels = label_trieu) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
        theme_quant(grid = "y") +
        labs(x = "Tổng GWP/NV (VND) — Log", y = "Số NV")

      ggplotly(p) %>% ly_quant(b = 25)
    })

    # ──────────── 7. CHANNEL COMP ────────────
    output$plot_channel_comp <- renderPlotly({
      df_chan <- get_data() %>%
        filter(TENKENHBAN != "Chưa xác định") %>%
        group_by(TENKENHBAN) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")

      p <- ggplot(df_chan, aes(x = reorder(TENKENHBAN, GWP), y = GWP)) +
        geom_col(fill = COLORS$steel, width = SIZE$bar_width) +
        coord_flip() +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0, 0.08))) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "GWP (VND)")

      ggplotly(p) %>% ly_quant(b = 25)
    })

    # ──────────── 8. PRODUCTIVITY ────────────
    output$plot_productivity_cn <- renderPlotly({
      df_prod <- get_data() %>%
        filter(TEN_CN != "Chưa xác định",
               !grepl("Hội sở|Hoi so|TCT", TEN_CN, ignore.case = TRUE)) %>%
        group_by(TEN_CN, MA_NV) %>%
        summarise(GWP_NV = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        group_by(TEN_CN) %>%
        summarise(Avg_GWP_NV = mean(GWP_NV), N_NV = n(), .groups = "drop") %>%
        top_n(15, Avg_GWP_NV)

      p <- ggplot(df_prod, aes(x = reorder(TEN_CN, Avg_GWP_NV), y = Avg_GWP_NV)) +
        geom_col(fill = COLORS$gold, width = SIZE$bar_width) +
        coord_flip() +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0, 0.08))) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "GWP TB/NV (VND)")

      ggplotly(p) %>% ly_quant(b = 25)
    })

    # ──────────── 9. TOP STAFF TABLE ────────────
    output$table_top_staff <- renderDT({
      df <- get_data() %>%
        filter(!grepl("Hội sở|Hoi so|TCT", TEN_CN, ignore.case = TRUE))

      kenh_chinh <- df %>%
        group_by(MA_NV, TENKENHBAN) %>%
        summarise(gwp_kenh = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        group_by(MA_NV) %>%
        slice_max(gwp_kenh, n = 1, with_ties = FALSE) %>%
        select(MA_NV, Kenh_Chinh = TENKENHBAN)

      df_top <- df %>%
        group_by(MA_NV, TEN_CN) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE),
                  SL_Goi = n(),
                  So_Kenh = n_distinct(TENKENHBAN), .groups = "drop") %>%
        left_join(kenh_chinh, by = "MA_NV") %>%
        arrange(desc(GWP)) %>% head(15)

      dt_quant(df_top,
               colnames = c("Mã NV", "Chi nhánh", "Tổng GWP",
                            "Số gói", "Số kênh", "Kênh chính"),
               page_length = 15, dom = "t") %>%
        formatCurrency("GWP", currency = "", interval = 3, mark = ".", digits = 0) %>%
        formatRound("SL_Goi", digits = 0, interval = 3, mark = ".")
    })
  })
}