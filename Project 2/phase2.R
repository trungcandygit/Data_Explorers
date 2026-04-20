# =============================================================================
# PHASE 2 — KHAI PHÁ VĨ MÔ & XU HƯỚNG DOANH THU
# =============================================================================

phase2UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    page_header(
      "Phần 2 — Khai phá vĩ mô & xu hướng doanh thu",
      "GWP theo thời gian, cơ cấu sản phẩm và phân phối phí bảo hiểm."
    ),

    fluidRow(
      valueBoxOutput(ns("vbox_total_gwp"),   width = 3),
      valueBoxOutput(ns("vbox_avg_gwp"),     width = 3),
      valueBoxOutput(ns("vbox_new_pct"),     width = 3),
      valueBoxOutput(ns("vbox_renewal_pct"), width = 3)
    ),

    fluidRow(
      qbox("Xu hướng doanh thu GWP theo tháng",
           "Đường cong mượt — biến động tổng phí BH",
           plotlyOutput(ns("plot_gwp_trend"), height = "380px"), width = 12)
    ),

    fluidRow(
      qbox("Top 10 gói sản phẩm theo doanh thu", NULL,
           plotlyOutput(ns("plot_product"), height = "380px"), width = 6),
      qbox("Mật độ phân phối phí BH", "Histogram log-scale",
           plotlyOutput(ns("plot_density"), height = "380px"), width = 6)
    ),

    fluidRow(
      qbox("Doanh thu theo quý", NULL,
           plotlyOutput(ns("plot_qoq"), height = "380px"), width = 6),
      qbox("Cơ cấu HĐ Mới và Tái tục theo tháng",
           "Stacked area — so sánh hai loại hợp đồng",
           plotlyOutput(ns("plot_new_renewal"), height = "380px"), width = 6)
    ),

    fluidRow(
      qbox("Xu hướng doanh thu theo nhóm sản phẩm",
           "Top 5 nhóm SP theo GWP",
           plotlyOutput(ns("plot_product_trend"), height = "380px"), width = 6),
      qbox("Số lượng gói BH phát hành theo tháng",
           "Bar = số gói, line = trend",
           plotlyOutput(ns("plot_volume_trend"), height = "380px"), width = 6)
    )
  )
}

phase2Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {

    get_data <- reactive({ req(all_data()); all_data()$master })

    # ──────────── KPI BOXES ────────────
    output$vbox_total_gwp <- renderValueBox({
      valueBox(format_vnd(sum(get_data()$PHIBH, na.rm = TRUE)),
               "Tổng GWP", icon = icon("coins"), color = "blue")
    })
    output$vbox_avg_gwp <- renderValueBox({
      valueBox(format_vnd(mean(get_data()$PHIBH, na.rm = TRUE)),
               "GWP bình quân/gói", icon = icon("calculator"), color = "aqua")
    })
    output$vbox_new_pct <- renderValueBox({
      df <- get_data()
      pct <- round(sum(df$LOAI_HD == "Mới", na.rm = TRUE) / nrow(df) * 100, 1)
      valueBox(paste0(pct, "%"), "Hợp đồng mới",
               icon = icon("plus-circle"), color = "green")
    })
    output$vbox_renewal_pct <- renderValueBox({
      df <- get_data()
      pct <- round(sum(df$LOAI_HD == "Tái tục", na.rm = TRUE) / nrow(df) * 100, 1)
      valueBox(paste0(pct, "%"), "Tái tục",
               icon = icon("sync-alt"), color = "yellow")
    })

    # ──────────── 1. GWP TREND ────────────
    output$plot_gwp_trend <- renderPlotly({
      df_sum <- get_data() %>%
        group_by(THANG) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        arrange(THANG)

      p <- ggplot(df_sum, aes(x = THANG, y = GWP)) +
        geom_line(color = COLORS$steel, linewidth = SIZE$line) +
        geom_point(color = COLORS$navy, size = 1.5) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0.02, 0.08))) +
        theme_quant(grid = "y") +
        labs(x = NULL, y = "Doanh thu GWP")

      ggplotly(p, tooltip = c("x", "y")) %>%
        style(line = list(shape = "spline", smoothing = 1.3)) %>%
        ly_quant(t = 15)
    })

    # ──────────── 2. TOP 10 SP ────────────
    output$plot_product <- renderPlotly({
      df_prod <- get_data() %>%
        filter(!is.na(TENGOISANPHAM), TENGOISANPHAM != "Chưa xác định") %>%
        group_by(TENGOISANPHAM) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        top_n(10, GWP)

      p <- ggplot(df_prod, aes(x = reorder(TENGOISANPHAM, GWP), y = GWP)) +
        geom_col(fill = COLORS$steel, width = SIZE$bar_width) +
        coord_flip() +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0, 0.08))) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "GWP (VND)")

      ggplotly(p) %>% ly_quant(b = 20)
    })

    # ──────────── 3. DENSITY ────────────
    output$plot_density <- renderPlotly({
      df <- get_data() %>% filter(PHIBH > 0)

      p <- ggplot(df, aes(x = PHIBH)) +
        geom_density(fill = COLORS$teal, alpha = 0.55,
                     color = COLORS$navy, linewidth = 0.6) +
        scale_x_log10(labels = label_trieu) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
        theme_quant(grid = "y") +
        labs(x = "Phí BH (VND) — Log", y = "Mật độ")

      ggplotly(p) %>% ly_quant(b = 25)
    })

    # ──────────── 4. QUARTERLY ────────────
    output$plot_qoq <- renderPlotly({
      df_q <- get_data() %>%
        mutate(Quy = paste0(year(NGAY_KY_HD), "-Q", quarter(NGAY_KY_HD))) %>%
        group_by(Quy) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        arrange(Quy)

      p <- ggplot(df_q, aes(x = Quy, y = GWP)) +
        geom_col(fill = COLORS$steel, width = 0.55) +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0, 0.08))) +
        theme_quant(grid = "y") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = NULL, y = "GWP (VND)")

      ggplotly(p) %>% ly_quant(b = 30)
    })

    # ──────────── 5. NEW vs RENEWAL STACKED ────────────
    output$plot_new_renewal <- renderPlotly({
      df_nr <- get_data() %>%
        group_by(THANG, LOAI_HD) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")

      p <- ggplot(df_nr, aes(x = THANG, y = GWP, fill = LOAI_HD)) +
        geom_area(alpha = 0.7, position = "stack") +
        scale_fill_manual(values = c("Mới" = COLORS$new,
                                     "Tái tục" = COLORS$renewal)) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0, 0.05))) +
        theme_quant(grid = "y") +
        theme(legend.title = element_blank()) +
        labs(x = NULL, y = "GWP (VND)")

      ggplotly(p) %>% ly_quant(t = 40)
    })

    # ──────────── 6. PRODUCT TREND ────────────
    output$plot_product_trend <- renderPlotly({
      df <- get_data()
      top5 <- df %>%
        filter(NHOMSANPHAM != "Chưa xác định") %>%
        group_by(NHOMSANPHAM) %>% summarise(v = sum(PHIBH)) %>%
        top_n(5, v) %>% pull(NHOMSANPHAM)

      df_trend <- df %>%
        filter(NHOMSANPHAM %in% top5) %>%
        group_by(THANG, NHOMSANPHAM) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")

      p <- ggplot(df_trend, aes(x = THANG, y = GWP, color = NHOMSANPHAM)) +
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

    # ──────────── 7. VOLUME TREND ────────────
    output$plot_volume_trend <- renderPlotly({
      df_vol <- get_data() %>%
        group_by(THANG) %>%
        summarise(N = n(), .groups = "drop") %>%
        arrange(THANG)

      plot_ly(df_vol, x = ~THANG) %>%
        add_bars(y = ~N, name = "Số lượng gói",
                 marker = list(color = COLORS$steel, opacity = 0.55),
                 hovertemplate = "%{x|%b %Y}: %{y:,.0f} gói<extra></extra>") %>%
        add_lines(y = ~N, name = "Trend",
                  line = list(color = COLORS$navy, width = 2,
                              shape = "spline", smoothing = 1.3),
                  hoverinfo = "skip") %>%
        layout(xaxis = list(tickformat = "%b %Y", dtick = "M3"),
               bargap = 0.3) %>%
        ly_quant(t = 40)
    })
  })
}