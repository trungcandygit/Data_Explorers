# =============================================================================
# PHASE 3 — PHÂN TÍCH CHUYÊN SÂU SẢN PHẨM & KÊNH BÁN
# =============================================================================

phase3UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    page_header(
      "Phần 3 — Phân tích chuyên sâu sản phẩm & kênh bán",
      "Cấu trúc doanh thu, dòng SP chủ lực và hiệu quả kênh phân phối."
    ),

    fluidRow(
      valueBoxOutput(ns("vbox_avg_premium"), width = 4),
      valueBoxOutput(ns("vbox_top_product"), width = 4),
      valueBoxOutput(ns("vbox_top_channel"), width = 4)
    ),

    fluidRow(
      qbox("So sánh tỷ trọng 2 mảng theo chi nhánh",
           "Biểu đồ đối xứng — Bắt buộc (trái) vs Tự nguyện (phải)",
           plotlyOutput(ns("plot_grouped_bar_matrix"), height = "480px"), width = 8),
      qbox("Cơ cấu doanh thu theo kênh bán",
           "Tỷ trọng GWP của từng kênh chính",
           plotlyOutput(ns("plot_channel_pie"), height = "440px"), width = 4)
    ),

    fluidRow(
      qbox("Phân phối phí BH theo nhóm sản phẩm",
           "Violin + Box — ngưỡng phí phổ biến và độ phân tán",
           plotlyOutput(ns("plot_premium_dist"), height = "390px"), width = 6),
      qbox("Hiệu suất Top 15 kênh bán chi tiết", NULL,
           plotlyOutput(ns("plot_detail_channel_bar"), height = "390px"), width = 6)
    ),

    fluidRow(
      qbox("Top 10 gói SP — Doanh thu & % tích luỹ",
           "Bar = GWP, nhãn = % và Σ% (Pareto-style)",
           plotlyOutput(ns("plot_top10_cumulative"), height = "390px"), width = 6),
      qbox("Cơ cấu nhóm SP theo kênh bán (Stacked %)",
           "Mỗi cột = 100% — kênh nào tập trung nhóm SP nào",
           plotlyOutput(ns("plot_channel_product_stack"), height = "390px"), width = 6)
    ),

    fluidRow(
      qbox("Phí bình quân/gói theo nhóm SP (APPP)",
           "Nhóm nào giá trị TB cao? Nhóm nào volume lớn nhưng phí thấp?",
           plotlyOutput(ns("plot_appp_by_group"), height = "390px"), width = 6),
      qbox("Số lượng gói × Phí TB × Tổng GWP",
           "Bubble: x = số gói, y = phí TB, size = GWP",
           plotlyOutput(ns("plot_bubble_product"), height = "390px"), width = 6)
    ),

    fluidRow(
      qbox("Bảng tổng hợp hiệu suất đa chiều", NULL,
           DTOutput(ns("table_deep_stats")), width = 12)
    )
  )
}

phase3Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {

    get_data <- reactive({ req(all_data()); all_data()$master })

    output$vbox_avg_premium <- renderValueBox({
      valueBox(format_vnd(mean(get_data()$PHIBH, na.rm = TRUE)),
               "Phí bình quân/gói (APPP)", icon = icon("dollar-sign"), color = "blue")
    })
    output$vbox_top_product <- renderValueBox({
      top <- get_data() %>%
        filter(TENGOISANPHAM != "Chưa xác định") %>%
        group_by(TENGOISANPHAM) %>% summarise(v = sum(PHIBH)) %>%
        arrange(desc(v)) %>% slice(1)
      valueBox(top$TENGOISANPHAM, "SP doanh thu cao nhất",
               icon = icon("star"), color = "aqua")
    })
    output$vbox_top_channel <- renderValueBox({
      top <- get_data() %>%
        filter(TENKENHBAN != "Chưa xác định") %>%
        group_by(TENKENHBAN) %>% summarise(v = sum(PHIBH)) %>%
        arrange(desc(v)) %>% slice(1)
      valueBox(top$TENKENHBAN, "Kênh dẫn đầu",
               icon = icon("trophy"), color = "green")
    })

    # ──────────── 1. SYMMETRIC GROUPED BAR ────────────
    output$plot_grouped_bar_matrix <- renderPlotly({
      df_matrix <- get_data() %>%
        filter(TEN_CN != "Chưa xác định", TEN_CN != "Hội sở",
               NHOMSANPHAM != "Chưa xác định") %>%
        group_by(TEN_CN, NHOMSANPHAM) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(TEN_CN)) %>%
        mutate(GWP_Ty = GWP / 1e9)

      df_bb <- df_matrix %>% filter(NHOMSANPHAM == "Bảo hiểm bắt buộc")
      df_tn <- df_matrix %>% filter(NHOMSANPHAM == "Bảo hiểm tự nguyện")

      p1 <- plot_ly(df_bb, x = ~GWP_Ty, y = ~TEN_CN, type = "bar", orientation = "h",
                    name = "Bảo hiểm bắt buộc",
                    marker = list(color = COLORS$steel),
                    hovertemplate = "%{x:,.1f} tỷ<extra></extra>") %>%
        layout(xaxis = list(autorange = "reversed", title = "",
                            gridcolor = COLORS$grid, zeroline = FALSE,
                            ticksuffix = " tỷ"),
               yaxis = list(side = "right", title = "",
                            tickfont = list(family = FONT_FAMILY_PLOTLY,
                                            size = SIZE$axis_text,
                                            color = COLORS$text_main)))

      p2 <- plot_ly(df_tn, x = ~GWP_Ty, y = ~TEN_CN, type = "bar", orientation = "h",
                    name = "Bảo hiểm tự nguyện",
                    marker = list(color = COLORS$teal),
                    hovertemplate = "%{x:,.1f} tỷ<extra></extra>") %>%
        layout(xaxis = list(title = "", gridcolor = COLORS$grid,
                            zeroline = FALSE, ticksuffix = " tỷ"),
               yaxis = list(showticklabels = FALSE))

      subplot(p1, p2, shareY = FALSE, titleX = FALSE, margin = 0.08) %>%
        layout(font = PLOTLY_FONT,
               margin = list(l = 10, r = 10, t = 40, b = 20),
               showlegend = TRUE,
               legend = list(orientation = "h", x = 0.5, xanchor = "center",
                             y = 1.15, yanchor = "bottom",
                             font = list(size = SIZE$legend,
                                         family = FONT_FAMILY_PLOTLY)),
               hovermode = "y unified",
               paper_bgcolor = COLORS$bg, plot_bgcolor = COLORS$bg) %>%
        config(displayModeBar = FALSE)
    })

    # ──────────── 2. CHANNEL PIE ────────────
    output$plot_channel_pie <- renderPlotly({
      df_chan <- get_data() %>%
        filter(TENKENHBAN != "Chưa xác định") %>%
        group_by(TENKENHBAN) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(GWP))

      plot_ly(df_chan, labels = ~TENKENHBAN, values = ~GWP, type = "pie",
              textinfo = "percent", hole = 0.5,
              marker = list(colors = PALETTE_MAIN[1:nrow(df_chan)],
                            line = list(color = "#FFFFFF", width = 2)),
              textfont = list(family = FONT_FAMILY_PLOTLY, size = 12,
                              color = "#FFFFFF")) %>%
        layout(font = PLOTLY_FONT,
               showlegend = TRUE,
               legend = list(orientation = "h", x = 0.5, xanchor = "center",
                             y = -0.05, yanchor = "top",
                             font = list(size = SIZE$legend,
                                         family = FONT_FAMILY_PLOTLY)),
               margin = list(l = 10, r = 10, t = 20, b = 40),
               paper_bgcolor = COLORS$bg) %>%
        config(displayModeBar = FALSE)
    })

    # ──────────── 3. VIOLIN + BOX ────────────
    output$plot_premium_dist <- renderPlotly({
      df <- get_data() %>% filter(PHIBH > 0, NHOMSANPHAM != "Chưa xác định")

      p <- ggplot(df, aes(x = NHOMSANPHAM, y = PHIBH, fill = NHOMSANPHAM)) +
        geom_violin(alpha = 0.6, scale = "width", color = NA) +
        geom_boxplot(width = 0.13, fill = "white", alpha = 0.85,
                     color = COLORS$navy, outlier.shape = NA, linewidth = 0.4) +
        scale_y_log10(labels = label_trieu) +
        scale_fill_manual(values = PALETTE_MAIN) +
        theme_quant(grid = "y", legend_pos = "none") +
        theme(axis.text.x = element_text(angle = 22, hjust = 1)) +
        labs(x = NULL, y = "Phí BH (VNĐ) — Log")

      ggplotly(p) %>% ly_quant(l = 60, b = 55)
    })

    # ──────────── 4. TOP 15 DETAIL CHANNEL ────────────
    output$plot_detail_channel_bar <- renderPlotly({
      df_det <- get_data() %>%
        filter(TENKENHBANCHITIET != "Chưa xác định") %>%
        group_by(TENKENHBANCHITIET) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        top_n(15, GWP)

      p <- ggplot(df_det, aes(x = reorder(TENKENHBANCHITIET, GWP), y = GWP)) +
        geom_col(fill = COLORS$steel, width = SIZE$bar_width) +
        coord_flip() +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0, 0.08))) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "GWP (VNĐ)")

      ggplotly(p) %>% ly_quant(b = 25)
    })

    # ──────────── 5. TOP 10 CUMULATIVE ────────────
    output$plot_top10_cumulative <- renderPlotly({
      total_gwp <- sum(get_data()$PHIBH, na.rm = TRUE)
      df_top <- get_data() %>%
        filter(TENGOISANPHAM != "Chưa xác định", !is.na(TENGOISANPHAM)) %>%
        group_by(TENGOISANPHAM) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(GWP)) %>% head(10) %>%
        mutate(Cum_Pct = round(cumsum(GWP) / total_gwp * 100, 1),
               Pct     = round(GWP / total_gwp * 100, 1))

      p <- ggplot(df_top, aes(x = reorder(TENGOISANPHAM, GWP), y = GWP)) +
        geom_col(fill = COLORS$steel, width = SIZE$bar_width) +
        geom_text(aes(label = paste0(Pct, "% | Σ", Cum_Pct, "%")),
                  hjust = -0.05, size = SIZE$label - 0.2,
                  color = COLORS$navy, fontface = "bold") +
        coord_flip() +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0, 0.35))) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "GWP (VNĐ)")

      ggplotly(p, tooltip = "y") %>% ly_quant(b = 25)
    })

    # ──────────── 6. CHANNEL × PRODUCT STACKED ────────────
    output$plot_channel_product_stack <- renderPlotly({
      df_stack <- get_data() %>%
        filter(TENKENHBAN != "Chưa xác định", NHOMSANPHAM != "Chưa xác định") %>%
        group_by(TENKENHBAN, NHOMSANPHAM) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")

      p <- ggplot(df_stack, aes(x = TENKENHBAN, y = GWP, fill = NHOMSANPHAM)) +
        geom_col(position = "fill", width = SIZE$bar_width) +
        scale_y_continuous(labels = scales::percent_format(),
                           expand = expansion(mult = c(0, 0.02))) +
        scale_fill_manual(values = PALETTE_MAIN) +
        theme_quant(grid = "y") +
        theme(axis.text.x = element_text(angle = 30, hjust = 1),
              legend.title = element_blank()) +
        labs(x = NULL, y = "Tỷ trọng (%)")

      ggplotly(p) %>% ly_quant(b = 55, t = 40)
    })

    # ──────────── 7. APPP BY GROUP ────────────
    output$plot_appp_by_group <- renderPlotly({
      df_appp <- get_data() %>%
        filter(NHOMSANPHAM != "Chưa xác định") %>%
        group_by(NHOMSANPHAM) %>%
        summarise(APPP = mean(PHIBH, na.rm = TRUE),
                  SL = n(), .groups = "drop")

      p <- ggplot(df_appp, aes(x = reorder(NHOMSANPHAM, APPP),
                               y = APPP, fill = NHOMSANPHAM)) +
        geom_col(width = SIZE$bar_width) +
        geom_text(aes(label = label_trieu(APPP)),
                  hjust = -0.05, size = SIZE$label,
                  color = COLORS$navy, fontface = "bold") +
        coord_flip() +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0, 0.25))) +
        scale_fill_manual(values = PALETTE_MAIN) +
        theme_quant(grid = "x", legend_pos = "none") +
        labs(x = NULL, y = "Phí bình quân/gói (VNĐ)")

      ggplotly(p, tooltip = "y") %>% ly_quant(b = 25)
    })

    # ──────────── 8. BUBBLE ────────────
    output$plot_bubble_product <- renderPlotly({
      df_bub <- get_data() %>%
        filter(NHOMSANPHAM != "Chưa xác định") %>%
        group_by(NHOMSANPHAM) %>%
        summarise(Volume = n(),
                  Revenue = sum(PHIBH, na.rm = TRUE),
                  APPP = Revenue / Volume,
                  .groups = "drop")

      p <- ggplot(df_bub, aes(x = Volume, y = APPP, size = Revenue,
                              color = NHOMSANPHAM, text = NHOMSANPHAM)) +
        geom_point(alpha = 0.8) +
        scale_y_continuous(labels = label_trieu) +
        scale_x_continuous(labels = label_int) +
        scale_size_continuous(range = c(5, 22), guide = "none") +
        scale_color_manual(values = PALETTE_MAIN) +
        theme_quant(grid = "both") +
        theme(legend.title = element_blank()) +
        labs(x = "Số lượng gói BH", y = "Phí bình quân (VNĐ)")

      ggplotly(p, tooltip = c("text", "x", "y")) %>% ly_quant(t = 40)
    })

    # ──────────── 9. DEEP STATS TABLE ────────────
    output$table_deep_stats <- renderDT({
      df_stat <- get_data() %>%
        filter(TEN_CN != "Chưa xác định") %>%
        group_by(TEN_CN, NHOMSANPHAM, TENKENHBAN) %>%
        summarise(SL_Goi = n(),
                  Tong_GWP = sum(PHIBH, na.rm = TRUE),
                  APPP = round(Tong_GWP / SL_Goi, 0),
                  .groups = "drop") %>%
        arrange(desc(Tong_GWP))

      dt_quant(df_stat,
               colnames = c("Chi nhánh", "Nhóm SP", "Kênh bán",
                            "Số gói", "Tổng GWP", "Phí BQ/gói"),
               filter = "top") %>%
        formatCurrency(c("Tong_GWP", "APPP"), currency = "",
                       interval = 3, mark = ".", digits = 0) %>%
        formatRound("SL_Goi", digits = 0, interval = 3, mark = ".")
    })
  })
}