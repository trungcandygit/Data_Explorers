# =============================================================================
# PHASE 7 — CHIẾN LƯỢC KINH DOANH THỰC CHIẾN
# =============================================================================

phase7UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    page_header(
      "Phần 7 — Chiến lược kinh doanh thực chiến",
      "BCG Matrix, cơ hội Up-sell / Cross-sell và tiềm năng tăng trưởng."
    ),

    fluidRow(
      qbox("Ma trận BCG: nhóm sản phẩm",
           "Bubble = GWP. Đường đứt = trung bình. Star / Cash Cow / ? / Dog",
           plotlyOutput(ns("plot_bcg"), height = "480px"), width = 12)
    ),

    fluidRow(
      qbox("Cross-sell: nhóm SP × xếp hạng KH",
           "Heatmap — vùng nhạt là cơ hội cross-sell",
           plotlyOutput(ns("plot_cross_sell"), height = "390px"), width = 6),
      qbox("Tỷ lệ tái tục theo nhóm SP",
           "Nhóm nào giữ chân khách hàng tốt nhất?",
           plotlyOutput(ns("plot_renewal_rate"), height = "390px"), width = 6)
    ),

    fluidRow(
      qbox("Phí BH trung bình theo xếp hạng KH",
           "Hạng cao chi trả phí bình quân cao hơn bao nhiêu?",
           plotlyOutput(ns("plot_avg_premium_xhkh"), height = "390px"), width = 6),
      qbox("Số gói TB / KH theo xếp hạng",
           "Hạng cao có cross-sell nhiều hơn không?",
           plotlyOutput(ns("plot_avg_packages_xhkh"), height = "390px"), width = 6)
    ),

    fluidRow(
      qbox("Tỷ lệ HĐ Mới vs Tái tục theo nhóm SP",
           "Nhóm nào chủ yếu là HĐ mới? Nhóm nào đã có nền tái tục?",
           plotlyOutput(ns("plot_new_vs_renew_group"), height = "390px"), width = 6),
      qbox("GWP theo nhóm SP — Mới vs Tái tục (Stacked)",
           "Đóng góp GWP từ HĐ mới và tái tục tại mỗi nhóm SP",
           plotlyOutput(ns("plot_gwp_new_renew_stack"), height = "390px"), width = 6)
    ),

    fluidRow(
      qbox("Cơ hội Up-sell — KH phí thấp nhưng giá trị tài sản cao",
           "Xe > 500 triệu, phí BH < 3 triệu — dấu hiệu thiếu bảo vệ",
           DTOutput(ns("table_upsell_leads")), width = 12)
    )
  )
}

phase7Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {

    get_data <- reactive({ req(all_data()); all_data()$master })

    # ──────────── 1. BCG MATRIX ────────────
    output$plot_bcg <- renderPlotly({
      df_strat <- get_data() %>%
        filter(NHOMSANPHAM != "Chưa xác định", !is.na(NHOMSANPHAM)) %>%
        group_by(NHOMSANPHAM) %>%
        summarise(Volume = n(),
                  Revenue = sum(PHIBH, na.rm = TRUE),
                  Avg_Premium = Revenue / Volume,
                  .groups = "drop")

      avg_vol  <- mean(df_strat$Volume)
      avg_prem <- mean(df_strat$Avg_Premium)

      df_strat <- df_strat %>%
        mutate(Quadrant = case_when(
          Volume >= avg_vol & Avg_Premium >= avg_prem ~ "Ngôi sao (Star)",
          Volume >= avg_vol & Avg_Premium <  avg_prem ~ "Bò sữa (Cash Cow)",
          Volume <  avg_vol & Avg_Premium >= avg_prem ~ "Dấu hỏi (?)",
          TRUE                                         ~ "Chó (Dog)"))

      plot_ly(df_strat, x = ~Volume, y = ~Avg_Premium, size = ~Revenue,
              color = ~Quadrant,
              text = ~paste0(NHOMSANPHAM,
                             "\nVolume: ", label_int(Volume),
                             "\nPhí TB: ", label_trieu(Avg_Premium),
                             "\nGWP: ",   label_trieu(Revenue)),
              hoverinfo = "text", type = "scatter", mode = "markers",
              sizes = c(30, 120),
              marker = list(opacity = 0.85,
                            line = list(color = COLORS$navy, width = 1.5)),
              colors = c("Ngôi sao (Star)"   = COLORS$teal,
                         "Bò sữa (Cash Cow)" = COLORS$steel,
                         "Dấu hỏi (?)"       = COLORS$gold,
                         "Chó (Dog)"         = COLORS$red)) %>%
        add_annotations(x = df_strat$Volume, y = df_strat$Avg_Premium,
                        text = df_strat$NHOMSANPHAM, showarrow = FALSE,
                        font = list(size = 9, color = COLORS$navy,
                                    family = FONT_FAMILY_PLOTLY),
                        yshift = 18) %>%
        layout(
          shapes = list(
            list(type = "line", x0 = avg_vol, x1 = avg_vol,
                 y0 = 0, y1 = max(df_strat$Avg_Premium) * 1.15,
                 line = list(color = COLORS$navy, width = 1, dash = "dash")),
            list(type = "line", x0 = 0, x1 = max(df_strat$Volume) * 1.1,
                 y0 = avg_prem, y1 = avg_prem,
                 line = list(color = COLORS$navy, width = 1, dash = "dash"))
          ),
          annotations = list(
            list(x = max(df_strat$Volume) * 0.95,
                 y = max(df_strat$Avg_Premium) * 1.08, text = "★ Star",
                 showarrow = FALSE,
                 font = list(size = 13, color = COLORS$teal,
                             family = FONT_FAMILY_PLOTLY))
          ),
          xaxis = list(title = "Volume (số gói BH)",
                       gridcolor = COLORS$grid, tickformat = ",.0f"),
          yaxis = list(title = "Phí bình quân (VND)",
                       gridcolor = COLORS$grid, tickformat = ",.0f")
        ) %>%
        ly_quant(t = 20, b = 50)
    })

    # ──────────── 2. CROSS-SELL HEATMAP ────────────
    output$plot_cross_sell <- renderPlotly({
      df_cs <- get_data() %>% filter(NHOMSANPHAM != "Chưa xác định") %>%
        group_by(NHOMSANPHAM, XHKH) %>%
        summarise(N = n(), .groups = "drop")

      p <- ggplot(df_cs, aes(x = XHKH, y = NHOMSANPHAM, fill = N)) +
        geom_tile(color = "white", linewidth = 0.6) +
        scale_fill_gradient(low = COLORS$heat_low, high = COLORS$heat_high) +
        theme_quant(grid = "none") +
        theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold"),
              axis.text.y = element_text(face = "bold")) +
        labs(x = NULL, y = NULL)

      ggplotly(p, tooltip = c("x", "y", "fill")) %>% ly_quant(b = 45)
    })

    # ──────────── 3. RENEWAL RATE ────────────
    output$plot_renewal_rate <- renderPlotly({
      df_rr <- get_data() %>% filter(NHOMSANPHAM != "Chưa xác định") %>%
        group_by(NHOMSANPHAM) %>%
        summarise(Total = n(),
                  Pct = round(sum(LOAI_HD == "Tái tục", na.rm = TRUE) / Total * 100, 1),
                  .groups = "drop")

      p <- ggplot(df_rr, aes(x = reorder(NHOMSANPHAM, Pct), y = Pct)) +
        geom_col(fill = COLORS$teal, width = SIZE$bar_width) +
        geom_text(aes(label = paste0(Pct, "%")),
                  hjust = -0.08, size = SIZE$label,
                  color = COLORS$navy, fontface = "bold") +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "Tỷ lệ tái tục (%)")

      ggplotly(p, tooltip = "y") %>% ly_quant(b = 25)
    })

    # ──────────── 4. AVG PREMIUM XHKH ────────────
    output$plot_avg_premium_xhkh <- renderPlotly({
      df_avg <- get_data() %>%
        group_by(XHKH) %>%
        summarise(Avg_Phi = mean(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        mutate(XHKH = factor(XHKH,
                             levels = c("Đồng", "Bạc", "Vàng",
                                        "Bạch kim", "Kim cương")))

      p <- ggplot(df_avg, aes(x = XHKH, y = Avg_Phi, fill = XHKH)) +
        geom_col(width = 0.6) +
        geom_text(aes(label = label_trieu(Avg_Phi)),
                  vjust = -0.5, size = SIZE$label,
                  color = COLORS$navy, fontface = "bold") +
        scale_fill_manual(values = PALETTE_XHKH) +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0, 0.18))) +
        theme_quant(grid = "y", legend_pos = "none") +
        theme(axis.text.x = element_text(face = "bold", size = 10)) +
        labs(x = NULL, y = "Phí BH trung bình (VND)")

      ggplotly(p, tooltip = "y") %>% ly_quant(b = 25)
    })

    # ──────────── 5. AVG PACKAGES XHKH ────────────
    output$plot_avg_packages_xhkh <- renderPlotly({
      df_pkg <- get_data() %>%
        group_by(XHKH, MA_KH) %>%
        summarise(N_goi = n(), .groups = "drop") %>%
        group_by(XHKH) %>%
        summarise(Avg_goi = round(mean(N_goi), 2), .groups = "drop") %>%
        mutate(XHKH = factor(XHKH,
                             levels = c("Đồng", "Bạc", "Vàng",
                                        "Bạch kim", "Kim cương")))

      p <- ggplot(df_pkg, aes(x = XHKH, y = Avg_goi, fill = XHKH)) +
        geom_col(width = 0.6) +
        geom_text(aes(label = Avg_goi),
                  vjust = -0.5, size = SIZE$label + 0.2,
                  color = COLORS$navy, fontface = "bold") +
        scale_fill_manual(values = PALETTE_XHKH) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
        theme_quant(grid = "y", legend_pos = "none") +
        theme(axis.text.x = element_text(face = "bold", size = 10)) +
        labs(x = NULL, y = "Số gói TB / khách hàng")

      ggplotly(p, tooltip = "y") %>% ly_quant(b = 25)
    })

    # ──────────── 6. NEW vs RENEW STACKED % ────────────
    output$plot_new_vs_renew_group <- renderPlotly({
      df_nr <- get_data() %>% filter(NHOMSANPHAM != "Chưa xác định") %>%
        group_by(NHOMSANPHAM, LOAI_HD) %>%
        summarise(N = n(), .groups = "drop")

      p <- ggplot(df_nr, aes(x = NHOMSANPHAM, y = N, fill = LOAI_HD)) +
        geom_col(position = "fill", width = SIZE$bar_width) +
        scale_y_continuous(labels = scales::percent_format(),
                           expand = expansion(mult = c(0, 0.02))) +
        scale_fill_manual(values = c("Mới" = COLORS$new, "Tái tục" = COLORS$renewal)) +
        theme_quant(grid = "y") +
        theme(axis.text.x = element_text(angle = 25, hjust = 1),
              legend.title = element_blank()) +
        labs(x = NULL, y = "Tỷ lệ (%)")

      ggplotly(p) %>% ly_quant(b = 55, t = 40)
    })

    # ──────────── 7. GWP NEW/RENEW STACKED ────────────
    output$plot_gwp_new_renew_stack <- renderPlotly({
      df_gwp <- get_data() %>% filter(NHOMSANPHAM != "Chưa xác định") %>%
        group_by(NHOMSANPHAM, LOAI_HD) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")

      p <- ggplot(df_gwp, aes(x = reorder(NHOMSANPHAM, -GWP),
                              y = GWP, fill = LOAI_HD)) +
        geom_col(width = SIZE$bar_width) +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0, 0.05))) +
        scale_fill_manual(values = c("Mới" = COLORS$new, "Tái tục" = COLORS$renewal)) +
        theme_quant(grid = "y") +
        theme(axis.text.x = element_text(angle = 25, hjust = 1),
              legend.title = element_blank()) +
        labs(x = NULL, y = "GWP (VND)")

      ggplotly(p) %>% ly_quant(b = 55, t = 40)
    })

    # ──────────── 8. UPSELL TABLE ────────────
    output$table_upsell_leads <- renderDT({
      upsell <- get_data() %>%
        filter(!is.na(GIA_TIEN_XE), GIA_TIEN_XE > 500000000, PHIBH < 3000000) %>%
        select(MA_KH, MA_HD, TEN_DONG_XE, TEN_HANG_XE, GIA_TIEN_XE,
               PHIBH, TENGOISANPHAM, TEN_CN) %>%
        arrange(desc(GIA_TIEN_XE)) %>% head(100)

      dt_quant(upsell,
               colnames = c("Mã KH", "Mã HĐ", "Dòng xe", "Hãng",
                            "Giá trị xe", "Phí BH", "Gói đang dùng", "Chi nhánh"),
               page_length = 8) %>%
        formatCurrency(c("GIA_TIEN_XE", "PHIBH"), currency = "",
                       interval = 3, mark = ".", digits = 0)
    })
  })
}