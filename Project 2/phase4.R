# =============================================================================
# PHASE 4 — PHÂN CỤM KHÁCH HÀNG RFM & K-MEANS
# =============================================================================

phase4UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    page_header(
      "Phần 4 — Phân cụm khách hàng RFM & K-Means",
      "Phân cụm dựa trên Recency–Frequency–Monetary, log-transform + Z-score, nstart = 25."
    ),

    fluidRow(
      qbox("Cấu hình mô hình", NULL, width = 12,
        fluidRow(
          column(3, numericInput(ns("clusters"), "Số cụm (K):",
                                  value = 4, min = 2, max = 6)),
          column(3, selectInput(ns("seed_val"), "Random Seed:",
                                  choices = c(42, 123, 456, 789), selected = 42)),
          column(3, br(),
                 actionButton(ns("run_kmeans"), "Chạy K-Means",
                              icon = icon("play-circle"),
                              class = "btn-success btn-lg")),
          column(3, tags$p("Log-transform + Z-score trước khi phân cụm. nstart = 25.",
                            style = sprintf(
                              "color:%s; font-size:12px; margin-top:14px; font-family:%s;",
                              COLORS$text_light, FONT_FAMILY)))
        )
      )
    ),

    uiOutput(ns("results_display"))
  )
}

phase4Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    clustering_results <- reactiveVal(NULL)
    elbow_data <- reactiveVal(NULL)

    observeEvent(input$run_kmeans, {
      withProgress(message = "Đang tính RFM & K-Means...", value = 0, {

        req(all_data()); df <- all_data()$master
        incProgress(0.2, detail = "Tính chỉ số RFM...")

        max_date <- max(df$NGAY_KY_HD, na.rm = TRUE)
        rfm_data <- df %>%
          group_by(MA_KH) %>%
          summarise(
            Recency   = as.numeric(max_date - max(NGAY_KY_HD, na.rm = TRUE)),
            Frequency = n(),
            Monetary  = sum(PHIBH, na.rm = TRUE),
            XHKH      = first(XHKH),
            TEN_CN    = first(TEN_CN),
            .groups = "drop"
          ) %>% filter(Monetary > 0, Frequency > 0)

        incProgress(0.4, detail = "Log-transform & chuẩn hoá...")
        rfm_scaled <- rfm_data %>%
          mutate(R_log = log1p(Recency),
                 F_log = log1p(Frequency),
                 M_log = log1p(Monetary))
        scale_matrix <- scale(rfm_scaled[, c("R_log", "F_log", "M_log")])

        incProgress(0.5, detail = "Tính Elbow (WSS)...")
        wss <- sapply(2:8, function(k) {
          set.seed(as.numeric(input$seed_val))
          kmeans(scale_matrix, centers = k, nstart = 25)$tot.withinss
        })
        elbow_data(data.frame(K = 2:8, WSS = wss))

        incProgress(0.7, detail = "Phân cụm K-Means...")
        set.seed(as.numeric(input$seed_val))
        km <- kmeans(scale_matrix, centers = input$clusters, nstart = 25)
        rfm_data$Cluster <- as.factor(km$cluster)

        clust_summary <- rfm_data %>%
          group_by(Cluster) %>%
          summarise(avg_m = mean(Monetary),
                    avg_r = mean(Recency),
                    avg_f = mean(Frequency)) %>%
          arrange(desc(avg_m))

        label_map <- setNames(
          c("VIP / Doanh nghiệp", "KH Trung thành", "KH Tiềm năng",
            "KH Rời bỏ", "Chi tiêu thấp", "Nhóm Mới")[1:input$clusters],
          clust_summary$Cluster
        )

        rfm_data$Cluster_Label <- label_map[as.character(rfm_data$Cluster)]
        rfm_data$R_scaled <- scale_matrix[, 1]
        rfm_data$F_scaled <- scale_matrix[, 2]
        rfm_data$M_scaled <- scale_matrix[, 3]
        attr(rfm_data, "km") <- km

        clustering_results(rfm_data)
        incProgress(1, detail = "Hoàn tất!")
      })
    })

    output$results_display <- renderUI({
      req(clustering_results())
      tagList(
        fluidRow(
          qbox("Phương pháp Elbow — chọn K tối ưu",
               "WSS giảm dần theo K — điểm 'gập khuỷu tay'",
               plotlyOutput(ns("plot_elbow"), height = "370px"), width = 4),
          qbox("Frequency × Monetary (theo cụm)",
               "F cao = mua nhiều gói, M cao = giá trị cao",
               plotlyOutput(ns("plot_kmeans_scatter"), height = "370px"), width = 8)
        ),
        fluidRow(
          qbox("Recency × Monetary (theo cụm)",
               "R cao = lâu không mua, M cao = giá trị cao",
               plotlyOutput(ns("plot_kmeans_rm"), height = "370px"), width = 6),
          qbox("Radar R-F-M trung bình theo cụm",
               "Đặc trưng hành vi tổng hợp",
               plotlyOutput(ns("plot_radar"), height = "370px"), width = 6)
        ),
        fluidRow(
          qbox("Phân bố khách hàng theo cụm", NULL,
               plotlyOutput(ns("plot_cluster_bar"), height = "350px"), width = 5),
          qbox("Phân phối Monetary theo cụm",
               "Boxplot log-scale",
               plotlyOutput(ns("plot_cluster_monetary_box"), height = "350px"), width = 7)
        ),
        fluidRow(
          qbox("Thông tin chi tiết RFM theo cụm", NULL,
               DTOutput(ns("table_cluster_stats")), width = 12)
        ),
        fluidRow(
          qbox("Đánh giá chất lượng phân cụm", NULL,
               uiOutput(ns("ui_quality_note")), width = 12)
        )
      )
    })

    # ──────────── ELBOW ────────────
    output$plot_elbow <- renderPlotly({
      edf <- elbow_data(); req(edf)

      p <- ggplot(edf, aes(x = K, y = WSS)) +
        geom_line(color = COLORS$steel, linewidth = SIZE$line) +
        geom_point(color = COLORS$steel, size = 3) +
        geom_point(data = edf %>% filter(K == input$clusters),
                   color = COLORS$red, size = 5.5, shape = 18) +
        theme_quant(grid = "both") +
        labs(x = "Số cụm (K)", y = "WSS")

      ggplotly(p) %>% ly_quant()
    })

    # ──────────── F × M SCATTER ────────────
    output$plot_kmeans_scatter <- renderPlotly({
      res <- clustering_results(); req(res)
      res_plot <- if (nrow(res) > 12000) {
        set.seed(42); res %>% sample_n(12000)
      } else res

      p <- ggplot(res_plot, aes(x = Frequency, y = Monetary, color = Cluster_Label)) +
        geom_point(alpha = 0.4, size = 1.3) +
        scale_y_continuous(labels = label_trieu) +
        scale_color_manual(values = PALETTE_MAIN[1:input$clusters]) +
        theme_quant(grid = "both") +
        theme(legend.title = element_blank()) +
        labs(x = "Frequency (số gói BH)", y = "Monetary (VND)")

      ggplotly(p) %>% ly_quant(t = 40)
    })

    # ──────────── R × M SCATTER ────────────
    output$plot_kmeans_rm <- renderPlotly({
      res <- clustering_results(); req(res)
      res_plot <- if (nrow(res) > 12000) {
        set.seed(42); res %>% sample_n(12000)
      } else res

      p <- ggplot(res_plot, aes(x = Recency, y = Monetary, color = Cluster_Label)) +
        geom_point(alpha = 0.4, size = 1.3) +
        scale_y_continuous(labels = label_trieu) +
        scale_color_manual(values = PALETTE_MAIN[1:input$clusters]) +
        theme_quant(grid = "both") +
        theme(legend.title = element_blank()) +
        labs(x = "Recency (ngày)", y = "Monetary (VND)")

      ggplotly(p) %>% ly_quant(t = 40)
    })

    # ──────────── RADAR ────────────
    output$plot_radar <- renderPlotly({
      res <- clustering_results(); req(res)

      radar_df <- res %>%
        group_by(Cluster_Label) %>%
        summarise(R = mean(R_scaled),
                  F = mean(F_scaled),
                  M = mean(M_scaled), .groups = "drop")

      fig <- plot_ly(type = "scatterpolar", fill = "toself")
      for (i in seq_len(nrow(radar_df))) {
        col <- PALETTE_MAIN[i]
        fig <- fig %>% add_trace(
          r = c(radar_df$R[i], radar_df$F[i], radar_df$M[i], radar_df$R[i]),
          theta = c("Recency", "Frequency", "Monetary", "Recency"),
          name = radar_df$Cluster_Label[i],
          fillcolor = paste0(col, "40"),
          line = list(color = col, width = 2.2)
        )
      }

      fig %>% layout(
        font = PLOTLY_FONT,
        polar = list(
          radialaxis  = list(visible = TRUE, gridcolor = COLORS$grid,
                             linecolor = COLORS$border),
          angularaxis = list(gridcolor = COLORS$grid, linecolor = COLORS$border,
                             tickfont = list(size = 11, color = COLORS$navy,
                                             family = FONT_FAMILY_PLOTLY))
        ),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0.5, xanchor = "center",
                      y = -0.15, yanchor = "top",
                      font = list(size = SIZE$legend,
                                  family = FONT_FAMILY_PLOTLY)),
        margin = list(t = 30, b = 60, l = 50, r = 50),
        paper_bgcolor = COLORS$bg
      ) %>% config(displayModeBar = FALSE)
    })

    # ──────────── CLUSTER BAR ────────────
    output$plot_cluster_bar <- renderPlotly({
      res <- clustering_results(); req(res)

      df_bar <- res %>%
        group_by(Cluster_Label) %>%
        summarise(N = n(), .groups = "drop") %>%
        mutate(Pct = round(N / sum(N) * 100, 1))

      p <- ggplot(df_bar, aes(x = reorder(Cluster_Label, N), y = N,
                              fill = Cluster_Label)) +
        geom_col(width = SIZE$bar_width) +
        geom_text(aes(label = paste0(label_int(N), " (", Pct, "%)")),
                  hjust = -0.05, size = SIZE$label,
                  color = COLORS$navy, fontface = "bold") +
        coord_flip() +
        scale_fill_manual(values = PALETTE_MAIN[1:input$clusters]) +
        scale_y_continuous(labels = label_int,
                           expand = expansion(mult = c(0, 0.32))) +
        theme_quant(grid = "x", legend_pos = "none") +
        labs(x = NULL, y = "Số khách hàng")

      ggplotly(p) %>% ly_quant(b = 25)
    })

    # ──────────── MONETARY BOXPLOT ────────────
    output$plot_cluster_monetary_box <- renderPlotly({
      res <- clustering_results(); req(res)

      p <- ggplot(res, aes(x = Cluster_Label, y = Monetary, fill = Cluster_Label)) +
        geom_boxplot(alpha = 0.7, color = COLORS$navy, linewidth = 0.35,
                     outlier.colour = COLORS$red, outlier.alpha = 0.25,
                     outlier.size = 0.7) +
        scale_y_log10(labels = label_trieu) +
        scale_fill_manual(values = PALETTE_MAIN[1:input$clusters]) +
        theme_quant(grid = "y", legend_pos = "none") +
        theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
        labs(x = NULL, y = "Monetary (VND) — Log")

      ggplotly(p) %>% ly_quant(l = 60, b = 50)
    })

    # ──────────── CLUSTER TABLE ────────────
    output$table_cluster_stats <- renderDT({
      res <- clustering_results(); req(res)

      stats <- res %>%
        group_by(Cum = Cluster_Label) %>%
        summarise(
          So_KH      = label_int(n()),
          R_Ngay_TB  = round(mean(Recency), 0),
          F_Goi_TB   = round(mean(Frequency), 1),
          M_TB       = format_vnd(mean(Monetary)),
          M_Tong     = format_vnd(sum(Monetary)),
          Pct_KH     = paste0(round(n() / nrow(res) * 100, 1), "%"),
          .groups = "drop"
        )

      dt_quant(stats,
               colnames = c("Cụm", "Số KH", "R (Ngày TB)", "F (Gói TB)",
                            "M Trung bình", "M Tổng", "% KH"),
               page_length = 10, dom = "t", ordering = FALSE)
    })

    # ──────────── QUALITY NOTE ────────────
    output$ui_quality_note <- renderUI({
      res <- clustering_results(); req(res)
      km <- attr(res, "km")
      bss_pct <- round(km$betweenss / km$totss * 100, 1)

      mkrow <- function(label, value, extra = NULL) tags$tr(
        tags$td(style = sprintf("padding:8px; border-bottom:1px solid %s; font-weight:bold; width:40%%;",
                                COLORS$border), label),
        tags$td(style = sprintf("padding:8px; border-bottom:1px solid %s; color:%s;",
                                COLORS$border, COLORS$navy),
                value, extra)
      )

      tags$div(style = sprintf("font-family:%s; color:%s;",
                                FONT_FAMILY, COLORS$text_main),
        tags$table(style = "width:100%; border-collapse:collapse;",
          mkrow("Số cụm (K)",             input$clusters),
          mkrow("Tổng KH phân cụm",       label_int(nrow(res))),
          mkrow("Total Within SS",        formatC(round(km$tot.withinss, 1),
                                                  format = "f", big.mark = ".",
                                                  digits = 1)),
          mkrow("Between SS / Total SS",  paste0(bss_pct, "%"),
            if (bss_pct >= 50)
              tags$span(" — Tốt",
                style = sprintf("color:%s; font-weight:bold;", COLORS$teal))
            else
              tags$span(" — Cần xem xét",
                style = sprintf("color:%s; font-weight:bold;", COLORS$red))
          ),
          mkrow("Preprocessing", "log1p() → scale() (Z-score)"),
          mkrow("nstart",        "25 (tránh local optima)")
        )
      )
    })
  })
}