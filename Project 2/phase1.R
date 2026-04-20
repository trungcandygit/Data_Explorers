# =============================================================================
# PHASE 1 — KIỂM SOÁT CHẤT LƯỢNG & LÀM SẠCH DỮ LIỆU
# Tiêu thụ: COLORS, PALETTE_MAIN, theme_quant, ly_quant, dt_quant,
#           page_header, box_title, qbox, format_vnd, label_trieu, label_int
# =============================================================================

phase1UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    page_header(
      "Phần 1 — Kiểm soát chất lượng & làm sạch dữ liệu",
      "Phân tích missing, outlier, bất thường logic và dữ liệu trùng lặp."
    ),

    fluidRow(
      valueBoxOutput(ns("box_total_goi"),   width = 3),
      valueBoxOutput(ns("box_total_hd"),    width = 3),
      valueBoxOutput(ns("box_total_kh"),    width = 3),
      valueBoxOutput(ns("box_missing_pct"), width = 3)
    ),

    fluidRow(
      qbox("Tỷ lệ thiếu dữ liệu các cột",
           "Top 15 cột có tỷ lệ NA / 'Chưa xác định' cao nhất",
           plotlyOutput(ns("plot_missing"), height = "370px"), width = 6),
      qbox("Phân phối phí bảo hiểm",
           "Ngoại lai theo quy tắc IQR × 1.5",
           plotlyOutput(ns("plot_outlier"), height = "370px"), width = 6)
    ),

    fluidRow(
      qbox("Phí BH theo nhóm sản phẩm",
           "Boxplot log-scale — so sánh phân phối phí",
           plotlyOutput(ns("plot_outlier_by_group"), height = "370px"), width = 6),
      qbox("Phân phối giá trị bảo hiểm",
           "Histogram log-scale của GIA_BH",
           plotlyOutput(ns("plot_gia_bh_dist"), height = "370px"), width = 6)
    ),

    fluidRow(
      qbox("Mật độ phí: HĐ Mới vs Tái tục",
           "So sánh phân phối log-phí giữa hai loại hợp đồng",
           plotlyOutput(ns("plot_density_overlay"), height = "370px"), width = 6),
      qbox("Biến động số gói BH theo tháng",
           "% thay đổi MoM (xanh = tăng, đỏ = giảm)",
           plotlyOutput(ns("plot_mom_change"), height = "370px"), width = 6)
    ),

    fluidRow(
      qbox("Báo cáo chất lượng dữ liệu đầu vào", NULL,
           DTOutput(ns("table_quality_report")), width = 12)
    ),

    fluidRow(
      qbox("Nhật ký Audit — hợp đồng bất thường & ngoại lai",
           "Top 200 bản ghi bị flag",
           DTOutput(ns("table_audit")), width = 12)
    )
  )
}

phase1Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {

    get_data <- reactive({ req(all_data()); all_data()$master })

    # ──────────── KPI BOXES ────────────
    output$box_total_goi <- renderValueBox({
      valueBox(label_int(nrow(get_data())), "Tổng số gói BH",
               icon = icon("database"), color = "blue")
    })
    output$box_total_hd <- renderValueBox({
      valueBox(label_int(n_distinct(get_data()$MA_HD)), "Số lượng hợp đồng",
               icon = icon("file-contract"), color = "aqua")
    })
    output$box_total_kh <- renderValueBox({
      valueBox(label_int(n_distinct(get_data()$MA_KH)), "Khách hàng unique",
               icon = icon("users"), color = "green")
    })
    output$box_missing_pct <- renderValueBox({
      df <- get_data()
      total_cells <- nrow(df) * ncol(df)
      na_n  <- sum(is.na(df))
      cxd_n <- sum(sapply(df, function(x)
        if (is.character(x)) sum(x == "Chưa xác định", na.rm = TRUE) else 0))
      pct <- round((na_n + cxd_n) / total_cells * 100, 2)
      valueBox(paste0(pct, "%"), "Tỷ lệ thiếu dữ liệu",
               icon = icon("exclamation-triangle"),
               color = ifelse(pct > 5, "yellow", "green"))
    })

    # ──────────── 1. MISSING ────────────
    output$plot_missing <- renderPlotly({
      df <- get_data()
      miss_df <- data.frame(
        Cot = names(df),
        NA_Count  = colSums(is.na(df)),
        CXD_Count = sapply(df, function(x)
          if (is.character(x)) sum(x == "Chưa xác định", na.rm = TRUE) else 0),
        stringsAsFactors = FALSE
      ) %>%
        mutate(Total = NA_Count + CXD_Count,
               Pct   = round(Total / nrow(df) * 100, 2)) %>%
        filter(Total > 0) %>% arrange(desc(Total)) %>% head(15)

      if (nrow(miss_df) == 0)
        miss_df <- data.frame(Cot = "Sạch 100%", Total = 0, Pct = 0)

      p <- ggplot(miss_df, aes(x = reorder(Cot, Total), y = Pct)) +
        geom_col(fill = COLORS$steel, width = SIZE$bar_width) +
        geom_text(aes(label = paste0(Pct, "%")),
                  hjust = -0.1, size = SIZE$label,
                  color = COLORS$navy, fontface = "bold") +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "Tỷ lệ Missing (%)")

      ggplotly(p, tooltip = "y") %>% ly_quant(l = 100, b = 30)
    })

    # ──────────── 2. OUTLIER BOXPLOT ────────────
    output$plot_outlier <- renderPlotly({
      df <- get_data() %>% filter(PHIBH > 0)

      q1 <- quantile(df$PHIBH, 0.25); q3 <- quantile(df$PHIBH, 0.75)
      iqr <- q3 - q1
      n_out <- sum(df$PHIBH > q3 + 1.5 * iqr | df$PHIBH < q1 - 1.5 * iqr)

      p <- ggplot(df, aes(y = PHIBH, x = "Phí BH")) +
        geom_boxplot(fill = COLORS$steel, alpha = 0.55,
                     color = COLORS$navy, width = 0.45,
                     outlier.colour = COLORS$red,
                     outlier.alpha = 0.35, outlier.size = 1.2) +
        scale_y_log10(labels = label_trieu) +
        theme_quant(grid = "y") +
        labs(x = NULL, y = "Phí BH (VND) — Log",
             subtitle = paste0(label_int(n_out), " outliers (IQR × 1.5)"))

      ggplotly(p) %>% ly_quant(l = 70, b = 35, t = 30)
    })

    # ──────────── 3. OUTLIER × NHÓM SP ────────────
    output$plot_outlier_by_group <- renderPlotly({
      df <- get_data() %>% filter(PHIBH > 0, !is.na(NHOMSANPHAM))

      p <- ggplot(df, aes(x = NHOMSANPHAM, y = PHIBH, fill = NHOMSANPHAM)) +
        geom_boxplot(alpha = 0.75, color = COLORS$navy, linewidth = 0.35,
                     outlier.colour = COLORS$red, outlier.alpha = 0.2,
                     outlier.size = 0.7) +
        scale_fill_manual(values = PALETTE_MAIN) +
        scale_y_log10(labels = label_trieu) +
        theme_quant(grid = "y", legend_pos = "none") +
        theme(axis.text.x = element_text(angle = 22, hjust = 1)) +
        labs(x = NULL, y = "Phí BH (VND) — Log")

      ggplotly(p) %>% ly_quant(l = 65, b = 70)
    })

    # ──────────── 4. GIA_BH HISTOGRAM ────────────
    output$plot_gia_bh_dist <- renderPlotly({
      df <- get_data() %>% filter(GIA_BH > 0)

      p <- ggplot(df, aes(x = GIA_BH)) +
        geom_histogram(bins = 45, fill = COLORS$teal, alpha = 0.85,
                       color = "white", linewidth = 0.2) +
        scale_x_log10(labels = label_trieu) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                           labels = label_int) +
        theme_quant(grid = "y") +
        labs(x = "Giá trị BH (VND) — Log", y = "Số lượng")

      ggplotly(p) %>% ly_quant(l = 55, b = 42)
    })

    # ──────────── 5. DENSITY OVERLAY MỚI / TÁI TỤC ────────────
    output$plot_density_overlay <- renderPlotly({
      df <- get_data() %>% filter(PHIBH > 0, LOAI_HD %in% c("Mới", "Tái tục"))

      p <- ggplot(df, aes(x = PHIBH, fill = LOAI_HD, color = LOAI_HD)) +
        geom_density(alpha = 0.30, linewidth = 1) +
        scale_x_log10(labels = label_trieu) +
        scale_fill_manual(values  = c("Mới" = COLORS$new, "Tái tục" = COLORS$renewal)) +
        scale_color_manual(values = c("Mới" = COLORS$new, "Tái tục" = COLORS$renewal)) +
        theme_quant(grid = "y") +
        theme(legend.title = element_blank()) +
        labs(x = "Phí BH (VND) — Log", y = "Mật độ")

      ggplotly(p) %>% ly_quant(l = 50, b = 42)
    })

    # ──────────── 6. MoM CHANGE ────────────
    output$plot_mom_change <- renderPlotly({
      df_m <- get_data() %>%
        group_by(THANG) %>% summarise(N = n(), .groups = "drop") %>%
        arrange(THANG) %>%
        mutate(Pct = round((N / lag(N) - 1) * 100, 1),
               Direction = ifelse(Pct >= 0, "Tăng", "Giảm")) %>%
        filter(!is.na(Pct))

      p <- ggplot(df_m, aes(x = THANG, y = Pct, fill = Direction)) +
        geom_col(width = 22, alpha = 0.9) +
        geom_hline(yintercept = 0, color = COLORS$navy, linewidth = 0.4) +
        scale_fill_manual(values = c("Tăng" = COLORS$up, "Giảm" = COLORS$down)) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
        theme_quant(grid = "y", legend_pos = "none") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(x = NULL, y = "% Thay đổi so với tháng trước")

      ggplotly(p) %>% ly_quant(l = 50, b = 60)
    })

    # ──────────── 7. QUALITY TABLE ────────────
    output$table_quality_report <- renderDT({
      df <- get_data()
      report <- data.frame(
        Chi_tieu = c("Tổng số gói bảo hiểm", "Tổng hợp đồng", "Tổng số KH unique",
                     "Khoảng thời gian", "Phí BH bình quân", "Phí BH trung vị",
                     "Phí BH min", "Phí BH max", "Số gói phí BH âm",
                     "Số gói thiếu giá BH", "Tỷ lệ HĐ mới", "Tỷ lệ tái tục"),
        Gia_tri = c(
          paste(label_int(nrow(df)), "gói"),
          paste(label_int(n_distinct(df$MA_HD)), "hợp đồng"),
          paste(label_int(n_distinct(df$MA_KH)), "khách hàng"),
          paste(format(min(df$NGAY_KY_HD, na.rm = TRUE), "%d/%m/%Y"), "→",
                format(max(df$NGAY_KY_HD, na.rm = TRUE), "%d/%m/%Y")),
          format_vnd(mean(df$PHIBH, na.rm = TRUE)),
          format_vnd(median(df$PHIBH, na.rm = TRUE)),
          format_vnd(min(df$PHIBH, na.rm = TRUE)),
          format_vnd(max(df$PHIBH, na.rm = TRUE)),
          label_int(sum(df$PHIBH <= 0, na.rm = TRUE)),
          label_int(sum(is.na(df$GIA_BH))),
          paste0(round(sum(df$LOAI_HD == "Mới",     na.rm = TRUE) / nrow(df) * 100, 1), "%"),
          paste0(round(sum(df$LOAI_HD == "Tái tục", na.rm = TRUE) / nrow(df) * 100, 1), "%")
        ), stringsAsFactors = FALSE)

      dt_quant(report,
               colnames = c("Chỉ tiêu", "Giá trị"),
               page_length = 20, dom = "t", ordering = FALSE,
               scrollX = FALSE)
    })

    # ──────────── 8. AUDIT TABLE ────────────
    output$table_audit <- renderDT({
      df <- get_data()
      q3 <- quantile(df$PHIBH, 0.75, na.rm = TRUE)
      iqr <- IQR(df$PHIBH, na.rm = TRUE)
      upper <- q3 + 1.5 * iqr

      audit <- df %>%
        mutate(Flag = case_when(
          PHIBH > upper & XHKH %in% c("Kim cương", "Bạch kim") ~ "VIP + Outlier",
          PHIBH > upper                                         ~ "Outlier phí cao",
          PHIBH <= 1000                                         ~ "Phí cực thấp",
          TRUE ~ NA_character_)) %>%
        filter(!is.na(Flag)) %>%
        select(MA_HD, MA_KH, TENGOISANPHAM, TEN_CN, PHIBH, XHKH, Flag) %>%
        arrange(desc(PHIBH)) %>% head(200)

      dt_quant(audit, page_length = 10) %>%
        formatCurrency("PHIBH", currency = "", interval = 3, mark = ".", digits = 0) %>%
        formatStyle("Flag",
          backgroundColor = styleEqual(
            c("VIP + Outlier", "Outlier phí cao", "Phí cực thấp"),
            c("#FCE4E4",       "#FFF3D4",         "#E0F2EE")),
          fontWeight = "bold",
          color = COLORS$navy)
    })
  })
}