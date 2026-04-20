# =============================================================================
# PHASE 8 — QUẢN TRỊ KPI CHUYÊN SÂU
# =============================================================================

phase8UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    page_header(
      "Phần 8 — Quản trị KPI chuyên sâu",
      "Mức độ hoàn thành chỉ tiêu theo đơn vị, kênh bán và sản phẩm."
    ),

    fluidRow(
      valueBoxOutput(ns("vbox_kpi_overall"), width = 4),
      valueBoxOutput(ns("vbox_kpi_sl"),      width = 4),
      valueBoxOutput(ns("vbox_kpi_gap"),     width = 4)
    ),

    fluidRow(
      qbox("Hoàn thành KPI doanh thu theo tháng",
           "Steel = thực tế, Gold (mờ) = mục tiêu",
           plotlyOutput(ns("plot_kpi_monthly"), height = "410px"), width = 12)
    ),

    fluidRow(
      qbox("KPI theo kênh bán (Thực tế vs Mục tiêu)", NULL,
           plotlyOutput(ns("plot_kpi_channel"), height = "390px"), width = 6),
      qbox("Top 10 gói SP theo doanh thu thực tế", NULL,
           plotlyOutput(ns("plot_kpi_product"), height = "390px"), width = 6)
    ),

    fluidRow(
      qbox("Tỷ lệ hoàn thành KPI doanh thu theo kênh (%)",
           "Đường nét đứt đỏ = mốc 100%",
           plotlyOutput(ns("plot_kpi_pct_channel"), height = "390px"), width = 6),
      qbox("Chênh lệch (Actual − Target) theo kênh",
           "Teal = vượt KPI, Đỏ = chưa đạt",
           plotlyOutput(ns("plot_kpi_gap_channel"), height = "390px"), width = 6)
    ),

    fluidRow(
      qbox("Chi tiết KPI theo kênh × tháng", NULL,
           DTOutput(ns("table_kpi_detail")), width = 12)
    )
  )
}

phase8Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {

    get_data <- reactive({ req(all_data()); all_data()$master })
    get_kpi  <- reactive({ req(all_data()); all_data()$kpi })

    output$vbox_kpi_overall <- renderValueBox({
      total_act    <- sum(get_data()$PHIBH, na.rm = TRUE)
      total_target <- sum(get_kpi()$DOANH_THU, na.rm = TRUE)
      pct <- ifelse(total_target > 0, round(total_act / total_target * 100, 1), 0)
      valueBox(paste0(pct, "%"), "Hoàn thành doanh thu",
               icon = icon("check-double"),
               color = ifelse(pct >= 100, "green",
                              ifelse(pct >= 80, "yellow", "red")))
    })

    output$vbox_kpi_sl <- renderValueBox({
      total_target_sl <- sum(get_kpi()$SLHD, na.rm = TRUE)
      pct <- ifelse(total_target_sl > 0,
                    round(nrow(get_data()) / total_target_sl * 100, 1), 0)
      valueBox(paste0(pct, "%"), "Hoàn thành số lượng",
               icon = icon("file-alt"), color = "blue")
    })

    output$vbox_kpi_gap <- renderValueBox({
      gap <- sum(get_kpi()$DOANH_THU, na.rm = TRUE) -
             sum(get_data()$PHIBH, na.rm = TRUE)
      valueBox(format_vnd(abs(gap)),
               ifelse(gap > 0, "Còn thiếu so với KPI", "Vượt KPI"),
               icon = icon(ifelse(gap > 0, "arrow-down", "arrow-up")),
               color = ifelse(gap > 0, "red", "green"))
    })

    # ──────────── 1. KPI MONTHLY ────────────
    output$plot_kpi_monthly <- renderPlotly({
      act_sum <- get_data() %>%
        mutate(YM = format(NGAY_KY_HD, "%Y%m")) %>%
        group_by(YM) %>%
        summarise(Actual = sum(PHIBH, na.rm = TRUE), .groups = "drop")
      kpi_sum <- get_kpi() %>%
        group_by(YM = as.character(THANGNAM)) %>%
        summarise(Target = sum(DOANH_THU, na.rm = TRUE), .groups = "drop")

      df_combo <- full_join(act_sum, kpi_sum, by = "YM") %>%
        arrange(YM) %>%
        mutate(Label = paste0(substr(YM, 5, 6), "/", substr(YM, 1, 4)))

      plot_ly(df_combo, x = ~Label) %>%
        add_bars(y = ~Actual, name = "Thực tế",
                 marker = list(color = COLORS$steel)) %>%
        add_bars(y = ~Target, name = "Mục tiêu (KPI)",
                 marker = list(color = COLORS$gold, opacity = 0.55)) %>%
        layout(barmode = "group", bargap = 0.25,
               xaxis = list(tickangle = -45)) %>%
        ly_quant(t = 40, b = 50)
    })

    # ──────────── 2. KPI CHANNEL ────────────
    output$plot_kpi_channel <- renderPlotly({
      df_act <- get_data() %>% filter(TENKENHBAN != "Chưa xác định") %>%
        group_by(MA_KENHBAN, TENKENHBAN) %>%
        summarise(Actual = sum(PHIBH, na.rm = TRUE), .groups = "drop")
      df_kpi <- get_kpi() %>% group_by(MA_KENHBAN) %>%
        summarise(Target = sum(DOANH_THU, na.rm = TRUE), .groups = "drop")

      df_long <- left_join(df_act, df_kpi, by = "MA_KENHBAN") %>%
        select(TENKENHBAN, Actual, Target) %>%
        pivot_longer(cols = c(Actual, Target),
                     names_to = "Type", values_to = "Value")

      p <- ggplot(df_long, aes(x = reorder(TENKENHBAN, Value),
                               y = Value, fill = Type)) +
        geom_col(position = "dodge", width = 0.7) + coord_flip() +
        scale_fill_manual(values = c("Actual" = COLORS$steel,
                                     "Target" = COLORS$gold),
                          labels = c("Thực tế", "Mục tiêu")) +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0, 0.08))) +
        theme_quant(grid = "x") +
        theme(legend.title = element_blank()) +
        labs(x = NULL, y = "Doanh thu (VND)")

      ggplotly(p) %>% ly_quant(t = 40)
    })

    # ──────────── 3. KPI PRODUCT TOP 10 ────────────
    output$plot_kpi_product <- renderPlotly({
      df_prod <- get_data() %>%
        filter(TENGOISANPHAM != "Chưa xác định") %>%
        group_by(TENGOISANPHAM) %>%
        summarise(Actual = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(Actual)) %>% head(10)

      p <- ggplot(df_prod, aes(x = reorder(TENGOISANPHAM, Actual), y = Actual)) +
        geom_col(fill = COLORS$steel, width = SIZE$bar_width) + coord_flip() +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0, 0.08))) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "Doanh thu thực tế (VND)")

      ggplotly(p) %>% ly_quant(b = 25)
    })

    # ──────────── 4. KPI % CHANNEL ────────────
    output$plot_kpi_pct_channel <- renderPlotly({
      df_act <- get_data() %>% filter(TENKENHBAN != "Chưa xác định") %>%
        group_by(MA_KENHBAN, TENKENHBAN) %>%
        summarise(Actual = sum(PHIBH, na.rm = TRUE), .groups = "drop")
      df_kpi <- get_kpi() %>% group_by(MA_KENHBAN) %>%
        summarise(Target = sum(DOANH_THU, na.rm = TRUE), .groups = "drop")

      df_pct <- left_join(df_act, df_kpi, by = "MA_KENHBAN") %>%
        mutate(Pct = ifelse(!is.na(Target) & Target > 0,
                            round(Actual / Target * 100, 1), 0))

      p <- ggplot(df_pct, aes(x = reorder(TENKENHBAN, Pct), y = Pct,
                              fill = Pct >= 100)) +
        geom_col(width = SIZE$bar_width) +
        geom_hline(yintercept = 100, linetype = "dashed",
                   color = COLORS$red, linewidth = 0.7) +
        geom_text(aes(label = paste0(Pct, "%")),
                  hjust = -0.08, size = SIZE$label,
                  color = COLORS$navy, fontface = "bold") +
        coord_flip() +
        scale_fill_manual(values = c("TRUE" = COLORS$teal,
                                     "FALSE" = COLORS$red),
                          guide = "none") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "% Hoàn thành")

      ggplotly(p, tooltip = "y") %>% ly_quant(b = 25)
    })

    # ──────────── 5. KPI GAP CHANNEL ────────────
    output$plot_kpi_gap_channel <- renderPlotly({
      df_act <- get_data() %>% filter(TENKENHBAN != "Chưa xác định") %>%
        group_by(MA_KENHBAN, TENKENHBAN) %>%
        summarise(Actual = sum(PHIBH, na.rm = TRUE), .groups = "drop")
      df_kpi <- get_kpi() %>% group_by(MA_KENHBAN) %>%
        summarise(Target = sum(DOANH_THU, na.rm = TRUE), .groups = "drop")

      df_gap <- left_join(df_act, df_kpi, by = "MA_KENHBAN") %>%
        mutate(Gap = Actual - ifelse(is.na(Target), 0, Target),
               Direction = ifelse(Gap >= 0, "Vượt", "Thiếu"))

      p <- ggplot(df_gap, aes(x = reorder(TENKENHBAN, Gap), y = Gap,
                              fill = Direction)) +
        geom_col(width = SIZE$bar_width) +
        geom_hline(yintercept = 0, color = COLORS$navy, linewidth = 0.5) +
        coord_flip() +
        scale_fill_manual(values = c("Vượt" = COLORS$teal,
                                     "Thiếu" = COLORS$red),
                          guide = "none") +
        scale_y_continuous(labels = label_trieu) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "Chênh lệch (VND)")

      ggplotly(p) %>% ly_quant(b = 25)
    })

    # ──────────── 6. KPI DETAIL TABLE ────────────
    output$table_kpi_detail <- renderDT({
      df_act <- get_data() %>%
        mutate(YM = format(NGAY_KY_HD, "%Y%m")) %>%
        group_by(YM, MA_KENHBAN, TENKENHBAN) %>%
        summarise(Actual_DT = sum(PHIBH, na.rm = TRUE),
                  Actual_SL = n(), .groups = "drop")
      df_kpi <- get_kpi() %>%
        group_by(YM = as.character(THANGNAM), MA_KENHBAN) %>%
        summarise(Target_DT = sum(DOANH_THU, na.rm = TRUE),
                  Target_SL = sum(SLHD, na.rm = TRUE), .groups = "drop")

      df_detail <- full_join(df_act, df_kpi, by = c("YM", "MA_KENHBAN")) %>%
        mutate(
          Pct_DT = ifelse(!is.na(Target_DT) & Target_DT > 0,
                          round(Actual_DT / Target_DT * 100, 1), NA),
          Pct_SL = ifelse(!is.na(Target_SL) & Target_SL > 0,
                          round(Actual_SL / Target_SL * 100, 1), NA)
        ) %>%
        arrange(YM, MA_KENHBAN) %>%
        select(YM, TENKENHBAN, Actual_DT, Target_DT, Pct_DT,
               Actual_SL, Target_SL, Pct_SL)

      dt_quant(df_detail,
               colnames = c("Tháng", "Kênh bán",
                            "DT thực tế", "DT mục tiêu", "% HT DT",
                            "SL thực tế", "SL mục tiêu", "% HT SL"),
               filter = "top") %>%
        formatCurrency(c("Actual_DT", "Target_DT"), currency = "",
                       interval = 3, mark = ".", digits = 0) %>%
        formatStyle("Pct_DT",
          backgroundColor = styleInterval(c(80, 100),
                                          c("#FCE4E4", "#FFF3D4", "#E0F2EE")),
          fontWeight = "bold")
    })
  })
}