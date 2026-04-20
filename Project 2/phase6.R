# =============================================================================
# PHASE 6 — KHÁCH HÀNG TIỀM NĂNG & DỰ BÁO NHU CẦU
# =============================================================================

phase6UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    page_header(
      "Phần 6 — Khách hàng tiềm năng & dự báo nhu cầu",
      "Toàn bộ ~400K khách hàng: ai đã mua, ai chưa, cơ hội chuyển đổi ở đâu."
    ),

    fluidRow(
      valueBoxOutput(ns("vb_total_kh_all"),    width = 3),
      valueBoxOutput(ns("vb_kh_bought"),       width = 3),
      valueBoxOutput(ns("vb_kh_not_bought"),   width = 3),
      valueBoxOutput(ns("vb_conversion_rate"), width = 3)
    ),

    fluidRow(
      qbox("Tỷ lệ đã mua vs chưa mua",
           "Bao nhiêu % KH hệ thống đã phát sinh giao dịch?",
           plotlyOutput(ns("plot_bought_vs_not"), height = "360px"), width = 5),
      qbox("Phân bố xếp hạng — đã mua vs chưa mua",
           "Hạng nào còn nhiều KH chưa khai thác?",
           plotlyOutput(ns("plot_xhkh_compare"), height = "360px"), width = 7)
    ),

    fluidRow(
      qbox("Nhóm tuổi KH chưa mua",
           "Nhóm tuổi nào tiềm năng nhất?",
           plotlyOutput(ns("plot_age_not_bought"), height = "370px"), width = 6),
      qbox("Top 15 nghề nghiệp KH chưa mua",
           "Nghề nào có tệp tiềm năng lớn nhất?",
           plotlyOutput(ns("plot_job_not_bought"), height = "370px"), width = 6)
    ),

    fluidRow(
      qbox("Tỷ lệ chuyển đổi theo xếp hạng",
           "Hạng nào có tỷ lệ mua cao nhất?",
           plotlyOutput(ns("plot_conversion_by_xhkh"), height = "370px"), width = 6),
      qbox("Doanh thu tiềm năng nếu chuyển đổi",
           "Số KH chưa mua × chi tiêu TB của hạng tương ứng",
           plotlyOutput(ns("plot_potential_revenue"), height = "370px"), width = 6)
    ),

    fluidRow(
      qbox("Giới tính KH chưa mua", NULL,
           plotlyOutput(ns("plot_gender_not_bought"), height = "360px"), width = 6),
      qbox("Tỷ lệ chuyển đổi theo nhóm tuổi",
           "Nhóm tuổi nào dễ chuyển đổi nhất?",
           plotlyOutput(ns("plot_conversion_by_age"), height = "360px"), width = 6)
    ),

    fluidRow(
      qbox("Bảng tổng hợp tiềm năng theo xếp hạng", NULL,
           DTOutput(ns("table_potential_summary")), width = 12)
    )
  )
}

phase6Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {

    get_master <- reactive({ req(all_data()); all_data()$master })

    all_kh <- reactive({
      req(file.exists("kh_full.rds"))
      readRDS("kh_full.rds")
    })

    kh_tagged <- reactive({
      bought_ids <- unique(get_master()$MA_KH)
      all_kh() %>%
        mutate(Status = ifelse(MA_KH %in% bought_ids, "Đã mua", "Chưa mua"))
    })

    avg_spend <- reactive({
      get_master() %>%
        group_by(XHKH, MA_KH) %>%
        summarise(T = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        group_by(XHKH) %>%
        summarise(Avg = mean(T), .groups = "drop")
    })

    output$vb_total_kh_all <- renderValueBox({
      valueBox(label_int(nrow(all_kh())), "Tổng KH hệ thống",
               icon = icon("address-book"), color = "blue")
    })
    output$vb_kh_bought <- renderValueBox({
      valueBox(label_int(sum(kh_tagged()$Status == "Đã mua")),
               "KH đã mua", icon = icon("check-circle"), color = "green")
    })
    output$vb_kh_not_bought <- renderValueBox({
      valueBox(label_int(sum(kh_tagged()$Status == "Chưa mua")),
               "KH chưa mua", icon = icon("user-clock"), color = "yellow")
    })
    output$vb_conversion_rate <- renderValueBox({
      pct <- round(sum(kh_tagged()$Status == "Đã mua") / nrow(kh_tagged()) * 100, 1)
      valueBox(paste0(pct, "%"), "Tỷ lệ chuyển đổi",
               icon = icon("percentage"),
               color = ifelse(pct >= 50, "green", "red"))
    })

    # ──────────── 1. BOUGHT vs NOT (donut) ────────────
    output$plot_bought_vs_not <- renderPlotly({
      df <- kh_tagged() %>% group_by(Status) %>%
        summarise(N = n(), .groups = "drop")

      plot_ly(df, labels = ~Status, values = ~N, type = "pie", hole = 0.5,
              textinfo = "percent+value",
              marker = list(colors = c(COLORS$steel, COLORS$gold),
                            line = list(color = "#FFFFFF", width = 2)),
              textfont = list(size = 12, color = "#FFFFFF",
                              family = FONT_FAMILY_PLOTLY)) %>%
        layout(font = PLOTLY_FONT,
               showlegend = TRUE,
               legend = list(orientation = "h", x = 0.5, xanchor = "center",
                             y = -0.05, yanchor = "top",
                             font = list(family = FONT_FAMILY_PLOTLY,
                                         size = SIZE$legend)),
               margin = list(l = 10, r = 10, t = 10, b = 40),
               paper_bgcolor = COLORS$bg) %>%
        config(displayModeBar = FALSE)
    })

    # ──────────── 2. XHKH COMPARE ────────────
    output$plot_xhkh_compare <- renderPlotly({
      df <- kh_tagged() %>%
        group_by(XHKH, Status) %>%
        summarise(N = n(), .groups = "drop") %>%
        mutate(XHKH = factor(XHKH,
                             levels = c("Đồng", "Bạc", "Vàng",
                                        "Bạch kim", "Kim cương")))

      p <- ggplot(df, aes(x = XHKH, y = N, fill = Status)) +
        geom_col(position = "dodge", width = 0.7) +
        scale_fill_manual(values = c("Đã mua" = COLORS$steel,
                                     "Chưa mua" = COLORS$gold)) +
        scale_y_continuous(labels = label_int,
                           expand = expansion(mult = c(0, 0.08))) +
        theme_quant(grid = "y") +
        theme(legend.title = element_blank()) +
        labs(x = NULL, y = "Số khách hàng")

      ggplotly(p) %>% ly_quant(t = 40)
    })

    # ──────────── 3. AGE NOT BOUGHT ────────────
    output$plot_age_not_bought <- renderPlotly({
      df <- kh_tagged() %>% filter(Status == "Chưa mua", !is.na(NHOM_TUOI)) %>%
        group_by(NHOM_TUOI) %>% summarise(N = n(), .groups = "drop")

      p <- ggplot(df, aes(x = reorder(NHOM_TUOI, N), y = N)) +
        geom_col(fill = COLORS$navy, width = SIZE$bar_width) +
        geom_text(aes(label = label_int(N)),
                  hjust = -0.05, size = SIZE$label,
                  color = COLORS$navy, fontface = "bold") +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.22)),
                           labels = label_int) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "Số KH chưa mua")

      ggplotly(p, tooltip = "y") %>% ly_quant(b = 25)
    })

    # ──────────── 4. JOB NOT BOUGHT ────────────
    output$plot_job_not_bought <- renderPlotly({
      df <- kh_tagged() %>% filter(Status == "Chưa mua") %>%
        group_by(NGHENGHIEP) %>% summarise(N = n(), .groups = "drop") %>%
        top_n(15, N)

      p <- ggplot(df, aes(x = reorder(NGHENGHIEP, N), y = N)) +
        geom_col(fill = COLORS$teal, width = SIZE$bar_width) +
        geom_text(aes(label = label_int(N)),
                  hjust = -0.05, size = SIZE$label - 0.2,
                  color = COLORS$navy, fontface = "bold") +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.22)),
                           labels = label_int) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "Số KH chưa mua")

      ggplotly(p, tooltip = "y") %>% ly_quant(b = 25)
    })

    # ──────────── 5. CONVERSION BY XHKH ────────────
    output$plot_conversion_by_xhkh <- renderPlotly({
      df <- kh_tagged() %>%
        group_by(XHKH) %>%
        summarise(Total = n(),
                  Pct = round(sum(Status == "Đã mua") / Total * 100, 1),
                  .groups = "drop") %>%
        mutate(XHKH = factor(XHKH,
                             levels = c("Đồng", "Bạc", "Vàng",
                                        "Bạch kim", "Kim cương")))

      p <- ggplot(df, aes(x = XHKH, y = Pct, fill = XHKH)) +
        geom_col(width = 0.6) +
        geom_text(aes(label = paste0(Pct, "%")),
                  vjust = -0.5, size = SIZE$label + 0.2,
                  color = COLORS$navy, fontface = "bold") +
        scale_fill_manual(values = PALETTE_XHKH) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        theme_quant(grid = "y", legend_pos = "none") +
        theme(axis.text.x = element_text(face = "bold", size = 10)) +
        labs(x = NULL, y = "Tỷ lệ chuyển đổi (%)")

      ggplotly(p, tooltip = "y") %>% ly_quant(b = 25)
    })

    # ──────────── 6. POTENTIAL REVENUE ────────────
    output$plot_potential_revenue <- renderPlotly({
      not_b <- kh_tagged() %>% filter(Status == "Chưa mua") %>%
        group_by(XHKH) %>% summarise(N = n(), .groups = "drop")

      df <- left_join(not_b, avg_spend(), by = "XHKH") %>%
        mutate(Pot = N * Avg,
               XHKH = factor(XHKH,
                             levels = c("Đồng", "Bạc", "Vàng",
                                        "Bạch kim", "Kim cương")))

      p <- ggplot(df, aes(x = XHKH, y = Pot, fill = XHKH)) +
        geom_col(width = 0.6) +
        geom_text(aes(label = label_trieu(Pot)),
                  vjust = -0.5, size = SIZE$label,
                  color = COLORS$navy, fontface = "bold") +
        scale_fill_manual(values = PALETTE_XHKH) +
        scale_y_continuous(labels = label_trieu,
                           expand = expansion(mult = c(0, 0.18))) +
        theme_quant(grid = "y", legend_pos = "none") +
        theme(axis.text.x = element_text(face = "bold", size = 10)) +
        labs(x = NULL, y = "Doanh thu tiềm năng (VND)")

      ggplotly(p, tooltip = "y") %>% ly_quant(b = 25)
    })

    # ──────────── 7. GENDER NOT BOUGHT ────────────
    output$plot_gender_not_bought <- renderPlotly({
      df <- kh_tagged() %>% filter(Status == "Chưa mua") %>%
        group_by(GIOI_TINH) %>% summarise(N = n(), .groups = "drop")

      plot_ly(df, labels = ~GIOI_TINH, values = ~N, type = "pie",
              hole = 0.5, textinfo = "percent+label",
              marker = list(colors = c(COLORS$steel, COLORS$gold, COLORS$gray),
                            line = list(color = "#FFFFFF", width = 2)),
              textfont = list(size = 12, color = "#FFFFFF",
                              family = FONT_FAMILY_PLOTLY)) %>%
        layout(font = PLOTLY_FONT, showlegend = FALSE,
               margin = list(l = 10, r = 10, t = 10, b = 10),
               paper_bgcolor = COLORS$bg) %>%
        config(displayModeBar = FALSE)
    })

    # ──────────── 8. CONVERSION BY AGE ────────────
    output$plot_conversion_by_age <- renderPlotly({
      df <- kh_tagged() %>% filter(!is.na(NHOM_TUOI)) %>%
        group_by(NHOM_TUOI) %>%
        summarise(Total = n(),
                  Pct = round(sum(Status == "Đã mua") / Total * 100, 1),
                  .groups = "drop")

      p <- ggplot(df, aes(x = reorder(NHOM_TUOI, Pct), y = Pct)) +
        geom_col(fill = COLORS$steel, width = SIZE$bar_width) +
        geom_text(aes(label = paste0(Pct, "%")),
                  hjust = -0.08, size = SIZE$label,
                  color = COLORS$navy, fontface = "bold") +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
        theme_quant(grid = "x") +
        labs(x = NULL, y = "Tỷ lệ chuyển đổi (%)")

      ggplotly(p, tooltip = "y") %>% ly_quant(b = 25)
    })

    # ──────────── 9. POTENTIAL TABLE ────────────
    output$table_potential_summary <- renderDT({
      not_b  <- kh_tagged() %>% filter(Status == "Chưa mua") %>%
        group_by(XHKH) %>% summarise(KH_Chua = n(), .groups = "drop")
      bought <- kh_tagged() %>% filter(Status == "Đã mua") %>%
        group_by(XHKH) %>% summarise(KH_Da = n(), .groups = "drop")

      df <- left_join(not_b, bought, by = "XHKH") %>%
        left_join(avg_spend(), by = "XHKH") %>%
        mutate(Ty_le = round(KH_Da / (KH_Da + KH_Chua) * 100, 1),
               DT_Pot = KH_Chua * Avg) %>%
        arrange(desc(DT_Pot))

      dt_quant(df,
               colnames = c("Xếp hạng", "KH chưa mua", "KH đã mua",
                            "Chi tiêu TB", "Tỷ lệ CĐ (%)", "DT tiềm năng"),
               page_length = 10, dom = "t") %>%
        formatCurrency(c("Avg", "DT_Pot"), currency = "",
                       interval = 3, mark = ".", digits = 0) %>%
        formatRound(c("KH_Chua", "KH_Da"), digits = 0, interval = 3, mark = ".")
    })
  })
}