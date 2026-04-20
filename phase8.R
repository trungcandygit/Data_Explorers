phase8UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    h2("Phần 8: Quản trị KPI chuyên sâu",
       style = "color:#303983; font-weight:700; margin-bottom:4px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),
    p("Phân tích chi tiết mức độ hoàn thành chỉ tiêu theo từng đơn vị, kênh bán và sản phẩm.",
      style = "color:#6C7A89; margin-bottom:18px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),

    # --- KPI Cards ---
    fluidRow(
      valueBoxOutput(ns("vbox_kpi_overall"), width = 4),
      valueBoxOutput(ns("vbox_kpi_sl"), width = 4),
      valueBoxOutput(ns("vbox_kpi_gap"), width = 4)
    ),

    # --- Row 1: KPI monthly ---
    fluidRow(
      box(title = NULL, width = 12, status = "primary", solidHeader = FALSE,
          tags$h4("Hoàn thành KPI doanh thu theo tháng (Thực tế vs Mục tiêu)",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Bar steel blue = thực tế, bar hồng nhạt = mục tiêu KPI.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_kpi_monthly"), height = "410px"))
    ),

    # --- Row 2: Kênh bán + Sản phẩm ---
    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("KPI theo kênh bán (Thực tế vs Mục tiêu)",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("So sánh doanh thu thực tế và chỉ tiêu theo từng kênh.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_kpi_channel"), height = "390px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("KPI theo gói sản phẩm (Top 10)",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Top 10 gói sản phẩm có doanh thu thực tế cao nhất.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_kpi_product"), height = "390px"))
    ),

    # --- Row 3 MỚI: % hoàn thành theo kênh (bullet-style) + Gap phân tích ---
    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Tỷ lệ hoàn thành KPI doanh thu theo kênh (%)",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Đường đứt nét đỏ = mốc 100%. Kênh nào vượt, kênh nào thiếu?",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_kpi_pct_channel"), height = "390px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Chênh lệch doanh thu (Actual − Target) theo kênh",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Xanh = vượt KPI, hồng = chưa đạt. Giá trị tuyệt đối.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_kpi_gap_channel"), height = "390px"))
    ),

    # --- Row 4: Bảng chi tiết ---
    fluidRow(
      box(title = NULL, width = 12, status = "primary", solidHeader = FALSE,
          tags$h4("Chi tiết KPI theo kênh × tháng",
                   style = "color:#303983; font-weight:700; margin:0 0 6px 0; font-family:'Helvetica Neue',sans-serif;"),
          DTOutput(ns("table_kpi_detail")))
    )
  )
}

phase8Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {

    get_data <- reactive({ req(all_data()); all_data()$master })
    get_kpi  <- reactive({ req(all_data()); all_data()$kpi })

    pf <- list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", color = "#4A4A4A")

    ly <- function(p, bl = 10, bb = 10, bt = 10, br = 10) {
      p %>% layout(font = pf, margin = list(l = bl, r = br, t = bt, b = bb),
                   xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)
      ) %>% config(displayModeBar = FALSE)
    }

 
    output$vbox_kpi_overall <- renderValueBox({
      total_act <- sum(get_data()$PHIBH, na.rm = TRUE)
      total_target <- sum(get_kpi()$DOANH_THU, na.rm = TRUE)
      pct <- ifelse(total_target > 0, round(total_act / total_target * 100, 1), 0)
      valueBox(paste0(pct, "%"), "Tỷ lệ hoàn thành doanh thu", icon = icon("check-double"),
               color = ifelse(pct >= 100, "green", ifelse(pct >= 80, "yellow", "red")))
    })

    output$vbox_kpi_sl <- renderValueBox({
      total_target_sl <- sum(get_kpi()$SLHD, na.rm = TRUE)
      pct <- ifelse(total_target_sl > 0, round(nrow(get_data()) / total_target_sl * 100, 1), 0)
      valueBox(paste0(pct, "%"), "Tỷ lệ hoàn thành số lượng", icon = icon("file-alt"), color = "blue")
    })

    output$vbox_kpi_gap <- renderValueBox({
      gap <- sum(get_kpi()$DOANH_THU, na.rm = TRUE) - sum(get_data()$PHIBH, na.rm = TRUE)
      valueBox(format_vnd(abs(gap)),
               ifelse(gap > 0, "Còn thiếu so với KPI", "Vượt KPI"),
               icon = icon(ifelse(gap > 0, "arrow-down", "arrow-up")),
               color = ifelse(gap > 0, "red", "green"))
    })


    output$plot_kpi_monthly <- renderPlotly({
      act_sum <- get_data() %>% mutate(YM = format(NGAY_KY_HD, "%Y%m")) %>%
        group_by(YM) %>% summarise(Actual = sum(PHIBH, na.rm = TRUE), .groups = "drop")
      kpi_sum <- get_kpi() %>% group_by(YM = as.character(THANGNAM)) %>%
        summarise(Target = sum(DOANH_THU, na.rm = TRUE), .groups = "drop")

      df_combo <- full_join(act_sum, kpi_sum, by = "YM") %>% arrange(YM) %>%
        mutate(Label = paste0(substr(YM, 5, 6), "/", substr(YM, 1, 4)))

      plot_ly(df_combo, x = ~Label) %>%
        add_bars(y = ~Actual, name = "Thực tế", marker = list(color = "#4A6FA5")) %>%
        add_bars(y = ~Target, name = "Mục tiêu (KPI)", marker = list(color = "#F06292", opacity = 0.6)) %>%
        layout(
          font = pf,
          yaxis = list(title = list(text = "Doanh thu (VNĐ)", font = list(size = 11)),
                       tickformat = ",.0f", gridcolor = "#EBEBEB"),
          xaxis = list(title = "", tickangle = -45, gridcolor = "#EBEBEB"),
          barmode = "group", bargap = 0.25,
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom"),
          margin = list(l = 10, r = 10, t = 40, b = 45)
        ) %>% config(displayModeBar = FALSE)
    })


    output$plot_kpi_channel <- renderPlotly({
      df_act <- get_data() %>% filter(TENKENHBAN != "Chưa xác định") %>%
        group_by(MA_KENHBAN, TENKENHBAN) %>% summarise(Actual = sum(PHIBH, na.rm = TRUE), .groups = "drop")
      df_kpi <- get_kpi() %>% group_by(MA_KENHBAN) %>%
        summarise(Target = sum(DOANH_THU, na.rm = TRUE), .groups = "drop")

      df_long <- left_join(df_act, df_kpi, by = "MA_KENHBAN") %>%
        select(TENKENHBAN, Actual, Target) %>%
        pivot_longer(cols = c(Actual, Target), names_to = "Type", values_to = "Value")

      p <- ggplot(df_long, aes(x = reorder(TENKENHBAN, Value), y = Value, fill = Type)) +
        geom_col(position = "dodge", width = 0.7) + coord_flip() +
        scale_fill_manual(values = c("Actual" = "#4A6FA5", "Target" = "#F06292"),
                          labels = c("Thực tế", "Mục tiêu")) +
        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0, 0.08))) +
        theme_minimal(base_family = "sans") +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),
          axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"),
          legend.title = element_blank(),
          plot.margin = margin(0, 0, 0, 0)
        ) + labs(x = "", y = "Doanh thu (VNĐ)")

      ggplotly(p) %>%
        layout(font = pf, margin = list(l = 10, r = 10, t = 40, b = 20),
               xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE),
               legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom")
        ) %>% config(displayModeBar = FALSE)
    })


    output$plot_kpi_product <- renderPlotly({
      df_prod <- get_data() %>%
        filter(TENGOISANPHAM != "Chưa xác định") %>%
        group_by(TENGOISANPHAM) %>% summarise(Actual = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(Actual)) %>% head(10)

      p <- ggplot(df_prod, aes(x = reorder(TENGOISANPHAM, Actual), y = Actual)) +
        geom_col(fill = "#4A6FA5", width = 0.68) + coord_flip() +
        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0, 0.08))) +
        theme_minimal(base_family = "sans") +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),
          axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"),
          plot.margin = margin(0, 0, 0, 0)
        ) + labs(x = "", y = "Doanh thu thực tế (VNĐ)")

      ggplotly(p) %>% ly(bb = 25)
    })

    output$plot_kpi_pct_channel <- renderPlotly({
      df_act <- get_data() %>% filter(TENKENHBAN != "Chưa xác định") %>%
        group_by(MA_KENHBAN, TENKENHBAN) %>% summarise(Actual = sum(PHIBH, na.rm = TRUE), .groups = "drop")
      df_kpi <- get_kpi() %>% group_by(MA_KENHBAN) %>%
        summarise(Target = sum(DOANH_THU, na.rm = TRUE), .groups = "drop")

      df_pct <- left_join(df_act, df_kpi, by = "MA_KENHBAN") %>%
        mutate(Pct = ifelse(!is.na(Target) & Target > 0, round(Actual / Target * 100, 1), 0)) %>%
        mutate(Color = ifelse(Pct >= 100, "#2EC4B6", "#F06292"))

      p <- ggplot(df_pct, aes(x = reorder(TENKENHBAN, Pct), y = Pct, fill = Pct >= 100)) +
        geom_col(width = 0.62) +
        geom_hline(yintercept = 100, linetype = "dashed", color = "#EF5350", linewidth = 0.7) +
        geom_text(aes(label = paste0(Pct, "%")), hjust = -0.08, size = 3.3, color = "#303983", fontface = "bold") +
        coord_flip() +
        scale_fill_manual(values = c("TRUE" = "#2EC4B6", "FALSE" = "#F06292"), guide = "none") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
        theme_minimal(base_family = "sans") +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),
          axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"),
          plot.margin = margin(0, 0, 0, 0)
        ) + labs(x = "", y = "% Hoàn thành")

      ggplotly(p, tooltip = "y") %>% ly(bb = 25)
    })


    output$plot_kpi_gap_channel <- renderPlotly({
      df_act <- get_data() %>% filter(TENKENHBAN != "Chưa xác định") %>%
        group_by(MA_KENHBAN, TENKENHBAN) %>% summarise(Actual = sum(PHIBH, na.rm = TRUE), .groups = "drop")
      df_kpi <- get_kpi() %>% group_by(MA_KENHBAN) %>%
        summarise(Target = sum(DOANH_THU, na.rm = TRUE), .groups = "drop")

      df_gap <- left_join(df_act, df_kpi, by = "MA_KENHBAN") %>%
        mutate(Gap = Actual - ifelse(is.na(Target), 0, Target),
               Direction = ifelse(Gap >= 0, "Vượt", "Thiếu"))

      p <- ggplot(df_gap, aes(x = reorder(TENKENHBAN, Gap), y = Gap, fill = Direction)) +
        geom_col(width = 0.62) +
        geom_hline(yintercept = 0, color = "#303983", linewidth = 0.5) +
        coord_flip() +
        scale_fill_manual(values = c("Vượt" = "#2EC4B6", "Thiếu" = "#F06292"), guide = "none") +
        scale_y_continuous(labels = label_trieu) +
        theme_minimal(base_family = "sans") +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),
          axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"),
          plot.margin = margin(0, 0, 0, 0)
        ) + labs(x = "", y = "Chênh lệch (VNĐ)")

      ggplotly(p) %>% ly(bb = 25)
    })


    output$table_kpi_detail <- renderDT({
      df_act <- get_data() %>% mutate(YM = format(NGAY_KY_HD, "%Y%m")) %>%
        group_by(YM, MA_KENHBAN, TENKENHBAN) %>%
        summarise(Actual_DT = sum(PHIBH, na.rm = TRUE), Actual_SL = n(), .groups = "drop")
      df_kpi <- get_kpi() %>% group_by(YM = as.character(THANGNAM), MA_KENHBAN) %>%
        summarise(Target_DT = sum(DOANH_THU, na.rm = TRUE), Target_SL = sum(SLHD, na.rm = TRUE), .groups = "drop")

      df_detail <- full_join(df_act, df_kpi, by = c("YM", "MA_KENHBAN")) %>%
        mutate(
          Pct_DT = ifelse(!is.na(Target_DT) & Target_DT > 0, round(Actual_DT / Target_DT * 100, 1), NA),
          Pct_SL = ifelse(!is.na(Target_SL) & Target_SL > 0, round(Actual_SL / Target_SL * 100, 1), NA)
        ) %>%
        arrange(YM, MA_KENHBAN) %>%
        select(YM, TENKENHBAN, Actual_DT, Target_DT, Pct_DT, Actual_SL, Target_SL, Pct_SL)

      datatable(df_detail,
        colnames = c("Tháng", "Kênh bán", "DT thực tế", "DT mục tiêu", "% Hoàn thành DT",
                     "SL thực tế", "SL mục tiêu", "% Hoàn thành SL"),
        filter = "top", rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE,
          initComplete = JS(
            "function(settings, json) {",
            "  $(this.api().table().header()).css({",
            "    'background-color':'#303983','color':'#fff',",
            "    'font-family':'Helvetica Neue,Helvetica,Arial,sans-serif','font-weight':'600'",
            "  });",
            "  $(this.api().table().body()).css({",
            "    'font-family':'Helvetica Neue,Helvetica,Arial,sans-serif'",
            "  });",
            "}"
          )
        )
      ) %>%
        formatCurrency(c("Actual_DT", "Target_DT"), currency = "", interval = 3, mark = ".", digits = 0) %>%
        formatStyle("Pct_DT",
          backgroundColor = styleInterval(c(80, 100), c("#FADBD8", "#FCF3CF", "#D5F5E3")),
          fontWeight = "bold")
    })
  })
}