
phase5UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    h2("Phần 5: Phân tích mạng lưới chi nhánh và hiệu suất vận hành",
       style = "color:#303983; font-weight:700; margin-bottom:4px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),
    p("Đánh giá sức khỏe mạng lưới, phân tích hiệu suất chi nhánh và nhân viên, giám sát KPI.",
      style = "color:#6C7A89; margin-bottom:18px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),

    # ===== SECTION 1: TỔNG QUAN CN =====
    tags$h3("1. Tổng quan mạng lưới chi nhánh",
            style = "color:#4A6FA5; font-weight:700; font-size:17px; margin:10px 0 14px 0; font-family:'Helvetica Neue',sans-serif; border-bottom:2px solid #EBEBEB; padding-bottom:6px;"),
    fluidRow(
      box(title = NULL, width = 7, status = "primary", solidHeader = FALSE,
          tags$h4("Top chi nhánh theo doanh thu GWP",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Xếp hạng các chi nhánh có tổng GWP cao nhất.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_branch_gwp_bar"), height = "380px")),
      box(title = NULL, width = 5, status = "primary", solidHeader = FALSE,
          tags$h4("Cơ cấu xếp hạng KH tại Top 3 CN",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Tỷ lệ Đồng/Bạc/Vàng/Bạch kim/Kim cương tại 3 CN lớn nhất.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_branch_xhkh"), height = "380px"))
    ),

    # ===== SECTION 2: XU HƯỚNG =====
    tags$h3("2. Xu hướng Doanh thu & Cơ cấu Sản phẩm",
            style = "color:#4A6FA5; font-weight:700; font-size:17px; margin:20px 0 14px 0; font-family:'Helvetica Neue',sans-serif; border-bottom:2px solid #EBEBEB; padding-bottom:6px;"),
    fluidRow(
      box(title = NULL, width = 12, status = "primary", solidHeader = FALSE,
          tags$h4("Biến động Doanh thu Top 5 CN theo Tháng (Loại Hội sở)",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Multi-line chart: Top 5 CN có doanh thu cao nhất, loại trừ Hội sở. Đường cong mượt mà giúp nhận diện xu hướng rõ hơn.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_branch_trend_multi"), height = "380px"))
    ),
    fluidRow(
      box(title = NULL, width = 12, status = "primary", solidHeader = FALSE,
          tags$h4("Ma trận Sản phẩm × Chi nhánh (Top 10 CN)",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Heatmap: Doanh thu GWP theo nhóm sản phẩm tại 10 CN có doanh thu cao nhất. Màu sắc đậm nhạt thể hiện mức độ đóng góp của từng nhóm SP tại mỗi CN.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_matrix_prod_branch"), height = "440px"))
    ),

    # ===== SECTION 3: KPI =====
    tags$h3("3. Quản trị mục tiêu (KPI)",
            style = "color:#4A6FA5; font-weight:700; font-size:17px; margin:20px 0 14px 0; font-family:'Helvetica Neue',sans-serif; border-bottom:2px solid #EBEBEB; padding-bottom:6px;"),
    fluidRow(
      box(title = NULL, width = 9, status = "primary", solidHeader = FALSE,
          tags$h4("Giám sát doanh thu: Thực tế vs mục tiêu (KPI)",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Combo chart: Cột thể hiện doanh thu thực tế theo tháng, đường nét đứt thể hiện mục tiêu KPI. Cho phép chọn lọc theo kênh bán để đánh giá hiệu quả từng kênh.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_kpi_combo"), height = "420px")),
      box(title = NULL, width = 3, status = "primary", solidHeader = FALSE,
          tags$h4("Bộ lọc", style = "color:#303983; font-weight:700; margin:0 0 8px 0; font-family:'Helvetica Neue',sans-serif;"),
          selectInput(ns("select_kpi_channel"), "Chọn Kênh:", choices = "Tất cả"),
          hr(),
          valueBoxOutput(ns("vbox_kpi_pct"), width = 12))
    ),

    # ===== SECTION 4: NHÂN VIÊN =====
    tags$h3("4. Phân tích năng suất nhân viên",
            style = "color:#4A6FA5; font-weight:700; font-size:17px; margin:20px 0 14px 0; font-family:'Helvetica Neue',sans-serif; border-bottom:2px solid #EBEBEB; padding-bottom:6px;"),
    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Phân phối năng suất NV",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Histogram thể hiện số lượng nhân viên theo các mức năng suất GWP, sử dụng thang log để hiển thị rõ hơn sự phân hóa giữa các nhân viên có năng suất thấp và cao.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_staff_density"), height = "370px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("So sánh GWP theo kênh bán",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Tổng doanh thu GWP của từng kênh bán chính.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_channel_comp"), height = "370px"))
    ),
    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Năng suất Trung bình/NV theo Chi Nhánh (Top 15)",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("GWP trung bình mỗi nhân viên tại từng chi nhánh.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_productivity_cn"), height = "370px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Bảng xếp hạng Top 15 nhân viên xuất sắc",
                   style = "color:#303983; font-weight:700; margin:0 0 6px 0; font-family:'Helvetica Neue',sans-serif;"),
          DTOutput(ns("table_top_staff")))
    )
  )
}

phase5Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {

    get_data <- reactive({ req(all_data()); all_data()$master })
    get_kpi  <- reactive({ req(all_data()); all_data()$kpi })

    observe({
      kb <- get_data() %>% filter(TENKENHBAN != "Chưa xác định") %>% pull(TENKENHBAN) %>% unique() %>% sort()
      updateSelectInput(session, "select_kpi_channel", choices = c("Tất cả", kb))
    })

    pf  <- list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", color = "#4A4A4A")
    pal <- c("#4A6FA5", "#2EC4B6", "#7E57C2", "#F06292", "#FFCA28", "#55bded", "#66BB6A")

    ly <- function(p, bl = 10, bb = 10, bt = 10, br = 10) {
      p %>% layout(
        font = pf, margin = list(l = bl, r = br, t = bt, b = bb),
        xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)
      ) %>% config(displayModeBar = FALSE)
    }

    output$plot_branch_gwp_bar <- renderPlotly({
      df_cn <- get_data() %>%
        filter(TEN_CN != "Chưa xác định") %>%
        group_by(TEN_CN) %>% summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        top_n(15, GWP)

      p <- ggplot(df_cn, aes(x = reorder(TEN_CN, GWP), y = GWP)) +
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
        ) + labs(x = "", y = "GWP (VND)")

      ggplotly(p) %>% ly(bb = 25)
    })


    output$plot_branch_xhkh <- renderPlotly({
      df <- get_data()
      top3 <- df %>% filter(TEN_CN != "Chưa xác định") %>%
        group_by(TEN_CN) %>% summarise(v = sum(PHIBH)) %>% top_n(3, v) %>% pull(TEN_CN)

      df_xh <- df %>% filter(TEN_CN %in% top3) %>%
        group_by(TEN_CN, XHKH) %>% summarise(N = n_distinct(MA_KH), .groups = "drop")

      p <- ggplot(df_xh, aes(x = TEN_CN, y = N, fill = XHKH)) +
        geom_col(position = "fill", width = 0.6) +
        scale_fill_manual(values = pal) +
        scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.02))) +
        theme_minimal(base_family = "sans") +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(face = "bold", size = 9, color = "#4A4A4A"),
          axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"),
          legend.title = element_blank(),
          plot.margin = margin(0, 0, 0, 0)
        ) + labs(x = "", y = "Ty le")

      ggplotly(p) %>%
        layout(font = pf, margin = list(l = 10, r = 10, t = 40, b = 20),
               xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE),
               legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom",
                             font = list(size = 9))
        ) %>% config(displayModeBar = FALSE)
    })


    output$plot_branch_trend_multi <- renderPlotly({
      df <- get_data()
      top5 <- df %>%
        filter(TEN_CN != "Chưa xác định", !grepl("Hội sở|Hoi so", TEN_CN, ignore.case = TRUE)) %>%
        group_by(TEN_CN) %>% summarise(v = sum(PHIBH)) %>% top_n(5, v) %>% pull(TEN_CN)

      df_trend <- df %>% filter(TEN_CN %in% top5) %>%
        group_by(THANG, TEN_CN) %>% summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")

      p <- ggplot(df_trend, aes(x = THANG, y = GWP, color = TEN_CN)) +
        geom_line(linewidth = 1.15) +
        scale_color_manual(values = pal[1:5]) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0.02, 0.08))) +
        theme_minimal(base_family = "sans") +
        theme(
          panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.4),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"),
          axis.text = element_text(color = "#6C7A89"),
          plot.margin = margin(0, 0, 0, 0)
        ) + labs(x = "Date", y = "GWP (VND)")

      ggplotly(p) %>%
        style(line = list(shape = "spline", smoothing = 1.3)) %>%
        layout(font = pf, margin = list(l = 10, r = 10, t = 40, b = 15),
               xaxis = list(automargin = TRUE, gridcolor = "#EBEBEB"),
               yaxis = list(automargin = TRUE, gridcolor = "#EBEBEB"),
               legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom",
                             font = list(size = 10))
        ) %>% config(displayModeBar = FALSE)
    })


    output$plot_matrix_prod_branch <- renderPlotly({
      df <- get_data()
      top10 <- df %>% filter(TEN_CN != "Chưa xác định") %>%
        group_by(TEN_CN) %>% summarise(v = sum(PHIBH)) %>% top_n(10, v) %>% pull(TEN_CN)

      df_mat <- df %>%
        filter(TEN_CN %in% top10, NHOMSANPHAM != "Chưa xác định") %>%
        group_by(TEN_CN, NHOMSANPHAM) %>% summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")

      p <- ggplot(df_mat, aes(x = TEN_CN, y = NHOMSANPHAM, fill = GWP)) +
        geom_tile(color = "white", linewidth = 0.6) +
        scale_fill_gradient(low = "#f0f2f7", high = "#303983", labels = label_trieu) +
        theme_minimal(base_family = "sans") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 9, color = "#4A4A4A"),
          axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),
          panel.grid = element_blank(), axis.title = element_blank(),
          plot.margin = margin(0, 0, 0, 0)
        )

      ggplotly(p) %>% ly(bl = 10, bb = 65)
    })


    output$plot_kpi_combo <- renderPlotly({
      df_act <- get_data(); df_kpi <- get_kpi()

      if (input$select_kpi_channel != "Tất cả") {
        kb_code <- df_act %>% filter(TENKENHBAN == input$select_kpi_channel) %>%
          pull(MA_KENHBAN) %>% unique() %>% first()
        df_act <- df_act %>% filter(TENKENHBAN == input$select_kpi_channel)
        df_kpi <- df_kpi %>% filter(MA_KENHBAN == kb_code)
      }

      act_sum <- df_act %>% mutate(YM = format(NGAY_KY_HD, "%Y%m")) %>%
        group_by(YM) %>% summarise(Actual = sum(PHIBH, na.rm = TRUE), .groups = "drop")
      kpi_sum <- df_kpi %>% group_by(YM = as.character(THANGNAM)) %>%
        summarise(Target = sum(DOANH_THU, na.rm = TRUE), .groups = "drop")

      df_combo <- full_join(act_sum, kpi_sum, by = "YM") %>% arrange(YM) %>%
        mutate(Label = paste0(substr(YM, 5, 6), "/", substr(YM, 1, 4)))

      plot_ly(df_combo, x = ~Label) %>%
        add_bars(y = ~Actual, name = "Thuc Te", marker = list(color = "#4A6FA5")) %>%
        add_lines(y = ~Target, name = "Muc Tieu (KPI)",
                  line = list(color = "#F06292", width = 3, dash = "dash", shape = "spline")) %>%
        layout(
          font = pf,
          yaxis = list(title = list(text = "Doanh Thu (VND)", font = list(size = 11)),
                       tickformat = ",.0f", gridcolor = "#EBEBEB"),
          xaxis = list(title = "", tickangle = -45, gridcolor = "#EBEBEB"),
          barmode = "group", bargap = 0.3,
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom"),
          margin = list(l = 10, r = 10, t = 40, b = 45)
        ) %>% config(displayModeBar = FALSE)
    })

    output$vbox_kpi_pct <- renderValueBox({
      total_act <- sum(get_data()$PHIBH, na.rm = TRUE)
      total_target <- sum(get_kpi()$DOANH_THU, na.rm = TRUE)
      pct <- ifelse(total_target > 0, round(total_act / total_target * 100, 1), 0)
      valueBox(paste0(pct, "%"), "Tien do KPI", icon = icon("check-double"),
               color = ifelse(pct >= 100, "green", ifelse(pct >= 80, "yellow", "red")))
    })


    output$plot_staff_density <- renderPlotly({
      df_staff <- get_data() %>%
        group_by(MA_NV) %>% summarise(Total_GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        filter(Total_GWP > 0)

      p <- ggplot(df_staff, aes(x = Total_GWP)) +
        geom_histogram(bins = 45, fill = "#2EC4B6", alpha = 0.8, color = "white", linewidth = 0.2) +
        scale_x_log10(labels = label_trieu) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
        theme_minimal(base_family = "sans") +
        theme(
          panel.grid.major = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
          panel.grid.minor = element_blank(),
          axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"),
          axis.text = element_text(color = "#6C7A89"),
          plot.margin = margin(0, 0, 0, 0)
        ) + labs(x = "Tong GWP/NV (VND) — Log", y = "So NV")

      ggplotly(p) %>% ly(bb = 25)
    })


    output$plot_channel_comp <- renderPlotly({
      df_chan <- get_data() %>% filter(TENKENHBAN != "Chưa xác định") %>%
        group_by(TENKENHBAN) %>% summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")

      p <- ggplot(df_chan, aes(x = reorder(TENKENHBAN, GWP), y = GWP)) +
        geom_col(fill = "#4A6FA5", width = 0.62) + coord_flip() +
        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0, 0.08))) +
        theme_minimal(base_family = "sans") +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),
          axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"),
          plot.margin = margin(0, 0, 0, 0)
        ) + labs(x = "", y = "GWP (VND)")

      ggplotly(p) %>% ly(bb = 25)
    })


    output$plot_productivity_cn <- renderPlotly({
      df_prod <- get_data() %>%
        filter(TEN_CN != "Chưa xác định", !grepl("Hội sở|Hoi so|TCT", TEN_CN, ignore.case = TRUE)) %>%
        group_by(TEN_CN, MA_NV) %>%
        summarise(GWP_NV = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        group_by(TEN_CN) %>%
        summarise(Avg_GWP_NV = mean(GWP_NV), N_NV = n(), .groups = "drop") %>%
        top_n(15, Avg_GWP_NV)

      p <- ggplot(df_prod, aes(x = reorder(TEN_CN, Avg_GWP_NV), y = Avg_GWP_NV)) +
        geom_col(fill = "#FFCA28", width = 0.65) + coord_flip() +
        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0, 0.08))) +
        theme_minimal(base_family = "sans") +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),
          axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"),
          plot.margin = margin(0, 0, 0, 0)
        ) + labs(x = "", y = "GWP TB/NV (VND)")

      ggplotly(p) %>% ly(bb = 25)
    })


    output$table_top_staff <- renderDT({
      df <- get_data() %>%
        filter(!grepl("Hội sở|Hoi so|TCT", TEN_CN, ignore.case = TRUE))

      # Tìm kênh chính của mỗi NV (kênh có GWP cao nhất)
      kenh_chinh <- df %>%
        group_by(MA_NV, TENKENHBAN) %>%
        summarise(gwp_kenh = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        group_by(MA_NV) %>%
        slice_max(gwp_kenh, n = 1, with_ties = FALSE) %>%
        select(MA_NV, Kenh_Chinh = TENKENHBAN)

      df_top <- df %>%
        group_by(MA_NV, TEN_CN) %>%
        summarise(GWP = sum(PHIBH, na.rm = TRUE), SL_Goi = n(),
                  So_Kenh = n_distinct(TENKENHBAN), .groups = "drop") %>%
        left_join(kenh_chinh, by = "MA_NV") %>%
        arrange(desc(GWP)) %>% head(15)

      datatable(df_top,
        colnames = c("Mã NV", "Chi nhánh", "Tổng GWP", "Số gói", "Số kênh", "Kênh chính"),
        rownames = FALSE,
        options = list(dom = "t", pageLength = 15,
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
        formatCurrency("GWP", currency = "", interval = 3, mark = ".", digits = 0) %>%
        formatRound("SL_Goi", digits = 0, interval = 3, mark = ".")
    })
  })
}