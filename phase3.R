phase3UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    h2("Phần 3: Phân tích chuyên sâu sản phẩm và kênh bán",
       style = "color:#303983; font-weight:700; margin-bottom:4px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),
    p("Phân tích cấu trúc doanh thu, xác định dòng sản phẩm chủ lực và hiệu quả kênh phân phối.",
      style = "color:#6C7A89; margin-bottom:18px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),

    fluidRow(
      valueBoxOutput(ns("vbox_avg_premium"), width = 4),
      valueBoxOutput(ns("vbox_top_product"), width = 4),
      valueBoxOutput(ns("vbox_top_channel"), width = 4)
    ),

    fluidRow(
      box(title = NULL, width = 8, status = "primary", solidHeader = FALSE,
          tags$h4("So sánh tỷ trọng 2 mảng theo chi nhánh",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Biểu đồ đối xứng 2 bên với thang đo độc lập. Giúp thấy rõ độ mạnh yếu của từng chi nhánh ở cả mảng doanh thu lớn và nhỏ.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_grouped_bar_matrix"), height = "480px")),
      box(title = NULL, width = 4, status = "primary", solidHeader = FALSE,
          tags$h4("Cơ cấu doanh thu theo kênh bán",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Tỷ trọng GWP của từng kênh bán chính.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_channel_pie"), height = "440px"))
    ),

    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Phân phối phí BH theo nhóm sản phẩm (Violin + Box)",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Xác định ngưỡng phí phổ biến và độ phân tán của từng nhóm.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_premium_dist"), height = "390px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Hiệu suất Top 15 kênh bán chi tiết",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Xếp hạng 15 kênh bán chi tiết có GWP cao nhất.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_detail_channel_bar"), height = "390px"))
    ),

    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Top 10 gói sản phẩm: Doanh thu & Tỷ trọng tích lũy",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Bar = GWP từng gói, nhãn = % tích lũy. Cho thấy mấy gói tạo ra phần lớn doanh thu.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_top10_cumulative"), height = "390px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Cơ cấu nhóm sản phẩm theo kênh bán (Stacked %)",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Mỗi cột = 100%. Cho thấy kênh nào tập trung vào nhóm sản phẩm nào.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_channel_product_stack"), height = "390px"))
    ),

    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Phí bình quân/gói theo nhóm sản phẩm (APPP)",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Nhóm nào có giá trị trung bình/gói cao nhất? Nhóm nào volume lớn nhưng giá trị thấp?",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_appp_by_group"), height = "390px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Số lượng gói BH và GWP theo nhóm sản phẩm",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Trục X = số gói, trục Y = phí bình quân, kích thước = tổng GWP.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_bubble_product"), height = "390px"))
    ),

    fluidRow(
      box(title = NULL, width = 12, status = "primary", solidHeader = FALSE,
          tags$h4("Bảng tổng hợp hiệu suất kinh doanh đa chiều",
                   style = "color:#303983; font-weight:700; margin:0 0 6px 0; font-family:'Helvetica Neue',sans-serif;"),
          DTOutput(ns("table_deep_stats")))
    )
  )
}

phase3Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {

    get_data <- reactive({ req(all_data()); all_data()$master })
    pf <- list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", color = "#4A4A4A")
    pal <- c("#4A6FA5", "#2EC4B6", "#7E57C2", "#F06292", "#FFCA28", "#55bded", "#66BB6A", "#EF5350", "#95A5A6")
    ly <- function(p, bl = 10, bb = 10, bt = 10, br = 10) {
      p %>% layout(font = pf, margin = list(l = bl, r = br, t = bt, b = bb),
                   xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)) %>% config(displayModeBar = FALSE)
    }

    output$vbox_avg_premium <- renderValueBox({
      valueBox(format_vnd(mean(get_data()$PHIBH, na.rm = TRUE)), "Phí bình quân/gói (APPP)", icon = icon("dollar-sign"), color = "blue")
    })
    output$vbox_top_product <- renderValueBox({
      top <- get_data() %>% filter(TENGOISANPHAM != "Chưa xác định") %>%
        group_by(TENGOISANPHAM) %>% summarise(v = sum(PHIBH)) %>% arrange(desc(v)) %>% slice(1)
      valueBox(top$TENGOISANPHAM, "SP doanh thu cao nhất", icon = icon("star"), color = "blue")
    })
    output$vbox_top_channel <- renderValueBox({
      top <- get_data() %>% filter(TENKENHBAN != "Chưa xác định") %>%
        group_by(TENKENHBAN) %>% summarise(v = sum(PHIBH)) %>% arrange(desc(v)) %>% slice(1)
      valueBox(top$TENKENHBAN, "Kênh dẫn đầu", icon = icon("trophy"), color = "teal")
    })

    output$plot_grouped_bar_matrix <- renderPlotly({
      # 1. Lọc và tính tổng data
      df_matrix <- get_data() %>%
        filter(TEN_CN != "Chưa xác định", TEN_CN != "Hội sở", NHOMSANPHAM != "Chưa xác định") %>%
        group_by(TEN_CN, NHOMSANPHAM) %>% 
        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(TEN_CN)) 
      
      # Chuyển đổi dữ liệu ra đơn vị "Tỷ" để trục hiển thị đẹp hơn
      df_matrix <- df_matrix %>% mutate(GWP_Ty = GWP / 1e9)
      
      # 2. Tách data làm 2 nhánh
      df_bb <- df_matrix %>% filter(NHOMSANPHAM == "Bảo hiểm bắt buộc")
      df_tn <- df_matrix %>% filter(NHOMSANPHAM == "Bảo hiểm tự nguyện")
      
      # 3. Cánh Trái (Bắt buộc)
      p1 <- plot_ly(df_bb, x = ~GWP_Ty, y = ~TEN_CN, type = 'bar', orientation = 'h',
                    name = "Bảo hiểm bắt buộc", marker = list(color = "#4A6FA5"),
                    hovertemplate = "%{x:,.1f} tỷ<extra></extra>") %>%
        layout(xaxis = list(autorange = "reversed", title = "", gridcolor = "#EBEBEB", zeroline = FALSE, ticksuffix = " tỷ"),
               yaxis = list(side = "right", title = "", tickfont = list(family = "'Helvetica Neue',sans-serif", size = 11, color = "#4A4A4A")))
      
      # 4. Cánh Phải (Tự nguyện)
      p2 <- plot_ly(df_tn, x = ~GWP_Ty, y = ~TEN_CN, type = 'bar', orientation = 'h',
                    name = "Bảo hiểm tự nguyện", marker = list(color = "#2EC4B6"),
                    hovertemplate = "%{x:,.1f} tỷ<extra></extra>") %>%
        layout(xaxis = list(title = "", gridcolor = "#EBEBEB", zeroline = FALSE, ticksuffix = " tỷ"),
               yaxis = list(showticklabels = FALSE)) 
      
      # 5. Ghép 2 biểu đồ (Ép margin nhỏ lại còn 0.08 để biểu đồ khít vào nhau)
      subplot(p1, p2, shareY = FALSE, titleX = FALSE, margin = 0.08) %>%
        layout(font = list(family = "'Helvetica Neue',sans-serif", color = "#4A4A4A"),
               margin = list(l = 10, r = 10, t = 40, b = 20),
               showlegend = TRUE,
               legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.15, yanchor = "bottom", font = list(size = 10)),
               hovermode = "y unified") %>% 
        config(displayModeBar = FALSE)
    })

    output$plot_channel_pie <- renderPlotly({
      df_chan <- get_data() %>% filter(TENKENHBAN != "Chưa xác định") %>%
        group_by(TENKENHBAN) %>% summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>% arrange(desc(GWP))
      plot_ly(df_chan, labels = ~TENKENHBAN, values = ~GWP, type = "pie", textinfo = "percent", hole = 0.45,
              marker = list(colors = pal[1:nrow(df_chan)], line = list(color = "#FFFFFF", width = 1.5)),
              textfont = list(family = "'Helvetica Neue',sans-serif", size = 11, color = "#FFFFFF")) %>%
        layout(font = pf, showlegend = TRUE,
               legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom", font = list(size = 10)),
               margin = list(l = 10, r = 10, t = 45, b = 10)) %>% config(displayModeBar = FALSE)
    })

    output$plot_premium_dist <- renderPlotly({
      df <- get_data() %>% filter(PHIBH > 0, NHOMSANPHAM != "Chưa xác định")
      p <- ggplot(df, aes(x = NHOMSANPHAM, y = PHIBH, fill = NHOMSANPHAM)) +
        geom_violin(alpha = 0.65, scale = "width", color = NA) +
        geom_boxplot(width = 0.13, fill = "white", alpha = 0.85, color = "#303983", outlier.shape = NA, linewidth = 0.4) +
        scale_y_log10(labels = label_trieu) + scale_fill_manual(values = pal) +
        theme_minimal(base_family = "sans") +
        theme(axis.text.x = element_text(angle = 22, hjust = 1, face = "bold", size = 9, color = "#4A4A4A"),
              panel.grid.major.y = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) +
        labs(x = "", y = "Phí BH (VNĐ) — Log")
      ggplotly(p) %>% ly(bl = 60, bb = 55)
    })

    output$plot_detail_channel_bar <- renderPlotly({
      df_det <- get_data() %>% filter(TENKENHBANCHITIET != "Chưa xác định") %>%
        group_by(TENKENHBANCHITIET) %>% summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>% top_n(15, GWP)
      p <- ggplot(df_det, aes(x = reorder(TENKENHBANCHITIET, GWP), y = GWP)) +
        geom_col(fill = "#4A6FA5", width = 0.68) + coord_flip() +
        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0, 0.08))) +
        theme_minimal(base_family = "sans") +
        theme(axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) +
        labs(x = "", y = "GWP (VNĐ)")
      ggplotly(p) %>% ly(bl = 10, bb = 25)
    })

    output$plot_top10_cumulative <- renderPlotly({
      df_top <- get_data() %>%
        filter(TENGOISANPHAM != "Chưa xác định", !is.na(TENGOISANPHAM)) %>%
        group_by(TENGOISANPHAM) %>% summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(GWP)) %>% head(10) %>%
        mutate(Cum_Pct = round(cumsum(GWP) / sum(get_data()$PHIBH, na.rm = TRUE) * 100, 1),
               Pct = round(GWP / sum(get_data()$PHIBH, na.rm = TRUE) * 100, 1))

      p <- ggplot(df_top, aes(x = reorder(TENGOISANPHAM, GWP), y = GWP)) +
        geom_col(fill = "#4A6FA5", width = 0.68) +
        geom_text(aes(label = paste0(Pct, "% | Σ", Cum_Pct, "%")),
                  hjust = -0.03, size = 3, color = "#303983", fontface = "bold") +
        coord_flip() +
        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0, 0.35))) +
        theme_minimal(base_family = "sans") +
        theme(axis.text.y = element_text(face = "bold", size = 8.5, color = "#4A4A4A"),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) +
        labs(x = "", y = "GWP (VNĐ)")
      ggplotly(p, tooltip = "y") %>% ly(bl = 10, bb = 25)
    })

    output$plot_channel_product_stack <- renderPlotly({
      df_stack <- get_data() %>%
        filter(TENKENHBAN != "Chưa xác định", NHOMSANPHAM != "Chưa xác định") %>%
        group_by(TENKENHBAN, NHOMSANPHAM) %>% summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")
      p <- ggplot(df_stack, aes(x = TENKENHBAN, y = GWP, fill = NHOMSANPHAM)) +
        geom_col(position = "fill", width = 0.65) +
        scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.02))) +
        scale_fill_manual(values = pal) +
        theme_minimal(base_family = "sans") +
        theme(axis.text.x = element_text(angle = 30, hjust = 1, face = "bold", size = 9, color = "#4A4A4A"),
              panel.grid.major.x = element_blank(),
              panel.grid.major.y = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(), legend.title = element_blank(),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) +
        labs(x = "", y = "Tỷ trọng (%)")
      ggplotly(p) %>%
        layout(font = pf, margin = list(l = 10, r = 10, t = 40, b = 55),
               xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE),
               legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom", font = list(size = 9))
        ) %>% config(displayModeBar = FALSE)
    })

    output$plot_appp_by_group <- renderPlotly({
      df_appp <- get_data() %>%
        filter(NHOMSANPHAM != "Chưa xác định") %>%
        group_by(NHOMSANPHAM) %>%
        summarise(APPP = mean(PHIBH, na.rm = TRUE), SL = n(), .groups = "drop")
      p <- ggplot(df_appp, aes(x = reorder(NHOMSANPHAM, APPP), y = APPP, fill = NHOMSANPHAM)) +
        geom_col(width = 0.62) +
        geom_text(aes(label = label_trieu(APPP)), hjust = -0.05, size = 3.2, color = "#303983", fontface = "bold") +
        coord_flip() +
        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0, 0.25))) +
        scale_fill_manual(values = pal) +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(), legend.position = "none",
              axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) +
        labs(x = "", y = "Phí bình quân/gói (VNĐ)")
      ggplotly(p, tooltip = "y") %>% ly(bl = 10, bb = 25)
    })

    output$plot_bubble_product <- renderPlotly({
      df_bub <- get_data() %>%
        filter(NHOMSANPHAM != "Chưa xác định") %>%
        group_by(NHOMSANPHAM) %>%
        summarise(Volume = n(), Revenue = sum(PHIBH, na.rm = TRUE), APPP = Revenue / Volume, .groups = "drop")

      p <- ggplot(df_bub, aes(x = Volume, y = APPP, size = Revenue, color = NHOMSANPHAM, text = NHOMSANPHAM)) +
        geom_point(alpha = 0.8) +
        scale_y_continuous(labels = label_trieu) +
        scale_x_continuous(labels = function(x) formatC(x, format = "d", big.mark = ".")) +
        scale_size_continuous(range = c(5, 22), guide = "none") +
        scale_color_manual(values = pal) +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.4),
              panel.grid.minor = element_blank(), legend.title = element_blank(),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"),
              axis.text = element_text(color = "#6C7A89"), plot.margin = margin(0,0,0,0)) +
        labs(x = "Số lượng gói BH", y = "Phí bình quân (VNĐ)")
      ggplotly(p, tooltip = c("text", "x", "y")) %>%
        layout(font = pf, margin = list(l = 10, r = 10, t = 40, b = 20),
               xaxis = list(automargin = TRUE, gridcolor = "#EBEBEB"),
               yaxis = list(automargin = TRUE, gridcolor = "#EBEBEB"),
               legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom", font = list(size = 9))
        ) %>% config(displayModeBar = FALSE)
    })

    output$table_deep_stats <- renderDT({
      df_stat <- get_data() %>% filter(TEN_CN != "Chưa xác định") %>%
        group_by(TEN_CN, NHOMSANPHAM, TENKENHBAN) %>%
        summarise(SL_Goi = n(), Tong_GWP = sum(PHIBH, na.rm = TRUE), APPP = round(Tong_GWP / SL_Goi, 0), .groups = "drop") %>%
        arrange(desc(Tong_GWP))
      datatable(df_stat,
        colnames = c("Chi nhánh", "Nhóm SP", "Kênh bán", "Số gói", "Tổng GWP", "Phí BQ/gói"),
        filter = "top", rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE,
          initComplete = JS("function(s,j){$(this.api().table().header()).css({'background-color':'#303983','color':'#fff','font-family':'Helvetica Neue,sans-serif','font-weight':'600'});$(this.api().table().body()).css({'font-family':'Helvetica Neue,sans-serif'});}"))) %>%
        formatCurrency(c("Tong_GWP", "APPP"), currency = "", interval = 3, mark = ".", digits = 0) %>%
        formatRound("SL_Goi", digits = 0, interval = 3, mark = ".")
    })
  })
}