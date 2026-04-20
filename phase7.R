phase7UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    h2("Phần 7: Chiến lược kinh doanh thực chiến",
       style = "color:#303983; font-weight:700; margin-bottom:4px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),
    p("Phân tích BCG Matrix, cơ hội Up-sell/Cross-sell và tiềm năng tăng trưởng dựa trên dữ liệu.",
      style = "color:#6C7A89; margin-bottom:18px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),

    fluidRow(
      box(title = NULL, width = 12, status = "primary", solidHeader = FALSE,
          tags$h4("Ma trận BCG: Nhóm sản phẩm (Volume × Phí trung bình × Doanh thu)",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Kích thước bubble = tổng GWP. Đường đứt = trung bình. Góc phần tư: Star / Cash Cow / Dấu hỏi / Dog.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_bcg"), height = "480px"))
    ),

    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Cross-sell: Nhóm sản phẩm × Xếp hạng khách hàng",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Heatmap: vùng đậm = nhiều gói bán ra → cơ hội cross-sell ở vùng nhạt.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_cross_sell"), height = "390px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Tỷ lệ tái tục theo nhóm sản phẩm",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Nhóm nào giữ chân khách hàng tốt nhất? Nhóm nào cần cải thiện?",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_renewal_rate"), height = "390px"))
    ),

    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Phí bảo hiểm trung bình theo xếp hạng khách hàng",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Khách hàng hạng cao chi trả phí bình quân cao hơn bao nhiêu?",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_avg_premium_xhkh"), height = "390px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Số gói trung bình mỗi khách hàng theo xếp hạng",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Hạng cao có cross-sell nhiều hơn không? (Gói TB/KH)",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_avg_packages_xhkh"), height = "390px"))
    ),

    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Tỷ lệ hợp đồng Mới vs Tái tục theo nhóm sản phẩm",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Nhóm nào chủ yếu là HĐ mới? Nhóm nào đã có nền tái tục?",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_new_vs_renew_group"), height = "390px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Doanh thu theo nhóm sản phẩm: Mới vs Tái tục (Stacked)",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Phần đóng góp GWP từ HĐ mới và tái tục tại mỗi nhóm SP.",
                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_gwp_new_renew_stack"), height = "390px"))
    ),

    fluidRow(
      box(title = NULL, width = 12, status = "primary", solidHeader = FALSE,
          tags$h4("Cơ hội Up-sell: Khách hàng phí thấp nhưng giá trị tài sản cao",
                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Xe giá trị > 500 triệu, phí BH < 3 triệu — dấu hiệu thiếu bảo vệ, cần tiếp cận up-sell.",
                  style = "color:#6C7A89; font-size:12px; font-style:italic; margin-bottom:6px;"),
          DTOutput(ns("table_upsell_leads")))
    )
  )
}

phase7Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    get_data <- reactive({ req(all_data()); all_data()$master })
    pf <- list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", color = "#4A4A4A")
    pal <- c("#4A6FA5", "#2EC4B6", "#7E57C2", "#F06292", "#FFCA28", "#55bded", "#66BB6A", "#EF5350", "#95A5A6")
    ly <- function(p, bl = 10, bb = 10, bt = 10, br = 10) {
      p %>% layout(font = pf, margin = list(l = bl, r = br, t = bt, b = bb),
                   xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)) %>% config(displayModeBar = FALSE)
    }
    output$plot_bcg <- renderPlotly({
      df_strat <- get_data() %>%
        filter(NHOMSANPHAM != "Chưa xác định", !is.na(NHOMSANPHAM)) %>%
        group_by(NHOMSANPHAM) %>%
        summarise(Volume = n(), Revenue = sum(PHIBH, na.rm = TRUE), Avg_Premium = Revenue / Volume, .groups = "drop")
      avg_vol <- mean(df_strat$Volume); avg_prem <- mean(df_strat$Avg_Premium)
      df_strat <- df_strat %>%
        mutate(Quadrant = case_when(
          Volume >= avg_vol & Avg_Premium >= avg_prem ~ "Ngôi sao (Star)",
          Volume >= avg_vol & Avg_Premium <  avg_prem ~ "Bò sữa (Cash Cow)",
          Volume <  avg_vol & Avg_Premium >= avg_prem ~ "Dấu hỏi (?)",
          TRUE ~ "Chó (Dog)"))

      plot_ly(df_strat, x = ~Volume, y = ~Avg_Premium, size = ~Revenue, color = ~Quadrant,
              text = ~paste0(NHOMSANPHAM, "\nVolume: ", formatC(Volume, format = "d", big.mark = "."),
                             "\nPhí TB: ", label_trieu(Avg_Premium), "\nGWP: ", label_trieu(Revenue)),
              hoverinfo = "text", type = "scatter", mode = "markers",
              sizes = c(60, 250), # Tăng kích thước bubble ở đây
              marker = list(opacity = 0.8, line = list(color = "#303983", width = 1.5)),
              colors = c("Ngôi sao (Star)" = "#2EC4B6", "Bò sữa (Cash Cow)" = "#4A6FA5",
                         "Dấu hỏi (?)" = "#FFCA28", "Chó (Dog)" = "#F06292")) %>%
        add_annotations(x = df_strat$Volume, y = df_strat$Avg_Premium,
                        text = df_strat$NHOMSANPHAM, showarrow = FALSE,
                        font = list(size = 11, color = "#303983", family = "Helvetica Neue"),
                        yshift = 30) %>% # Tăng khoảng cách nhãn tên sản phẩm so với bubble
        layout(
          font = pf,
          shapes = list(
            list(type = "line", x0 = avg_vol, x1 = avg_vol, y0 = 0, y1 = max(df_strat$Avg_Premium) * 1.15,
                 line = list(color = "#303983", width = 1, dash = "dash")),
            list(type = "line", x0 = 0, x1 = max(df_strat$Volume) * 1.1, y0 = avg_prem, y1 = avg_prem,
                 line = list(color = "#303983", width = 1, dash = "dash"))
          ),
          annotations = list(
            # Dùng hệ tọa độ 'paper' để ép các nhãn phân loại ra 4 góc, tránh đè lên điểm dữ liệu
            list(x = 0.98, y = 0.98, xref = "paper", yref = "paper", text = "★ Star",
                 showarrow = FALSE, font = list(size = 13, color = "#2EC4B6", family = "Helvetica Neue"), xanchor = "right"),
            list(x = 0.98, y = 0.05, xref = "paper", yref = "paper", text = "Cash Cow",
                 showarrow = FALSE, font = list(size = 13, color = "#4A6FA5", family = "Helvetica Neue"), xanchor = "right"),
            list(x = 0.02, y = 0.98, xref = "paper", yref = "paper", text = "? Dấu hỏi",
                 showarrow = FALSE, font = list(size = 13, color = "#FFCA28", family = "Helvetica Neue"), xanchor = "left"),
            list(x = 0.02, y = 0.05, xref = "paper", yref = "paper", text = "Dog",
                 showarrow = FALSE, font = list(size = 13, color = "#F06292", family = "Helvetica Neue"), xanchor = "left")
          ),
          xaxis = list(title = list(text = "Số lượng gói BH (Volume)", font = list(size = 11, color = "#4A4A4A")),
                       gridcolor = "#EBEBEB", separatethousands = TRUE),
          yaxis = list(title = list(text = "Phí BH trung bình (VNĐ)", font = list(size = 11, color = "#4A4A4A")),
                       gridcolor = "#EBEBEB", tickformat = ",.0f"),
          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.22, yanchor = "top", font = list(size = 10)), # Hạ thấp legend
          margin = list(l = 10, r = 10, t = 15, b = 80) # Tăng lề dưới tránh đè legend vào title
        ) %>% config(displayModeBar = FALSE)
    })

    output$plot_cross_sell <- renderPlotly({
      df_cs <- get_data() %>% filter(NHOMSANPHAM != "Chưa xác định") %>%
        group_by(NHOMSANPHAM, XHKH) %>% summarise(N = n(), .groups = "drop")
      p <- ggplot(df_cs, aes(x = XHKH, y = NHOMSANPHAM, fill = N)) +
        geom_tile(color = "white", linewidth = 0.6) +
        scale_fill_gradient(low = "#f0f2f7", high = "#303983") +
        theme_minimal(base_family = "sans") +
        theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold", size = 9, color = "#4A4A4A"),
              axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),
              panel.grid = element_blank(), axis.title = element_blank(), plot.margin = margin(0,0,0,0))
      ggplotly(p, tooltip = c("x","y","fill")) %>% ly(bl = 10, bb = 45)
    })

    output$plot_renewal_rate <- renderPlotly({
      df_rr <- get_data() %>% filter(NHOMSANPHAM != "Chưa xác định") %>%
        group_by(NHOMSANPHAM) %>%
        summarise(Total = n(), Pct = round(sum(LOAI_HD == "Tái tục", na.rm = TRUE) / Total * 100, 1), .groups = "drop")
      p <- ggplot(df_rr, aes(x = reorder(NHOMSANPHAM, Pct), y = Pct)) +
        geom_col(fill = "#2EC4B6", width = 0.62) +
        geom_text(aes(label = paste0(Pct, "%")), hjust = -0.08, size = 3.3, color = "#303983", fontface = "bold") +
        coord_flip() + scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(), axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) +
        labs(x = "", y = "Tỷ lệ tái tục (%)")
      ggplotly(p, tooltip = "y") %>% ly(bb = 25)
    })

    output$plot_avg_premium_xhkh <- renderPlotly({
      df_avg <- get_data() %>% group_by(XHKH) %>% summarise(Avg_Phi = mean(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        mutate(XHKH = factor(XHKH, levels = c("Đồng", "Bạc", "Vàng", "Bạch kim", "Kim cương")))
      p <- ggplot(df_avg, aes(x = XHKH, y = Avg_Phi, fill = XHKH)) +
        geom_col(width = 0.6) +
        geom_text(aes(label = label_trieu(Avg_Phi)), vjust = -0.5, size = 3.3, color = "#303983", fontface = "bold") +
        scale_fill_manual(values = c("Đồng"="#95A5A6","Bạc"="#BDC3C7","Vàng"="#FFCA28","Bạch kim"="#7E57C2","Kim cương"="#303983")) +
        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0, 0.18))) +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(), legend.position = "none",
              axis.text.x = element_text(face = "bold", size = 10, color = "#4A4A4A"),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) +
        labs(x = "", y = "Phí BH trung bình (VNĐ)")
      ggplotly(p, tooltip = "y") %>% ly(bb = 25)
    })

    output$plot_avg_packages_xhkh <- renderPlotly({
      df_pkg <- get_data() %>% group_by(XHKH, MA_KH) %>% summarise(N_goi = n(), .groups = "drop") %>%
        group_by(XHKH) %>% summarise(Avg_goi = round(mean(N_goi), 2), .groups = "drop") %>%
        mutate(XHKH = factor(XHKH, levels = c("Đồng", "Bạc", "Vàng", "Bạch kim", "Kim cương")))
      p <- ggplot(df_pkg, aes(x = XHKH, y = Avg_goi, fill = XHKH)) +
        geom_col(width = 0.6) +
        geom_text(aes(label = Avg_goi), vjust = -0.5, size = 3.5, color = "#303983", fontface = "bold") +
        scale_fill_manual(values = c("Đồng"="#95A5A6","Bạc"="#BDC3C7","Vàng"="#FFCA28","Bạch kim"="#7E57C2","Kim cương"="#303983")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(), legend.position = "none",
              axis.text.x = element_text(face = "bold", size = 10, color = "#4A4A4A"),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) +
        labs(x = "", y = "Số gói TB / khách hàng")
      ggplotly(p, tooltip = "y") %>% ly(bb = 25)
    })

    output$plot_new_vs_renew_group <- renderPlotly({
      df_nr <- get_data() %>% filter(NHOMSANPHAM != "Chưa xác định") %>%
        group_by(NHOMSANPHAM, LOAI_HD) %>% summarise(N = n(), .groups = "drop")
      p <- ggplot(df_nr, aes(x = NHOMSANPHAM, y = N, fill = LOAI_HD)) +
        geom_col(position = "fill", width = 0.62) +
        scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.02))) +
        scale_fill_manual(values = c("Mới" = "#4A6FA5", "Tái tục" = "#FFCA28")) +
        theme_minimal(base_family = "sans") +
        theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold", size = 9, color = "#4A4A4A"),
              panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(), legend.title = element_blank(),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) +
        labs(x = "", y = "Tỷ lệ (%)")
      ggplotly(p) %>%
        layout(font = pf, margin = list(l = 10, r = 10, t = 40, b = 55),
               legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom")) %>% config(displayModeBar = FALSE)
    })

    output$plot_gwp_new_renew_stack <- renderPlotly({
      df_gwp <- get_data() %>% filter(NHOMSANPHAM != "Chưa xác định") %>%
        group_by(NHOMSANPHAM, LOAI_HD) %>% summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")
      p <- ggplot(df_gwp, aes(x = reorder(NHOMSANPHAM, -GWP), y = GWP, fill = LOAI_HD)) +
        geom_col(width = 0.62) +
        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0, 0.05))) +
        scale_fill_manual(values = c("Mới" = "#4A6FA5", "Tái tục" = "#FFCA28")) +
        theme_minimal(base_family = "sans") +
        theme(axis.text.x = element_text(angle = 25, hjust = 1, face = "bold", size = 9, color = "#4A4A4A"),
              panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(), legend.title = element_blank(),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) +
        labs(x = "", y = "GWP (VNĐ)")
      ggplotly(p) %>%
        layout(font = pf, margin = list(l = 10, r = 10, t = 40, b = 55),
               legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom")) %>% config(displayModeBar = FALSE)
    })

    output$table_upsell_leads <- renderDT({
      upsell <- get_data() %>%
        filter(!is.na(GIA_TIEN_XE), GIA_TIEN_XE > 500000000, PHIBH < 3000000) %>%
        select(MA_KH, MA_HD, TEN_DONG_XE, TEN_HANG_XE, GIA_TIEN_XE, PHIBH, TENGOISANPHAM, TEN_CN) %>%
        arrange(desc(GIA_TIEN_XE)) %>% head(100)
      datatable(upsell,
        colnames = c("Mã KH","Mã HĐ","Dòng xe","Hãng","Giá trị xe","Phí BH","Gói đang dùng","Chi nhánh"),
        rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE,
          initComplete = JS("function(s,j){$(this.api().table().header()).css({'background-color':'#303983','color':'#fff','font-family':'Helvetica Neue,sans-serif','font-weight':'600'});$(this.api().table().body()).css({'font-family':'Helvetica Neue,sans-serif'});}"))) %>%
        formatCurrency(c("GIA_TIEN_XE","PHIBH"), currency = "", interval = 3, mark = ".", digits = 0)
    })
  })
}