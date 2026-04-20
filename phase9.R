phase9UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    h2("Phần 9: Quản trị khách hàng tiềm năng và tái tục",
       style = "color:#303983; font-weight:700; margin-bottom:4px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),
    p("Cảnh báo sớm hợp đồng sắp hết hạn, gợi ý tệp khách hàng tiềm năng và phân tích phễu chuyển đổi.",
      style = "color:#6C7A89; margin-bottom:18px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),
    fluidRow(
      valueBoxOutput(ns("vbox_expiring_30d"), width = 3), valueBoxOutput(ns("vbox_expiring_60d"), width = 3),
      valueBoxOutput(ns("vbox_high_value_leads"), width = 3), valueBoxOutput(ns("vbox_renewal_rate"), width = 3)
    ),
    fluidRow(
      box(title = NULL, width = 12, status = "primary", solidHeader = FALSE,
          tags$h4("Gói BH hết hạn trong 60 ngày tới", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Danh sách khách hàng cần liên hệ gấp để duy trì tỷ lệ tái tục.", style = "color:#EF5350; font-size:12px; font-weight:bold; font-style:italic; margin-bottom:6px;"),
          DTOutput(ns("table_renewal_leads")))
    ),
    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Tệp khách hàng tiềm năng (Hạng cao)", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Kim cương/Bạch kim — chưa đa dạng gói sản phẩm.", style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          DTOutput(ns("table_high_value_leads"))),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Phễu chuyển đổi dự kiến", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Từ tổng KH → hạng cao → sắp hết hạn → đã tái tục.", style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_lead_funnel"), height = "380px"))
    ),
    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Phân bố ngày hết hạn theo tháng (12 tháng tới)", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Bar = số gói hết hạn, line = xu hướng.", style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_expiry_timeline"), height = "380px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Doanh thu tái tục tiềm năng theo nhóm sản phẩm", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("GWP tiềm năng nếu tái tục thành công toàn bộ gói sắp hết hạn.", style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_renewal_value"), height = "380px"))
    ),
    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Tỷ lệ tái tục theo chi nhánh (Top 15)", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Chi nhánh nào giữ chân khách hàng tốt nhất?", style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_renewal_by_cn"), height = "380px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Phân bố thời hạn còn lại của hợp đồng (Histogram)", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Đường đứt đỏ = mốc 30, 60, 90 ngày.", style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_ttl_histogram"), height = "380px"))
    )
  )
}

phase9Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    get_data <- reactive({ req(all_data()); all_data()$master })
    today_date <- as.Date("2026-04-16")
    pf <- list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", color = "#4A4A4A")
    pal <- c("#303983", "#4A6FA5", "#2EC4B6", "#F06292", "#FFCA28", "#55bded")
    ly <- function(p, bl = 10, bb = 10, bt = 10, br = 10) {
      p %>% layout(font = pf, margin = list(l = bl, r = br, t = bt, b = bb),
                   xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)) %>% config(displayModeBar = FALSE)
    }

    output$vbox_expiring_30d <- renderValueBox({
      n <- get_data() %>% filter(NGAY_HET_HAN >= today_date, NGAY_HET_HAN <= today_date + 30) %>% nrow()
      valueBox(formatC(n, format = "d", big.mark = "."), "Hết hạn < 30 ngày", icon = icon("clock"), color = "red")
    })
    output$vbox_expiring_60d <- renderValueBox({
      n <- get_data() %>% filter(NGAY_HET_HAN >= today_date, NGAY_HET_HAN <= today_date + 60) %>% nrow()
      valueBox(formatC(n, format = "d", big.mark = "."), "Hết hạn < 60 ngày", icon = icon("hourglass-half"), color = "yellow")
    })
    output$vbox_high_value_leads <- renderValueBox({
      n <- get_data() %>% filter(XHKH %in% c("Kim cương", "Bạch kim")) %>% pull(MA_KH) %>% n_distinct()
      valueBox(formatC(n, format = "d", big.mark = "."), "KH hạng cao", icon = icon("gem"), color = "purple")
    })
    output$vbox_renewal_rate <- renderValueBox({
      df <- get_data(); pct <- round(sum(df$LOAI_HD == "Tái tục", na.rm = TRUE) / nrow(df) * 100, 1)
      valueBox(paste0(pct, "%"), "Tỷ lệ tái tục", icon = icon("sync"), color = "green")
    })

    output$table_renewal_leads <- renderDT({
      renewal <- get_data() %>%
        filter(NGAY_HET_HAN >= today_date, NGAY_HET_HAN <= (today_date + 60)) %>%
        mutate(So_ngay = as.numeric(NGAY_HET_HAN - today_date),
               Muc_do = case_when(So_ngay <= 7 ~ "Khẩn cấp", So_ngay <= 30 ~ "Cần xử lý", TRUE ~ "Theo dõi")) %>%
        select(MA_KH, MA_HD, TENGOISANPHAM, TEN_CN, PHIBH, NGAY_HET_HAN, So_ngay, Muc_do, SDT_NV) %>%
        arrange(So_ngay) %>% head(200)
      datatable(renewal, colnames = c("Mã KH","Mã HĐ","Gói SP","Chi nhánh","Phí BH","Ngày hết hạn","Số ngày còn","Mức độ","SĐT NV"),
        rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE,
          initComplete = JS("function(s,j){$(this.api().table().header()).css({'background-color':'#303983','color':'#fff','font-family':'Helvetica Neue,sans-serif','font-weight':'600'});$(this.api().table().body()).css({'font-family':'Helvetica Neue,sans-serif'});}"))) %>%
        formatCurrency("PHIBH", currency = "", interval = 3, mark = ".", digits = 0) %>%
        formatStyle("Muc_do", backgroundColor = styleEqual(c("Khẩn cấp","Cần xử lý","Theo dõi"), c("#EF5350","#FFCA28","#55bded")), color = "white", fontWeight = "bold")
    })

    output$table_high_value_leads <- renderDT({
      high_val <- get_data() %>% filter(XHKH %in% c("Kim cương", "Bạch kim")) %>%
        group_by(MA_KH, XHKH, TEN_CN) %>%
        summarise(So_goi = n(), Tong_Phi = sum(PHIBH, na.rm = TRUE), SP_da_co = paste(unique(NHOMSANPHAM), collapse = ", "), .groups = "drop") %>%
        arrange(desc(Tong_Phi)) %>% head(50)
      datatable(high_val, colnames = c("Mã KH","Xếp hạng","Chi nhánh","Số gói","Tổng phí","SP đã có"),
        rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE,
          initComplete = JS("function(s,j){$(this.api().table().header()).css({'background-color':'#303983','color':'#fff','font-family':'Helvetica Neue,sans-serif','font-weight':'600'});$(this.api().table().body()).css({'font-family':'Helvetica Neue,sans-serif'});}"))) %>%
        formatCurrency("Tong_Phi", currency = "", interval = 3, mark = ".", digits = 0)
    })

    output$plot_lead_funnel <- renderPlotly({
      df <- get_data()
      plot_ly(type = "funnel",
              y = c("Tổng KH unique", "KH hạng Vàng+", "KH sắp hết hạn (90d)", "KH đã tái tục"),
              x = c(n_distinct(df$MA_KH),
                    df %>% filter(XHKH %in% c("Kim cương","Bạch kim","Vàng")) %>% pull(MA_KH) %>% n_distinct(),
                    df %>% filter(NGAY_HET_HAN >= today_date, NGAY_HET_HAN <= today_date + 90) %>% pull(MA_KH) %>% n_distinct(),
                    df %>% filter(LOAI_HD == "Tái tục") %>% pull(MA_KH) %>% n_distinct()),
              textinfo = "value+percent initial", marker = list(color = pal[1:4])) %>%
        layout(font = pf, margin = list(l = 140, r = 20, t = 20, b = 20)) %>% config(displayModeBar = FALSE)
    })

    output$plot_expiry_timeline <- renderPlotly({
      df_exp <- get_data() %>% filter(NGAY_HET_HAN >= today_date, NGAY_HET_HAN <= today_date + 365) %>%
        mutate(Thang_HH = floor_date(NGAY_HET_HAN, "month")) %>%
        group_by(Thang_HH) %>% summarise(N = n(), .groups = "drop")
      plot_ly(df_exp, x = ~Thang_HH) %>%
        add_bars(y = ~N, name = "Số gói", marker = list(color = "#55bded", opacity = 0.6)) %>%
        add_lines(y = ~N, name = "Xu hướng", line = list(color = "#303983", width = 2, shape = "spline", smoothing = 1.3)) %>%
        layout(font = pf, margin = list(l = 10, r = 10, t = 40, b = 15),
               xaxis = list(automargin = TRUE, gridcolor = "#EBEBEB", tickformat = "%b %Y", dtick = "M2"),
               yaxis = list(automargin = TRUE, gridcolor = "#EBEBEB", title = list(text = "Số gói", font = list(size = 11)), separatethousands = TRUE),
               legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom"), bargap = 0.3) %>% config(displayModeBar = FALSE)
    })

    output$plot_renewal_value <- renderPlotly({
      df_rv <- get_data() %>% filter(NGAY_HET_HAN >= today_date, NGAY_HET_HAN <= today_date + 365, NHOMSANPHAM != "Chưa xác định") %>%
        group_by(NHOMSANPHAM) %>% summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")
      p <- ggplot(df_rv, aes(x = reorder(NHOMSANPHAM, GWP), y = GWP)) +
        geom_col(fill = "#2EC4B6", width = 0.62) + coord_flip() +
        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0, 0.08))) +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(), axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) +
        labs(x = "", y = "GWP tiềm năng (VNĐ)")
      ggplotly(p) %>% ly(bb = 25)
    })

    output$plot_renewal_by_cn <- renderPlotly({
      df_rr <- get_data() %>% filter(TEN_CN != "Chưa xác định", !grepl("Hội sở|TCT", TEN_CN, ignore.case = TRUE)) %>%
        group_by(TEN_CN) %>% summarise(Total = n(), Pct = round(sum(LOAI_HD == "Tái tục", na.rm = TRUE) / Total * 100, 1), .groups = "drop") %>%
        top_n(15, Total)
      p <- ggplot(df_rr, aes(x = reorder(TEN_CN, Pct), y = Pct)) +
        geom_col(fill = "#7E57C2", width = 0.62) +
        geom_text(aes(label = paste0(Pct, "%")), hjust = -0.08, size = 3.3, color = "#303983", fontface = "bold") +
        coord_flip() + scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(), axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) +
        labs(x = "", y = "Tỷ lệ tái tục (%)")
      ggplotly(p, tooltip = "y") %>% ly(bb = 25)
    })

    output$plot_ttl_histogram <- renderPlotly({
      df_ttl <- get_data() %>% filter(NGAY_HET_HAN >= today_date) %>%
        mutate(TTL = as.numeric(NGAY_HET_HAN - today_date)) %>% filter(TTL <= 365)
      p <- ggplot(df_ttl, aes(x = TTL)) +
        geom_histogram(binwidth = 15, fill = "#4A6FA5", alpha = 0.8, color = "white", linewidth = 0.2) +
        geom_vline(xintercept = c(30, 60, 90), linetype = "dashed", color = "#EF5350", linewidth = 0.6) +
        annotate("text", x = 30, y = Inf, label = "30d", vjust = 2, hjust = -0.2, color = "#EF5350", size = 3, fontface = "bold") +
        annotate("text", x = 60, y = Inf, label = "60d", vjust = 2, hjust = -0.2, color = "#EF5350", size = 3, fontface = "bold") +
        annotate("text", x = 90, y = Inf, label = "90d", vjust = 2, hjust = -0.2, color = "#EF5350", size = 3, fontface = "bold") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.08)), labels = function(x) formatC(x, format = "d", big.mark = ".")) +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.major.y = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) +
        labs(x = "Số ngày còn lại", y = "Số gói BH")
      ggplotly(p) %>% ly(bb = 25)
    })
  })
}