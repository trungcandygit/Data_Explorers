phase6UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
    h2("Phần 6: Phân tích khách hàng tiềm năng và Dự báo nhu cầu",
       style = "color:#303983; font-weight:700; margin-bottom:4px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),
    p("Phân tích toàn bộ 400 nghìn khách hàng: ai đã mua, ai chưa mua, và cơ hội chuyển đổi ở đâu.",
      style = "color:#6C7A89; margin-bottom:18px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),
    fluidRow(
      valueBoxOutput(ns("vb_total_kh_all"), width = 3), valueBoxOutput(ns("vb_kh_bought"), width = 3),
      valueBoxOutput(ns("vb_kh_not_bought"), width = 3), valueBoxOutput(ns("vb_conversion_rate"), width = 3)
    ),
    fluidRow(
      box(title = NULL, width = 5, status = "primary", solidHeader = FALSE,
          tags$h4("Tỷ lệ đã mua vs chưa mua", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Trong toàn bộ KH hệ thống, bao nhiêu % đã phát sinh giao dịch?", style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_bought_vs_not"), height = "360px")),
      box(title = NULL, width = 7, status = "primary", solidHeader = FALSE,
          tags$h4("Phân bố xếp hạng: Đã mua vs Chưa mua", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Hạng nào còn nhiều KH chưa được khai thác?", style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_xhkh_compare"), height = "360px"))
    ),
    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Nhóm tuổi KH chưa mua", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Nhóm tuổi nào có nhiều KH tiềm năng nhất?", style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_age_not_bought"), height = "370px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Nghề nghiệp KH chưa mua (Top 15)", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Nghề nào có tệp KH tiềm năng lớn nhất?", style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_job_not_bought"), height = "370px"))
    ),
    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Tỷ lệ chuyển đổi theo xếp hạng", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Hạng nào có tỷ lệ mua cao nhất?", style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_conversion_by_xhkh"), height = "370px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Doanh thu tiềm năng nếu chuyển đổi KH chưa mua", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Ước tính: KH chưa mua × chi tiêu TB hạng tương ứng.", style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_potential_revenue"), height = "370px"))
    ),
    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Giới tính KH chưa mua", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Tỷ lệ nam/nữ trong tệp KH tiềm năng.", style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_gender_not_bought"), height = "360px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Tỷ lệ chuyển đổi theo nhóm tuổi", style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Nhóm tuổi nào dễ chuyển đổi nhất?", style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),
          plotlyOutput(ns("plot_conversion_by_age"), height = "360px"))
    ),
    fluidRow(
      box(title = NULL, width = 12, status = "primary", solidHeader = FALSE,
          tags$h4("Bảng tổng hợp tiềm năng theo xếp hạng", style = "color:#303983; font-weight:700; margin:0 0 6px 0; font-family:'Helvetica Neue',sans-serif;"),
          DTOutput(ns("table_potential_summary")))
    )
  )
}

phase6Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    get_master <- reactive({ req(all_data()); all_data()$master })
    all_kh <- reactive({ req(file.exists("kh_full.rds")); readRDS("kh_full.rds") })
    kh_tagged <- reactive({
      bought_ids <- unique(get_master()$MA_KH)
      all_kh() %>% mutate(Status = ifelse(MA_KH %in% bought_ids, "Đã mua", "Chưa mua"))
    })
    avg_spend <- reactive({
      get_master() %>% group_by(XHKH, MA_KH) %>% summarise(T = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%
        group_by(XHKH) %>% summarise(Avg = mean(T), .groups = "drop")
    })
    pf <- list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", color = "#4A4A4A")

    output$vb_total_kh_all <- renderValueBox({ valueBox(formatC(nrow(all_kh()), format = "d", big.mark = "."), "Tổng KH hệ thống", color = "blue") })
    output$vb_kh_bought <- renderValueBox({ valueBox(formatC(sum(kh_tagged()$Status == "Đã mua"), format = "d", big.mark = "."), "KH đã mua", color = "teal") })
    output$vb_kh_not_bought <- renderValueBox({ valueBox(formatC(sum(kh_tagged()$Status == "Chưa mua"), format = "d", big.mark = "."), "KH chưa mua", color = "yellow") })
    output$vb_conversion_rate <- renderValueBox({
      pct <- round(sum(kh_tagged()$Status == "Đã mua") / nrow(kh_tagged()) * 100, 1)
      valueBox(paste0(pct, "%"), "Tỷ lệ chuyển đổi", color = ifelse(pct >= 50, "green", "red"))
    })

    output$plot_bought_vs_not <- renderPlotly({
      df <- kh_tagged() %>% group_by(Status) %>% summarise(N = n(), .groups = "drop")
      plot_ly(df, labels = ~Status, values = ~N, type = "pie", hole = 0.5, textinfo = "percent+value",
              marker = list(colors = c("#4A6FA5","#FFCA28"), line = list(color = "#FFF", width = 1.5)),
              textfont = list(size = 12, color = "#303983", family = "Helvetica Neue")) %>%
        layout(font = pf, showlegend = TRUE, legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.05),
               margin = list(l = 10, r = 10, t = 10, b = 30)) %>% config(displayModeBar = FALSE)
    })

    output$plot_xhkh_compare <- renderPlotly({
      df <- kh_tagged() %>% group_by(XHKH, Status) %>% summarise(N = n(), .groups = "drop") %>%
        mutate(XHKH = factor(XHKH, levels = c("Đồng","Bạc","Vàng","Bạch kim","Kim cương")))
      p <- ggplot(df, aes(x = XHKH, y = N, fill = Status)) + geom_col(position = "dodge", width = 0.7) +
        scale_fill_manual(values = c("Đã mua"="#4A6FA5","Chưa mua"="#FFCA28")) +
        scale_y_continuous(labels = function(x) formatC(x, format = "d", big.mark = "."), expand = expansion(mult = c(0, 0.08))) +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(), legend.title = element_blank(),
              axis.text.x = element_text(face = "bold", size = 10, color = "#4A4A4A"),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) + labs(x = "", y = "Số KH")
      ggplotly(p) %>% layout(font = pf, margin = list(l = 10, r = 10, t = 40, b = 20),
               legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom")) %>% config(displayModeBar = FALSE)
    })

    output$plot_age_not_bought <- renderPlotly({
      df <- kh_tagged() %>% filter(Status == "Chưa mua", !is.na(NHOM_TUOI)) %>% group_by(NHOM_TUOI) %>% summarise(N = n(), .groups = "drop")
      p <- ggplot(df, aes(x = reorder(NHOM_TUOI, N), y = N)) + geom_col(fill = "#7E57C2", width = 0.62) +
        geom_text(aes(label = formatC(N, format = "d", big.mark = ".")), hjust = -0.05, size = 3.2, color = "#303983", fontface = "bold") +
        coord_flip() + scale_y_continuous(expand = expansion(mult = c(0, 0.22)), labels = function(x) formatC(x, format = "d", big.mark = ".")) +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(), axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) + labs(x = "", y = "Số KH chưa mua")
      ggplotly(p, tooltip = "y") %>% layout(font = pf, margin = list(l = 10, r = 20, t = 10, b = 25),
                                             xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)) %>% config(displayModeBar = FALSE)
    })

    output$plot_job_not_bought <- renderPlotly({
      df <- kh_tagged() %>% filter(Status == "Chưa mua") %>% group_by(NGHENGHIEP) %>% summarise(N = n(), .groups = "drop") %>% top_n(15, N)
      p <- ggplot(df, aes(x = reorder(NGHENGHIEP, N), y = N)) + geom_col(fill = "#2EC4B6", width = 0.62) +
        geom_text(aes(label = formatC(N, format = "d", big.mark = ".")), hjust = -0.05, size = 3, color = "#303983", fontface = "bold") +
        coord_flip() + scale_y_continuous(expand = expansion(mult = c(0, 0.22)), labels = function(x) formatC(x, format = "d", big.mark = ".")) +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(), axis.text.y = element_text(face = "bold", size = 8.5, color = "#4A4A4A"),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) + labs(x = "", y = "Số KH chưa mua")
      ggplotly(p, tooltip = "y") %>% layout(font = pf, margin = list(l = 10, r = 20, t = 10, b = 25),
                                             xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)) %>% config(displayModeBar = FALSE)
    })

    output$plot_conversion_by_xhkh <- renderPlotly({
      df <- kh_tagged() %>% group_by(XHKH) %>%
        summarise(Total = n(), Pct = round(sum(Status == "Đã mua") / Total * 100, 1), .groups = "drop") %>%
        mutate(XHKH = factor(XHKH, levels = c("Đồng","Bạc","Vàng","Bạch kim","Kim cương")))
      p <- ggplot(df, aes(x = XHKH, y = Pct, fill = XHKH)) + geom_col(width = 0.6) +
        geom_text(aes(label = paste0(Pct, "%")), vjust = -0.5, size = 3.5, color = "#303983", fontface = "bold") +
        scale_fill_manual(values = c("Đồng"="#95A5A6","Bạc"="#BDC3C7","Vàng"="#FFCA28","Bạch kim"="#7E57C2","Kim cương"="#303983")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(), legend.position = "none",
              axis.text.x = element_text(face = "bold", size = 10, color = "#4A4A4A"),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) + labs(x = "", y = "Tỷ lệ chuyển đổi (%)")
      ggplotly(p, tooltip = "y") %>% layout(font = pf, margin = list(l = 10, r = 10, t = 10, b = 25),
                                             xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)) %>% config(displayModeBar = FALSE)
    })

    output$plot_potential_revenue <- renderPlotly({
      not_b <- kh_tagged() %>% filter(Status == "Chưa mua") %>% group_by(XHKH) %>% summarise(N = n(), .groups = "drop")
      df <- left_join(not_b, avg_spend(), by = "XHKH") %>% mutate(Pot = N * Avg,
            XHKH = factor(XHKH, levels = c("Đồng","Bạc","Vàng","Bạch kim","Kim cương")))
      p <- ggplot(df, aes(x = XHKH, y = Pot, fill = XHKH)) + geom_col(width = 0.6) +
        geom_text(aes(label = label_trieu(Pot)), vjust = -0.5, size = 3.2, color = "#303983", fontface = "bold") +
        scale_fill_manual(values = c("Đồng"="#95A5A6","Bạc"="#BDC3C7","Vàng"="#FFCA28","Bạch kim"="#7E57C2","Kim cương"="#303983")) +
        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0, 0.15))) +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(), legend.position = "none",
              axis.text.x = element_text(face = "bold", size = 10, color = "#4A4A4A"),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) + labs(x = "", y = "DT tiềm năng (VNĐ)")
      ggplotly(p, tooltip = "y") %>% layout(font = pf, margin = list(l = 10, r = 10, t = 10, b = 25),
                                             xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)) %>% config(displayModeBar = FALSE)
    })

    output$plot_gender_not_bought <- renderPlotly({
      df <- kh_tagged() %>% filter(Status == "Chưa mua") %>% group_by(GIOI_TINH) %>% summarise(N = n(), .groups = "drop")
      plot_ly(df, labels = ~GIOI_TINH, values = ~N, type = "pie", hole = 0.45, textinfo = "percent+label",
              marker = list(colors = c("#4A6FA5","#F06292","#95A5A6"), line = list(color = "#FFF", width = 1.5)),
              textfont = list(size = 12, color = "#303983", family = "Helvetica Neue")) %>%
        layout(font = pf, showlegend = FALSE, margin = list(l = 10, r = 10, t = 10, b = 10)) %>% config(displayModeBar = FALSE)
    })

    output$plot_conversion_by_age <- renderPlotly({
      df <- kh_tagged() %>% filter(!is.na(NHOM_TUOI)) %>% group_by(NHOM_TUOI) %>%
        summarise(Total = n(), Pct = round(sum(Status == "Đã mua") / Total * 100, 1), .groups = "drop")
      p <- ggplot(df, aes(x = reorder(NHOM_TUOI, Pct), y = Pct)) + geom_col(fill = "#55bded", width = 0.62) +
        geom_text(aes(label = paste0(Pct, "%")), hjust = -0.08, size = 3.3, color = "#303983", fontface = "bold") +
        coord_flip() + scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
        theme_minimal(base_family = "sans") +
        theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "#EBEBEB", linetype = "dashed", linewidth = 0.35),
              panel.grid.minor = element_blank(), axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),
              axis.title = element_text(face = "bold", size = 10, color = "#4A4A4A"), plot.margin = margin(0,0,0,0)) + labs(x = "", y = "Tỷ lệ chuyển đổi (%)")
      ggplotly(p, tooltip = "y") %>% layout(font = pf, margin = list(l = 10, r = 20, t = 10, b = 25),
                                             xaxis = list(automargin = TRUE), yaxis = list(automargin = TRUE)) %>% config(displayModeBar = FALSE)
    })

    output$table_potential_summary <- renderDT({
      not_b <- kh_tagged() %>% filter(Status == "Chưa mua") %>% group_by(XHKH) %>% summarise(KH_Chua = n(), .groups = "drop")
      bought <- kh_tagged() %>% filter(Status == "Đã mua") %>% group_by(XHKH) %>% summarise(KH_Da = n(), .groups = "drop")
      df <- left_join(not_b, bought, by = "XHKH") %>% left_join(avg_spend(), by = "XHKH") %>%
        mutate(Ty_le = round(KH_Da / (KH_Da + KH_Chua) * 100, 1), DT_Pot = KH_Chua * Avg) %>% arrange(desc(DT_Pot))
      datatable(df, colnames = c("Xếp hạng","KH chưa mua","KH đã mua","Chi tiêu TB","Tỷ lệ CD (%)","DT tiềm năng"),
        rownames = FALSE, options = list(dom = "t", pageLength = 10,
          initComplete = JS("function(s,j){$(this.api().table().header()).css({'background-color':'#303983','color':'#fff','font-family':'Helvetica Neue,sans-serif','font-weight':'600'});$(this.api().table().body()).css({'font-family':'Helvetica Neue,sans-serif'});}"))) %>%
        formatCurrency(c("Avg","DT_Pot"), currency = "", interval = 3, mark = ".", digits = 0) %>%
        formatRound(c("KH_Chua","KH_Da"), digits = 0, interval = 3, mark = ".")
    })
  })
}