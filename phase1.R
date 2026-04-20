PAL <- list(
  navy      = "#1F3864",   
  steel     = "#4472C4",   
  teal      = "#5B9BD5",   
  purple    = "#9467BD",   
  pink      = "#D9D7F1",   
  skyblue   = "#5B9BD5",   
  green     = "#2CA02C",   
  gold      = "#FF7F0E",   
  red_soft  = "#D62728",   
  green_bar = "#2CA02C",   
  txt_main  = "#1F3864",   
  txt_light = "#4472C4",   
  grid_col  = "#D9D7F1",   
  bg        = "#D9D7F1"    
)

theme_pptx <- function(base_size = 11.5) {
  theme_minimal(base_size = base_size, base_family = "sans") %+replace%
    theme(
      text             = element_text(family = "sans", color = PAL$txt_main),
      plot.title       = element_text(face = "bold", color = PAL$navy, size = base_size + 2, hjust = 0,
                                      margin = margin(b = 6)),
      plot.subtitle    = element_text(color = PAL$txt_light, size = base_size - 1, margin = margin(b = 8)),
      axis.title       = element_text(face = "bold", size = base_size - 0.5, color = PAL$txt_main),
      axis.text        = element_text(size = base_size - 1.5, color = PAL$txt_main),
      panel.grid.major.y = element_line(color = PAL$grid_col, linetype = "dashed", linewidth = 0.35),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position  = "top",
      legend.title     = element_text(face = "bold", size = base_size - 1),
      legend.text      = element_text(size = base_size - 1.5),
      strip.text       = element_text(face = "bold"),
      plot.margin      = margin(2, 2, 2, 2)
    )
}

ly_layout <- function(p, bl = 70, bb = 40, bt = 10, br = 15) {
  p %>% layout(
    margin = list(l = bl, r = br, t = bt, b = bb),
    xaxis  = list(automargin = TRUE),
    yaxis  = list(automargin = TRUE),
    font   = list(family = "Helvetica Neue, Helvetica, Arial, sans-serif", color = PAL$txt_main)
  )
}

phase1UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,

    tags$style(HTML("
      .bg-blue { background-color: #1F3864 !important; }
      .bg-teal { background-color: #5B9BD5 !important; }
      .bg-yellow { background-color: #FF7F0E !important; }
      .bg-green { background-color: #2CA02C !important; }
      .small-box .icon-large { color: rgba(255, 255, 255, 0.2) !important; }
      .small-box { color: #FFFFFF !important; }
    ")),

    h2("Phần 1: Kiểm soát chất lượng và làm sạch dữ liệu",
       style = "color:#1F3864; font-weight:700; margin-bottom:4px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),
    p("Phân tích toàn diện về chất lượng dữ liệu: Missing, Outliers, bất thường logic và dữ liệu trùng lặp.",
      style = "color:#4472C4; margin-bottom:18px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),

    fluidRow(
      valueBoxOutput(ns("box_total_goi"), width = 3),
      valueBoxOutput(ns("box_total_hd"),  width = 3),
      valueBoxOutput(ns("box_total_kh"),  width = 3),
      valueBoxOutput(ns("box_missing_pct"), width = 3)
    ),

    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Tỷ lệ thiếu dữ liệu các cột", 
                   style = "color:#1F3864; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Các cột có tỷ lệ thiếu cao nhất",
                  style = "color:#4472C4; font-size:12px; margin-bottom:6px; font-family:'Helvetica Neue',sans-serif;"),
          plotlyOutput(ns("plot_missing"), height = "370px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Phân phối phí bảo hiểm",
                   style = "color:#1F3864; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("Ngoại lai theo quy tắc IQR x 1.5.",
                  style = "color:#4472C4; font-size:12px; margin-bottom:6px; font-family:'Helvetica Neue',sans-serif;"),
          plotlyOutput(ns("plot_outlier"), height = "370px"))
    ),

    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Thống kê phí bảo hiểm theo nhóm sản phẩm",
                   style = "color:#1F3864; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("So sánh phân phối phí.",
                  style = "color:#4472C4; font-size:12px; margin-bottom:6px; font-family:'Helvetica Neue',sans-serif;"),
          plotlyOutput(ns("plot_outlier_by_group"), height = "370px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Phân phối giá trị bảo hiểm",
                   style = "color:#1F3864; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("",
                  style = "color:#4472C4; font-size:12px; margin-bottom:6px; font-family:'Helvetica Neue',sans-serif;"),
          plotlyOutput(ns("plot_gia_bh_dist"), height = "370px"))
    ),

    fluidRow(
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Mật độ phí bảo hiểm mới và tái tục",
                   style = "color:#1F3864; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("So sánh phân phối giữa hợp đồng mới và tái tục",
                  style = "color:#4472C4; font-size:12px; margin-bottom:6px; font-family:'Helvetica Neue',sans-serif;"),
          plotlyOutput(ns("plot_density_overlay"), height = "370px")),
      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,
          tags$h4("Biến động SL gói BH theo tháng",
                   style = "color:#1F3864; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),
          tags$p("",
                  style = "color:#4472C4; font-size:12px; margin-bottom:6px; font-family:'Helvetica Neue',sans-serif;"),
          plotlyOutput(ns("plot_mom_change"), height = "370px"))
    ),

    fluidRow(
      box(title = NULL, width = 12, status = "primary", solidHeader = FALSE,
          tags$h4("Chất lượng dữ liệu đầu vào",
                   style = "color:#1F3864; font-weight:700; margin:0 0 6px 0; font-family:'Helvetica Neue',sans-serif;"),
          DTOutput(ns("table_quality_report")))
    ),

    fluidRow(
      box(title = NULL, width = 12, status = "primary", solidHeader = FALSE,
          tags$h4("Nhật ký Audit: Hợp đồng bất thường và ngoại lai",
                   style = "color:#1F3864; font-weight:700; margin:0 0 6px 0; font-family:'Helvetica Neue',sans-serif;"),
          DTOutput(ns("table_audit")))
    )
  )
}

phase1Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {

    get_data <- reactive({ req(all_data()); all_data()$master })

    output$box_total_goi <- renderValueBox({
      valueBox(format(nrow(get_data()), big.mark = "."), "Tổng số gói BH",
               icon = icon("database"), color = "blue")
    })
    output$box_total_hd <- renderValueBox({
      valueBox(format(n_distinct(get_data()$MA_HD), big.mark = "."), "Số lượng hợp đồng",
               icon = icon("file-contract"), color = "blue")
    })
    output$box_total_kh <- renderValueBox({
      valueBox(format(n_distinct(get_data()$MA_KH), big.mark = "."), "Khách hàng unique",
               icon = icon("users"), color = "teal")
    })
    output$box_missing_pct <- renderValueBox({
      df <- get_data()
      total_cells <- nrow(df) * ncol(df)
      na_n <- sum(is.na(df))
      cxd_n <- sum(sapply(df, function(x) if(is.character(x)) sum(x == "Chưa xác định", na.rm = TRUE) else 0))
      pct <- round((na_n + cxd_n) / total_cells * 100, 2)
      valueBox(paste0(pct, "%"), "Tỷ lệ missing",
               icon = icon("exclamation-triangle"), color = ifelse(pct > 5, "yellow", "green"))
    })

    output$plot_missing <- renderPlotly({
      df <- get_data()
      miss_df <- data.frame(
        Cot = names(df),
        NA_Count = colSums(is.na(df)),
        CXD_Count = sapply(df, function(x) if(is.character(x)) sum(x == "Chưa xác định", na.rm = TRUE) else 0),
        stringsAsFactors = FALSE
      ) %>%
        mutate(Total = NA_Count + CXD_Count, Pct = round(Total / nrow(df) * 100, 2)) %>%
        filter(Total > 0) %>% arrange(desc(Total)) %>% head(15)

      if (nrow(miss_df) == 0) miss_df <- data.frame(Cot = "Sach 100%", Total = 0, Pct = 0)

      p <- ggplot(miss_df, aes(x = reorder(Cot, Total), y = Pct)) +
        geom_col(fill = PAL$steel, width = 0.72) +
        geom_text(aes(label = paste0(Pct, "%")), hjust = -0.08, size = 3.3,
                  color = PAL$navy, fontface = "bold") +
        coord_flip() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
        theme_pptx() +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(color = PAL$grid_col, linetype = "dashed", linewidth = 0.3)) +
        labs(x = NULL, y = "Ty le Missing (%)")

      ggplotly(p, tooltip = c("y")) %>% ly_layout(bl = 100)
    })

    output$plot_outlier <- renderPlotly({
      df <- get_data() %>% filter(PHIBH > 0)

      n_out <- {
        q1 <- quantile(df$PHIBH, 0.25); q3 <- quantile(df$PHIBH, 0.75)
        iqr <- q3 - q1; sum(df$PHIBH > q3 + 1.5 * iqr | df$PHIBH < q1 - 1.5 * iqr)
      }

      p <- ggplot(df, aes(y = PHIBH, x = "Phí BH")) +
        geom_boxplot(fill = PAL$skyblue, color = PAL$navy, width = 0.45,
                     outlier.colour = PAL$pink, outlier.size = 1.3) +
        scale_y_log10(labels = label_trieu) +
        theme_pptx() +
        theme(axis.text.x = element_text(face = "bold", size = 11)) +
        labs(x = NULL, y = "Phí BH",
             subtitle = paste0(format(n_out, big.mark = "."), " outliers detected (IQR x 1.5)"))

      ggplotly(p) %>% ly_layout(bl = 70, bb = 35)
    })

    output$plot_outlier_by_group <- renderPlotly({
      df <- get_data() %>% filter(PHIBH > 0, !is.na(NHOMSANPHAM))

      pal6 <- c(PAL$navy, PAL$steel, PAL$teal, PAL$purple, PAL$green, PAL$gold, PAL$red_soft, PAL$pink)

      p <- ggplot(df, aes(x = NHOMSANPHAM, y = PHIBH, fill = NHOMSANPHAM)) +
        geom_boxplot(color = PAL$navy, linewidth = 0.35,
                     outlier.colour = PAL$pink, outlier.size = 0.7) +
        scale_fill_manual(values = pal6) +
        scale_y_log10(labels = label_trieu) +
        theme_pptx() +
        theme(axis.text.x = element_text(angle = 22, hjust = 1, face = "bold"),
              legend.position = "none") +
        labs(x = NULL, y = "Phi BH (VND) — Log")

      ggplotly(p) %>% ly_layout(bl = 65, bb = 70)
    })

    output$plot_gia_bh_dist <- renderPlotly({
      df <- get_data() %>% filter(GIA_BH > 0)

      p <- ggplot(df, aes(x = GIA_BH)) +
        geom_histogram(bins = 45, fill = PAL$teal, color = "#FFFFFF", linewidth = 0.2) +
        scale_x_log10(labels = label_trieu) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                           labels = function(x) format(x, big.mark = ".")) +
        theme_pptx() +
        theme(panel.grid.major.x = element_blank()) +
        labs(x = "Gia tri BH (VND) — Log", y = "So luong")

      ggplotly(p) %>% ly_layout(bl = 55, bb = 42)
    })

    output$plot_density_overlay <- renderPlotly({
      df <- get_data() %>% filter(PHIBH > 0, LOAI_HD %in% c("Mới", "Tái tục"))

      p <- ggplot(df, aes(x = PHIBH, fill = LOAI_HD, color = LOAI_HD)) +
        geom_density(linewidth = 1) +
        scale_x_log10(labels = label_trieu) +
        scale_fill_manual(values = c("Mới" = PAL$steel, "Tái tục" = PAL$gold)) +
        scale_color_manual(values = c("Mới" = PAL$steel, "Tái tục" = PAL$gold)) +
        theme_pptx() +
        theme(legend.title = element_blank(),
              panel.grid.major.x = element_blank()) +
        labs(x = "Phí bảo hiểm", y = "Mật độ")

      ggplotly(p) %>% ly_layout(bl = 50, bb = 42)
    })

    output$plot_mom_change <- renderPlotly({
      df_m <- get_data() %>%
        group_by(THANG) %>% summarise(N = n(), .groups = "drop") %>%
        arrange(THANG) %>%
        mutate(Pct = round((N / lag(N) - 1) * 100, 1),
               Direction = ifelse(Pct >= 0, "Tang", "Giam"))

      df_m <- df_m %>% filter(!is.na(Pct))

      p <- ggplot(df_m, aes(x = THANG, y = Pct, fill = Direction)) +
        geom_col(width = 22) +
        geom_hline(yintercept = 0, color = PAL$navy, linewidth = 0.4) +
        scale_fill_manual(values = c("Tang" = PAL$green_bar, "Giam" = PAL$red_soft)) +
        scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
        theme_pptx() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
        labs(x = NULL, y = "% Thay doi so voi thang truoc")

      ggplotly(p) %>% ly_layout(bl = 50, bb = 60)
    })

    output$table_quality_report <- renderDT({
      df <- get_data()
      report <- data.frame(
        Chi_so = c("Tổng số gói bảo hiểm", "Tổng hợp đồng",
                    "Tổng số KH unique", "Thời gian",
                    "Phí BH bình quân ", "Phí BH trung vị",
                    "Phí BH min", "Phí BH max",
                    "Số gói phí BH âm", "Số gói thiếu giá BH",
                    "Tỷ lệ HĐ mới", "Tỷ lệ tái tục"),
        Gia_tri = c(
          paste0(formatC(nrow(df), format = "d", big.mark = "."), " gói"),
          paste0(formatC(n_distinct(df$MA_HD), format = "d", big.mark = "."), " hợp đồng"),
          paste0(formatC(n_distinct(df$MA_KH), format = "d", big.mark = "."), " khách hàng"),
          paste(format(min(df$NGAY_KY_HD, na.rm = T), "%d/%m/%Y"), "->", format(max(df$NGAY_KY_HD, na.rm = T), "%d/%m/%Y")),
          format_vnd(mean(df$PHIBH, na.rm = T)),
          format_vnd(median(df$PHIBH, na.rm = T)),
          format_vnd(min(df$PHIBH, na.rm = T)),
          format_vnd(max(df$PHIBH, na.rm = T)),
          formatC(sum(df$PHIBH <= 0, na.rm = T), format = "d", big.mark = "."),
          formatC(sum(is.na(df$GIA_BH)), format = "d", big.mark = "."),
          paste0(round(sum(df$LOAI_HD == "Mới", na.rm = T) / nrow(df) * 100, 1), "%"),
          paste0(round(sum(df$LOAI_HD == "Tái tục", na.rm = T) / nrow(df) * 100, 1), "%")
        ), stringsAsFactors = FALSE
      )

      datatable(report,
        colnames = c("Chỉ tiêu", "Giá trị"), rownames = FALSE,
        options = list(dom = "t", pageLength = 20, ordering = FALSE,
          initComplete = JS(
            "function(settings, json) {",
            "  $(this.api().table().header()).css({",
            "    'background-color':'#1F3864','color':'#FFFFFF',",
            "    'font-family':'Helvetica Neue,Helvetica,Arial,sans-serif','font-weight':'600'",
            "  });",
            "  $(this.api().table().body()).css({",
            "    'font-family':'Helvetica Neue,Helvetica,Arial,sans-serif'",
            "  });",
            "}"
          )
        )
      )
    })

    output$table_audit <- renderDT({
      df <- get_data()
      q3 <- quantile(df$PHIBH, 0.75, na.rm = T)
      iqr <- IQR(df$PHIBH, na.rm = T)
      upper <- q3 + 1.5 * iqr

      audit <- df %>%
        mutate(Flag = case_when(
          PHIBH > upper & XHKH %in% c("Kim cương", "Bạch kim") ~ "VIP + Outlier",
          PHIBH > upper ~ "Outlier Phi cao",
          PHIBH <= 1000 ~ "Phi cuc thap",
          TRUE ~ NA_character_
        )) %>%
        filter(!is.na(Flag)) %>%
        select(MA_HD, MA_KH, TENGOISANPHAM, TEN_CN, PHIBH, XHKH, Flag) %>%
        arrange(desc(PHIBH)) %>% head(200)

      datatable(audit, rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10,
          initComplete = JS(
            "function(settings, json) {",
            "  $(this.api().table().header()).css({",
            "    'background-color':'#1F3864','color':'#FFFFFF',",
            "    'font-family':'Helvetica Neue,Helvetica,Arial,sans-serif','font-weight':'600'",
            "  });",
            "  $(this.api().table().body()).css({",
            "    'font-family':'Helvetica Neue,Helvetica,Arial,sans-serif'",
            "  });",
            "}"
          )
        )
      ) %>%
        formatCurrency("PHIBH", currency = "", interval = 3, mark = ".", digits = 0) %>%
        formatStyle("Flag",
          backgroundColor = styleEqual(
            c("VIP + Outlier", "Outlier Phi cao", "Phi cuc thap"),
            c("#D62728",       "#FF7F0E",         "#2CA02C")),
          color = "#FFFFFF",
          fontWeight = "bold")
    })
  })
}