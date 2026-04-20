phase2UI <- function(id) {

  ns <- NS(id)

  tabItem(tabName = id,

    h2("Phần 2: Khai phá vĩ mô và xu hướng doanh thu",

       style = "color:#303983; font-weight:700; margin-bottom:4px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),

    p("Phân tích xu hướng GWP theo thời gian, cơ cấu sản phẩm và phân phối phí bảo hiểm.",

      style = "color:#6C7A89; margin-bottom:18px; font-family:'Helvetica Neue',Helvetica,Arial,sans-serif;"),


    # --- KPI Cards ---

    fluidRow(

      valueBoxOutput(ns("vbox_total_gwp"), width = 3),

      valueBoxOutput(ns("vbox_avg_gwp"), width = 3),

      valueBoxOutput(ns("vbox_new_pct"), width = 3),

      valueBoxOutput(ns("vbox_renewal_pct"), width = 3)

    ),



    # --- Row 1: Xu hướng GWP ---

    fluidRow(

      box(title = NULL, width = 12, status = "primary", solidHeader = FALSE,

          tags$h4("Xu hướng doanh thu GWP theo tháng",

                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),

          tags$p("Biến động tổng phí BH",

                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),

          plotlyOutput(ns("plot_gwp_trend"), height = "380px"))

    ),



    # --- Row 2: Top SP + Density ---

    fluidRow(

      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,

          tags$h4("Top 10 gói sản phẩm theo doanh thu",

                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),

          tags$p("",

                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),

          plotlyOutput(ns("plot_product"), height = "380px")),

      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,

          tags$h4("Mật độ phân phối phí BH",

                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),

          tags$p("",

                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),

          plotlyOutput(ns("plot_density"), height = "380px"))

    ),



    # --- Row 3: Quarterly + New/Renewal ---

    fluidRow(

      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,

          tags$h4("Doanh thu theo quý",

                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),

          tags$p("",

                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),

          plotlyOutput(ns("plot_qoq"), height = "380px")),

      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,

          tags$h4("Cơ cấu HĐ và tái tục theo tháng",

                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),

          tags$p("So sánh doanh thu hợp đồng mới và tái tục qua các tháng.",

                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),

          plotlyOutput(ns("plot_new_renewal"), height = "380px"))

    ),



    # --- Row 4 MỚI: Doanh thu theo Nhóm SP (multi-line) + Số lượng gói MoM ---

    fluidRow(

      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,

          tags$h4("Xu hướng DT theo nhóm SP",

                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),

          tags$p("",

                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),

          plotlyOutput(ns("plot_product_trend"), height = "380px")),

      box(title = NULL, width = 6, status = "primary", solidHeader = FALSE,

          tags$h4("Số lượng gói BH theo tháng",

                   style = "color:#303983; font-weight:700; margin:0 0 2px 0; font-family:'Helvetica Neue',sans-serif;"),

          tags$p("Biến động phát hành mỗi tháng.",

                  style = "color:#6C7A89; font-size:12px; margin-bottom:6px;"),

          plotlyOutput(ns("plot_volume_trend"), height = "380px"))

    )

  )

}



phase2Server <- function(id, all_data) {

  moduleServer(id, function(input, output, session) {



    get_data <- reactive({ req(all_data()); all_data()$master })



    # Font chuẩn plotly (giống PPTX)

    pf <- list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", color = "#4A4A4A")



    # Layout helper — ôm sát box, grid nhạt

    ly <- function(p, bl = 10, bb = 10, bt = 20, br = 10) {

      p %>% layout(

        font = pf,

        margin = list(l = bl, r = br, t = bt, b = bb),

        xaxis = list(automargin = TRUE, gridcolor = "#EBEBEB", gridwidth = 0.5),

        yaxis = list(automargin = TRUE, gridcolor = "#EBEBEB", gridwidth = 0.5)

      ) %>% config(displayModeBar = FALSE)

    }



    # ================================================================

    # VALUE BOXES

    # ================================================================

    output$vbox_total_gwp <- renderValueBox({

      valueBox(format_vnd(sum(get_data()$PHIBH, na.rm = TRUE)), "Tổng GWP",

               icon = icon("coins"), color = "blue")

    })

    output$vbox_avg_gwp <- renderValueBox({

      valueBox(format_vnd(mean(get_data()$PHIBH, na.rm = TRUE)), "GWP bình quân/gói",

               icon = icon("calculator"), color = "blue")

    })

    output$vbox_new_pct <- renderValueBox({

      df <- get_data()

      pct <- round(sum(df$LOAI_HD == "Mới", na.rm = TRUE) / nrow(df) * 100, 1)

      valueBox(paste0(pct, "%"), "Hợp đồng mới", icon = icon("plus-circle"), color = "teal")

    })

    output$vbox_renewal_pct <- renderValueBox({

      df <- get_data()

      pct <- round(sum(df$LOAI_HD == "Tái tục", na.rm = TRUE) / nrow(df) * 100, 1)

      valueBox(paste0(pct, "%"), "Tái tục", icon = icon("sync-alt"), color = "yellow")

    })



    output$plot_gwp_trend <- renderPlotly({

      df_sum <- get_data() %>%

        group_by(THANG) %>%

        summarise(GWP = sum(PHIBH, na.rm = TRUE), SL = n(), .groups = "drop") %>%

        arrange(THANG)



      p <- ggplot(df_sum, aes(x = THANG, y = GWP)) +

        geom_line(color = "#4A6FA5", linewidth = 1.2) +

        scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +

        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0.02, 0.08))) +

        theme_minimal(base_family = "sans") +

        theme(

          panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.4),

          panel.grid.minor = element_blank(),

          axis.text.x = element_text(color = "#6C7A89", size = 9),

          axis.text.y = element_text(color = "#6C7A89"),

          axis.title = element_text(color = "#4A4A4A", face = "bold", size = 10),

          plot.margin = margin(0, 0, 0, 0)

        ) +

        labs(x = "Date", y = "Doanh thu GWP")



      ggplotly(p, tooltip = c("x", "y")) %>%

        style(line = list(shape = "spline", smoothing = 1.3)) %>%

        ly(bt = 15)

    })





    output$plot_product <- renderPlotly({

      df_prod <- get_data() %>%

        filter(!is.na(TENGOISANPHAM), TENGOISANPHAM != "Chưa xác định") %>%

        group_by(TENGOISANPHAM) %>%

        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%

        top_n(10, GWP)



      p <- ggplot(df_prod, aes(x = reorder(TENGOISANPHAM, GWP), y = GWP)) +

        geom_col(fill = "#4A6FA5", width = 0.65) +

        coord_flip() +

        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0, 0.08))) +

        theme_minimal(base_family = "sans") +

        theme(

          panel.grid.major.y = element_blank(),

          panel.grid.major.x = element_line(color = "#EBEBEB", linewidth = 0.4),

          panel.grid.minor = element_blank(),

          axis.text.y = element_text(face = "bold", size = 9, color = "#4A4A4A"),

          axis.title = element_text(color = "#4A4A4A", face = "bold", size = 10),

          plot.margin = margin(0, 0, 0, 0)

        ) +

        labs(x = "", y = "GWP (VND)")



      ggplotly(p) %>% ly(bl = 10, bb = 20)

    })



    output$plot_density <- renderPlotly({

      df <- get_data() %>% filter(PHIBH > 0)



      p <- ggplot(df, aes(x = PHIBH)) +

        geom_density(fill = "#2EC4B6", alpha = 0.6, color = "white", linewidth = 0.5) +

        scale_x_log10(labels = label_trieu) +

        scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +

        theme_minimal(base_family = "sans") +

        theme(

          panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.4),

          panel.grid.minor = element_blank(),

          axis.title = element_text(color = "#4A4A4A", face = "bold", size = 10),

          axis.text = element_text(color = "#6C7A89"),

          plot.margin = margin(0, 0, 0, 0)

        ) +

        labs(x = "Phi BH (VND) — Log", y = "Mat do")



      ggplotly(p) %>% ly(bb = 25)

    })





    output$plot_qoq <- renderPlotly({

      df_q <- get_data() %>%

        mutate(Quy = paste0(year(NGAY_KY_HD), "-Q", quarter(NGAY_KY_HD))) %>%

        group_by(Quy) %>%

        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop") %>%

        arrange(Quy)



      p <- ggplot(df_q, aes(x = Quy, y = GWP)) +

        geom_col(fill = "#55bded", width = 0.55) +

        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0, 0.08))) +

        theme_minimal(base_family = "sans") +

        theme(

          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 9, color = "#4A4A4A"),

          panel.grid.major.x = element_blank(),

          panel.grid.major.y = element_line(color = "#EBEBEB", linewidth = 0.4),

          panel.grid.minor = element_blank(),

          axis.title = element_text(color = "#4A4A4A", face = "bold", size = 10),

          plot.margin = margin(0, 0, 0, 0)

        ) +

        labs(x = "", y = "GWP (VND)")



      ggplotly(p) %>% ly(bb = 30)

    })





    output$plot_new_renewal <- renderPlotly({

      df_nr <- get_data() %>%

        group_by(THANG, LOAI_HD) %>%

        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")



      p <- ggplot(df_nr, aes(x = THANG, y = GWP, fill = LOAI_HD)) +

        geom_area(alpha = 0.55, position = "stack") +

        scale_fill_manual(values = c("Mới" = "#4A6FA5", "Tái tục" = "#FFCA28")) +

        scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +

        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0, 0.05))) +

        theme_minimal(base_family = "sans") +

        theme(

          panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.4),

          panel.grid.minor = element_blank(),

          legend.title = element_blank(),

          axis.title = element_text(color = "#4A4A4A", face = "bold", size = 10),

          axis.text = element_text(color = "#6C7A89"),

          plot.margin = margin(0, 0, 0, 0)

        ) +

        labs(x = "Date", y = "GWP (VND)")



      ggplotly(p) %>%

        layout(

          font = pf,

          margin = list(l = 10, r = 10, t = 40, b = 15),

          xaxis = list(automargin = TRUE, gridcolor = "#EBEBEB"),

          yaxis = list(automargin = TRUE, gridcolor = "#EBEBEB"),

          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom")

        ) %>% config(displayModeBar = FALSE)

    })





    output$plot_product_trend <- renderPlotly({

      df <- get_data()

      top5 <- df %>%

        filter(NHOMSANPHAM != "Chưa xác định") %>%

        group_by(NHOMSANPHAM) %>% summarise(v = sum(PHIBH)) %>%

        top_n(5, v) %>% pull(NHOMSANPHAM)



      df_trend <- df %>%

        filter(NHOMSANPHAM %in% top5) %>%

        group_by(THANG, NHOMSANPHAM) %>%

        summarise(GWP = sum(PHIBH, na.rm = TRUE), .groups = "drop")



      pal5 <- c("#7E57C2", "#2EC4B6", "#F06292", "#64B5F6", "#FFCA28")



      p <- ggplot(df_trend, aes(x = THANG, y = GWP, color = NHOMSANPHAM)) +

        geom_line(linewidth = 1.1) +

        scale_color_manual(values = pal5) +

        scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +

        scale_y_continuous(labels = label_trieu, expand = expansion(mult = c(0.02, 0.08))) +

        theme_minimal(base_family = "sans") +

        theme(

          panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.4),

          panel.grid.minor = element_blank(),

          legend.title = element_blank(),

          axis.title = element_text(color = "#4A4A4A", face = "bold", size = 10),

          axis.text = element_text(color = "#6C7A89"),

          plot.margin = margin(0, 0, 0, 0)

        ) +

        labs(x = "Date", y = "GWP (VND)")



      ggplotly(p) %>%

        style(line = list(shape = "spline", smoothing = 1.3)) %>%

        layout(

          font = pf,

          margin = list(l = 10, r = 10, t = 40, b = 15),

          xaxis = list(automargin = TRUE, gridcolor = "#EBEBEB"),

          yaxis = list(automargin = TRUE, gridcolor = "#EBEBEB"),

          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom")

        ) %>% config(displayModeBar = FALSE)

    })





    output$plot_volume_trend <- renderPlotly({

      df_vol <- get_data() %>%

        group_by(THANG) %>%

        summarise(N = n(), .groups = "drop") %>%

        arrange(THANG)



      plot_ly(df_vol, x = ~THANG) %>%

        add_bars(y = ~N, name = "Số lượng gói",

                 marker = list(color = "#55bded", opacity = 0.6),

                 hovertemplate = "%{x|%b %Y}: %{y:,.0f} goi<extra></extra>") %>%

        add_lines(y = ~N, name = "Trend",

                  line = list(color = "#303983", width = 2, shape = "spline", smoothing = 1.3),

                  hoverinfo = "skip") %>%

        layout(

          font = pf,

          margin = list(l = 10, r = 10, t = 40, b = 15),

          xaxis = list(automargin = TRUE, gridcolor = "#EBEBEB", gridwidth = 0.5,

                       tickformat = "%b %Y", dtick = "M3"),

          yaxis = list(automargin = TRUE, gridcolor = "#EBEBEB", gridwidth = 0.5,

                       title = list(text = "Số lượng gói BH", font = list(size = 11, color = "#4A4A4A")),

                       separatethousands = TRUE),

          legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.12, yanchor = "bottom"),

          showlegend = TRUE,

          bargap = 0.3

        ) %>% config(displayModeBar = FALSE)

    })

  })

}