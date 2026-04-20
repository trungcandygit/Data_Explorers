library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(cluster) # Thư viện để tính Silhouette
library(scales)  # Thư viện để Min-Max scaling

phase4UI <- function(id) {
  ns <- NS(id)
  tabItem(tabName = id,
          h2("Phần 4: Phân cụm khách hàng RFM & K-Means",
             style = "color:#303983; font-weight:700; margin-bottom:4px;"),
          p("Phân tích phân khúc dựa trên thuật toán K-Means kết hợp kiểm định Silhouette và WSS.",
            style = "color:#6C7A89; margin-bottom:18px;"),
          
          # --- Control panel ---
          fluidRow(
            box(title = NULL, width = 12, status = "primary", solidHeader = FALSE,
                tags$h4("Cấu hình Thuật toán", style = "color:#303983; font-weight:700; margin:0 0 8px 0;"),
                fluidRow(
                  column(3, numericInput(ns("clusters"), "Số cụm (K):", value = 4, min = 2, max = 6)),
                  column(3, selectInput(ns("seed_val"), "Random Seed:", choices = c(42, 123, 456), selected = 42)),
                  column(3, br(), actionButton(ns("run_kmeans"), "Chạy Mô Hình",
                                               icon = icon("play-circle"), class = "btn-success btn-lg")),
                  column(3, tags$p("Tối ưu hóa tốc độ: Silhouette tính trên tập mẫu n=10,000",
                                   style = "color:#E67E22; font-weight:bold; font-size:12px; margin-top:12px;"))
                )
            )
          ),
          
          # --- Nơi hiển thị toàn bộ Dashboard ---
          uiOutput(ns("results_display"))
  )
}

phase4Server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    clustering_results <- reactiveVal(NULL)
    diagnostics_data <- reactiveVal(NULL)
    
    pf <- list(family = "'Helvetica Neue', Helvetica, Arial, sans-serif", color = "#4A4A4A")
    pal <- c("#4A6FA5", "#2EC4B6", "#7E57C2", "#F06292", "#FFCA28", "#55bded")
    
    # Format layout chung cho Plotly
    ly <- function(p, bl = 10, bb = 10, bt = 15, br = 10) {
      p %>% layout(
        font = pf, margin = list(l = bl, r = br, t = bt, b = bb),
        xaxis = list(automargin = TRUE, gridcolor = "#EBEBEB"),
        yaxis = list(automargin = TRUE, gridcolor = "#EBEBEB")
      ) %>% config(displayModeBar = FALSE)
    }
    
    observeEvent(input$run_kmeans, {
      withProgress(message = "Đang huấn luyện mô hình...", value = 0, {
        
        req(all_data()); df <- all_data()$master
        incProgress(0.2, detail = "Tính toán RFM...")
        
        max_date <- max(df$NGAY_KY_HD, na.rm = TRUE)
        rfm_data <- df %>%
          group_by(MA_KH) %>%
          summarise(
            Recency   = as.numeric(max_date - max(NGAY_KY_HD, na.rm = TRUE)),
            Frequency = n(),
            Monetary  = sum(PHIBH, na.rm = TRUE),
            .groups = "drop"
          ) %>% filter(Monetary > 0, Frequency > 0)
        
        incProgress(0.4, detail = "Chuẩn hóa & Log-transform...")
        rfm_scaled <- rfm_data %>%
          mutate(R_log = log1p(Recency), F_log = log1p(Frequency), M_log = log1p(Monetary))
        scale_matrix <- scale(rfm_scaled[, c("R_log", "F_log", "M_log")])
        
        incProgress(0.5, detail = "Kiểm định WSS & Silhouette (Sampling)...")
        # DÙNG SAMPLING ĐỂ TRÁNH TRÀN RAM KHI TÍNH SILHOUETTE CHO BIG DATA
        set.seed(42)
        samp_idx <- sample(1:nrow(scale_matrix), min(10000, nrow(scale_matrix)))
        samp_scale <- scale_matrix[samp_idx, ]
        dist_samp <- dist(samp_scale)
        
        wss <- numeric()
        sil_scores <- numeric()
        
        for(k in 2:8) {
          set.seed(as.numeric(input$seed_val))
          km_temp <- kmeans(scale_matrix, centers = k, nstart = 25)
          wss[k-1] <- km_temp$tot.withinss
          # Tính Silhouette trên tập mẫu
          sil_temp <- cluster::silhouette(km_temp$cluster[samp_idx], dist_samp)
          sil_scores[k-1] <- mean(sil_temp[, 3])
        }
        diagnostics_data(data.frame(K = 2:8, WSS = wss, Silhouette = sil_scores))
        
        incProgress(0.8, detail = "Phân cụm dữ liệu gốc...")
        set.seed(as.numeric(input$seed_val))
        km <- kmeans(scale_matrix, centers = input$clusters, nstart = 25)
        rfm_data$Cluster <- as.factor(km$cluster)
        
        # Gán nhãn dựa trên Monetary
        clust_summary <- rfm_data %>% group_by(Cluster) %>% summarise(avg_m = mean(Monetary)) %>% arrange(desc(avg_m))
        label_map <- setNames(c("VIP / Doanh nghiệp", "KH Trung thành", "KH Tiềm năng", "KH Rời bỏ", "Nhóm 5", "Nhóm 6")[1:input$clusters], clust_summary$Cluster)
        rfm_data$Cluster_Label <- label_map[as.character(rfm_data$Cluster)]
        
        attr(rfm_data, "km") <- km
        clustering_results(rfm_data)
        incProgress(1, detail = "Hoàn tất!")
      })
    })
    
    # --- RENDER UI LAYOUT TỔNG THỂ ---
    output$results_display <- renderUI({
      req(clustering_results())
      tagList(
        # Row 1: Model Diagnostics (Elbow & Silhouette)
        fluidRow(
          box(title = NULL, width = 6, status = "primary",
              tags$h4("Elbow Method (WSS)", style = "color:#303983; font-weight:700; font-size:14px;"),
              plotlyOutput(ns("plot_elbow"), height = "250px")),
          box(title = NULL, width = 6, status = "primary",
              tags$h4("Kiểm định Silhouette", style = "color:#303983; font-weight:700; font-size:14px;"),
              tags$p("Điểm Silhouette càng gần 1, các cụm càng tách biệt rõ ràng.", style = "color:#6C7A89; font-size:11px; margin:0;"),
              plotlyOutput(ns("plot_silhouette"), height = "250px"))
        ),
        
        # Row 2: Bảng Hồ sơ Phân cụm (Chính) & Radar
        fluidRow(
          box(title = NULL, width = 7, status = "primary",
              tags$h4("Hồ sơ Chân dung Khách hàng (Cluster Profiling)", style = "color:#303983; font-weight:700;"),
              DTOutput(ns("table_profiling"))),
          box(title = NULL, width = 5, status = "primary",
              tags$h4("Hình thái R-F-M (Dải điểm 0-100)", style = "color:#303983; font-weight:700;"),
              plotlyOutput(ns("plot_radar"), height = "350px"))
        ),
        
        # Row 3: Scatter Plot & Bảng Đánh giá chất lượng
        fluidRow(
          box(title = NULL, width = 8, status = "primary",
              tags$h4("Phân tách Không gian: Recency x Monetary", style = "color:#303983; font-weight:700;"),
              tags$p("Hiển thị đại diện mẫu 10,000 điểm dữ liệu để tránh Overplotting.", style = "color:#6C7A89; font-size:11px; margin:0;"),
              plotlyOutput(ns("plot_kmeans_rm"), height = "350px")),
          
          box(title = NULL, width = 4, status = "primary",
              tags$h4("Đánh giá mô hình (Model Evaluation)", style = "color:#303983; font-weight:700;"),
              uiOutput(ns("ui_quality_note")))
        )
      )
    })
    
    # --- CÁC HÀM VẼ BIỂU ĐỒ ---
    
    # 1. Biểu đồ Elbow
    output$plot_elbow <- renderPlotly({
      edf <- diagnostics_data(); req(edf)
      p <- ggplot(edf, aes(x = K, y = WSS)) + geom_line(color = "#4A6FA5", linewidth = 1) + geom_point(color = "#4A6FA5", size = 2) +
        geom_point(data = edf %>% filter(K == input$clusters), color = "#EF5350", size = 4) + theme_minimal() + labs(x="K", y="WSS")
      ggplotly(p) %>% ly()
    })
    
    # 2. Biểu đồ Silhouette
    output$plot_silhouette <- renderPlotly({
      edf <- diagnostics_data(); req(edf)
      p <- ggplot(edf, aes(x = K, y = Silhouette)) + geom_line(color = "#2EC4B6", linewidth = 1) + geom_point(color = "#2EC4B6", size = 2) +
        geom_point(data = edf %>% filter(K == input$clusters), color = "#EF5350", size = 4) + theme_minimal() + labs(x="K", y="Silhouette Score")
      ggplotly(p) %>% ly()
    })
    
    # 3. Radar Chart (Trục Min-Max Scaling 0-100)
    output$plot_radar <- renderPlotly({
      res <- clustering_results(); req(res)
      
      radar_df <- res %>%
        group_by(Cluster_Label) %>%
        summarise(R_mean = mean(Recency), F_mean = mean(Frequency), M_mean = mean(Monetary), .groups = "drop") %>%
        mutate(
          R_Score = rescale(R_mean, to = c(0, 100)),
          F_Score = rescale(F_mean, to = c(0, 100)),
          M_Score = rescale(M_mean, to = c(0, 100))
        )
      
      fig <- plot_ly(type = "scatterpolar", fill = "toself")
      for (i in 1:nrow(radar_df)) {
        fig <- fig %>% add_trace(
          r = c(radar_df$R_Score[i], radar_df$F_Score[i], radar_df$M_Score[i], radar_df$R_Score[i]),
          theta = c("Recency", "Frequency", "Monetary", "Recency"),
          name = radar_df$Cluster_Label[i],
          fillcolor = paste0(pal[i], "30"),
          line = list(color = pal[i], width = 2)
        )
      }
      
      fig %>% layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1)
      ) %>% config(displayModeBar = FALSE)
    })
    
    # 4. Data Table Tích hợp In-cell Bar chart
    output$table_profiling <- renderDT({
      res <- clustering_results(); req(res)
      
      stats <- res %>%
        group_by(Cum = Cluster_Label) %>%
        summarise(
          So_KH = n(),
          Pct_Num = round(n() / nrow(res) * 100, 1),
          R_Days = round(mean(Recency), 0),
          F_Freq = round(mean(Frequency), 1),
          M_Value = sum(Monetary),
          .groups = "drop"
        ) %>%
        arrange(desc(M_Value)) %>%
        mutate(
          So_KH = formatC(So_KH, format = "d", big.mark = "."),
          M_Value = paste0(round(M_Value / 1e9, 1), " Tỷ")
        )
      
      datatable(stats,
                colnames = c("Cụm", "Số lượng", "Tỷ trọng (%)", "R (Ngày)", "F (Tần suất)", "M (Tổng Chi)"),
                rownames = FALSE,
                options = list(dom = "t", ordering = FALSE, pageLength = 6)
      ) %>%
        formatStyle(
          'Pct_Num',
          background = styleColorBar(c(0, 100), '#E3F2FD'),
          backgroundSize = '98% 80%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center'
        )
    })
    
    # 5. Scatter Plot (Với kỹ thuật Sampling)
    output$plot_kmeans_rm <- renderPlotly({
      res <- clustering_results(); req(res)
      
      # Lấy mẫu ngẫu nhiên nếu dữ liệu > 10.000 dòng để biểu đồ mượt và không bị đè màu
      res_plot <- if(nrow(res) > 10000) { 
        set.seed(42); res %>% sample_n(10000) 
      } else { res }
      
      p <- ggplot(res_plot, aes(x = Recency, y = Monetary, color = Cluster_Label)) +
        geom_point(alpha = 0.4, size = 1.2) + 
        scale_y_continuous(labels = function(x) paste0(x / 1e6, " Tr")) + 
        scale_color_manual(values = pal[1:input$clusters]) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(x = "Recency (Ngày)", y = "Monetary (Triệu VNĐ)")
      
      ggplotly(p) %>% ly() %>% layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.1))
    })
    
    # 6. Bảng Đánh giá chất lượng mô hình
    output$ui_quality_note <- renderUI({
      res <- clustering_results(); req(res)
      km <- attr(res, "km")
      bss_pct <- round(km$betweenss / km$totss * 100, 1)
      
      tags$div(style = "font-family:'Helvetica Neue',sans-serif; color:#4A4A4A; font-size: 13px;",
               tags$table(style = "width:100%; border-collapse:collapse;",
                          tags$tr(tags$td(style = "padding:8px; border-bottom:1px solid #EBEBEB; font-weight:bold; width:45%;", "Số cụm (K)"),
                                  tags$td(style = "padding:8px; border-bottom:1px solid #EBEBEB;", input$clusters)),
                          tags$tr(tags$td(style = "padding:8px; border-bottom:1px solid #EBEBEB; font-weight:bold;", "Tổng KH phân cụm"),
                                  tags$td(style = "padding:8px; border-bottom:1px solid #EBEBEB;", formatC(nrow(res), format = "d", big.mark = "."))),
                          tags$tr(tags$td(style = "padding:8px; border-bottom:1px solid #EBEBEB; font-weight:bold;", "Between SS / Total SS"),
                                  tags$td(style = "padding:8px; border-bottom:1px solid #EBEBEB;",
                                          paste0(bss_pct, "%"),
                                          if(bss_pct >= 50) tags$span(" — Tốt", style = "color:#66BB6A; font-weight:bold;")
                                          else tags$span(" — Cần xem xét", style = "color:#EF5350; font-weight:bold;"))),
                          tags$tr(tags$td(style = "padding:8px; border-bottom:1px solid #EBEBEB; font-weight:bold;", "Preprocessing"),
                                  tags$td(style = "padding:8px; border-bottom:1px solid #EBEBEB;", "log1p() -> scale(Z-score)")),
                          tags$tr(tags$td(style = "padding:8px; font-weight:bold;", "Init (nstart)"),
                                  tags$td(style = "padding:8px;", "25 (Tránh Local Optima)"))
               )
      )
    })
    
  })
}