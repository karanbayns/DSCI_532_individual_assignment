library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(ellmer)

# Load in data
df_raw <- read_csv("data/StudentPerformanceFactors.csv")

# Only keep rows where our primary filters have values
df <- df_raw %>%
  filter(!is.na(School_Type), !is.na(Parental_Education_Level))

income_order <- c("Low", "Medium", "High")
involvement_order <- c("Low", "Medium", "High")

# --- UI DEFINITION ---
ui <- page_fluid(
  titlePanel("Academic Performance Dashboard"),
  navset_tab(
    nav_panel(
      "Dashboard",
      layout_sidebar(
        sidebar = sidebar(
          h4("Global Filters"),
          checkboxGroupInput(
            "school_type",
            "School Type",
            choices = c("Public", "Private"),
            selected = c("Public", "Private")
          ),
          checkboxGroupInput(
            "parent_edu",
            "Parental Education Level",
            choices = sort(unique(df$Parental_Education_Level)),
            selected = sort(unique(df$Parental_Education_Level))
          ),
          hr(),
          markdown("**Authors:** Group Project | **DSCI 532**"),
          open = "desktop"
        ),
        layout_columns(
          value_box("AVG Exam Score", textOutput("avg_score"), theme = "primary"),
          value_box("AVG Hours Studied", textOutput("avg_hours")),
          value_box("AVG Attendance", textOutput("avg_attendance")),
          fill = FALSE
        ),
        layout_columns(
          card(
            card_header("Study Habits vs. Performance"),
            plotOutput("scatter_plot"),
            full_screen = TRUE
          ),
          card(
            card_header("Attendance vs. Exam Score"),
            plotOutput("attendance_scatter"),
            full_screen = TRUE
          ),
          col_widths = c(6, 6)
        ),
        div(style = "height: 16px;"),
        layout_columns(
          card(
            card_header("Score Distribution by Family Income"),
            plotOutput("income_boxplot"),
            full_screen = TRUE
          ),
          card(
            card_header("Impact of Parental Involvement"),
            plotOutput("involvement_bar"),
            full_screen = TRUE
          ),
          col_widths = c(6, 6)
        )
      )
    )
  )
)

# --- SERVER DEFINITION ---
server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    # Stop execution if inputs are NULL (before UI renders)
    req(input$school_type, input$parent_edu) 
    
    df %>%
      filter(
        School_Type %in% input$school_type,
        Parental_Education_Level %in% input$parent_edu
      )
  })
  
  # Value Boxes
  output$avg_score <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("N/A")
    sprintf("%.1f%%", mean(data$Exam_Score, na.rm = TRUE))
  })
  
  output$avg_hours <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("N/A")
    sprintf("%.1f hrs", mean(data$Hours_Studied, na.rm = TRUE))
  })
  
  output$avg_attendance <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("N/A")
    sprintf("%.1f%%", mean(data$Attendance, na.rm = TRUE))
  })
  
  # Reusable theme for ggplot2
  base_theme <- theme_minimal() +
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16),
      panel.grid.minor = element_blank()
    )
  
  # Plots
  output$scatter_plot <- renderPlot({
    data <- filtered_data() %>% filter(!is.na(Hours_Studied), !is.na(Exam_Score))
    if (nrow(data) == 0) return(ggplot() + theme_void())
    
    ggplot(data, aes(x = Hours_Studied, y = Exam_Score)) +
      geom_point(alpha = 0.4, color = "#21918c", size = 3) +
      geom_smooth(method = "loess", color = "red", linewidth = 1.5, se = FALSE) +
      scale_y_continuous(limits = c(40, 100)) +
      labs(x = "Hours Studied", y = "Exam Score") +
      base_theme
  })
  
  output$income_boxplot <- renderPlot({
    data <- filtered_data() %>% filter(!is.na(Family_Income), !is.na(Exam_Score))
    if (nrow(data) == 0) return(ggplot() + theme_void())
    
    # Ensure factor order matches your Python arrays
    data$Family_Income <- factor(data$Family_Income, levels = income_order)
    
    ggplot(data, aes(x = Family_Income, y = Exam_Score, fill = Family_Income)) +
      geom_boxplot() +
      scale_fill_viridis_d(guide = "none") +
      coord_cartesian(ylim = c(55, 80)) + 
      labs(x = "Family Income", y = "Exam Score") +
      base_theme
  })
  
  output$involvement_bar <- renderPlot({
    data <- filtered_data() %>% filter(!is.na(Parental_Involvement), !is.na(Exam_Score))
    if (nrow(data) == 0) return(ggplot() + theme_void())
    
    summary_df <- data %>%
      group_by(Parental_Involvement) %>%
      summarize(mean_score = mean(Exam_Score, na.rm = TRUE)) %>%
      mutate(Parental_Involvement = factor(Parental_Involvement, levels = involvement_order))
    
    ggplot(summary_df, aes(x = Parental_Involvement, y = mean_score, fill = Parental_Involvement)) +
      geom_col() +
      scale_fill_viridis_d(guide = "none") +
      coord_cartesian(ylim = c(60, 72)) + 
      labs(x = "Involvement Level", y = "Average Exam Score") +
      base_theme
  })
  
  output$attendance_scatter <- renderPlot({
    data <- filtered_data() %>% filter(!is.na(Attendance), !is.na(Exam_Score))
    if (nrow(data) == 0) return(ggplot() + theme_void())
    
    ggplot(data, aes(x = Attendance, y = Exam_Score)) +
      geom_point(alpha = 0.4, color = "#21918c", size = 3) +
      geom_smooth(method = "loess", color = "red", linewidth = 1.5, se = FALSE) +
      scale_x_continuous(limits = c(60, 100)) +
      scale_y_continuous(limits = c(40, 100)) +
      labs(x = "Attendance (%)", y = "Exam Score") +
      base_theme
  })
  
  # --- AI Data Output ---
  # Instead of the dummy message, let's preview the first 10 rows of the filtered data
  output$ai_data_table <- renderTable({
    head(filtered_data(), 10)
  })
  
  # Download button now pulls the full filtered dataset
  output$download_ai_output <- downloadHandler(
    filename = function() { 
      paste0("ai_filtered_data_", Sys.Date(), ".csv") 
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
