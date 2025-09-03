library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(forcats)
library(stringr)
library(reshape2)
library(readxl) # Added for robust Excel file reading
library(tidyr)  # For pivot_longer function
library(ggrepel) # For better label placement in plots
library(broom) 


df <- read_excel("C:/Users/mtk78/OneDrive/Desktop/DV/DV PRoject/data.xlsx")

df_clean <- df %>%
  # Standardize issuer names for consistency
  mutate(Issuer = str_to_title(Issuer),
         Issuer = case_when(
           str_detect(Issuer, "Dbs") ~ "DBS",
           str_detect(Issuer, "Ocbc") ~ "OCBC",
           str_detect(Issuer, "Uob") ~ "UOB",
           str_detect(Issuer, "Citibank") ~ "Citibank",
           str_detect(Issuer, "Hsbc") ~ "HSBC",
           str_detect(Issuer, "Maybank") ~ "Maybank",
           str_detect(Issuer, "Amex") ~ "AMEX",
           str_detect(Issuer, "Standard Chartered") ~ "Standard Chartered",
           TRUE ~ Issuer
         )) %>%
  # Filter for the main issuers mentioned in the report
  filter(Issuer %in% c("DBS", "UOB", "OCBC", "Citibank", "HSBC", "Maybank", "AMEX", "Standard Chartered")) %>%
  # Convert specified numeric columns, replacing values > 90 with NA
  mutate(across(c(satis, confirm, ideal, overallx, customx, wrongx, poverq, soverq, recomm, starts_with("vn_7002_T")), 
                ~replace(as.numeric(.), . > 90, NA))) %>%
  # Convert categorical columns to factors with meaningful labels
  mutate(
    # repur_recode: 1 for loyal, 0 for not.
    loyalty = factor(repur_recode, levels = c(0, 1), labels = c("Not Loyal", "Loyal")),
    # pincome: Monthly Personal Income
    income_level = factor(pincome,
                          levels = 1:9,
                          labels = c("Under $2K", "$2K-$3K", "$3K-$4K", "$4K-$6K",
                                     "$6K-$8K", "$8K-$10K", "$10K-$15K", "$15K-$20K", "$20K+"),
                          ordered = TRUE),
    # gender: 1 for Male, 2 for Female
    gender = factor(gender, levels = c(1, 2), labels = c("Male", "Female")),
    # age: Create age groups from numeric age
    age_group = cut(age,
                    breaks = c(17, 24, 34, 44, 54, 64, Inf),
                    labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                    right = TRUE)
  )

# --- 3. Define the User Interface (UI) ---
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Credit Card Analysis"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      # Filter 1: Card issuers
      selectInput(
        "selected_issuers",
        "Select Card Issuer(s):",
        choices = sort(unique(df_clean$Issuer)),
        selected = c("OCBC", "DBS", "UOB", "Citibank", "AMEX", "HSBC", "Maybank", "Standard Chartered"), # Default selection
        multiple = TRUE
      ),
      # Filter 2: Gender
      radioButtons(
        "gender_select",
        "Select Gender:",
        choices = c("All", "Male", "Female"),
        selected = "All"
      ),
      # Filter 3: Age
      sliderInput(
        "age_slider",
        "Select Age Range:",
        min = min(df_clean$age, na.rm = TRUE),
        max = max(df_clean$age, na.rm = TRUE),
        value = c(min(df_clean$age, na.rm = TRUE), max(df_clean$age, na.rm = TRUE))
      )
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
            # Plot 1: Customer Gender Distribution by Issuer
            box(
                title = "Customer Gender Distribution by Issuer",
                status = "primary", solidHeader = TRUE, collapsible = TRUE,
                plotlyOutput("genderDistributionPlot"), width = 12
            )
        ),
        fluidRow(
            # Plot 2: Market Share Pie Chart
            box(
                title = "Market Share by Card Issuer",
                status = "primary", solidHeader = TRUE, collapsible = TRUE,
                plotlyOutput("marketSharePieChart"), width = 12
            )
        ),
        fluidRow(
            # Plot 3: Attribute Comparison Radar Chart
            box(
                title = "Attribute Performance Comparison",
                status = "primary", solidHeader = TRUE, collapsible = TRUE,
                plotlyOutput("radarChart"), width = 12
            )
        ),
        fluidRow(
            # Plot 4: Pareto Analysis for OCBC Improvement
            box(
                title = "OCBC Improvement Priorities (Pareto Analysis)",
                status = "warning", solidHeader = TRUE, collapsible = TRUE,
                plotlyOutput("paretoChart"), width = 12
            )
        ),
        fluidRow(
          # Plot 5: Average Satisfaction Score
          box(
            title = "Average Customer Satisfaction Score by Issuer",
            status = "primary", solidHeader = TRUE, collapsible = TRUE,
            plotlyOutput("satisfactionPlot"), width = 6
          ),
          # Plot 6: Customer Loyalty Distribution
          box(
            title = "Customer Loyalty Distribution by Issuer",
            status = "primary", solidHeader = TRUE, collapsible = TRUE,
            plotlyOutput("loyaltyPlot"), width = 6
          )
        ),
        fluidRow(
          # Plot 7: Satisfaction vs. Likelihood to Recommend
          box(
            title = "Satisfaction vs. Likelihood to Recommend",
            status = "primary", solidHeader = TRUE, collapsible = TRUE,
            plotlyOutput("scatterPlot"), width = 6
          ),
          # Plot 8: Correlation Heatmap of Quality Metrics
          box(
            title = "Correlation Between Key Quality Metrics",
            status = "primary", solidHeader = TRUE, collapsible = TRUE,
            plotlyOutput("heatmapPlot"), width = 6
          )
        ),
        fluidRow(
            # Plot 9: Customer Income Profile
            box(
                title = "Customer Income Profile by Issuer",
                status = "primary", solidHeader = TRUE, collapsible = TRUE,
                plotlyOutput("incomePlot"), width = 12
            )
        ),
        fluidRow(
            # Plot 10: Service Quality by Gender
            box(
                title = "Average Service Quality Score by Gender",
                status = "primary", solidHeader = TRUE, collapsible = TRUE,
                plotlyOutput("genderPlot"), width = 6
            ),
            # Plot 11: Service Quality by Age
            box(
                title = "Average Service Quality Score by Age Group",
                status = "primary", solidHeader = TRUE, collapsible = TRUE,
                plotlyOutput("agePlot"), width = 6
            )
        ),
        fluidRow(
            # Plot 12: Satisfaction vs. Market Share Analysis
            box(
                title = "Satisfaction vs. Market Share Analysis",
                status = "primary", solidHeader = TRUE, collapsible = TRUE,
                plotlyOutput("marketSharePlot"), width = 12
            )
        ),
        fluidRow(
          # NEW Plot 13: Loyalty Driver Quadrant Chart
          box(
            title = "Loyalty Driver Analysis",
            status = "success", solidHeader = TRUE, collapsible = TRUE,
            plotlyOutput("loyaltyDriverPlot"), width = 12
          )
        ),
        fluidRow(
            # NEW Plot 15: Violin Plot of Satisfaction Scores
            box(
                title = "Satisfaction Score Distribution by Issuer",
                status = "info", solidHeader = TRUE, collapsible = TRUE,
                plotlyOutput("violinPlot"), width = 12
            )
        )
      )
    )
  )
)


# --- 4. Define the Server Logic ---
server <- function(input, output) {

  # Create a reactive dataframe that filters based on all user selections
  filtered_data <- reactive({
    req(input$selected_issuers, input$gender_select, input$age_slider)

    # Start with base clean data and filter by issuers
    temp_df <- df_clean %>%
      filter(Issuer %in% input$selected_issuers)

    # Conditionally filter by gender
    if (input$gender_select != "All") {
      temp_df <- temp_df %>% filter(gender == input$gender_select)
    }

    # Filter by the age slider range
    temp_df <- temp_df %>%
      filter(age >= input$age_slider[1] & age <= input$age_slider[2])

    return(temp_df)
  })

  # Render Plot 1: Customer Gender Distribution
  output$genderDistributionPlot <- renderPlotly({
    gender_counts <- filtered_data() %>%
      filter(!is.na(gender)) %>%
      count(Issuer, gender)

    p <- ggplot(gender_counts, aes(x = Issuer, y = n, fill = gender,
                                   text = paste("Issuer:", Issuer, "<br>Gender:", gender, "<br>Count:", n))) +
      geom_col(position = "dodge") +
      labs(
        x = "Card Issuer", y = "Number of Customers", fill = "Gender"
      ) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set2")

    ggplotly(p, tooltip = "text")
  })
  
  # Render Plot 2: Market Share Pie Chart
  output$marketSharePieChart <- renderPlotly({
    market_data <- filtered_data() %>%
      count(Issuer, name = "count")

    plot_ly(market_data, labels = ~Issuer, values = ~count, type = 'pie',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(count, ' respondents'),
            marker = list(line = list(color = '#FFFFFF', width = 1)),
            showlegend = TRUE) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # Render Plot 3: Attribute Comparison Radar Chart
  output$radarChart <- renderPlotly({
      radar_data <- filtered_data() %>%
        select(Issuer, satis, poverq, soverq, recomm, confirm, ideal) %>%
        rename(
            Satisfaction = satis,
            "Product Quality" = poverq,
            "Service Quality" = soverq,
            Recommendation = recomm,
            "Confirms Expectations" = confirm,
            "Close to Ideal" = ideal
        ) %>%
        group_by(Issuer) %>%
        summarise(across(everything(), ~mean(., na.rm = TRUE)), .groups = 'drop')

      p <- plot_ly(type = 'scatterpolar', fill = 'toself')

      for (i in 1:nrow(radar_data)) {
          issuer_name <- radar_data$Issuer[i]
          values <- as.numeric(radar_data[i, -1])
          
          p <- p %>% add_trace(
              r = values,
              theta = names(radar_data)[-1],
              name = issuer_name
          )
      }
      
      p %>% layout(
          polar = list(
              radialaxis = list(
                  visible = T,
                  range = c(0, 10)
              )
          ),
          showlegend = TRUE
      )
  })

  # Render Plot 4: Pareto Analysis for OCBC
  output$paretoChart <- renderPlotly({
      req("OCBC" %in% input$selected_issuers)

      # 1. Calculate performance scores
      perf_data <- filtered_data() %>%
        select(Issuer, "Card Benefits" = vn_7002_T07, "Customer Service" = vn_7002_T08, 
               "Online Experience" = vn_7002_T19, "Rates/Fees" = vn_7002_T03) %>%
        pivot_longer(-Issuer, names_to = "Attribute", values_to = "Score") %>%
        group_by(Issuer, Attribute) %>%
        summarise(AvgScore = mean(Score, na.rm = TRUE), .groups = 'drop')

      ocbc_perf <- perf_data %>% filter(Issuer == "OCBC")
      competitor_perf <- perf_data %>% filter(Issuer != "OCBC") %>%
        group_by(Attribute) %>%
        summarise(AvgScore = mean(AvgScore, na.rm = TRUE), .groups = 'drop')

      # 2. Calculate Importance scores
      importance_data <- filtered_data() %>%
        select("Card Benefits" = L1_2, "Customer Service" = L1_8, 
               "Online Experience" = L1_9, "Rates/Fees" = L1_3) %>%
        pivot_longer(everything(), names_to = "Attribute", values_to = "IsImportant") %>%
        group_by(Attribute) %>%
        summarise(Importance = mean(IsImportant, na.rm = TRUE) * 100, .groups = 'drop')

      # 3. Combine and calculate gap
      analysis_df <- left_join(ocbc_perf, competitor_perf, by = "Attribute", suffix = c("_ocbc", "_comp")) %>%
        left_join(importance_data, by = "Attribute") %>%
        mutate(PerformanceGap = AvgScore_comp - AvgScore_ocbc) %>%
        filter(!is.na(PerformanceGap))

      # 4. Create the plot
      p <- ggplot(analysis_df, aes(x = fct_reorder(Attribute, -PerformanceGap), y = PerformanceGap)) +
        geom_col(aes(text = paste("Attribute:", Attribute, "<br>Performance Gap:", round(PerformanceGap, 2))),
                 fill = "firebrick") +
        geom_line(aes(y = Importance / 10, group = 1, 
                      text = paste("Attribute:", Attribute, "<br>Importance:", paste0(round(Importance, 1), "%"))),
                  color = "steelblue", size = 1) +
        geom_point(aes(y = Importance / 10), color = "steelblue", size = 3) +
        scale_y_continuous(
          name = "Performance Gap (Competitor Avg - OCBC Avg)",
          sec.axis = sec_axis(~ . * 10, name = "Importance to Customers (%)")
        ) +
        labs(
            title = "Top Priorities: Where OCBC Lags on Important Attributes",
            x = "Card Attribute", y = "Performance Gap"
        ) +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(p, tooltip = "text")
  })

  # Render Plot 5: Average Satisfaction Bar Chart
  output$satisfactionPlot <- renderPlotly({
    satis_data <- filtered_data() %>%
      group_by(Issuer) %>%
      summarise(avg_satisfaction = mean(satis, na.rm = TRUE)) %>%
      ungroup()
    p <- ggplot(satis_data, aes(x = fct_reorder(Issuer, -avg_satisfaction), y = avg_satisfaction, fill = Issuer,
                                text = paste("Issuer:", Issuer, "<br>Avg. Satisfaction:", round(avg_satisfaction, 2)))) +
      geom_col() +
      labs(title = NULL, x = "Card Issuer", y = "Avg. Satisfaction (1-10)") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })

  # Render Plot 6: Loyalty Distribution Stacked Bar Chart
  output$loyaltyPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Issuer, fill = loyalty, text = paste("Issuer:", Issuer, "<br>Status:", loyalty))) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = NULL, x = "Card Issuer", y = "% of Customers", fill = "Loyalty Status") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set2")
    ggplotly(p, tooltip = "text")
  })

  # Render Plot 7: Satisfaction vs. Recommendation Scatter Plot
  output$scatterPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = satis, y = recomm, color = Issuer, text = paste("Issuer:", Issuer, "<br>Satisfaction:", satis, "<br>Recommend Score:", recomm))) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, aes(color = NULL), color = "black", linetype = "dashed") +
      labs(title = NULL, x = "Customer Satisfaction Score", y = "Likelihood to Recommend", color = "Card Issuer") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  # Render Plot 8: Correlation Heatmap
  output$heatmapPlot <- renderPlotly({
    corr_data <- filtered_data() %>%
      select(satis, confirm, ideal, poverq, soverq, recomm) %>%
      rename(
        Satisfaction = satis, Confirmation = confirm, "Close to Ideal" = ideal,
        "Product Quality" = poverq, "Service Quality" = soverq, Recommendation = recomm
      )
    cor_matrix <- round(cor(corr_data, use = "pairwise.complete.obs"), 2)
    melted_cor <- melt(cor_matrix)
    p <- ggplot(melted_cor, aes(Var1, Var2, fill = value, label = value)) +
      geom_tile() +
      geom_text(size = 3.5, color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name = "Correlation") +
      labs(title = NULL, x = "", y = "") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    ggplotly(p, tooltip = "label")
  })

  # Render Plot 9: Income Profile Bar Chart
  output$incomePlot <- renderPlotly({
    income_data <- filtered_data() %>%
        filter(!is.na(income_level)) %>%
        count(Issuer, income_level) %>%
        group_by(Issuer) %>%
        mutate(proportion = n / sum(n))
    p <- ggplot(income_data, aes(x = income_level, y = proportion, fill = Issuer,
                                 text = paste("Issuer:", Issuer, "<br>Income Level:", income_level, "<br>Proportion:", scales::percent(proportion, accuracy = 0.1)))) +
        geom_col(position = "dodge") +
        scale_y_continuous(labels = scales::percent) +
        labs(x = "Monthly Income (SGD)", y = "Proportion of Customers", fill = "Card Issuer") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })

  # Render Plot 10: Service Quality by Gender
  output$genderPlot <- renderPlotly({
    gender_data <- filtered_data() %>%
      filter(!is.na(gender)) %>%
      group_by(Issuer, gender) %>%
      summarise(avg_soverq = mean(soverq, na.rm = TRUE), .groups = 'drop')
    p <- ggplot(gender_data, aes(x = Issuer, y = avg_soverq, fill = gender,
                                 text = paste("Issuer:", Issuer, "<br>Gender:", gender, "<br>Avg. Service Quality:", round(avg_soverq, 2)))) +
      geom_col(position = "dodge") +
      labs(title = NULL, x = "Card Issuer", y = "Avg. Service Quality (1-10)", fill = "Gender") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Pastel1")
    ggplotly(p, tooltip = "text")
  })

  # Render Plot 11: Service Quality by Age Group
  output$agePlot <- renderPlotly({
    age_data <- filtered_data() %>%
      filter(!is.na(age_group)) %>%
      group_by(Issuer, age_group) %>%
      summarise(avg_soverq = mean(soverq, na.rm = TRUE), .groups = 'drop')
    p <- ggplot(age_data, aes(x = age_group, y = avg_soverq, color = Issuer, group = Issuer,
                              text = paste("Issuer:", Issuer, "<br>Age Group:", age_group, "<br>Avg. Service Quality:", round(avg_soverq, 2)))) +
      geom_line() +
      geom_point() +
      labs(title = NULL, x = "Age Group", y = "Avg. Service Quality (1-10)", color = "Card Issuer") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })

  # Render Plot 12: Satisfaction vs. Market Share
  output$marketSharePlot <- renderPlotly({
    market_data <- filtered_data() %>%
      group_by(Issuer) %>%
      summarise(
        avg_satisfaction = mean(satis, na.rm = TRUE),
        card_count = n(),
        .groups = 'drop'
      ) %>%
      mutate(
        market_share = (card_count / sum(card_count)) * 100
      )

    p <- ggplot(market_data, aes(x = avg_satisfaction, y = market_share, size = card_count, color = Issuer,
                                 text = paste("Issuer:", Issuer,
                                              "<br>Avg. Satisfaction:", round(avg_satisfaction, 2),
                                              "<br>Market Share:", paste0(round(market_share, 1), "%"),
                                              "<br>Respondents:", card_count))) +
      geom_point(alpha = 0.7) +
      scale_size(range = c(5, 25), name = "Number of Respondents") +
      labs(
        x = "Average Customer Satisfaction Score (1-10)",
        y = "Market Share (by % of Respondents)",
        color = "Card Issuer"
      ) +
      theme_minimal(base_size = 12)

    ggplotly(p, tooltip = "text")
  })

  # Render NEW Plot 13: Loyalty Driver Analysis
  output$loyaltyDriverPlot <- renderPlotly({
    
    # Prepare data for modeling
    loyalty_model_data <- filtered_data() %>%
      select(loyalty, "Cashback" = L1_1, "Rewards" = L1_2, "Service" = L1_8, 
             "Online Exp" = L1_9, "Rates/Fees" = L1_3) %>%
      mutate(loyalty_numeric = as.numeric(loyalty) - 1) %>%
      na.omit()
    
    # Check for sufficient data
    if(nrow(loyalty_model_data) < 20 | length(unique(loyalty_model_data$loyalty)) < 2) {
      return(NULL) # Not enough data to build a model
    }
    
    # Build logistic regression model to find loyalty drivers
    model <- glm(loyalty_numeric ~ . - loyalty, data = loyalty_model_data, family = "binomial")
    
    # Extract coefficients (impact on loyalty)
    loyalty_impact <- broom::tidy(model) %>%
      filter(term != "(Intercept)") %>%
      select(Attribute = term, Impact = estimate)
    
    # Calculate importance
    importance_data <- loyalty_model_data %>%
      select(-loyalty, -loyalty_numeric) %>%
      pivot_longer(everything(), names_to = "Attribute", values_to = "IsImportant") %>%
      group_by(Attribute) %>%
      summarise(Importance = mean(IsImportant, na.rm = TRUE), .groups = 'drop')
    
    # Combine and create the plot
    driver_df <- left_join(importance_data, loyalty_impact, by = "Attribute")
    
    p <- ggplot(driver_df, aes(x = Importance, y = Impact, label = Attribute)) +
      geom_point(color = "blue", size = 4, alpha = 0.7) +
      geom_text_repel(box.padding = 0.5) +
      geom_vline(xintercept = mean(driver_df$Importance, na.rm = TRUE), linetype = "dashed", color = "gray50") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(
        title = "Key Drivers of Customer Loyalty",
        x = "Importance to Customer",
        y = "Impact on Loyalty (Model Coefficient)"
      ) +
      theme_minimal(base_size = 12) +
      annotate("text", x = Inf, y = Inf, label = "Key Drivers", hjust = 1.1, vjust = 1.5, color = "gray30", fontface = "italic") +
      annotate("text", x = -Inf, y = Inf, label = "Hidden Gems", hjust = -0.1, vjust = 1.5, color = "gray30", fontface = "italic") +
      annotate("text", x = Inf, y = -Inf, label = "Basic Expectations", hjust = 1.1, vjust = -0.5, color = "gray30", fontface = "italic") +
      annotate("text", x = -Inf, y = -Inf, label = "Low Priority", hjust = -0.1, vjust = -0.5, color = "gray30", fontface = "italic")
    
    ggplotly(p)
  })


  # Render NEW Plot 15: Satisfaction Score Distribution Violin Plot
  output$violinPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Issuer, y = satis, fill = Issuer)) +
      geom_violin(trim = FALSE, alpha = 0.7) +
      geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA, alpha = 0.7) +
      labs(
        title = NULL,
        x = "Card Issuer",
        y = "Satisfaction Score (1-10)"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p, tooltip = "y")
  })

}


# --- 5. Run the Shiny App ---
shinyApp(ui = ui, server = server)

