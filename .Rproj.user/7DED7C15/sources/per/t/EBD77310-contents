library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)
library(lubridate)
library(gridExtra)
library(tidyr)
mf_data <- read.csv("merged_mutual_funds.csv", stringsAsFactors = FALSE)
risk_labels <- c("1" = "Very Low", "2" = "Low", "3" = "Moderate", 
                 "4" = "Moderate High", "5" = "High", "6" = "Very High")

mf_data <- mf_data %>%
  mutate(
    Risk_Label = factor(risk_level, levels = 1:6, labels = risk_labels),
    Risk_Color = dplyr::case_when(
      risk_level == 1 ~ "#2ecc71",
      risk_level == 2 ~ "#27ae60",
      risk_level == 3 ~ "#f39c12",
      risk_level == 4 ~ "#e74c3c",
      risk_level == 5 ~ "#c0392b",
      risk_level == 6 ~ "#8b0000",
      TRUE ~ "#95a5a6"
    ),
    SIP_Status = ifelse(is.na(min_sip), "No", "Yes"),
    Investment_Range = dplyr::case_when(
      min_lumpsum <= 1089 ~ "₹99 - ₹1,089",
      min_lumpsum > 1089 & min_lumpsum <= 5049 ~ "₹1,090 - ₹5,049",
      min_lumpsum > 5049 ~ "₹5,050 - ₹10,000+",
      TRUE ~ "Unknown"
    ),
    AUM_Label = dplyr::case_when(
      AUM..Cr. < 10 ~ "< ₹10 Cr",
      AUM..Cr. >= 10 & AUM..Cr. < 100 ~ "₹10 - ₹100 Cr",
      AUM..Cr. >= 100 & AUM..Cr. < 1000 ~ "₹100 - ₹1,000 Cr",
      AUM..Cr. >= 1000 & AUM..Cr. < 10000 ~ "₹1,000 - ₹10,000 Cr",
      AUM..Cr. >= 10000 ~ "> ₹10,000 Cr",
      TRUE ~ "Unknown"
    )
  )

get_kpi_data <- function(data) {
  list(
    total_schemes = nrow(data),
    categories = n_distinct(data$category),
    avg_1y_return = mean(data$X1Y.Returns...., na.rm = TRUE),
    avg_3y_return = mean(data$X3Y.Returns...., na.rm = TRUE),
    avg_5y_return = mean(data$X5Y.Returns...., na.rm = TRUE),
    avg_expense_ratio = mean(data$expense_ratio, na.rm = TRUE),
    total_aum = sum(data$AUM..Cr., na.rm = TRUE),
    avg_aum = mean(data$AUM..Cr., na.rm = TRUE),
    pct_sip = (sum(!is.na(data$min_sip)) / nrow(data)) * 100,
    amc_count = n_distinct(data$amc_name)
  )
}
ui <- dashboardPage(
  title = "Mutual Funds India Analytics 2025",
  
  dashboardHeader(
    title = div(
      span("Mutual Funds India", style = "font-weight: bold; font-size: 18px;"),
      span("Analytics 2025", style = "font-weight: normal; font-size: 14px; color: #2ecc71; margin-left: 10px;")
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar",
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("chart-pie")),
      menuItem("Returns Analysis", tabName = "returns", icon = icon("line-chart")),
      menuItem("Risk & Ratios", tabName = "risk", icon = icon("sliders")),
      menuItem("AMC Insights", tabName = "amc", icon = icon("building")),
      menuItem("Fund Comparison", tabName = "comparison", icon = icon("columns")),
      menuItem("Performance Analytics", tabName = "performance", icon = icon("line-chart")),
      menuItem("Portfolio Metrics", tabName = "portfolio", icon = icon("pie-chart")),
      menuItem("Sector & Allocation", tabName = "sector", icon = icon("th")),
      menuItem("Raw Data Explorer", tabName = "datatable", icon = icon("table")),
      
      hr(style = "border-color: rgba(255,255,255,0.2);"),
      h4("Global Filters", style = "color: white; padding: 0 15px; margin-top: 20px; font-size: 13px; text-transform: uppercase;"),
      
      div(class = "filter-box", style = "margin: 15px;",
          p("Category", style = "font-size: 12px; font-weight: 600; margin-bottom: 8px; color: #1e3557;"),
          shinyWidgets::pickerInput("filter_category", label = NULL,
                                    choices = c("All", sort(unique(mf_data$category))), selected = "All", multiple = FALSE,
                                    options = shinyWidgets::pickerOptions(noneResultsText = "No matches"))
      ),
      
      div(class = "filter-box", style = "margin: 15px;",
          p("Sub-Category", style = "font-size: 12px; font-weight: 600; margin-bottom: 8px; color: #1e3557;"),
          shinyWidgets::pickerInput("filter_subcategory", label = NULL,
                                    choices = c("All"), selected = "All", multiple = TRUE,
                                    options = shinyWidgets::pickerOptions(noneResultsText = "No matches"))
      ),
      
      div(class = "filter-box", style = "margin: 15px;",
          p("Risk Level", style = "font-size: 12px; font-weight: 600; margin-bottom: 8px; color: #1e3557;"),
          shinyWidgets::pickerInput("filter_risk", label = NULL,
                                    choices = c("All", names(risk_labels)), selected = "All", multiple = TRUE,
                                    options = shinyWidgets::pickerOptions(noneResultsText = "No matches"))
      ),
      
      div(class = "filter-box", style = "margin: 15px;",
          p("AMC Name", style = "font-size: 12px; font-weight: 600; margin-bottom: 8px; color: #1e3557;"),
          shinyWidgets::pickerInput("filter_amc", label = NULL,
                                    choices = c("All", sort(unique(mf_data$amc_name))), selected = "All", multiple = TRUE,
                                    options = shinyWidgets::pickerOptions(noneResultsText = "No matches"))
      ),
      
      div(class = "filter-box", style = "margin: 15px;",
          p("SIP Available", style = "font-size: 12px; font-weight: 600; margin-bottom: 8px; color: #1e3557;"),
          shinyWidgets::pickerInput("filter_sip", label = NULL,
                                    choices = c("All", "Yes", "No"), selected = "All", multiple = FALSE,
                                    options = shinyWidgets::pickerOptions(noneResultsText = "No matches"))
      ),
      
      div(class = "filter-box", style = "margin: 15px;",
          p("Min Investment (₹)", style = "font-size: 12px; font-weight: 600; margin-bottom: 8px; color: #1e3557;"),
          sliderInput("filter_investment", label = NULL, min = 0, max = 25000, value = c(0, 25000), step = 100, ticks = FALSE)
      ),
      
      div(class = "filter-box", style = "margin: 15px;",
          p("AUM Range (₹ Cr)", style = "font-size: 12px; font-weight: 600; margin-bottom: 8px; color: #1e3557;"),
          sliderInput("filter_aum", label = NULL, min = 0, max = 60000, value = c(0, 60000), step = 1000, ticks = FALSE)
      ),
      
      div(style = "margin: 15px;",
          actionButton("reset_filters", "Reset All Filters", class = "btn-block",
                       style = "background-color: #e74c3c; color: white; border: none; border-radius: 5px; padding: 10px;")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        :root { --primary-color: #1e3557; --accent-color: #2ecc71; }
        body { background-color: #f8f9fa; font-family: 'Segoe UI', Tahoma, Geneva, sans-serif; }
        .main-header .navbar { background-color: #1e3557; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }
        .sidebar { background-color: #1e3557; color: white; }
        .sidebar .sidebar-menu > li > a { color: rgba(255,255,255,0.8); border-left: 4px solid transparent; transition: all 0.3s ease; }
        .sidebar .sidebar-menu > li.active > a { background-color: #2ecc71; color: white; border-left: 4px solid white; }
        .sidebar .sidebar-menu > li > a:hover { background-color: rgba(46, 204, 113, 0.2); border-left-color: #2ecc71; }
        .kpi-card { background: white; border-radius: 8px; padding: 20px; margin-bottom: 15px; box-shadow: 0 1px 4px rgba(0,0,0,0.08); border-left: 4px solid #2ecc71; transition: all 0.3s; }
        .kpi-card:hover { transform: translateY(-2px); box-shadow: 0 4px 12px rgba(0,0,0,0.15); }
        .kpi-value { font-size: 28px; font-weight: bold; color: #1e3557; margin: 10px 0; }
        .kpi-label { font-size: 13px; color: #7f8c8d; text-transform: uppercase; letter-spacing: 0.5px; }
        .kpi-unit { font-size: 11px; color: #95a5a6; }
        .chart-container { background: white; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 1px 4px rgba(0,0,0,0.08); }
        .chart-title { font-size: 14px; font-weight: 600; color: #1e3557; margin-bottom: 15px; border-bottom: 2px solid #2ecc71; padding-bottom: 10px; }
        .filter-box { background: white; border-radius: 8px; padding: 15px; margin-bottom: 15px; box-shadow: 0 1px 3px rgba(0,0,0,0.05); }
        .navbar-custom { background-color: #1e3557; padding: 15px 20px; color: white; border-bottom: 2px solid #2ecc71; margin-bottom: 20px; border-radius: 8px; }
        .page-title { font-size: 24px; font-weight: 600; color: white; margin: 0; }
        .page-subtitle { font-size: 13px; color: rgba(255,255,255,0.7); margin: 5px 0 0 0; }
        .box { box-shadow: 0 1px 4px rgba(0,0,0,0.08); border-top: 4px solid #2ecc71; }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "overview",
              div(class = "navbar-custom",
                  div(
                    h2("Dashboard Overview", class = "page-title"),
                    p("Industry metrics, fund distribution & key insights", class = "page-subtitle")
                  )
              ),
              
              fluidRow(
                column(2, uiOutput("kpi_total_schemes")),
                column(2, uiOutput("kpi_categories")),
                column(2, uiOutput("kpi_avg_1y")),
                column(2, uiOutput("kpi_avg_expense")),
                column(2, uiOutput("kpi_total_aum")),
                column(2, uiOutput("kpi_sip_pct"))
              ),
              
              fluidRow(
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Fund Distribution by Category"),
                              plotlyOutput("chart_category_dist", height = "350px"))),
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Risk Level Breakdown"),
                              plotlyOutput("chart_risk_dist", height = "350px")))
              ),
              
              fluidRow(
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "1Y Returns Distribution"),
                              plotlyOutput("chart_returns_hist", height = "350px"))),
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Expense Ratio vs Risk Level"),
                              plotlyOutput("chart_aum_dist", height = "350px")))
              ),
              
              fluidRow(
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Minimum Investment Segments"),
                              plotlyOutput("chart_min_invest", height = "350px"))),
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Top 10 AMCs by AUM"),
                              plotlyOutput("chart_top_amcs", height = "350px")))
              )
      ),
      
      tabItem(tabName = "returns",
              div(class = "navbar-custom",
                  div(
                    h2("Returns Analysis", class = "page-title"),
                    p("Historical returns, trends & comprehensive category comparison", class = "page-subtitle")
                  )
              ),
              
              fluidRow(
                column(4, div(class = "kpi-card",
                              p("1Y Avg Return", class = "kpi-label"),
                              p(textOutput("card_1y_return"), class = "kpi-value"))),
                column(4, div(class = "kpi-card",
                              p("3Y Avg Return", class = "kpi-label"),
                              p(textOutput("card_3y_return"), class = "kpi-value"))),
                column(4, div(class = "kpi-card",
                              p("5Y Avg Return", class = "kpi-label"),
                              p(textOutput("card_5y_return"), class = "kpi-value")))
              ),
              
              fluidRow(
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Returns by Risk Level"),
                              plotlyOutput("chart_returns_by_risk", height = "400px"))),
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", " Returns by Category"),
                              plotlyOutput("chart_returns_by_category", height = "400px")))
              ),
              
              fluidRow(
                column(12, div(class = "chart-container",
                               h4(class = "chart-title", " Category-wise Returns Distribution (1Y, 3Y, 5Y)"),
                               plotlyOutput("chart_category_boxplot", height = "450px")))
              ),
              
              fluidRow(
                column(12, div(class = "chart-container",
                               h4(class = "chart-title", " Data Completeness Heatmap"),
                               plotlyOutput("chart_data_heatmap", height = "200px")))
              )
      ),
      
      tabItem(tabName = "risk",
              div(class = "navbar-custom",
                  div(
                    h2("Risk & Ratio Analysis", class = "page-title"),
                    p("Risk metrics, alpha, beta, Sharpe & Sortino ratios", class = "page-subtitle")
                  )
              ),
              
              fluidRow(
                column(12, div(class = "filter-box",
                               p("Select Return Period", style = "font-size: 12px; font-weight: 600;"),
                               radioButtons("risk_return_period", label = NULL,
                                            choices = list("1 Year" = "1Y", "3 Year" = "3Y", "5 Year" = "5Y"),
                                            selected = "1Y", inline = TRUE)))
              ),
              
              fluidRow(
                column(12, div(class = "chart-container",
                               h4(class = "chart-title", "Risk vs Return Scatter Plot"),
                               plotlyOutput("chart_risk_return_scatter", height = "500px")))
              ),
              
              fluidRow(
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", " Alpha Distribution"),
                              plotlyOutput("chart_alpha_dist", height = "400px"))),
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", " Sharpe Ratio Distribution"),
                              plotlyOutput("chart_sharpe_dist", height = "400px")))
              ),
              
              fluidRow(
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", " Beta Distribution"),
                              plotlyOutput("chart_beta_dist", height = "400px"))),
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", " Sortino Ratio Distribution"),
                              plotlyOutput("chart_sortino_dist", height = "400px")))
              ),
              
              fluidRow(
                column(12, div(class = "chart-container",
                               h4(class = "chart-title", "Top 5 Funds - Quality Indicators (Radar)"),
                               plotlyOutput("chart_radar", height = "500px")))
              )
      ),
      
      tabItem(tabName = "amc",
              div(class = "navbar-custom",
                  div(
                    h2("AMC Insights", class = "page-title"),
                    p("Asset management company performance, rankings & detailed analytics", class = "page-subtitle")
                  )
              ),
              
              fluidRow(
                column(4, div(class = "kpi-card",
                              p("Total AMCs", class = "kpi-label"),
                              p(textOutput("kpi_amc_count"), class = "kpi-value"))),
                column(4, div(class = "kpi-card",
                              p("Avg Funds per AMC", class = "kpi-label"),
                              p(textOutput("kpi_avg_funds_per_amc"), class = "kpi-value"))),
                column(4, div(class = "kpi-card",
                              p("Top AMC AUM", class = "kpi-label"),
                              p(textOutput("kpi_top_amc_aum"), class = "kpi-value")))
              ),
              
              fluidRow(
                column(12, div(class = "chart-container",
                               h4(class = "chart-title", "Top 15 AMCs by Total AUM"),
                               plotlyOutput("chart_amc_ranking", height = "400px")))
              ),
              
              fluidRow(
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "AMC Performance Matrix (Bubble)"),
                              plotlyOutput("chart_amc_performance", height = "400px"))),
                
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Avg Returns by AMC (1Y, 3Y, 5Y)"),
                              plotlyOutput("chart_amc_returns", height = "400px")))
              ),
              
              fluidRow(
                column(12, div(class = "chart-container",
                               h4(class = "chart-title", "Top AMCs: Fund Distribution by Category"),
                               plotlyOutput("chart_amc_category_pie", height = "400px")))
              )
      ),
      
      tabItem(tabName = "comparison",
              div(class = "navbar-custom",
                  div(
                    h2("Fund Comparison Tool", class = "page-title"),
                    p("Compare up to 3 mutual funds side-by-side with advanced analytics", class = "page-subtitle")
                  )
              ),
              
              fluidRow(
                column(4, div(class = "filter-box",
                              p("Fund 1", style = "font-weight: 600;"),
                              shinyWidgets::pickerInput("comp_fund_1", label = NULL,
                                                        choices = c("Select..." = "", mf_data$Scheme.Name),
                                                        options = shinyWidgets::pickerOptions(liveSearch = TRUE)))),
                column(4, div(class = "filter-box",
                              p("Fund 2", style = "font-weight: 600;"),
                              shinyWidgets::pickerInput("comp_fund_2", label = NULL,
                                                        choices = c("Select..." = "", mf_data$Scheme.Name),
                                                        options = shinyWidgets::pickerOptions(liveSearch = TRUE)))),
                column(4, div(class = "filter-box",
                              p("Fund 3 (Optional)", style = "font-weight: 600;"),
                              shinyWidgets::pickerInput("comp_fund_3", label = NULL,
                                                        choices = c("Select..." = "", mf_data$Scheme.Name),
                                                        options = shinyWidgets::pickerOptions(liveSearch = TRUE))))
              ),
              
              fluidRow(
                column(12, div(class = "chart-container",
                               h4(class = "chart-title", " Comparison Details"),
                               DTOutput("table_comparison_details")))
              ),
              
              fluidRow(
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Returns Comparison"),
                              plotlyOutput("chart_comp_returns", height = "350px"))),
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Risk Metrics Comparison"),
                              plotlyOutput("chart_comp_risk", height = "350px")))
              ),
              
              fluidRow(
                column(12, div(class = "chart-container",
                               h4(class = "chart-title", "Comprehensive Fund Quality Radar"),
                               plotlyOutput("chart_comp_radar", height = "500px")))
              )
      ),
      
      tabItem(tabName = "performance",
              div(class = "navbar-custom",
                  div(
                    h2("Performance Analytics", class = "page-title"),
                    p("Risk-adjusted metrics, rolling returns & performance benchmarking", class = "page-subtitle")
                  )
              ),
              
              fluidRow(
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", " Volatility vs Consistency Score"),
                              plotlyOutput("chart_volatility_consistency", height = "380px"))),
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Information Ratio Distribution"),
                              plotlyOutput("chart_info_ratio", height = "380px")))
              ),
              
              fluidRow(
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Cumulative Returns by Risk Bucket"),
                              plotlyOutput("chart_cumulative_returns", height = "380px"))),
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Downside Capture Ratio Analysis"),
                              plotlyOutput("chart_downside_capture", height = "380px")))
              ),fluidRow(
                column(12, div(class = "chart-container",
                               h4(class = "chart-title", "Top Risk-Adjusted Performers"),
                               plotlyOutput("chart_risk_adjusted", height = "380px")))
              )
              
              
      ),
      
      tabItem(tabName = "portfolio",
              div(class = "navbar-custom",
                  div(
                    h2("Portfolio Metrics", class = "page-title"),
                    p("Fund correlations, efficiency frontier & diversification analysis", class = "page-subtitle")
                  )
              ),
              
              fluidRow(
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Fund Correlation Heatmap"),
                              plotlyOutput("chart_correlation_heatmap", height = "450px"))),
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", " Diversification Benefit Index"),
                              plotlyOutput("chart_diversification_index", height = "450px")))
              ),
              
              fluidRow(
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Efficient Frontier (Risk-Return)"),
                              plotlyOutput("chart_efficient_frontier", height = "400px"))),
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Concentration Index by Category"),
                              plotlyOutput("chart_concentration_index", height = "400px")))
              ),
              fluidRow(
                column(12, div(class = "chart-container",
                               h4(class = "chart-title", "Risk–Return Density Map (Hexbin)"),
                               plotlyOutput("chart_hexbin_risk_return", height = "400px")))
                
              )
              
              
      ),
      
      tabItem(tabName = "sector",
              div(class = "navbar-custom",
                  div(
                    h2("Sector & Allocation Analysis", class = "page-title"),
                    p("Category exposure, fund flow trends & allocation patterns", class = "page-subtitle")
                  )
              ),
              
              fluidRow(
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Category-wise AUM Sunburst"),
                              plotlyOutput("chart_category_sunburst", height = "450px"))),
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", " Fund Flow & Growth Trends"),
                              plotlyOutput("chart_fund_flow_trends", height = "450px")))
              ),
              
              fluidRow(
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Category Performance Waterfall"),
                              plotlyOutput("chart_category_waterfall", height = "400px"))),
                column(6, div(class = "chart-container",
                              h4(class = "chart-title", "Risk-Return Allocation Matrix"),
                              plotlyOutput("chart_allocation_matrix", height = "400px")))
              ),
              fluidRow(
                column(12, div(class = "chart-container",
                               h4(class = "chart-title", "Return Stability Ribbon Chart (Category-wise)"),
                               plotlyOutput("chart_stability_ribbon", height = "420px")))
              )
              
      ),
      
      tabItem(tabName = "datatable",
              div(class = "navbar-custom",
                  div(
                    h2("Raw Data Explorer", class = "page-title"),
                    p("Interactive data table with filtering, sorting & export options", class = "page-subtitle")
                  )
              ),
              
              fluidRow(
                column(12, div(class = "filter-box",
                               downloadButton("download_csv", "Download CSV"),
                               downloadButton("download_excel", "Download Excel")))
              ),
              
              fluidRow(
                column(12, div(class = "chart-container",
                               DTOutput("table_full_data")))
              )
      )
    )
  )
)
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- mf_data
    
    if (input$filter_category != "All") {
      data <- data %>% filter(category == input$filter_category)
    }
    
    if (length(input$filter_subcategory) > 0 && !("All" %in% input$filter_subcategory)) {
      data <- data %>% filter(sub_category %in% input$filter_subcategory)
    }
    
    if (length(input$filter_risk) > 0 && !("All" %in% input$filter_risk)) {
      risk_codes <- as.numeric(gsub("\\D", "", input$filter_risk))
      data <- data %>% filter(risk_level %in% risk_codes)
    }
    
    if (length(input$filter_amc) > 0 && !("All" %in% input$filter_amc)) {
      data <- data %>% filter(amc_name %in% input$filter_amc)
    }
    
    if (input$filter_sip != "All") {
      data <- data %>% filter(SIP_Status == input$filter_sip)
    }
    
    data <- data %>%
      filter(min_lumpsum >= input$filter_investment[1] & min_lumpsum <= input$filter_investment[2])
    
    data <- data %>%
      filter(AUM..Cr. >= input$filter_aum[1] & AUM..Cr. <= input$filter_aum[2])
    
    data
  })
  
  observe({
    cat <- input$filter_category
    if (cat == "All") {
      choices <- c("All", sort(unique(mf_data$sub_category)))
    } else {
      choices <- c("All", sort(unique(mf_data$sub_category[mf_data$category == cat])))
    }
    shinyWidgets::updatePickerInput(session, "filter_subcategory", choices = choices)
  })
  
  observeEvent(input$reset_filters, {
    shinyWidgets::updatePickerInput(session, "filter_category", selected = "All")
    shinyWidgets::updatePickerInput(session, "filter_subcategory", selected = "All")
    shinyWidgets::updatePickerInput(session, "filter_risk", selected = "All")
    shinyWidgets::updatePickerInput(session, "filter_amc", selected = "All")
    shinyWidgets::updatePickerInput(session, "filter_sip", selected = "All")
    updateSliderInput(session, "filter_investment", value = c(0, 25000))
    updateSliderInput(session, "filter_aum", value = c(0, 60000))
  })
  
  output$kpi_total_schemes <- renderUI({
    kpi <- get_kpi_data(filtered_data())
    div(class = "kpi-card",
        p("Total Schemes", class = "kpi-label"),
        p(format(kpi$total_schemes, big.mark = ","), class = "kpi-value"))
  })
  
  output$kpi_categories <- renderUI({
    kpi <- get_kpi_data(filtered_data())
    div(class = "kpi-card",
        p("Categories", class = "kpi-label"),
        p(kpi$categories, class = "kpi-value"))
  })
  
  output$kpi_avg_1y <- renderUI({
    kpi <- get_kpi_data(filtered_data())
    div(class = "kpi-card",
        p("Avg 1Y Return", class = "kpi-label"),
        p(paste0(round(kpi$avg_1y_return, 2), "%"), class = "kpi-value"))
  })
  
  output$kpi_avg_expense <- renderUI({
    kpi <- get_kpi_data(filtered_data())
    div(class = "kpi-card",
        p("Avg Expense Ratio", class = "kpi-label"),
        p(paste0(round(kpi$avg_expense_ratio, 2), "%"), class = "kpi-value"))
  })
  
  output$kpi_total_aum <- renderUI({
    kpi <- get_kpi_data(filtered_data())
    div(class = "kpi-card",
        p("Total AUM", class = "kpi-label"),
        p(paste0("₹", format(round(kpi$avg_aum, 0), big.mark = ","), " Cr"), class = "kpi-value"))
  })
  
  output$kpi_sip_pct <- renderUI({
    kpi <- get_kpi_data(filtered_data())
    div(class = "kpi-card",
        p("SIP Available", class = "kpi-label"),
        p(paste0(round(kpi$pct_sip, 0), "%"), class = "kpi-value"))
  })
  
  output$chart_category_dist <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    category_dist <- data %>%
      group_by(category) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(pct = count / sum(count) * 100) %>%
      arrange(desc(count))
    
    plot_ly(data = category_dist, x = ~reorder(category, -count), y = ~pct, type = 'bar',
            marker = list(color = '#2ecc71'), hovertemplate = "%{x}<br>%{y:.1f}%<extra></extra>") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "% of Schemes"),
             font = list(size = 12), margin = list(b = 80), showlegend = FALSE)
  })
  
  output$chart_risk_dist <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    risk_dist <- data %>%
      group_by(Risk_Label) %>%
      summarise(count = n(), .groups = 'drop')
    
    risk_colors <- c('#2ecc71', '#27ae60', '#f39c12', '#e74c3c', '#c0392b', '#8b0000')
    
    plot_ly(data = risk_dist, labels = ~Risk_Label, values = ~count, type = 'pie',
            marker = list(colors = risk_colors),
            hovertemplate = "%{label}<br>%{value} funds (%{percent})<extra></extra>") %>%
      layout(font = list(size = 12), showlegend = TRUE)
  })
  
  output$chart_returns_hist <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    plot_ly(data = data, x = ~X1Y.Returns...., type = 'histogram',
            marker = list(color = '#2ecc71', line = list(color = '#1e3557', width = 0.5)),
            nbinsx = 30, hovertemplate = "%{x:.2f}% - %{y} funds<extra></extra>") %>%
      layout(xaxis = list(title = "1Y Returns (%)"),
             yaxis = list(title = "Number of Funds"),
             font = list(size = 12), showlegend = FALSE)
  })
  
  output$chart_aum_dist <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    plot_data <- data %>%
      filter(!is.na(expense_ratio) & !is.na(risk_level) & expense_ratio > 0) %>%
      mutate(risk_num = as.numeric(risk_level))
    
    if (nrow(plot_data) > 0) {
      p <- plot_ly(data = plot_data, 
                   x = ~expense_ratio, 
                   y = ~risk_num,
                   color = ~Risk_Label,
                   type = 'scatter', mode = 'markers',
                   marker = list(size = 8, opacity = 0.6, line = list(width = 1)),
                   hovertemplate = "<b>%{text}</b><br>Expense: %{x:.2f}%<br>Risk: %{y}<br>%{customdata}<extra></extra>",
                   text = ~Scheme.Name,
                   customdata = ~category) %>%
        layout(
          title = list(text = "💰 Expense Ratio vs Risk Level", x = 0.5),
          xaxis = list(title = "Expense Ratio (%)", range = c(0, max(data$expense_ratio, na.rm = TRUE))),
          yaxis = list(title = "Risk Level", tickmode = 'array', tickvals = 1:6, ticktext = risk_labels),
          font = list(size = 12),
          showlegend = TRUE,
          height = 350
        )
      return(p)
    }
    
    cat_dist <- data %>%
      count(category, sort = TRUE) %>%
      head(10)
    
    plot_ly(data = cat_dist, 
            x = ~reorder(category, n), 
            y = ~n,
            type = 'bar',
            marker = list(color = '#e74c3c'),
            hovertemplate = "%{x}<br>%{y} funds<extra></extra>") %>%
      layout(
        title = list(text = " Top 10 Categories by Fund Count", x = 0.5),
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Number of Funds"),
        font = list(size = 12),
        showlegend = FALSE
      )
  })
  
  
  output$chart_min_invest <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    invest_dist <- data %>%
      group_by(Investment_Range) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(factor(Investment_Range, levels = c("₹99 - ₹1,089", "₹1,090 - ₹5,049", "₹5,050 - ₹10,000+")))
    
    plot_ly(data = invest_dist, x = ~Investment_Range, y = ~count, type = 'bar',
            marker = list(color = '#e74c3c'), hovertemplate = "%{x}<br>%{y} funds<extra></extra>") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Number of Funds"),
             font = list(size = 12), showlegend = FALSE)
  })
  
  output$chart_top_amcs <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    top_amcs <- data %>%
      group_by(amc_name) %>%
      summarise(total_aum = sum(AUM..Cr., na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_aum)) %>%
      head(10)
    
    plot_ly(data = top_amcs, x = ~reorder(amc_name, total_aum), y = ~total_aum, type = 'bar',
            marker = list(color = '#9b59b6'), hovertemplate = "%{x}<br>₹%{y:,.0f} Cr<extra></extra>") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "AUM (₹ Cr)"),
             font = list(size = 11), margin = list(l = 100, b = 100), showlegend = FALSE)
  })
  
  output$card_1y_return <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("N/A")
    paste0(round(mean(data$X1Y.Returns...., na.rm = TRUE), 2), "%")
  })
  
  output$card_3y_return <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("N/A")
    paste0(round(mean(data$X3Y.Returns...., na.rm = TRUE), 2), "%")
  })
  
  output$card_5y_return <- renderText({
    data <- filtered_data()
    if (nrow(data) == 0) return("N/A")
    paste0(round(mean(data$X5Y.Returns...., na.rm = TRUE), 2), "%")
  })
  
  output$chart_returns_by_risk <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    returns_risk <- data %>%
      group_by(Risk_Label) %>%
      summarise(`1Y Avg` = mean(X1Y.Returns...., na.rm = TRUE),
                `3Y Avg` = mean(X3Y.Returns...., na.rm = TRUE),
                `5Y Avg` = mean(X5Y.Returns...., na.rm = TRUE),
                .groups = 'drop')
    
    plot_ly(data = returns_risk) %>%
      add_trace(x = ~Risk_Label, y = ~`1Y Avg`, type = 'scatter', mode = 'lines+markers',
                line = list(color = '#2ecc71', width = 3), name = '1Y') %>%
      add_trace(x = ~Risk_Label, y = ~`3Y Avg`, type = 'scatter', mode = 'lines+markers',
                line = list(color = '#3498db', width = 3), name = '3Y') %>%
      add_trace(x = ~Risk_Label, y = ~`5Y Avg`, type = 'scatter', mode = 'lines+markers',
                line = list(color = '#e74c3c', width = 3), name = '5Y') %>%
      layout(xaxis = list(title = "Risk Level"),
             yaxis = list(title = "Average Returns (%)"),
             font = list(size = 12), hovermode = 'x unified')
  })
  
  output$chart_returns_by_category <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    returns_cat <- data %>%
      group_by(category) %>%
      summarise(`1Y Avg` = mean(X1Y.Returns...., na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(`1Y Avg`))
    
    plot_ly(data = returns_cat, x = ~reorder(category, -`1Y Avg`), y = ~`1Y Avg`, type = 'bar',
            marker = list(color = '#3498db'), hovertemplate = "%{x}<br>%{y:.2f}%<extra></extra>") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Avg Return (%)"),
             font = list(size = 12), showlegend = FALSE)
  })
  
  output$chart_category_boxplot <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    boxplot_data <- data %>%
      select(category, X1Y.Returns...., X3Y.Returns...., X5Y.Returns....) %>%
      pivot_longer(cols = c(X1Y.Returns...., X3Y.Returns...., X5Y.Returns....),
                   names_to = "Period", values_to = "Return") %>%
      mutate(Period = recode(Period, "X1Y.Returns...." = "1Y", "X3Y.Returns...." = "3Y", "X5Y.Returns...." = "5Y"))
    
    plot_ly(data = boxplot_data, x = ~category, y = ~Return, color = ~Period, type = 'box',
            marker = list(opacity = 0.6)) %>%
      layout(xaxis = list(title = "Category"), yaxis = list(title = "Returns (%)"),
             font = list(size = 12), boxmode = 'group')
  })
  
  output$chart_data_heatmap <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    completeness <- data.frame(
      Metric = c("1Y Returns", "3Y Returns", "5Y Returns"),
      Available = c(
        round(sum(!is.na(data$X1Y.Returns....)) / nrow(data) * 100, 1),
        round(sum(!is.na(data$X3Y.Returns....)) / nrow(data) * 100, 1),
        round(sum(!is.na(data$X5Y.Returns....)) / nrow(data) * 100, 1)
      ),
      Missing = c(
        round(sum(is.na(data$X1Y.Returns....)) / nrow(data) * 100, 1),
        round(sum(is.na(data$X3Y.Returns....)) / nrow(data) * 100, 1),
        round(sum(is.na(data$X5Y.Returns....)) / nrow(data) * 100, 1)
      )
    ) %>%
      pivot_longer(cols = c(Available, Missing), names_to = "Status", values_to = "Percentage")
    
    plot_ly(data = completeness, x = ~Status, y = ~Metric, z = ~Percentage, type = 'heatmap',
            colorscale = list(c(0, '#e74c3c'), c(1, '#2ecc71')),
            hovertemplate = "%{y}<br>%{x}<br>%{z:.1f}%<extra></extra>") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""),
             font = list(size = 12))
  })
  
  output$chart_risk_return_scatter <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    return_col <- switch(input$risk_return_period,
                         "1Y" = "X1Y.Returns....",
                         "3Y" = "X3Y.Returns....",
                         "5Y" = "X5Y.Returns....",
                         "X1Y.Returns....")
    
    plot_ly(data = data, x = ~Standard.Deviation, y = ~get(return_col),
            color = ~Risk_Label, type = 'scatter', mode = 'markers',
            marker = list(size = 8, opacity = 0.7),
            hovertemplate = "Risk: %{color}<br>StdDev: %{x:.2f}<br>Return: %{y:.2f}%<extra></extra>") %>%
      layout(xaxis = list(title = "Standard Deviation"),
             yaxis = list(title = paste(input$risk_return_period, "Return (%)")),
             font = list(size = 12))
  })
  
  output$chart_beta_dist <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    plot_ly(data = data, x = ~beta, type = 'histogram',
            marker = list(color = '#9b59b6'), nbinsx = 30,
            hovertemplate = "Beta: %{x:.2f} - %{y} funds<extra></extra>") %>%
      layout(xaxis = list(title = "Beta"), yaxis = list(title = "Frequency"),
             font = list(size = 12), showlegend = FALSE)
  })
  
  output$chart_sortino_dist <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    plot_ly(data = data, x = ~sortino, type = 'histogram',
            marker = list(color = '#1abc9c'), nbinsx = 30,
            hovertemplate = "Sortino: %{x:.2f} - %{y} funds<extra></extra>") %>%
      layout(xaxis = list(title = "Sortino Ratio"),
             yaxis = list(title = "Frequency"),
             font = list(size = 12), showlegend = FALSE)
  })
  
  output$chart_alpha_dist <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    plot_ly(data = data, x = ~alpha, type = 'histogram',
            marker = list(color = '#e74c3c'), nbinsx = 30,
            hovertemplate = "Alpha: %{x:.2f} - %{y} funds<extra></extra>") %>%
      layout(xaxis = list(title = "Alpha"), yaxis = list(title = "Frequency"),
             font = list(size = 12), showlegend = FALSE)
  })
  
  output$chart_sharpe_dist <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    plot_ly(data = data, x = ~sharpe, type = 'histogram',
            marker = list(color = '#3498db'), nbinsx = 30,
            hovertemplate = "Sharpe: %{x:.2f} - %{y} funds<extra></extra>") %>%
      layout(xaxis = list(title = "Sharpe Ratio"), yaxis = list(title = "Frequency"),
             font = list(size = 12), showlegend = FALSE)
  })
  
  output$chart_radar <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    top5 <- data %>%
      arrange(desc(X1Y.Returns....)) %>%
      slice(1:min(5, nrow(data))) %>%
      select(Scheme.Name, sharpe, alpha, X1Y.Returns...., expense_ratio)
    
    if (nrow(top5) == 0) return(plotly::plotly_empty())
    
    max_sharpe  <- max(as.numeric(data$sharpe), na.rm = TRUE)
    max_alpha   <- max(as.numeric(data$alpha), na.rm = TRUE)
    max_return  <- max(as.numeric(data$X1Y.Returns....), na.rm = TRUE)
    max_expense <- max(as.numeric(data$expense_ratio), na.rm = TRUE)
    
    max_sharpe  <- ifelse(is.finite(max_sharpe),  max_sharpe,  1)
    max_alpha   <- ifelse(is.finite(max_alpha),   max_alpha,   1)
    max_return  <- ifelse(is.finite(max_return),  max_return,  1)
    max_expense <- ifelse(is.finite(max_expense), max_expense, 1)
    
    p <- plot_ly()
    
    for (i in 1:nrow(top5)) {
      
      sharpe_val  <- ifelse(is.na(top5$sharpe[i]), 0, as.numeric(top5$sharpe[i]))
      alpha_val   <- ifelse(is.na(top5$alpha[i]), 0, as.numeric(top5$alpha[i]))
      return_val  <- ifelse(is.na(top5$X1Y.Returns....[i]), 0, as.numeric(top5$X1Y.Returns....[i]))
      expense_val <- ifelse(is.na(top5$expense_ratio[i]), 0, as.numeric(top5$expense_ratio[i]))
      
      r_vals <- c(
        (sharpe_val / max_sharpe) * 100,
        (alpha_val / max_alpha) * 100,
        (return_val / max_return) * 100,
        (1 - expense_val / max_expense) * 100,  
        100  
      )
      
      p <- p %>% add_trace(
        type = "scatterpolar",
        r = r_vals,
        theta = c("Sharpe", "Alpha", "1Y Return", "Low Expense", "Reference"),
        fill = "toself",
        name = substr(top5$Scheme.Name[i], 1, 20),
        hovertemplate = "%{theta}<br>%{r:.1f}%<extra></extra>"
      )
    }
    p %>% layout(
      polar = list(
        radialaxis = list(visible = TRUE, range = c(0, 100))
      ),
      showlegend = TRUE,
      font = list(size = 11),
      hovermode = "closest"
    )
  })
  output$kpi_amc_count <- renderText({
    n_distinct(filtered_data()$amc_name)
  })
  
  output$kpi_avg_funds_per_amc <- renderText({
    data <- filtered_data()
    amc_counts <- data %>% group_by(amc_name) %>% summarise(n = n(), .groups = 'drop')
    round(mean(amc_counts$n), 0)
  })
  
  output$kpi_top_amc_aum <- renderText({
    data <- filtered_data()
    top_aum <- data %>%
      group_by(amc_name) %>%
      summarise(total = sum(AUM..Cr., na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total)) %>%
      slice(1)
    paste0("₹", format(round(top_aum$total[1], 0), big.mark = ","), " Cr")
  })
  
  output$chart_amc_ranking <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    amc_rank <- data %>%
      group_by(amc_name) %>%
      summarise(total_aum = sum(AUM..Cr., na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(total_aum)) %>%
      head(15)
    
    plot_ly(data = amc_rank, x = ~reorder(amc_name, total_aum), y = ~total_aum, type = 'bar',
            marker = list(color = '#2ecc71'), hovertemplate = "%{x}<br>₹%{y:,.0f} Cr<extra></extra>") %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Total AUM (₹ Cr)"),
             font = list(size = 10), margin = list(b = 100, l = 100), showlegend = FALSE)
  })
  
  output$chart_amc_performance <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    amc_perf <- data %>%
      group_by(amc_name) %>%
      summarise(
        total_aum = sum(AUM..Cr., na.rm = TRUE),
        avg_return = mean(X1Y.Returns...., na.rm = TRUE),
        fund_count = n(),
        avg_expense = mean(expense_ratio, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(total_aum > 0, !is.na(avg_return)) %>%
      arrange(desc(total_aum)) %>%
      head(30)
    
    plot_ly(data = amc_perf, 
            x = ~avg_return, 
            y = ~log10(total_aum),
            size = ~fund_count, 
            color = ~avg_expense,
            hovertemplate = "<b>%{text}</b><br>1Y Return: %{x:.2f}%<br>AUM: ₹%{customdata:,.0f} Cr<br>Funds: %{size}<br>Expense: %{marker.color:.2f}%<extra></extra>",
            text = ~amc_name, 
            customdata = ~total_aum) %>%
      add_markers(marker = list(
        sizemode = 'area',
        sizeref = 0.5,
        line = list(width = 1, color = '#ffffff'),
        colorscale = 'Viridis',
        reversescale = TRUE
      )) %>%
      layout(
        title = list(text = "", x = 0.5),
        xaxis = list(title = "Avg 1Y Return (%)"),
        yaxis = list(title = "Log(AUM ₹ Cr)", tickformat = ".0f"),
        font = list(size = 12),
        showlegend = FALSE,
        hovermode = 'closest'
      )
  })
  
  output$chart_amc_returns <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    amc_returns <- data %>%
      group_by(amc_name) %>%
      summarise(
        `1Y_Avg` = mean(X1Y.Returns...., na.rm = TRUE),
        `3Y_Avg` = mean(X3Y.Returns...., na.rm = TRUE),
        `5Y_Avg` = mean(X5Y.Returns...., na.rm = TRUE),
        fund_count = n(),
        .groups = 'drop'
      ) %>%
      filter(!is.na(`1Y_Avg`)) %>%
      arrange(desc(`1Y_Avg`)) %>%
      head(20)
    
    plot_ly(data = amc_returns, 
            x = ~reorder(amc_name, `1Y_Avg`), 
            y = ~`1Y_Avg`,
            type = 'bar',
            name = '1Y',
            marker = list(color = '#2ecc71')) %>%
      add_trace(y = ~`3Y_Avg`, name = '3Y', marker = list(color = '#3498db')) %>%
      add_trace(y = ~`5Y_Avg`, name = '5Y', marker = list(color = '#e74c3c')) %>%
      layout(
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Average Returns (%)"),
        barmode = 'group',
        font = list(size = 11),
        showlegend = TRUE,
        legend = list(x = 1.05, y = 1),
        margin = list(l = 100, b = 120)
      )
  })
  
  output$chart_amc_category_pie <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    top_amcs <- data %>%
      group_by(amc_name) %>%
      summarise(total_funds = n(), .groups = 'drop') %>%
      arrange(desc(total_funds)) %>%
      head(6)
    
    amc_category_dist <- data %>%
      filter(amc_name %in% top_amcs$amc_name) %>%
      count(amc_name, category) %>%
      mutate(pct = n / sum(n) * 100)
    
    plot_ly(data = amc_category_dist, 
            labels = ~paste(amc_name, category, sep = " - "), 
            values = ~pct,
            type = 'pie',
            marker = list(colors = c('#2ecc71', '#3498db', '#e74c3c', '#f39c12', '#9b59b6', '#1abc9c')),
            hovertemplate = "%{label}<br>%{value:.1f}% (%{text} funds)<extra></extra>",
            text = ~n,
            textinfo = 'none') %>%
      layout(
        title = list(text = "Top AMCs: Fund Distribution by Category", x = 0.5),
        font = list(size = 12),
        showlegend = TRUE,
        legend = list(x = 1, y = 0.5)
      )
  })
  
  output$table_comparison_details <- renderDT({
    funds <- c(input$comp_fund_1, input$comp_fund_2, input$comp_fund_3)
    funds <- funds[funds != ""]
    
    if (length(funds) == 0) {
      return(datatable(data.frame(Message = "Select funds to compare"), 
                       options = list(dom = 't')))
    }
    
    comp_data <- mf_data %>%
      filter(Scheme.Name %in% funds) %>%
      select(Scheme.Name, category, expense_ratio, risk_level, AUM..Cr., X1Y.Returns....,
             sharpe, alpha, beta, sortino)
    
    datatable(comp_data, options = list(pageLength = 10, dom = 'tp', scrollX = TRUE))
  })
  
  output$chart_comp_returns <- renderPlotly({
    funds <- c(input$comp_fund_1, input$comp_fund_2, input$comp_fund_3)
    funds <- funds[funds != ""]
    if (length(funds) == 0) return(plotly::plotly_empty())
    
    comp_data <- mf_data %>%
      filter(Scheme.Name %in% funds) %>%
      select(Scheme.Name, X1Y.Returns...., X3Y.Returns...., X5Y.Returns....)
    
    plot_ly(comp_data, x = ~Scheme.Name, y = ~X1Y.Returns...., type = 'bar', name = '1Y') %>%
      add_trace(y = ~X3Y.Returns...., name = '3Y') %>%
      add_trace(y = ~X5Y.Returns...., name = '5Y') %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Returns (%)"),
             barmode = 'group', font = list(size = 11))
  })
  
  output$chart_comp_risk <- renderPlotly({
    funds <- c(input$comp_fund_1, input$comp_fund_2, input$comp_fund_3)
    funds <- funds[funds != ""]
    if (length(funds) == 0) return(plotly::plotly_empty())
    
    comp_data <- mf_data %>%
      filter(Scheme.Name %in% funds) %>%
      select(Scheme.Name, sharpe, alpha, beta, sortino)
    
    plot_ly(comp_data, x = ~Scheme.Name, y = ~sharpe, type = 'bar', name = 'Sharpe') %>%
      add_trace(y = ~alpha, name = 'Alpha') %>%
      add_trace(y = ~beta, name = 'Beta') %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Ratio Values"),
             barmode = 'group', font = list(size = 11))
  })
  
  output$chart_comp_radar <- renderPlotly({
    funds <- c(input$comp_fund_1, input$comp_fund_2, input$comp_fund_3)
    funds <- funds[funds != ""]
    if (length(funds) == 0) return(plotly::plotly_empty())
    
    comp_data <- mf_data %>%
      filter(Scheme.Name %in% funds) %>%
      select(Scheme.Name, sharpe, alpha, X1Y.Returns...., expense_ratio, Standard.Deviation)
    
    if (nrow(comp_data) == 0) return(plotly::plotly_empty())
    
    max_sharpe <- max(as.numeric(mf_data$sharpe), na.rm = TRUE)
    max_alpha <- max(as.numeric(mf_data$alpha), na.rm = TRUE)
    max_return <- max(as.numeric(mf_data$X1Y.Returns....), na.rm = TRUE)
    max_exp <- max(as.numeric(mf_data$expense_ratio), na.rm = TRUE)
    max_sd <- max(as.numeric(mf_data$Standard.Deviation), na.rm = TRUE)
    
    if (!is.finite(max_sharpe) || max_sharpe <= 0) max_sharpe <- 1
    if (!is.finite(max_alpha) || max_alpha <= 0) max_alpha <- 1
    if (!is.finite(max_return) || max_return <= 0) max_return <- 1
    if (!is.finite(max_exp) || max_exp <= 0) max_exp <- 1
    if (!is.finite(max_sd) || max_sd <= 0) max_sd <- 1
    
    traces <- list()
    for (i in 1:nrow(comp_data)) {
      sharpe_val <- as.numeric(comp_data$sharpe[i])
      alpha_val <- as.numeric(comp_data$alpha[i])
      return_val <- as.numeric(comp_data$X1Y.Returns....[i])
      expense_val <- as.numeric(comp_data$expense_ratio[i])
      sd_val <- as.numeric(comp_data$Standard.Deviation[i])
      
      sharpe_val <- ifelse(is.na(sharpe_val), 0, sharpe_val)
      alpha_val <- ifelse(is.na(alpha_val), 0, alpha_val)
      return_val <- ifelse(is.na(return_val), 0, return_val)
      expense_val <- ifelse(is.na(expense_val), 0, expense_val)
      sd_val <- ifelse(is.na(sd_val), 0, sd_val)
      
      r <- list(
        type = 'scatterpolar',
        r = c(
          (sharpe_val / max_sharpe) * 100,
          (alpha_val / max_alpha) * 100,
          (return_val / max_return) * 100,
          (1 - expense_val / max_exp) * 100,
          (1 - sd_val / max_sd) * 100
        ),
        theta = c('Sharpe', 'Alpha', '1Y Return', 'Low Expense', 'Low Risk'),
        fill = 'toself',
        name = substr(comp_data$Scheme.Name[i], 1, 25),
        opacity = 0.7,
        hovertemplate = "%{theta}<br>%{r:.1f}%<extra></extra>"
      )
      traces[[i]] <- r
    }
    
    if (length(traces) > 0) {
      p <- plot_ly()
      for (t in traces) {
        p <- add_trace(p, type = t$type, r = t$r, theta = t$theta, fill = t$fill, 
                       name = t$name, opacity = t$opacity)
      }
      
      p %>% layout(
        polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
        title = "Comprehensive Fund Quality Comparison",
        font = list(size = 12),
        hovermode = 'closest',
        height = 500
      )
    } else {
      plotly::plotly_empty()
    }
  })
  
  output$table_full_data <- renderDT({
    datatable(filtered_data(),
              options = list(pageLength = 20, scrollX = TRUE),
              filter = 'top')
  })
  
  output$download_csv <- downloadHandler(
    filename = function() { paste("mutual_funds_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(filtered_data(), file, row.names = FALSE) }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() { paste("mutual_funds_", Sys.Date(), ".xlsx", sep = "") },
    content = function(file) { openxlsx::write.xlsx(filtered_data(), file) }
  )
  
  
  output$chart_volatility_consistency <- renderPlotly({
    data <- as.data.frame(filtered_data())
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    safe_num <- function(x) suppressWarnings(as.numeric(x))
    
    perf_data <- data %>%
      mutate(
        Standard.Deviation = safe_num(Standard.Deviation),
        X1Y.Returns.... = safe_num(X1Y.Returns....)
      ) %>%
      filter(!is.na(Standard.Deviation) & !is.na(X1Y.Returns....)) %>%
      mutate(
        Consistency = X1Y.Returns.... / (Standard.Deviation + 0.01),
        Volatility_Group = cut(Standard.Deviation, breaks = 5,
                               labels = c("V.Low", "Low", "Mod", "High", "V.High"))
      ) %>%
      group_by(Volatility_Group) %>%
      summarise(
        Avg_Consistency = mean(Consistency, na.rm = TRUE),
        Avg_Volatility = mean(Standard.Deviation, na.rm = TRUE),
        Fund_Count = n(),
        .groups = 'drop'
      )
    
    if (nrow(perf_data) == 0) return(plotly::plotly_empty())
    max_size <- max(perf_data$Fund_Count, na.rm = TRUE)
    desired_max_px <- 60 
    sizeref_value <- 2.0 * max_size / (desired_max_px^2)
    
    plot_ly(
      data = perf_data,
      x = ~Avg_Volatility,
      y = ~Avg_Consistency,
      type = 'scatter',
      mode = 'markers+text',
      text = ~Volatility_Group,
      textposition = 'top center',
      color = ~Volatility_Group,
      size = ~Fund_Count,
      sizes = c(20, 60),                     
      marker = list(
        sizemode = "area",
        sizeref = sizeref_value,
        line = list(width = 2, color = 'white'),
        opacity = 0.75
      ),
      
      hoverinfo = "none",
      hovertemplate = paste(
        "<b>%{text}</b><br>",
        "Volatility: %{x:.2f}<br>",
        "Consistency: %{y:.2f}<br>",
        "Funds: %{customdata}<extra></extra>"
      ),
      customdata = ~Fund_Count
    ) %>%
      layout(
        title = list(text = "📉 Volatility vs Consistency", x = 0.5),
        xaxis = list(title = "Average Volatility (Std Dev)"),
        yaxis = list(title = "Consistency Score (Return/Risk)"),
        font = list(size = 12),
        showlegend = FALSE,
        hovermode = 'closest'
      )
  })
  output$chart_info_ratio <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    info_data <- data %>%
      filter(!is.na(Standard.Deviation) & !is.na(X1Y.Returns....)) %>%
      mutate(
        Info_Ratio = as.numeric(X1Y.Returns....) / (as.numeric(Standard.Deviation) + 0.01)
      ) %>%
      filter(!is.na(Info_Ratio) & is.finite(Info_Ratio)) %>%
      arrange(desc(Info_Ratio)) %>%
      head(30) %>%
      mutate(
        Fund_Short = substr(Scheme.Name, 1, 12)
      )
    
    if (nrow(info_data) == 0) return(plotly::plotly_empty())
    
    plot_ly(data = info_data,
            x = ~reorder(Fund_Short, Info_Ratio),
            y = ~Info_Ratio,
            type = 'bar',
            marker = list(
              color = ~Info_Ratio,
              colorscale = 'Viridis',
              showscale = TRUE,
              reversescale = FALSE,
              colorbar = list(title = "Info Ratio", thickness = 20, len = 0.7)
            ),
            hovertemplate = "%{x}<br>Ratio: %{y:.3f}<extra></extra>") %>%
      layout(
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Information Ratio"),
        font = list(size = 10),
        margin = list(b = 120, l = 60),
        showlegend = FALSE
      )
  })
  
  output$chart_cumulative_returns <- renderPlotly({
    data <- as.data.frame(filtered_data())
    if (nrow(data) == 0) return(plotly::plotly_empty())
    safe_num <- function(x) suppressWarnings(as.numeric(x))
    
    cum_data <- data %>%
      mutate(
        risk_level = safe_num(risk_level),
        X1Y.Returns.... = safe_num(X1Y.Returns....),
        X3Y.Returns.... = safe_num(X3Y.Returns....),
        X5Y.Returns.... = safe_num(X5Y.Returns....)
      ) %>%
      filter(!is.na(risk_level)) %>%
      mutate(
        Risk_Bucket = cut(
          risk_level, 
          breaks = c(0, 2, 4, 6), 
          labels = c("Low Risk", "Medium Risk", "High Risk"),
          include.lowest = TRUE
        )
      ) %>%
      group_by(Risk_Bucket) %>%
      summarise(
        `1Y Cumul` = mean(X1Y.Returns...., na.rm = TRUE),
        `3Y Cumul` = mean(X3Y.Returns...., na.rm = TRUE),
        `5Y Cumul` = mean(X5Y.Returns...., na.rm = TRUE),
        Fund_Count = n(),
        .groups = "drop"
      ) %>%
      filter(!is.na(Risk_Bucket))
    
    if (nrow(cum_data) == 0) return(plotly::plotly_empty())
    
    plot_ly(cum_data) %>%
      add_trace(
        x = ~Risk_Bucket, y = ~`1Y Cumul`,
        type = "scatter", mode = "lines+markers",
        line = list(color = "#2ecc71", width = 3),
        marker = list(size = 12),
        name = "1Y"
      ) %>%
      add_trace(
        x = ~Risk_Bucket, y = ~`3Y Cumul`,
        type = "scatter", mode = "lines+markers",
        line = list(color = "#3498db", width = 3),
        marker = list(size = 12),
        name = "3Y"
      ) %>%
      add_trace(
        x = ~Risk_Bucket, y = ~`5Y Cumul`,
        type = "scatter", mode = "lines+markers",
        line = list(color = "#e74c3c", width = 3),
        marker = list(size = 12),
        name = "5Y"
      ) %>%
      layout(
        title = list(text = "📈 Cumulative Returns by Risk Category", x = 0.5),
        xaxis = list(title = "Risk Category"),
        yaxis = list(title = "Cumulative Returns (%)"),
        hovermode = "x unified",
        showlegend = TRUE,
        font = list(size = 13),
        plot_bgcolor = "rgba(240,240,240,0.5)"
      )
    
  })
  
  output$chart_downside_capture <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    downside_data <- data %>%
      filter(!is.na(Standard.Deviation) & !is.na(X1Y.Returns....)) %>%
      mutate(
        Max_SD = max(as.numeric(Standard.Deviation), na.rm = TRUE),
        Downside_Capture = as.numeric(X1Y.Returns....) * (1 - (as.numeric(Standard.Deviation) / pmax(Max_SD, 0.01)))
      ) %>%
      filter(!is.na(Downside_Capture) & is.finite(Downside_Capture)) %>%
      arrange(desc(Downside_Capture)) %>%
      head(25) %>%
      mutate(
        Fund_Short = substr(Scheme.Name, 1, 12)
      )
    
    if (nrow(downside_data) == 0) return(plotly::plotly_empty())
    
    plot_ly(data = downside_data,
            y = ~reorder(Fund_Short, Downside_Capture),
            x = ~Downside_Capture,
            type = 'bar',
            orientation = 'h',
            marker = list(
              color = ~Downside_Capture,
              colorscale = 'RdYlGn',
              showscale = TRUE,
              colorbar = list(title = "Downside<br>Capture", thickness = 20, len = 0.7)
            ),
            hovertemplate = "%{y}<br>Score: %{x:.2f}%<extra></extra>") %>%
      layout(
        xaxis = list(title = "Downside Capture Ratio", showgrid = TRUE),
        yaxis = list(title = ""),
        font = list(size = 10),
        margin = list(l = 150),
        showlegend = FALSE
      )
  })
  output$chart_risk_adjusted <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    safe_num <- function(x) suppressWarnings(as.numeric(x))
    
    riskadj_data <- data %>%
      mutate(
        StdDev = safe_num(Standard.Deviation),
        Ret1Y = safe_num(X1Y.Returns....),
        RiskAdj = Ret1Y / (StdDev + 0.01)
      ) %>%
      filter(!is.na(RiskAdj) & is.finite(RiskAdj)) %>%
      arrange(desc(RiskAdj)) %>%
      slice(1:20) %>%
      mutate(Fund_Short = substr(Scheme.Name, 1, 15))
    
    plot_ly(
      data = riskadj_data,
      x = ~RiskAdj,
      y = ~reorder(Fund_Short, RiskAdj),
      type = "bar",
      orientation = "h",
      marker = list(
        color = ~RiskAdj,
        colorscale = "Blues",
        showscale = TRUE,
        colorbar = list(title = "Risk-Adj<br>Score", thickness = 20, len = 0.7)
      ),
      hovertemplate = "%{y}<br>Score: %{x:.3f}<extra></extra>"
    ) %>%
      layout(
        title = list(text = "📌 Top Risk-Adjusted Performers", x = 0.5),
        xaxis = list(title = "Risk-Adjusted Score", showgrid = TRUE),
        yaxis = list(title = ""),
        margin = list(l = 150),
        font = list(size = 11),
        showlegend = FALSE
      )
  })
  
  
  output$chart_correlation_heatmap <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    top_funds <- data %>%
      filter(!is.na(AUM..Cr.)) %>%
      arrange(desc(AUM..Cr.)) %>%
      head(12)
    
    if (nrow(top_funds) < 2) return(plotly::plotly_empty())
    
    returns_matrix <- top_funds %>%
      select(X1Y.Returns...., X3Y.Returns...., X5Y.Returns...., 
             Standard.Deviation, sharpe, alpha) %>%
      mutate(across(everything(), as.numeric)) %>%
      as.matrix()
    
    corr_matrix <- cor(returns_matrix, use = "complete.obs")
    
    if (anyNA(corr_matrix) || !is.matrix(corr_matrix)) {
      return(plotly::plotly_empty())
    }
    
    col_names <- c("1Y Return", "3Y Return", "5Y Return", "Std Dev", "Sharpe", "Alpha")
    
    plot_ly(z = corr_matrix,
            x = col_names,
            y = col_names,
            type = 'heatmap',
            colorscale = 'RdBu',
            zmid = 0,
            zmin = -1,
            zmax = 1,
            hovertemplate = "%{y} vs %{x}<br>Correlation: %{z:.3f}<extra></extra>",
            colorbar = list(title = "Correlation", thickness = 20, len = 0.7)) %>%
      layout(
        xaxis = list(title = "", tickangle = -45, side = 'bottom'),
        yaxis = list(title = ""),
        font = list(size = 10),
        margin = list(b = 100, l = 100),
        height = 450
      )
  })
  
  output$chart_diversification_index <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    div_data <- data %>%
      filter(!is.na(AUM..Cr.) & !is.na(X1Y.Returns....) & !is.na(Standard.Deviation)) %>%
      group_by(category) %>%
      summarise(
        Category_AUM = sum(as.numeric(AUM..Cr.), na.rm = TRUE),
        Fund_Count = n(),
        Avg_Return = mean(as.numeric(X1Y.Returns....), na.rm = TRUE),
        Volatility = mean(as.numeric(Standard.Deviation), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(!is.na(Avg_Return) & !is.na(Volatility)) %>%
      mutate(
        Total_Funds = sum(Fund_Count),
        Div_Index = as.numeric((Fund_Count / Total_Funds) * (1 + Avg_Return / 100) * pmax(1 - Volatility / 100, 0.01))
      ) %>%
      filter(!is.na(Div_Index) & is.finite(Div_Index)) %>%
      arrange(desc(Div_Index))
    
    if (nrow(div_data) == 0) return(plotly::plotly_empty())
    
    colors <- c('#2ecc71', '#3498db', '#e74c3c', '#f39c12', '#9b59b6', '#1abc9c', '#e67e22', '#34495e')
    
    plot_ly(data = div_data,
            labels = ~category,
            values = ~Div_Index,
            type = 'pie',
            marker = list(colors = colors[1:nrow(div_data)], line = list(width = 2, color = 'white')),
            textposition = 'inside',
            textinfo = 'label+percent',
            hovertemplate = "<b>%{label}</b><br>Diversification Index: %{value:.2f}<br>Funds: %{customdata}<extra></extra>",
            customdata = ~Fund_Count) %>%
      layout(
        title = list(text = "Diversification Index by Category", x = 0.5),
        font = list(size = 12),
        showlegend = TRUE
      )
  })
  
  output$chart_efficient_frontier <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    frontier_data <- data %>%
      filter(!is.na(Standard.Deviation) & !is.na(X1Y.Returns....)) %>%
      arrange(Standard.Deviation)
    
    plot_ly(data = frontier_data,
            x = ~Standard.Deviation,
            y = ~X1Y.Returns....,
            color = ~category,
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 8, opacity = 0.6, line = list(width = 1)),
            hovertemplate = "<b>%{text}</b><br>Risk: %{x:.2f}<br>Return: %{y:.2f}%<br>Category: %{color}<extra></extra>",
            text = ~substr(Scheme.Name, 1, 20)) %>%
      layout(
        xaxis = list(title = "Risk (Standard Deviation)"),
        yaxis = list(title = "Expected Return (1Y %)"),
        font = list(size = 12),
        hovermode = 'closest',
        showlegend = TRUE
      )
  })
  
  output$chart_concentration_index <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    conc_data <- data %>%
      group_by(category) %>%
      summarise(
        Total_AUM = sum(AUM..Cr., na.rm = TRUE),
        Fund_Count = n(),
        Avg_AUM = mean(AUM..Cr., na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        Concentration = (Total_AUM / sum(Total_AUM)) * 100
      ) %>%
      arrange(desc(Concentration))
    
    plot_ly(data = conc_data,
            x = ~reorder(category, Concentration),
            y = ~Concentration,
            type = 'bar',
            marker = list(
              color = ~Concentration,
              colorscale = 'Sunset',
              showscale = TRUE,
              colorbar = list(title = "% of Total AUM")
            ),
            hovertemplate = "%{x}<br>%{y:.1f}% of AUM<br>%{customdata} funds<extra></extra>",
            customdata = ~Fund_Count) %>%
      layout(
        xaxis = list(title = "Category"),
        yaxis = list(title = "Concentration (% of Total AUM)"),
        font = list(size = 12),
        showlegend = FALSE
      )
  })
  output$chart_hexbin_risk_return <- renderPlotly({
    data <- as.data.frame(filtered_data())
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    safe_num <- function(x) suppressWarnings(as.numeric(x))
    
    df <- data %>%
      mutate(
        StdDev = safe_num(Standard.Deviation),
        Ret1Y = safe_num(X1Y.Returns....)
      ) %>%
      filter(!is.na(StdDev) & !is.na(Ret1Y))
    
    if (nrow(df) == 0) return(plotly::plotly_empty())
    
    plot_ly(
      x = df$StdDev,
      y = df$Ret1Y,
      type = "histogram2dcontour",
      colorscale = "Electric",
      contours = list(
        showlines = TRUE,
        coloring = "heatmap"
      ),
      hovertemplate = "Volatility: %{x:.2f}<br>Return: %{y:.2f}<extra></extra>"
    ) %>%
      add_markers(
        x = df$StdDev,
        y = df$Ret1Y,
        opacity = 0.4,
        marker = list(size = 6)
      ) %>%
      layout(
        title = list(text = "🎯 Risk–Return Density Map (Hexbin Style)", x = 0.5),
        xaxis = list(title = "Volatility (Std Dev)"),
        yaxis = list(title = "1Y Return (%)"),
        hovermode = "closest",
        font = list(size = 11)
      )
  })
  
  
  output$chart_category_sunburst <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    sunburst_data <- data %>%
      filter(!is.na(category) & !is.na(AUM..Cr.) & !is.na(Risk_Label)) %>%
      mutate(
        AUM_Clean = as.numeric(AUM..Cr.)
      ) %>%
      group_by(category, Risk_Label) %>%
      summarise(
        AUM = sum(AUM_Clean, na.rm = TRUE),
        Count = n(),
        .groups = 'drop'
      ) %>%
      filter(AUM > 0)
    
    total_aum <- sum(sunburst_data$AUM, na.rm = TRUE)
    
    if (nrow(sunburst_data) == 0 || total_aum <= 0) {
      return(plotly::plotly_empty())
    }
    
    labels <- c("Total AUM", unique(sunburst_data$category), sunburst_data$Risk_Label)
    parents <- c("", rep("Total AUM", length(unique(sunburst_data$category))), sunburst_data$category)
    values <- c(total_aum, 
                tapply(sunburst_data$AUM, sunburst_data$category, sum, na.rm = TRUE),
                sunburst_data$AUM)
    
    plot_ly(
      labels = labels,
      parents = parents,
      values = as.numeric(values),
      type = 'sunburst',
      marker = list(line = list(width = 2, color = 'white'), colorscale = 'Viridis'),
      textposition = 'inside',
      hovertemplate = "<b>%{label}</b><br>AUM: ₹%{value:,.0f} Cr<extra></extra>"
    ) %>%
      layout(font = list(size = 11))
  })
  
  output$chart_fund_flow_trends <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    flow_data <- data %>%
      filter(!is.na(category)) %>%
      group_by(category) %>%
      summarise(
        Total_Funds = n(),
        Avg_AUM = mean(as.numeric(AUM..Cr.), na.rm = TRUE),
        Avg_Return = mean(as.numeric(X1Y.Returns....), na.rm = TRUE),
        Total_AUM = sum(as.numeric(AUM..Cr.), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(!is.na(Avg_Return))
    
    if (nrow(flow_data) == 0) return(plotly::plotly_empty())
    
    max_aum <- max(flow_data$Total_AUM, na.rm = TRUE)
    flow_data <- flow_data %>%
      mutate(
        Growth_Indicator = ifelse(max_aum > 0, Avg_Return * (Total_AUM / max_aum), Avg_Return)
      )
    
    plot_ly(data = flow_data) %>%
      add_trace(x = ~category, y = ~Total_Funds, type = 'bar', name = 'Fund Count',
                marker = list(color = '#2ecc71'), yaxis = 'y') %>%
      add_trace(x = ~category, y = ~Avg_Return, type = 'scatter', mode = 'lines+markers',
                name = 'Avg Return (%)', yaxis = 'y2', line = list(color = '#e74c3c', width = 3), 
                marker = list(size = 8)) %>%
      layout(
        xaxis = list(title = "Category", tickangle = -45),
        yaxis = list(title = "Fund Count", side = 'left'),
        yaxis2 = list(title = "Avg Return (%)", overlaying = 'y', side = 'right'),
        font = list(size = 12),
        hovermode = 'x unified',
        margin = list(b = 80),
        plot_bgcolor = 'rgba(240, 240, 240, 0.3)'
      )
  })
  
  output$chart_category_waterfall <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    waterfall_data <- data %>%
      group_by(category) %>%
      summarise(
        AUM_Value = sum(AUM..Cr., na.rm = TRUE),
        Avg_Return = mean(X1Y.Returns...., na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(AUM_Value)) %>%
      head(8) %>%
      mutate(
        Contribution = (AUM_Value / sum(AUM_Value)) * 100
      )
    
    plot_ly(
      x = waterfall_data$category,
      y = waterfall_data$Contribution,
      measure = c(rep("relative", nrow(waterfall_data) - 1), "total"),
      type = "waterfall",
      connector = list(line = list(color = "#1e3557")),
      increasing = list(marker = list(color = "#2ecc71")),
      decreasing = list(marker = list(color = "#e74c3c")),
      totals = list(marker = list(color = "#3498db")),
      hovertemplate = "%{x}<br>%{y:.1f}%<extra></extra>"
    ) %>%
      layout(
        title = "",
        xaxis = list(title = "Category"),
        yaxis = list(title = "% Contribution to Total AUM"),
        font = list(size = 12),
        showlegend = FALSE
      )
  })
  output$chart_stability_ribbon <- renderPlotly({
    data <- as.data.frame(filtered_data())
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    safe_num <- function(x) suppressWarnings(as.numeric(x))
    
    df <- data %>%
      mutate(
        R1 = safe_num(X1Y.Returns....),
        R3 = safe_num(X3Y.Returns....),
        R5 = safe_num(X5Y.Returns....)
      ) %>%
      filter(!is.na(Category), !is.na(R1), !is.na(R3), !is.na(R5)) %>%
      group_by(Category) %>%
      summarise(
        Min_Return = min(c(R1, R3, R5), na.rm = TRUE),
        Max_Return = max(c(R1, R3, R5), na.rm = TRUE),
        Avg_Return = mean(c(R1, R3, R5), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Avg_Return)
    
    if (nrow(df) == 0) return(plotly::plotly_empty())
    
    df <- df %>%
      mutate(
        HoverText = paste0(
          "<b>", Category, "</b><br>",
          "Min: ", sprintf("%.2f%%", Min_Return), "<br>",
          "Max: ", sprintf("%.2f%%", Max_Return), "<br>",
          "Avg: ", sprintf("%.2f%%", Avg_Return)
        ),
        RangeWidth = Max_Return - Min_Return
      )
    
    df$Category <- factor(df$Category, levels = df$Category)
    
    plot_ly() %>%
      add_trace(
        data = df,
        type = "bar",
        orientation = "h",
        x = ~RangeWidth,
        y = ~Category,
        base = ~Min_Return,
        marker = list(
          color = "rgba(52,152,219,0.35)",
          line = list(color = "rgba(52,152,219,0.8)", width = 1)
        ),
        hoverinfo = "none",
        showlegend = FALSE
      ) %>%
      add_trace(
        data = df,
        type = "scatter",
        mode = "markers+text",
        x = ~Avg_Return,
        y = ~Category,
        marker = list(size = 12, color = "#2c3e50"),
        text = ~sprintf("%.2f%%", Avg_Return),
        textposition = "right",
        hovertemplate = "%{text}<br>%{customdata}<extra></extra>",   
        customdata = df$HoverText
      ) %>%
      layout(
        title = list(text = "🌈 Return Stability Ribbon Chart (Category-wise)", x = 0.5),
        xaxis = list(title = "Return (%)"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 200, r = 30, t = 60, b = 40),
        font = list(size = 12),
        hovermode = "closest",
        bargap = 0.6
      )
  })
  
  output$chart_stability_ribbon <- renderPlotly({
    data <- as.data.frame(filtered_data())
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    safe_num <- function(x) suppressWarnings(as.numeric(x))
    
    df <- data %>%
      mutate(
        R1 = safe_num(X1Y.Returns....),
        R3 = safe_num(X3Y.Returns....),
        R5 = safe_num(X5Y.Returns....)
      ) %>%
      filter(!is.na(category), !is.na(R1), !is.na(R3), !is.na(R5)) %>%
      group_by(category) %>%
      summarise(
        Min_Return = min(c(R1, R3, R5), na.rm = TRUE),
        Max_Return = max(c(R1, R3, R5), na.rm = TRUE),
        Avg_Return = mean(c(R1, R3, R5), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Avg_Return)
    
    if (nrow(df) == 0) return(plotly::plotly_empty())
    df <- df %>% mutate(
      RangeWidth = Max_Return - Min_Return,
      custom_hover_max = Max_Return
    )
    df$category <- factor(df$category, levels = df$category)
    
    plot_ly() %>%
      add_trace(
        data = df,
        type = "bar",
        orientation = "h",
        x = ~RangeWidth,
        y = ~category,
        base = ~Min_Return,
        customdata = ~custom_hover_max,
        marker = list(
          color = "rgba(52,152,219,0.30)",
          line = list(color = "rgba(52,152,219,0.8)", width = 1)
        ),
        hovertemplate =
          "<b>%{y}</b><br>Min: %{base:.2f}%<br>Max: %{customdata:.2f}%<extra></extra>",
        showlegend = FALSE
      ) %>%
      add_trace(
        data = df,
        type = "scatter",
        mode = "markers",
        x = ~Avg_Return,
        y = ~category,
        marker = list(size = 12, color = "#2c3e50"),
        hovertemplate =
          "<b>%{y}</b><br>Avg: %{x:.2f}%<extra></extra>"
      ) %>%
      layout(
        title = "",
        xaxis = list(title = "Returns (%)"),
        yaxis = list(title = "", automargin = TRUE),
        margin = list(l = 180, r = 40, t = 10, b = 40),
        font = list(size = 13)
      )
  })
  output$chart_allocation_matrix <- renderPlotly({
    data <- filtered_data()
    if (nrow(data) == 0) return(plotly::plotly_empty())
    
    matrix_data <- data %>%
      mutate(
        Risk_Group = cut(risk_level, breaks = c(0, 2, 4, 6), 
                         labels = c("Low", "Medium", "High"))
      ) %>%
      group_by(category, Risk_Group) %>%
      summarise(
        Count = n(),
        Avg_Return = mean(X1Y.Returns...., na.rm = TRUE),
        .groups = 'drop'
      )
    
    plot_ly(data = matrix_data,
            x = ~Risk_Group,
            y = ~category,
            z = ~Count,
            type = 'heatmap',
            colorscale = 'Viridis',
            hovertemplate = "%{y} - %{x}<br>Funds: %{z}<br>Avg Return: %{customdata:.2f}%<extra></extra>",
            customdata = ~Avg_Return) %>%
      layout(
        title = "",
        xaxis = list(title = "Risk Category"),
        yaxis = list(title = "Fund Category"),
        font = list(size = 12)
      )
  })
  
}
shinyApp(ui = ui, server = server)
