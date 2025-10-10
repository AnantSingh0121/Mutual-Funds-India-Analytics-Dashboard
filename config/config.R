COLORS <- list(
  primary = "#1e3557",        
  accent = "#2ecc71",         
  background = "#f8f9fa",     
  surface = "#ffffff",        
  text_primary = "#2c3e50",   
  text_secondary = "#7f8c8d",
  text_light = "#95a5a6",     
  risk_very_low = "#2ecc71",  
  risk_low = "#27ae60",       
  risk_moderate = "#f39c12",  
  risk_mod_high = "#e74c3c",  
  risk_high = "#c0392b",      
  risk_very_high = "#8b0000", 
  chart_1 = "#2ecc71",       
  chart_2 = "#3498db", 
  chart_3 = "#9b59b6", 
  chart_4 = "#f39c12", 
  chart_5 = "#e74c3c",        
  chart_6 = "#1abc9c",
  status_success = "#27ae60",
  status_warning = "#f39c12",
  status_error = "#c0392b",
  status_info = "#3498db"
)
FILTERS <- list(
  category = list(
    enabled = TRUE,
    multiselect = FALSE,
    searchable = TRUE,
    reset_value = "All"
  ),
  
  # Sub-Category Filter
  subcategory = list(
    enabled = TRUE,
    multiselect = TRUE,
    searchable = TRUE,
    reset_value = c("All")
  ),
  
  risk_level = list(
    enabled = TRUE,
    multiselect = TRUE,
    searchable = FALSE,
    reset_value = c("All"),
    levels = c("Very Low", "Low", "Moderate", "Moderate High", "High", "Very High")
  ),
  
  amc = list(
    enabled = TRUE,
    multiselect = TRUE,
    searchable = TRUE,
    reset_value = c("All"),
    max_display = 50  # Show top 50 AMCs
  ),
  
  # SIP Filter
  sip = list(
    enabled = TRUE,
    multiselect = FALSE,
    options = c("All", "Yes", "No"),
    reset_value = "All"
  ),
  
  investment_range = list(
    enabled = TRUE,
    min = 0,
    max = 25000,
    step = 100,
    default = c(0, 25000),
    currency = "INR"
  ),
  
  aum_range = list(
    enabled = TRUE,
    min = 0,
    max = 60000,
    step = 1000,
    default = c(0, 60000),
    unit = "Cr"
  )
)

CHARTS <- list(
  category_dist = list(
    enabled = TRUE,
    height = "350px",
    show_values = TRUE,
    color = COLORS$accent
  ),
  
  risk_dist = list(
    enabled = TRUE,
    height = "350px",
    type = "pie",
    show_percentages = TRUE
  ),
  
  returns_hist = list(
    enabled = TRUE,
    height = "350px",
    bins = 30,
    show_median = TRUE
  ),
  
  aum_dist = list(
    enabled = TRUE,
    height = "350px",
    log_scale = TRUE,
    bins = 40
  ),
  
  risk_return_scatter = list(
    enabled = TRUE,
    height = "500px",
    marker_size = 8,
    show_legend = TRUE
  ),
  
  amc_ranking = list(
    enabled = TRUE,
    height = "400px",
    top_n = 15,
    sort_by = "total_aum"  # or "avg_return", "avg_expense"
  )
)

KPI_CARDS <- list(
  total_schemes = list(
    enabled = TRUE,
    label = "Total Schemes",
    format = "number",
    icon = "chart-pie"
  ),
  
  categories = list(
    enabled = TRUE,
    label = "Categories",
    format = "number",
    icon = "layer-group"
  ),
  
  avg_1y_return = list(
    enabled = TRUE,
    label = "Avg 1Y Return",
    format = "percentage",
    decimals = 2,
    icon = "arrow-up"
  ),
  
  avg_expense = list(
    enabled = TRUE,
    label = "Avg Expense Ratio",
    format = "percentage",
    decimals = 2,
    icon = "percent"
  ),
  
  total_aum = list(
    enabled = TRUE,
    label = "Avg AUM",
    format = "currency",
    currency = "INR",
    unit = "Cr",
    icon = "wallet"
  ),
  
  sip_pct = list(
    enabled = TRUE,
    label = "SIP Available",
    format = "percentage",
    decimals = 0,
    icon = "piggy-bank"
  )
)

TABLES <- list(
  # Data Explorer Table
  data_explorer = list(
    enabled = TRUE,
    page_length = 20,
    show_search = TRUE,
    show_pagination = TRUE,
    show_info = TRUE,
    scrollable = TRUE,
    # Columns to hide by default
    hidden_columns = c("ISIN", "rating")
  ),
  
  amc_metrics = list(
    enabled = TRUE,
    page_length = 10,
    sortable = TRUE,
    columns = c(
      "amc_name",
      "total_schemes",
      "total_aum",
      "avg_1y_return",
      "avg_expense_ratio",
      "avg_sharpe"
    )
  ),
  
  comparison = list(
    enabled = TRUE,
    max_funds = 3,
    min_funds = 2
  )
)

PAGES <- list(
  overview = list(
    enabled = TRUE,
    title = "Dashboard Overview",
    subtitle = "Industry metrics, fund distribution & key insights",
    show_kpi_cards = TRUE,
    show_charts = TRUE
  ),
  
  returns = list(
    enabled = TRUE,
    title = "Returns Analysis",
    subtitle = "Historical returns, trends & category comparison",
    periods = c("1Y", "3Y", "5Y")
  ),
  
  risk = list(
    enabled = TRUE,
    title = "Risk & Ratio Analysis",
    subtitle = "Risk metrics, alpha, beta, Sharpe & Sortino ratios",
    metrics = c("alpha", "beta", "sharpe", "sortino", "sd")
  ),
  
  amc = list(
    enabled = TRUE,
    title = "AMC Insights",
    subtitle = "Asset management company performance & rankings"
  ),
  
  comparison = list(
    enabled = TRUE,
    title = "Fund Comparison Tool",
    subtitle = "Compare up to 3 mutual funds side-by-side"
  ),
  
  data_table = list(
    enabled = TRUE,
    title = "Raw Data Explorer",
    subtitle = "Interactive data table with filtering, sorting & export options"
  )
)

EXPORT <- list(
  csv = list(
    enabled = TRUE,
    include_timestamp = TRUE
  ),
  
  excel = list(
    enabled = TRUE,
    include_timestamp = TRUE,
    multiple_sheets = FALSE,
    format_numbers = TRUE
  ),
  
  json = list(
    enabled = FALSE,
    pretty_print = TRUE
  )
)
DISPLAY <- list(
  breakpoints = list(
    mobile = 576,
    tablet = 768,
    desktop = 992,
    widescreen = 1200
  ),
  
  fonts = list(
    primary = "Segoe UI, Tahoma, Geneva, Verdana, sans-serif",
    mono = "Monaco, Courier New, monospace",
    size_base = "14px",
    size_heading = "24px"
  ),
  
  sidebar = list(
    width = 280,
    sticky_filters = TRUE,
    collapsible = TRUE
  ),
  
  charts = list(
    responsive = TRUE,
    displayModeBar = FALSE, 
    displaylogo = FALSE,
    responsive_threshold = 768
  )
)

DATA <- list(
  csv_path = "merged_mutual_funds.csv",
  
  encoding = "UTF-8",
  
  date_format = "%Y-%m-%d",
  
  currency = "INR",
  currency_symbol = "₹",
  
  number_decimals = 2,
  
  na_action = "omit",  
  cache_enabled = FALSE,
  cache_timeout = 3600  
)

VALIDATION <- list(
  required_columns = c(
    "Scheme.Name",
    "category",
    "sub_category",
    "risk_level",
    "amc_name",
    "X1Y.Returns....",
    "X3Y.Returns....",
    "X5Y.Returns....",
    "expense_ratio",
    "AUM.Cr.",
    "sharpe",
    "sortino",
    "alpha",
    "beta",
    "Standard.Deviation"
  ),
  
  ranges = list(
    risk_level = c(1, 6),
    returns = c(-100, 200),
    expense_ratio = c(0, 5),
    sharpe = c(-5, 5),
    aum = c(0, Inf)
  )
)

PERFORMANCE <- list(
  lazy_load = FALSE,
  max_visible_rows = 1000,
  chart_debounce = 300,  
  session_timeout = 3600,  
  max_request_size = "100MB",
  session_cache_dir = "shiny_session_cache"
)

INTEGRATIONS <- list(
  # Google Analytics
  google_analytics = list(
    enabled = FALSE,
    tracking_id = "UA-XXXXXXXXX-X"
  ),
  
  enable_api_updates = FALSE,
  
  enable_email_alerts = FALSE,
  alert_email = "admin@example.com"
)