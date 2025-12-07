# Mutual Funds India Analytics Dashboard - Setup Guide

## Overview

A production-ready **Shiny Dashboard** for comprehensive mutual funds analytics in India. Features interactive visualizations, advanced filtering, real-time KPIs and detailed fund analysis.

<img width="1919" height="1017" alt="Screenshot 2025-12-07 211629" src="https://github.com/user-attachments/assets/13ea5333-aa64-451e-bda2-ce6eab9dac9f" />

---

## Features

### 1. **Dashboard Overview**
- 6 KPI Cards: Total Schemes, Categories, Avg Returns, Expense Ratio, AUM, SIP Availability
- Fund distribution by category
- Risk level breakdown
- Returns distribution histogram
- AUM distribution (log scale)
- Minimum investment segments
- Top 10 AMCs by AUM

### 2. **Returns Analysis**
- 1Y, 3Y, 5Y average returns with statistics
- Returns trends by risk level
- Category-wise returns comparison
- Box plots for each category
- Data completeness heatmap

### 3. **Risk & Ratio Analysis**
- Risk vs Return scatter plot
- Alpha, Beta, Sharpe, Sortino distributions
- Top 5 funds quality indicators radar chart
- Return period selector (1Y/3Y/5Y)

### 4. **AMC Insights**
- AMC rankings by AUM
- Average expense ratio by AMC
- Average returns by AMC
- Complete AMC performance table
- Fund manager mapping

### 5. **Fund Comparison Tool**
- Select up to 3 funds for side-by-side comparison
- Detailed comparison cards
- Returns comparison bar chart
- Risk metrics comparison
- Comprehensive radar chart

### 6. **Raw Data Explorer**
- Interactive DataTable with sorting/filtering
- Download options (CSV, Excel, JSON)
- Column visibility toggle
- 814 mutual funds with 20+ metrics

---

## Design System

**Color Palette:**
- Primary: Deep Blue (#1e3557)
- Accent: Emerald Green (#2ecc71)
- Very High Risk: Red
- High Risk: Orange
- Moderate Risk: Yellow
- Low Risk: Green

**Features:**
- Responsive sidebar navigation
- Sticky global filters
- Dark/Light mode ready
- Mobile-optimized layout

---

## Installation & Setup

### Prerequisites
```r
# R version 4.0+
# Required packages:
install.packages(c(
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "plotly",
  "dplyr",
  "ggplot2",
  "DT",
  "scales",
  "lubridate",
  "gridExtra",
  "tidyr",
  "reactable",
  "openxlsx"  # For Excel export
))
```

### File Structure
```
mutual-funds-dashboard/
├── shiny_dashboard.R          # Main Shiny app
├── merged_mutual_funds.csv    # Data file (provided in Python output)
├── README.md                  # This file
├── requirements.txt           # Package requirements
└── config/
    └── colors.R              # Color theme definitions
```

### Running the Dashboard

**Option 1: Interactive Mode**
```r
# In RStudio
library(shiny)
runApp("shiny_dashboard.R")
```

**Option 2: Command Line**
```bash
R -e "shiny::runApp('shiny_dashboard.R', port = 3838)"
```

**Option 3: Shiny Server Deployment**
```bash
cp shiny_dashboard.R /srv/shiny-server/mutual-funds-dashboard/app.R
cp merged_mutual_funds.csv /srv/shiny-server/mutual-funds-dashboard/
sudo systemctl restart shiny-server
# Access at: http://your-server:3838/mutual-funds-dashboard
```

---

## Global Filters

All filters are **sticky** and affect all pages:

1. **Category** - Fund category (Equity, Debt, Hybrid, etc.)
2. **Sub-Category** - Specific fund type
3. **Risk Level** - 1 (Very Low) to 6 (Very High)
4. **AMC Name** - Asset Management Company (multi-select)
5. **SIP Available** - Whether SIP option available
6. **Min Investment Range** - ₹0 to ₹25,000 slider
7. **AUM Range** - ₹0 to ₹60,000 Cr slider

**Reset Button** - Clears all filters instantly

---

## Data Structure

### Input Dataset
- **Total Funds**: 814 mutual schemes
- **Columns**: 20+ metrics
- **Categories**: 5 (Equity, Debt, Hybrid, Solution Oriented, Other)
- **AMCs**: 39 asset management companies
- **Time Periods**: 1Y, 3Y, 5Y returns

### Key Metrics Calculated
```
KPI Calculations:
- Avg 1Y Return: 3.92%
- Avg 3Y Return: 18.53%
- Avg 5Y Return: 9.49%
- Avg Expense Ratio: 0.71%
- Total AUM: ₹31,03,663 Cr
- Avg AUM: ₹3,812.85 Cr
- SIP Available: 100%
```

---

## Customization

### Changing Color Theme
Edit the `risk_labels` and color assignments in the app:

```r
risk_labels <- c(
  "1" = "Very Low",    # Green
  "2" = "Low",         # Dark Green
  "3" = "Moderate",    # Orange
  "4" = "Moderate High", # Red-Orange
  "5" = "High",        # Dark Red
  "6" = "Very High"    # Deep Red
)
```

### Adjusting Filter Ranges
```r
# Min Investment slider
min = 0, max = 25000, step = 100

# AUM slider
min = 0, max = 60000, step = 1000
```

### Adding New Metrics
1. Add column to `merged_mutual_funds.csv`
2. Create reactive calculation in server
3. Add visualization (chart/table)

---

## Responsive Design

- **Desktop**: Full-width layout with 6-column sidebar
- **Tablet**: Sidebar collapses, 2-column charts
- **Mobile**: Sidebar hides, stacked single-column layout

---

## Chart Types & Interactions

| Chart | Type | Features |
|-------|------|----------|
| Category Distribution | Bar | Hover for values |
| Risk Breakdown | Pie | Click legend to toggle |
| Returns Distribution | Histogram | Shows median line |
| AUM Distribution | Histogram | Log scale for skewness |
| Risk vs Return | Scatter | Color-coded by risk level |
| AMC Ranking | Horizontal Bar | Sortable top 15 |
| Category Boxplot | Box | Multi-period comparison |
| Data Heatmap | Heatmap | Color intensity = availability |
| Radar Chart | Radar | 5 quality metrics |

---

## Performance Tips

1. **Large Datasets**: Use lazy loading
2. **Memory**: Pre-filter data on server load
3. **Rendering**: Optimize plotly with `displayModeBar = FALSE`
4. **Caching**: Use `reactive()` for expensive calculations

---

## Troubleshooting

### Issue: "Package not found"
```r
# Install missing packages
install.packages("package_name")
```

### Issue: Data not loading
```r
# Verify CSV file path
getwd()  # Check current directory
# Ensure merged_mutual_funds.csv in same folder
```

### Issue: Slow performance
```r
# Check for missing values
summary(mf_data)
# Reduce number of displayed rows
# Use DT options: pageLength = 10
```

### Issue: Charts not rendering
- Check browser console for errors
- Verify data has no missing values for chart
- Try refreshing browser

---

## Data Export

### Download Options
- **CSV**: Filtered data as CSV
- **Excel**: Formatted Excel workbook with multiple sheets
- **JSON**: JSON export (with custom handler)

### Export Includes
- All filtered records
- All selected columns
- Formatted numbers and dates

---

## Security Considerations

1. **Data Privacy**: No PII in dataset
2. **File Access**: Restrict server write permissions
3. **User Authentication**: Add `shinyauthr` for login
4. **API Rate Limiting**: If adding external data feeds

---

## Advanced Features

### Adding Real-time Data
```r
# Integrate with financial APIs
# Example: Use `quantmod` package for live prices
```

### Custom Alerts
```r
# Add conditional formatting for underperforming funds
# Highlight funds below certain thresholds
```

### User Sessions
```r
# Track user activity
# Save filter preferences
# Generate personalized reports
```

---

## Support & Maintenance

### Regular Updates
- Update data: Replace CSV monthly
- Audit filters: Ensure data ranges match new dataset
- Test charts: Verify all visualizations render

### Backup Strategy
```bash
# Backup CSV and app config
tar -czf backup_$(date +%Y%m%d).tar.gz merged_mutual_funds.csv shiny_dashboard.R
```

---

## License & Credits

- Dataset: Comprehensive Mutual Funds India 2025
- Built with: R, Shiny, Plotly, DT
- Dashboard design inspired by modern fintech standards

---
