## Task 2

# Set CRAN mirror explicitly
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install packages
install.packages(c("data.table", "ggplot2", "tidyr", "dplyr"))

# Load
library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)

# Load the file
data <- fread("C:/Users/princ/Desktop/adlina files/Portfolio Projects/Quantium- Retail Strategy and Analytics/Task 2/QVI_data.csv")

#### Set themes for plots
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

str(data)
data[, DATE := as.Date(DATE, format = "%d-%m-%Y")]

# Add yearmonth column
data[, yearmonth := year(DATE)*100 + month(DATE)]

# metrics
metrics <- data[, .(total_sales = sum(TOT_SALES),
                    n_customers = uniqueN(LYLTY_CARD_NBR),
                    nTXNpercust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR),
                    nchipsperTXN = sum(PROD_QTY)/uniqueN(TXN_ID),
                    avgpriceperunit = sum(TOT_SALES)/sum(PROD_QTY)), 
                by =.(STORE_NBR, yearmonth)]
metrics

## Filter to the pre-trial period and stores with full observation periods
fullops_stores <- unique(metrics[, .N, STORE_NBR][N == 12, STORE_NBR])
pretrial_metrics <- metrics[yearmonth < 201902 & STORE_NBR %in% fullops_stores,]


## Correlation function:
calc_correlation <- function(inputTable, metricCol, trial_store) {
  
  corr_table <- data.table(Store1 = numeric(), Store2 = numeric(), corr = numeric())
  
  storeNumbers <- unique(inputTable[, STORE_NBR])
  
  for (j in storeNumbers) {
    corr_measures <- data.table("Store1" = trial_store,
                                "Store2" = j,
                                "corr" = cor(inputTable[STORE_NBR == trial_store, eval(metricCol)],
                                             inputTable[STORE_NBR == j, eval(metricCol)])
    )
    
    corr_table <- rbind(corr_table, corr_measures)
  }
  return(corr_table)
}


## Magnitude distance function:
calc_magnitude <- function(inputTable, metricCol, trial_store){
  
  mag_table <- data.table(Store1 = numeric(), Store2 = numeric(), 
                          yearmonth = numeric(), distance = numeric())
  
  storeNumbers <- unique(inputTable[, STORE_NBR])
  
  for (j in storeNumbers) {
    mag_measures <- data.table("Store1" = trial_store,
                               "Store2" = j,
                               "yearmonth" = inputTable[STORE_NBR == trial_store, yearmonth],
                               "distance" = abs(inputTable[STORE_NBR == trial_store, eval(metricCol)] 
                                                - inputTable[STORE_NBR == j, eval(metricCol)])
    )
    
    mag_table <- rbind(mag_table, mag_measures)
  }
  
  min_max <- mag_table[, .(min_dist = min(distance), max_dist = max(distance)), 
                       by = c("yearmonth", "Store1")]
  std_mag <- merge(mag_table, min_max, by = c("yearmonth", "Store1"))
  std_mag <- std_mag[, .(magnitude = mean(1 - (distance - min_dist) / (max_dist - min_dist))), 
                     by = c("Store1", "Store2")]
  
  return(std_mag)
}


trial_store <- c(77, 86, 88)
control_store <- numeric(length(trial_store))

## Use the functions to calculate correlation and magnitude

sales_corr <- customers_corr <- sales_mag <- customers_mag <- list()

for(calc_index in seq_along(trial_store)) {
  
  sales_corr[[calc_index]] <- calc_correlation(pretrial_metrics, quote(total_sales), 
                                               trial_store[calc_index])
  customers_corr[[calc_index]] <- calc_correlation(pretrial_metrics, quote(n_customers), 
                                                   trial_store[calc_index])
  sales_mag[[calc_index]] <- calc_magnitude(pretrial_metrics, quote(total_sales), 
                                            trial_store[calc_index])
  customers_mag[[calc_index]] <- calc_magnitude(pretrial_metrics, quote(n_customers), 
                                                trial_store[calc_index])
}

ordered_sales_corr <- lapply(sales_corr, function(dt) dt[order(-corr)])
ordered_sales_corr

ordered_customers_corr <- lapply(customers_corr, function(dt) dt[order(-corr)])
ordered_customers_corr

ordered_sales_mag <- lapply(sales_mag, function(dt) dt[order(-magnitude)])
ordered_sales_mag

ordered_customers_mag <- lapply(customers_mag, function(dt) dt[order(-magnitude)])
ordered_customers_mag

## Create a combined score composed of correlation and magnitude
corr_weight <- 0.5
score_nSales <- score_nCust <- combined_scores <- list()

for(i in seq_along(trial_store)) {
  
  # Sales
  score_Sales <- cbind(sales_corr[[i]], sales_mag[[i]])
  score_Sales <- score_Sales[, scoreNSales := corr * corr_weight + magnitude * (1 - corr_weight), 
                             by = .(Store1, Store2)]
  score_nSales[[i]] <- score_Sales
  
  # Customers
  score_Cust <- cbind(customers_corr[[i]], customers_mag[[i]])
  score_Cust <- score_Cust[, scoreNCust := corr * corr_weight + magnitude * (1 - corr_weight), 
                           by = .(Store1, Store2)]
  score_nCust[[i]] <- score_Cust
  
  # Combined score
  combined_score <- cbind(score_nSales[[i]], score_nCust[[i]])
  combined_score <- combined_score[, finalControlScore := scoreNSales*0.5 + scoreNCust*0.5]
  combined_scores[[i]] <- combined_score[, .(Store1, Store2, scoreNSales, scoreNCust, 
                                             finalControlScore)]
}  

score_nSales
score_nCust
ordered_combined_scores <- lapply(combined_scores, function(dt) dt[order(-finalControlScore)])
ordered_combined_scores

## Find control store

for(i in seq_along(trial_store)) {
  
  control_store[i] <- combined_scores[[i]][Store1 == trial_store[i], 
  ][order(-finalControlScore)][-1, "Store2"][1]
  
  control_store <- unlist(control_store)
  control_store
  print(paste("trial_store:", trial_store[i], "=>", "control_store:", control_store[i]))
}  

## Visual checks on trends based on the drivers

# Visualizing pre-trial Sales and Customer Counts by Store type
measureOverTimeSales <- metrics
measureOverTimeCust <- metrics

combined_plots <- list()

for(i in seq_along(trial_store)) {
  
  current_trial_store <- trial_store[i]
  current_control_store <- control_store[i]
  
  # Sales plot
  pastSales <- measureOverTimeSales[, 
                Store_type := ifelse(STORE_NBR == current_trial_store, "Trial",
                              ifelse(STORE_NBR == current_control_store, "Control", 
                                                                  "Other stores"))]
  pastSales <- measureOverTimeSales[, .(totSales = mean(total_sales)), 
                                    by = c("yearmonth", "Store_type")]
  pastSales <- pastSales[, 
                TransactionMonth := as.Date(paste(yearmonth %/% 100, 
                                                  yearmonth %% 100, 1, 
                                                  sep = "-"), "%Y-%m-%d")]
  
  trialSales <- pastSales[TransactionMonth < as.Date("2019-03-01"), ]

  p_sales_plot <- ggplot(trialSales, aes(TransactionMonth, totSales, color = Store_type)) +
    geom_line() +
    labs(x = "Month of operation", y = "Total sales", title = "Total sales by month") +
    ggtitle(paste("Total sales for Trial:",current_trial_store, 
                  "& Control:",current_control_store))

  # Customers plot
  measureOverTimeCust[, Store_type := ifelse(STORE_NBR == current_trial_store, "Trial",
                                      ifelse(STORE_NBR == current_control_store, "Control", 
                                                    "Other stores"))]
  
  pastCustomers <- measureOverTimeCust[, .(cust_num = mean(n_customers)), 
                                       by = c("yearmonth", "Store_type")]
  pastCustomers[, TransactionMonth := as.Date(paste(yearmonth %/% 100, 
                                                    yearmonth %% 100, 1, 
                                                    sep = "-"), "%Y-%m-%d")]
  TrialCustomers <- pastCustomers[TransactionMonth < as.Date("2019-03-01"), ]
  
  p_cust_plot <- ggplot(TrialCustomers, aes(TransactionMonth, cust_num, color = Store_type)) +
    geom_line() +
    labs(x = "Month of operation", y = "Total number of Customers", 
         title = "Total Customers by month") +
    ggtitle(paste("# Customers for Trial:",current_trial_store, 
                  "& Control:",current_control_store))
  
  # Combined plot
  combined_plot <- plot_grid(p_sales_plot, p_cust_plot, ncol = 2)
  combined_plots[[i]] <- combined_plot
  
}

combined_plots

## Visualizing Sales and Customers count during Trial period

# Combine data for all trial and control stores
filtered_data <- measureOverTimeSales[STORE_NBR %in% c(77, 86, 88, 46, 164, 40)]
filtered_data <-filtered_data[, STORE_NBR := factor(STORE_NBR, 
                                        levels = c("40", "46", "77", "86", "88", "164"))]

filtered_data <- filtered_data[, Store_type := ifelse(STORE_NBR %in% c(77, 86, 88), 
                                                      "Trial", "Control")]
filtered_data <- filtered_data[, Store_type := factor(Store_type, 
                                                      levels = c("Trial", "Control"))]
filtered_data <- filtered_data[, TransactionMonth := as.Date(paste(yearmonth %/% 100,
                                                                   yearmonth %% 100, 1, 
                                                                   sep = "-"),
                                            "%Y-%m-%d")]
filtered_data <- filtered_data[, .(Store_type, total_sales, n_customers), 
                               by = .(STORE_NBR, TransactionMonth)]
filtered_data <- filtered_data[TransactionMonth > "2019-01-01" 
                               & TransactionMonth < "2019-05-01", ]

# Visualize sales and customers count on trial periods for trial and control stores
sales <- filtered_data[, total := sum(total_sales), by = (STORE_NBR)]
cust <- filtered_data[, count := sum(n_customers), by = (STORE_NBR)]
sales
cust

ggplot(sales, aes(x = STORE_NBR, y = total, fill = Store_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = scales::comma(total), y = total), 
            position = position_dodge(width = 0.7), vjust = -0.5) +
  labs(x = "Store", y = "Total sales", 
       title = "Total Sales for Trial and Control Stores on Trial periods") +
  theme_minimal()

ggplot(cust, aes(x = STORE_NBR, y = count, fill = Store_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = count, y = count), 
            position = position_dodge(width = 0.7), vjust = -0.5) +
  labs(x = "Store", y = "Customers Count", 
       title = "Customers Count for Trial and Control Stores on Trial periods") +
  theme_minimal()


### Assessment of trial

## Sales Assessment

# Scale pre-trial control sales to match pre-trial trial store sales

scaled_controlSales <- data.table()
scalingFactors <- numeric(length(trial_store))

for(i in seq_along(trial_store)) {
  scalingFactor <- pretrial_metrics[STORE_NBR == trial_store[i], sum(total_sales)]/
    pretrial_metrics[STORE_NBR == control_store[i], sum(total_sales)]
  
  scalingFactors[i] <- scalingFactor
  
  # Calculate and append the scaled control sales
  scaled_controlSales <- rbind(scaled_controlSales,
                               measureOverTimeSales[STORE_NBR == control_store[i], ]
                               [, controlSales := total_sales * scalingFactor])
}

paste("Control Store:", control_store, "=>", "Scaling Factor:", unlist(scalingFactors))
scaled_controlSales[, .(STORE_NBR, yearmonth, total_sales, controlSales)]


# Calculate the percentage difference between scaled control sales and trial sales
percentageDiff <- data.table()
stdDev <- numeric(length(trial_store))

for(i in seq_along(trial_store)) {
  store_percentageDiff <- merge(
    scaled_controlSales[STORE_NBR == control_store[i], .(STORE_NBR, yearmonth, controlSales)],
    measureOverTimeSales[STORE_NBR == trial_store[i], .(STORE_NBR, yearmonth, total_sales)],
    by = "yearmonth")
  
  percentageDiff <- rbind(percentageDiff, 
      store_percentageDiff[, percentageDifference := abs(controlSales - total_sales)/controlSales])
  
  std_dev <- sd(percentageDiff[STORE_NBR.y == trial_store[i], percentageDifference])
  stdDev[i] <- std_dev
}
percentageDiff
paste("Trial-Control Store Pair:", trial_store, "-", control_store, "=>",
      "Std Deviation:", unlist(stdDev))


# Calculate tValue and format TransactionMonth
tValue <- data.table()

subset_data <- percentageDiff[, 
                              TransactionMonth := as.Date(paste0(yearmonth %/% 100, "-",
                                                                 yearmonth %% 100, "-01"), 
                                                                  format = "%Y-%m-%d")
][TransactionMonth < as.Date("2019-05-01") & TransactionMonth > as.Date("2019-01-01"), ]

tValue <- rbind(tValue, subset_data[, tValue := (percentageDifference - 0) / stdDev[i]
][, .(TransactionMonth, STORE_NBR.y, STORE_NBR.x, tValue)])

tValue

degreesOfFreedom <- 7 # As there are 8 months in the pre-trial period 
# => 8 - 1 = 7 degrees of freedom
qt(0.95, df = degreesOfFreedom)

## Control store 95th percentile
pastSales_Controls95 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 + stdDev[i] * 2)
][, Store_type := "Control 95th % confidence interval"]

## Control store 5th percentile
pastSales_Controls5 <- pastSales[Store_type == "Control",
][, totSales := totSales * (1 - stdDev[i] * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessmentSales <- rbind(pastSales, pastSales_Controls95, pastSales_Controls5)

# Plotting these in one nice graph
sales_assess_plot <- 
  ggplot(trialAssessmentSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() +
  geom_point() +
  annotate("rect", xmin = as.Date("2019-02-01"), xmax = as.Date("2019-04-30"), 
           ymin = 0, ymax = Inf, alpha = 0.5) +
  labs(x = "Month of operation", y = "Total sales", title = "Total sales by month")


## Customers count Assessment

# Scale pre-trial control customers count to match pre-trial trial store customers count

scaled_controlCust <- data.table()
scalingFactors_cust <- numeric(length(trial_store))

for(i in seq_along(trial_store)) {
  scalingFactor_cust <- pretrial_metrics[STORE_NBR == trial_store[i], sum(n_customers)]/
    pretrial_metrics[STORE_NBR == control_store[i], sum(n_customers)]
  
  scalingFactors_cust[i] <- scalingFactor_cust
  
  # Calculate and append the scaled control sales
  scaled_controlCust <- rbind(scaled_controlCust,
                              measureOverTimeCust[STORE_NBR == control_store[i], ]
                              [, controlNcust := n_customers * scalingFactor_cust])
}

paste("Control Store:", control_store, "=>", "Scaling Factor:", unlist(scalingFactors_cust))
scaled_controlCust[, .(STORE_NBR, yearmonth, n_customers, controlNcust)]


# Calculate the percentage difference between scaled control sales and trial sales
percentageDiff_cust <- data.table()
stdDev_cust <- numeric(length(trial_store))

for(i in seq_along(trial_store)) {
  store_percentageDiff_cust <- merge(
    scaled_controlCust[STORE_NBR == control_store[i], .(STORE_NBR, yearmonth, controlNcust)],
    measureOverTimeCust[STORE_NBR == trial_store[i], .(STORE_NBR, yearmonth, n_customers)],
    by = "yearmonth")
  
  percentageDiff_cust <- rbind(percentageDiff_cust, 
store_percentageDiff_cust[, percentageDifference_cust := abs(controlNcust - n_customers)/
                            controlNcust])
  
  std_dev_cust <- sd(percentageDiff_cust[STORE_NBR.y == trial_store[i], percentageDifference_cust])
  stdDev_cust[i] <- std_dev_cust
}
percentageDiff_cust
paste("Trial-Control Store Pair:", trial_store, "-", control_store, "=>", 
      "Std Deviation:", unlist(stdDev_cust))


# Calculate tValue and format TransactionMonth
tValue_cust <- data.table()

subset_data_cust <- percentageDiff_cust[, 
                   TransactionMonth := as.Date(paste0(yearmonth %/% 100, "-", 
                                               yearmonth %% 100, "-01"),
                                              format = "%Y-%m-%d")
][TransactionMonth < as.Date("2019-05-01") & TransactionMonth > as.Date("2019-01-01"), ]

tValue_cust <- rbind(tValue_cust, 
    subset_data_cust[, tValue_cust := (percentageDifference_cust - 0)/ stdDev_cust[i]
][, .(TransactionMonth, STORE_NBR.y, STORE_NBR.x, tValue_cust)])

tValue_cust

degreesOfFreedom <- 7 # As there are 8 months in the pre-trial period 
# => 8 - 1 = 7 degrees of freedom
qt(0.95, df = degreesOfFreedom)

## Control store 95th percentile
pastCust_Controls95 <- pastCustomers[Store_type == "Control",
][, cust_num := cust_num * (1 + stdDev_cust[i] * 2)
][, Store_type := "Control 95th % confidence interval"]

## Control store 5th percentile
pastCust_Controls5 <- pastCustomers[Store_type == "Control",
][, cust_num := cust_num * (1 - stdDev_cust[i] * 2)
][, Store_type := "Control 5th % confidence interval"]

trialAssessmentCust <- rbind(pastCustomers, pastCust_Controls95, pastCust_Controls5)

# Plotting these in one nice graph
cust_assess_plot <- ggplot(trialAssessmentCust, aes(TransactionMonth, cust_num, color = Store_type)) +
  geom_line() +
  geom_point() +
  annotate("rect", xmin = as.Date("2019-02-01"), xmax = as.Date("2019-04-30"),
           ymin = 0, ymax = Inf, alpha = 0.5) +
  labs(x = "Month of operation", y = "Customer Counts", title = "Total no. of Customers by month")


combined_assessment <- plot_grid(sales_assess_plot, cust_assess_plot, nrow = 2)
combined_assessment
