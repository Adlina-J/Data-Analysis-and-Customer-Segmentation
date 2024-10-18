#Task 1 Solution: Data Preparation & Customer Analytics

# Set CRAN mirror explicitly
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Install and load necessary libraries
install.packages(c("data.table", "ggplot", "ggmosaic", "readr", 
                   "readxl" , "dplyr", "stringr", "arules", "arulesViz",
                   "ggrepel"))

# Load libraries
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(arules)
library(arulesViz)
library(cowplot)
library(ggrepel)

# Load transaction excel file
transaction <- read_excel("C:/Users/princ/Desktop/Adlina/adlina files/Portfolio Projects/Quantium- Retail Strategy and Analytics/Task 1/Dataset/QVI_transaction_data.xlsx")
# Load purchase csv file
purchase <- read.csv("C:/Users/princ/Desktop/Adlina/adlina files/Portfolio Projects/Quantium- Retail Strategy and Analytics/Task 1/Dataset/QVI_purchase_behaviour.csv")

# Data cleaning and merging
cleaned_data <- transaction %>%
  select(everything()) %>%
  left_join(purchase, by = "LYLTY_CARD_NBR")


## Exploratory data analysis

str(cleaned_data)
summary(is.na(cleaned_data))
summary(cleaned_data)

# Change Date from integer to date format
cleaned_data$DATE <- as.Date(cleaned_data$DATE, origin = "1899-12-30")
head(cleaned_data$DATE)

##Finding missing Date:

# Create a sequence of dates from 1 Jul 2018 to 30 Jun 2019
date_sequence <- seq(as.Date("2018-07-01"), as.Date("2019-06-30"), by = "days")
transaction_dates <- unique(cleaned_data$DATE)
missing_date <- setdiff(date_sequence, transaction_dates)
print(as.Date(missing_date))

## Categorical variables
summary(cleaned_data$PROD_NAME)

# Table of frequencies as data.frame
prod_name_counts <- table(cleaned_data$PROD_NAME) 
prod_name_df <- as.data.frame(prod_name_counts)

# Distribution of LIFESTAGE & PREMIUM
table(cleaned_data$LIFESTAGE)
table(cleaned_data$PREMIUM_CUSTOMER)


## Outliers - Data cleaning

# Boxplots for numerical variables
#boxplot(cleaned_data$LYLTY_CARD_NBR, main = "LYLTY_CARD_NBR")
#boxplot(cleaned_data$TXN_ID, main = "TXN_ID")
#boxplot(cleaned_data$PROD_NBR, main = "PROD_NBR")

# Find outliers by boxplot statistics for LYLTY_CARD_NBR
stats_LYLTY_CARD_NBR <- boxplot.stats(cleaned_data$LYLTY_CARD_NBR)
stats_LYLTY_CARD_NBR$out

# Identify row numbers of potential outliers based on IQR criterion for LYLTY_CARD_NBR
outliers_LYLTY_CARD_NBR <- which(cleaned_data$LYLTY_CARD_NBR %in% stats_LYLTY_CARD_NBR$out)
outliers_LYLTY_CARD_NBR

# Outlier rows of TXN_ID
stats_TXN_ID <- boxplot.stats(cleaned_data$TXN_ID)
stats_TXN_ID$out
outliers_TXN_ID <- which(cleaned_data$TXN_ID %in% stats_TXN_ID$out)
outliers_TXN_ID

# Find the index where TXN_ID is equal to 2415841
index_to_change <- which(cleaned_data$TXN_ID == 2415841)
cleaned_data$TXN_ID[index_to_change] <- 241584
cleaned_data[index_to_change, ]

# Boxplot and outlier rows of PROD_QTY, TOT_SALES
numeric_vars <- c("PROD_QTY", "TOT_SALES")
#boxplot(cleaned_data [, numeric_vars])

# Filter outliers for PROD_QTY and TOT_SALES
cleaned_data[cleaned_data$PROD_QTY >= 200, ]
cleaned_data[cleaned_data$TOT_SALES >= 200, ]

# All of the Retail customer(LYLTY_CARD_NBR) from outliers for PROD_QTY and TOT_SALES
cleaned_data[cleaned_data$LYLTY_CARD_NBR == 226000, ]

# Removing Retail Customer
cleaned_data <- cleaned_data[cleaned_data$LYLTY_CARD_NBR != 226000, ]
summary(cleaned_data)


### Data Exploration

## Transaction over Time
# Create a summary of transaction counts by date
transaction_summary <- cleaned_data %>%
  group_by(DATE) %>%
  summarise(Transaction_Count = n())
transaction_summary

# Setting plot themes to format graphs
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
# Plot transactions over time
ggplot(transaction_summary, aes(x = DATE, y = Transaction_Count)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Filter to include only December data
december_data <- transaction_summary %>%
  filter(format(DATE, "%B") == "December")
ggplot(december_data, aes(x = DATE, y = Transaction_Count)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions in December") +
  scale_x_date(breaks = "1 day", date_labels = "%d") + # Show individual days
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

## Stores 

# Total number of Stores
n_distinct(cleaned_data$STORE_NBR)

# Group by Store Number and calculate total sales for each store
store_sales <- cleaned_data %>%
  group_by(STORE_NBR) %>%
  summarise(Total_Sales = sum(TOT_SALES))
store_sales

# Store vs Total_Sales viz
ggplot(store_sales, aes(x = STORE_NBR, y = Total_Sales)) +
  geom_point() +
  labs(title = "Total Sales by Store", x = "Store Number", y = "Total Sales")

# Identify stores with the highest and lowest sales
# Top 5 stores based on Total Sales
top_stores <- store_sales %>%
  arrange(desc(Total_Sales)) %>%
  head(10)
top_stores

## Loyalty Card Holders

# Total number of Loyalty card holders
n_distinct(cleaned_data$LYLTY_CARD_NBR)

# loyalty_card_holders analysis based on purchase counts
loyalty_analysis <- cleaned_data %>%
  group_by(LYLTY_CARD_NBR, STORE_NBR) %>%
  summarise(Total_Sales = sum(TOT_SALES),
            purchase_counts = n()) %>%
  arrange(desc(purchase_counts))
loyalty_analysis

# loyalty_card_holders analysis based on Total sales
sales_lylty <- cleaned_data %>%
  group_by(LYLTY_CARD_NBR, STORE_NBR) %>%
  summarise(Total_Sales = sum(TOT_SALES),
            purchase_counts = n()) %>%
  arrange(desc(Total_Sales))
sales_lylty

## Brand Name

brand <- word(cleaned_data$PROD_NAME, start=1, end=1)
cleaned_data$BRAND = c(brand)
unique(cleaned_data$BRAND)

cleaned_data$BRAND <- gsub("Red", "RRD", cleaned_data$BRAND)
cleaned_data$BRAND <- gsub("Smiths", "Smith", cleaned_data$BRAND)
cleaned_data$BRAND <- gsub("\\bww\\b", "Woolworths", cleaned_data$BRAND, 
                           ignore.case = TRUE)
cleaned_data$BRAND <- gsub("\\bInfzns\\b", "Infuzions", cleaned_data$BRAND, 
                           ignore.case = TRUE)
cleaned_data$BRAND <- gsub("SNBTS", "Sunbites", cleaned_data$BRAND, 
                           ignore.case = TRUE)
cleaned_data$BRAND <- gsub("NCC", "Natural", cleaned_data$BRAND)
cleaned_data$BRAND <- gsub("Dorito", "Doritos", cleaned_data$BRAND)
cleaned_data$BRAND <- gsub("Doritoss", "Doritos", cleaned_data$BRAND)
cleaned_data$BRAND <- gsub("Grain", "GrnWves", cleaned_data$BRAND)

unique(cleaned_data$BRAND)


## Packet Size

# Extract pack sizes from PROD_NAME
pckt_size <- parse_number(cleaned_data$PROD_NAME)
cleaned_data$PCKT_SIZE = c(pckt_size)
unique(cleaned_data$PCKT_SIZE)

# size analysis
size_analysis <- cleaned_data %>%
  group_by(PCKT_SIZE) %>%
  summarise(pckt_sold = sum(PROD_QTY))
print(size_analysis, n = Inf)

# 'size_category' based on packet counts
size_category <- quantile(size_analysis$pckt_sold, 
                                      probs = c(0, 0.75/4, 1.85/4, 3/4, 1))
breaks <- size_category
labels <- c("Low", "Average", "Good", "High")
size_analysis <- size_analysis %>%
  mutate(size_category = cut(pckt_sold, 
                              breaks = breaks, 
                              labels = labels, 
                              include.lowest = TRUE)) %>%
  arrange(desc(pckt_sold))
print(size_analysis, n = Inf)

# Plot a histogram of PACK_SIZE using base R
hist(cleaned_data$PCKT_SIZE, col = "skyblue",main = "Distribution of Pack Sizes", 
     xlab = "Pack Size")

# Size, Brand and Sales Analysis 
size_brand_sales <- cleaned_data %>%
  group_by(PCKT_SIZE, BRAND) %>%
  summarise(Total_sales = sum(TOT_SALES)) %>%
  arrange(desc(Total_sales))

# 'Sales_Category' based on Total_sale
size_brand_sales_category <- quantile(size_brand_sales$Total_sales, 
                                      probs = c(0, 1/3, 2/3, 1))
breaks <- size_brand_sales_category
labels <- c("Low", "Average", "Good")
size_brand_sales <- size_brand_sales %>%
  mutate(Sales_Category = cut(Total_sales, 
                              breaks = breaks, 
                              labels = labels, 
                              include.lowest = TRUE))
print(size_brand_sales, n = Inf)

# Viz: Total Sales by PCKT_SIZE and Sales_Category"
ggplot(size_brand_sales, aes(x = PCKT_SIZE, y = Total_sales, label = BRAND)) +
  geom_point(size = 2) + 
  geom_text_repel(data = subset(size_brand_sales, Sales_Category == "Good"),  
                  aes(label = BRAND), 
                  nudge_y = 500,  # Adjust the position of the text labels
                  show.legend = FALSE) +  # Exclude from the legend
  facet_wrap(~ Sales_Category, nrow = 1) +
  labs(title = "Total Sales by PCKT_SIZE and Sales_Category",
       x = "PCKT_SIZE",
       y = "Total Sales")

## For write_csv
filePath <- "C:/Users/princ/Desktop/Adlina/adlina files/Portfolio Projects/Quantium- Retail Strategy and Analytics/Task 1/Dataset/QVI_data.csv"
write_csv(cleaned_data, paste0(filePath, "QVI_data.csv"))


## Data analysis on customer segments

# Calculate the number of customers by LIFESTAGE and PREMIUM_CUSTOMER
customer_count <- cleaned_data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(Customers = n_distinct(LYLTY_CARD_NBR)) %>%
  arrange(desc(Customers))
print(customer_count, n=nrow(customer_count))

# % of Total Sales: (customer segments contribute the most to the total sales)
brand_percentage_by_lifestage_premium <- cleaned_data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarize(Percentage_TOT_SALES = sum(TOT_SALES) 
            / sum(cleaned_data$TOT_SALES) * 100) %>%
  arrange(desc(Percentage_TOT_SALES))
print(brand_percentage_by_lifestage_premium, n=nrow(brand_percentage_by_lifestage_premium))

# Create a bar plot on total sales by LIFESTAGE and PREMIUM_CUSTOMER
p <- ggplot(data = brand_percentage_by_lifestage_premium) +
  geom_mosaic(aes(weight = Percentage_TOT_SALES,
                  x = product(PREMIUM_CUSTOMER, LIFESTAGE),
                  fill = PREMIUM_CUSTOMER)) +
  labs(title = "Proportion of Sales",
       x = "Lifestage",
       y = "Premium Customer flag") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Plot and label with proportion of sales
p + geom_text(data = ggplot_build(p)$data[[1]], 
              aes(x = (xmin + xmax)/2 , y = (ymin + ymax)/2, 
                  label = sprintf("%.1f%%", 
                   brand_percentage_by_lifestage_premium$Percentage_TOT_SALES)))

# Average Units: (buying behavior in terms of quantity)
average_units <- cleaned_data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(Avg_Units = mean(PROD_QTY),
            SE = sd(PROD_QTY) / sqrt(n()))

# bar plot 
ggplot(average_units, 
       aes(x = LIFESTAGE, y = Avg_Units, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Average Units Bought by Lifestage and Premium Customer",
       x = "Lifestage",
       y = "Average Units",
       fill = "Premium Customer") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Average price per unit: (the pricing dynamics and customer spending patterns)
average_price <- cleaned_data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(Avg_Price_Per_Unit = sum(TOT_SALES) / sum(PROD_QTY)) %>%
  arrange(desc(Avg_Price_Per_Unit))
print(average_price, n=nrow(average_price))

# bar plot for the average price per unit
ggplot(average_price, aes(x = LIFESTAGE, y = Avg_Price_Per_Unit, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Price per Unit by LIFESTAGE and PREMIUM_CUSTOMER",
       x = "LIFESTAGE",
       y = "Average Price per Unit",
       fill = "PREMIUM_CUSTOMER") +
  theme_minimal() +
  theme(legend.position = "bottom")

unique(cleaned_data$LIFESTAGE)
unique(cleaned_data$PREMIUM_CUSTOMER)

# Subset the data for the specified groups
mainstream_subset <- cleaned_data[cleaned_data$PREMIUM_CUSTOMER == "Mainstream" 
    & (cleaned_data$LIFESTAGE %in% c("MIDAGE SINGLES/COUPLES", "YOUNG SINGLES/COUPLES")), ]
premium_subset <- cleaned_data[cleaned_data$PREMIUM_CUSTOMER == "Premium" 
    & (cleaned_data$LIFESTAGE %in% c("MIDAGE SINGLES/COUPLES", "YOUNG SINGLES/COUPLES")), ]
budget_subset <- cleaned_data[cleaned_data$PREMIUM_CUSTOMER == "Budget" 
    & (cleaned_data$LIFESTAGE %in% c("MIDAGE SINGLES/COUPLES", "YOUNG SINGLES/COUPLES")), ]

uniqueN(mainstream_subset)
uniqueN(premium_subset)
uniqueN(budget_subset)

# Perform independent t-tests
t_test_mainstream_vs_premium <- 
  t.test(mainstream_subset$TOT_SALES / mainstream_subset$PROD_QTY, 
         premium_subset$TOT_SALES / premium_subset$PROD_QTY)
t_test_mainstream_vs_budget <- 
  t.test(mainstream_subset$TOT_SALES / mainstream_subset$PROD_QTY, 
         budget_subset$TOT_SALES / budget_subset$PROD_QTY)

# Print or summarize the results
t_test_mainstream_vs_premium
t_test_mainstream_vs_budget

# Create a combined subset for Premium and Budget
combined_subset <- cleaned_data[cleaned_data$PREMIUM_CUSTOMER %in% c("Premium", "Budget") 
                                & (cleaned_data$LIFESTAGE %in% c("MIDAGE SINGLES/COUPLES", "YOUNG SINGLES/COUPLES")), ]

# Perform the t-test comparing Mainstream to the combined Premium and Budget
t_test_mainstream_vs_combined <- t.test(mainstream_subset$TOT_SALES / mainstream_subset$PROD_QTY, 
                                        combined_subset$TOT_SALES / combined_subset$PROD_QTY)

# Print the results
print(t_test_mainstream_vs_combined)

# Calculate the average sales per unit for Mainstream and the combined Premium and Budget
percentage_difference <- ((mean(mainstream_subset$TOT_SALES / mainstream_subset$PROD_QTY) - 
                             mean(combined_subset$TOT_SALES / combined_subset$PROD_QTY)) / 
                            mean(combined_subset$TOT_SALES / combined_subset$PROD_QTY)) * 100

# Print the percentage difference
print(percentage_difference)


### Deep dive into specific customer segments for insights

## Market basket analysis

# 1. Apriori algorithm on specific customer segments

perform_market_basket_analysis <- function(customer_segment, lifestage) {
  # Filter data for the specified customer segment and lifestage
  subset_data <- cleaned_data[
    cleaned_data$PREMIUM_CUSTOMER == customer_segment & cleaned_data$LIFESTAGE == lifestage,
  ]
  
  # Transaction encoding
  transactions <- as(split(subset_data$BRAND, subset_data$TXN_ID), "transactions")

  # Generate rules
  rules <- apriori(transactions, parameter = list(support = 0.1, 
                                                  confidence = 0.1))
  # A support of 0.01 means that the itemset(brand) must appear in at least 10% of all transactions to be considered
  # A confidence of 0.1 means the algorithm will only generate rules where the consequent is purchased at least 10% of the time when the antecedent is purchased.
  
  # Convert rules to data frame
  rules_df <- as.data.frame(inspect(rules))
  
  # Create a barplot
  barplot(sort(rules_df$support), horiz = TRUE, 
          main = paste("Brand Support for", customer_segment, "-", lifestage), 
          xlab = "Support", ylab = "Brand", names.arg = rules_df$rhs) 
  
   # Add transaction count to the barplot
   text(x = sort(rules_df$support), y = seq_along(rules_df$lhs), 
            labels = rules_df$count, pos = 4, col = "red")
  
  return(rules)  # Optionally return rules if needed outside the function
}

rules_young <- perform_market_basket_analysis("Mainstream", "YOUNG SINGLES/COUPLES")
# rules_midage <- perform_market_basket_analysis("Mainstream", "MIDAGE SINGLES/COUPLES")


## 2. Brand Affinity analysis comparing specific customers with rest of population
## this analysis measures the association or preference of a particular customer group towards different brands compared to the broader population.

brand_affinity_analysis <- function(lifestage, premium_customer) {
  # Filter data for the specified customer segment
  segment_data <- filter(
    cleaned_data,
    LIFESTAGE == lifestage & PREMIUM_CUSTOMER == premium_customer
  )
  
  # Filter data for the rest of the population
  other_data <- filter(
    cleaned_data,
    !(
      LIFESTAGE == lifestage & PREMIUM_CUSTOMER == premium_customer
    )
  )
  
  # Calculating Quantity Sums
  quantity_segment <- sum(segment_data$PROD_QTY)
  quantity_other <- sum(other_data$PROD_QTY)
  
  # Brand Affinity Calculation
  quantity_segment_by_brand <- segment_data %>%
    group_by(BRAND) %>%
    summarize(targetSegment = sum(PROD_QTY) / quantity_segment)
  
  quantity_other_by_brand <- other_data %>%
    group_by(BRAND) %>%
    summarize(other = sum(PROD_QTY) / quantity_other)
  
  brand_proportions <- merge(
    quantity_segment_by_brand,
    quantity_other_by_brand,
    by = "BRAND"
  ) %>%
    mutate(affinityToBrand = targetSegment / other, 
           absoluteDifference = abs(targetSegment - other))
  
  brand_proportions <- brand_proportions[order(-brand_proportions$affinityToBrand), ]
  return(brand_proportions)
  
}

brand_affinity_analysis("YOUNG SINGLES/COUPLES", "Mainstream")
brand_affinity_analysis("MIDAGE SINGLES/COUPLES", "Mainstream")
