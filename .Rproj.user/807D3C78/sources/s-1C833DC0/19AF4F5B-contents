# PRODUCT DEMAND FORECASTING WITH MODELTIME ----

# SETUP ----

# 1.1 * Libraries ----
library(tidyverse)
library(lubridate)
library(timetk)
library(janitor)
library(tidymodels)
library(modeltime)
library(rules)

# 1.2  * Load Data ----
sales_tbl <- read.csv("Data/sales.csv") %>% as_tibble() %>% clean_names()

products_tbl <- read.csv("Data/products.csv") %>% as_tibble %>% clean_names()

stores_tbl <- read.csv("Data/stores.csv") %>% as_tibble() %>% clean_names()

inventory_tbl <- read.csv("Data/inventory.csv") %>% as_tibble() %>% clean_names()

# 1.3 * Merge Datasets ----
combined_tbl <- sales_tbl %>% 
    left_join(products_tbl) %>% 
    # left_join(stores_tbl) %>% 
    # left_join(inventory_tbl) %>% 
    select(-c(sale_id, store_id, product_id)) %>% 
    mutate(across(product_cost:product_price, .fns = . %>% str_replace_all("\\$", ""))) %>% 
    mutate(across(product_cost:product_price, .fns = . %>% str_trim(side = "both"))) %>% 
    mutate(across(product_cost:product_price, .fns = . %>% as.numeric)) %>% 
    mutate(date = ymd(date))

# combined_tbl %>% write.csv(file = "Data/combined_tbl.csv")


# * 1.4 Data Inspection ----

# Date Range
combined_tbl %>% 
    summarise(min_date = min(date),
              max_date = max(date))


# 2.0 Data Preparation / EDA ----

# 2.1 * Aggregate Units Sold By Week ----

# Aggregate Units Sold: All Products
combined_tbl_weekly_units <- combined_tbl %>% 
    summarise_by_time(
        .date_var  = date,
        .by        = "week",
        units_sold = sum(units)
    ) %>% 
    ungroup()


# Aggregate Units Sold: Product Categories
combined_weekly_by_category <- combined_tbl %>% 
    group_by(product_category) %>% 
    summarise_by_time(
        .date_var  = date,
        .by        = "week",
        units_sold = sum(units)
    ) %>% 
    ungroup() %>% 
    filter(date != as.Date("2018-09-30")) %>% 
    mutate(product_category = product_category %>% fct_relevel(
        "Art & Crafts", "Toys", "Games", "Sports & Outdoors", "Electronics")) 

# * 2.2 Visualize Units Sold Trend ----
combined_weekly_by_category %>% 
    plot_time_series(
        .date_var = date,
        .value = units_sold,
        .interactive = FALSE,
        .facet_vars = product_category,
        .facet_ncol = 2
    )
