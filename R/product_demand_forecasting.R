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

# * 2.3 Trend Diagnostics ----

# * 2.1 ACF & PACF Plot - Art & Crafts ----
combined_weekly_by_category %>% 
    filter(product_category == "Art & Crafts") %>% 
    plot_acf_diagnostics(
        .date_var = date, .value = units_sold, .show_white_noise_bars = T, .interactive = T
    )

# * 2.4 ACF & PACF Plot - Games ----
combined_weekly_by_category %>% 
    filter(product_category == "Games") %>% 
    plot_acf_diagnostics(
        .date_var = date, .value = units_sold, .show_white_noise_bars = T, .interactive = T
    )

# * 2.5 Anomaly Diagnostics ----
combined_weekly_by_category %>% 
    group_by(product_category) %>% 
    plot_anomaly_diagnostics(.date_var = date, .value = units_sold, .interactive = T,
                             .facet_ncol = 2)

# * 2.6 Seasonal Diagnostics ----
combined_weekly_by_category %>% 
    filter(product_category == "Art & Crafts") %>% 
    plot_seasonal_diagnostics(
        .date_var = date,
        .value = units_sold
    )


# 3.0 FEATURE ENGINEERING ----
forecast_horizon <- 12

# * 3.1 Feature Engineering Part I ----

full_data_tbl <- combined_weekly_by_category %>% 
    select(date, everything(.)) %>% 
    group_by(product_category) %>% 
    pad_by_time(.date_var = date, .by = "week", .pad_value = 0) %>% 
    ungroup() %>% 
    
    # Log Transformations
    mutate(units_sold = log(units_sold)) %>% 
    
    # Group-Wise Feature Transformation
    group_by(product_category) %>% 
    future_frame(date, .length_out = forecast_horizon, .bind_data = T) %>% 
    ungroup() %>% 
    
    # Lags, Rolling Features & Fourier Features
    group_by(product_category) %>% 
    group_split() %>% 
    map(.f = function(df){
        df %>% 
            arrange(date) %>% 
            tk_augment_fourier(date, .periods = c(2, 4)) %>% 
            tk_augment_lags(units_sold, .lags = 12) %>% 
            tk_augment_slidify(
                units_sold_lag12,
                .f = ~ mean(.x, na.rm = T),
                .period  = c(1, 4, 8),
                .partial = TRUE,
                .align   = "center"
            )
    }) %>% 
    bind_rows() %>% 
    rowid_to_column(var = "rowid")


# * 3.2 Feature Engineering Part II ----

# * Data Prepared ----
data_prepared_tbl <- full_data_tbl %>% 
    filter(!is.na(units_sold)) %>% 
    drop_na()

# * Future Data ----
future_tbl <- full_data_tbl %>% 
    filter(is.na(units_sold)) 

future_tbl <- future_tbl %>% 
    mutate(across(.cols = contains("_lag"), 
                  .fns  = ~ ifelse(is.nan(.x), NA, .x))
    ) %>% 
    fill(contains("_lag"), .direction = "up")


# * 3.3 Feature Engineering Part III: ----

# * Time Series Split ----
splits <- data_prepared_tbl %>% 
    time_series_split(date, assess = 12, cumulative = TRUE)

# Visualizing Time Series Split
splits %>%
    tk_time_series_cv_plan() %>% 
    filter(product_category == "Art & Crafts") %>%
    plot_time_series_cv_plan(
        date,
        units_sold,
        .title = "Time Series Split - Art & Crafts",
        .interactive = F) 


# 4.0 DATA PREPROCESSING ----

# * 4.1 Recipes ----
recipe_spec <- recipe(units_sold ~ ., data = training(splits)) %>% 
    update_role(rowid, new_role = "indicator") %>% 
    step_timeseries_signature(date) %>% 
    step_rm(matches("(.iso|.xts|second|hour|day|minute|am.pm|wday|day)")) %>% 
    step_normalize(date_index.num, date_year) %>% 
    step_dummy(all_nominal(), one_hot = TRUE)


recipe_spec %>% prep() %>% juice() %>% glimpse()

# 5.0 MODELING ----

# * 5.1 Prophet ----
workflow_fit_prophet <- workflow() %>% 
    add_model(
        spec = prophet_reg() %>% set_engine("prophet")
    ) %>% 
    add_recipe(recipe_spec) %>% 
    fit(training(splits))

# * 5.2 XGBOOST ----
workflow_fit_xgboost <- workflow() %>% 
    add_model(
        spec = boost_tree(mode = "regression") %>% set_engine("xgboost")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(training(splits))

# * 5.3 Prophet Boost ----
workflow_fit_phrophet_boost <- workflow() %>% 
    add_model(
        spec = prophet_boost(
            seasonality_daily = F,
            seasonality_weekly = F,
            seasonality_yearly = F
        ) %>% set_engine("prophet_xgboost")
    ) %>% add_recipe(recipe_spec) %>% 
    fit(training(splits))

# * 5.4 Random Forest ----
workflow_fit_ranger <- workflow() %>% 
    add_model(
        spec = rand_forest(mode = "regression") %>% set_engine("ranger")
    ) %>% add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(training(splits))


# * 5.5 Cubist ----
workflow_fit_cubist <- workflow() %>% 
    add_model(
        spec = cubist_rules() %>% set_engine("Cubist")
    ) %>% add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(training(splits))

# * 5.6 Mars ----
workflow_fit_mars <- workflow() %>% 
    add_model(
        spec = mars(mode = "regression") %>% set_engine("earth")
    ) %>% 
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>% 
    fit(training(splits))

# * 5.7 Model Evaluation (Accuracy Check) ----

# Modeltime Table
models_tbl <- modeltime_table(
    workflow_fit_prophet,
    workflow_fit_xgboost,
    workflow_fit_phrophet_boost,
    workflow_fit_ranger,
    workflow_fit_cubist,
    workflow_fit_mars
)

# Modeltime Accuracy
models_accuracy_tbl <- models_tbl %>% 
    modeltime_accuracy(testing(splits)) %>% 
    arrange(rmse)

# Saving Modeltime Accuracy Table
models_accuracy_tbl %>%
    select (-c(mape, mase, smape)) %>%
    write_rds(file = "Plots/models_accuracy_tbl.rds")

# * 5.8 Visualize Test Set Forecast ----

# Calibration
calibration_tbl <- models_tbl %>% 
    modeltime_calibrate(new_data = testing(splits))

forecast_plot <- function(model){
    
    forecast_data <- calibration_tbl %>% 
        modeltime_forecast(
            new_data = testing(splits),
            actual_data = data_prepared_tbl,
            keep_data = T
        ) %>% 
        filter(.model_desc %in% c(model, "ACTUAL")) %>% 
        mutate(units_sold = exp(units_sold),
               .value = exp(.value)) 
    
    forecast_plot <- forecast_data %>% 
        group_by(product_category) %>% 
        plot_modeltime_forecast(
            .facet_ncol = 2,
            .conf_interval_alpha = 0.1,
            .interactive = F
        )+
        scale_y_continuous(labels = scales::comma_format())
    
    return(forecast_plot)
    
}

forecast_plot(model = "PROPHET W/ REGRESSORS") %>% 
    write_rds("Plots/test_forecast_prophet_reg.rds")

forecast_plot(model = "EARTH") %>% 
    write_rds("Plots/test_forecast_earth.rds")

forecast_plot(model = "RANGER") %>% 
    write_rds("Plots/test_plot_ranger.rds")


# 7.0 FUTURE FORECAST ----

# * Refit ----
model_refit_tbl <- models_tbl %>% 
    filter(.model_id %in% c(1, 6, 4)) %>% 
    modeltime_refit(data_prepared_tbl)

# * Future Forecast ----
future_forecast_tbl <- model_refit_tbl %>% 
    modeltime_forecast(
        new_data = future_tbl,
        actual_data = data_prepared_tbl,
        keep_data = TRUE
    ) %>% 
    mutate(
        .value = exp(.value),
        .units_sold = exp(units_sold)
    )

future_forecast_tbl %>% 
    group_by(product_category) %>% 
    plot_modeltime_forecast(
        .facet_ncol = 2,
        .conf_interval_alpha = 0.1,
        .interactive = F
    )
