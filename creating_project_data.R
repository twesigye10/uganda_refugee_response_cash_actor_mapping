#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load packages
library(shiny)
library(sf)
library(tidyverse)
library(lubridate)
library(leaflet)
library(bslib)
library(ggreach)
library(highcharter)
library(billboarder)
library(glue)
library(janitor)

# Data --------------------------------------------------------------------

# currency conversion
currency_conversion_factor <- 1000
display_in_title <- " For All districts"
# CBI data
cbi_df_data <- read_csv(file = "data/RRP_5W_CBI_for_basic_needs_20210305_055004_UTC.csv")
cbi_df_data <- janitor::clean_names(cbi_df_data) %>% 
    separate(select_month, c("Month", "Year"), "-", remove= FALSE, extra = "drop") %>% 
    mutate(
        total_amount_of_cash_transfers = ifelse(!is.na(total_amount_of_cash_transfers), (total_amount_of_cash_transfers/currency_conversion_factor), total_amount_of_cash_transfers) ,
        Quarter = case_when(Month %in% c("Jan", "Feb", "Mar")~"Q1",
                            Month %in% c("Apr", "May", "Jun")~"Q2",
                            Month %in% c("Jul", "Aug", "Sep")~"Q3",
                            Month %in% c("Oct", "Nov", "Dec")~"Q4"  ),
        Date = my(select_month),
        Year = paste0("20",Year)
    ) %>% 
    arrange(desc(Year),desc(Quarter))

cbi_beneficiary_types <- cbi_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()

df_shape <- st_read("data/UGA_Admin/UGA_Admin_2_Districts_2020.shp", crs=4326 ) %>% 
    mutate(ADM2_EN = toupper(ADM2_EN))

# Food security data
df_food_security <- read_csv("data/Food_Security.csv")
df_food_security <- janitor::clean_names(df_food_security) %>% 
    mutate(
    fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers = ifelse(!is.na(fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers), (fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers/currency_conversion_factor), NA)
    )%>% 
    filter(!is.na(select_quarter))

fs_df_data <- df_food_security %>% 
    separate(select_quarter, c("Quarter", "Year"), " ", remove= FALSE, extra = "drop")

fs_beneficiary_types <- fs_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()

# Livelihoods data
df_emergency_livelihood_support <- read_csv("data/ELS_Increased_access_to_short-term_employment_opportunities.csv")
df_emergency_livelihood_support <- janitor::clean_names(df_emergency_livelihood_support) %>% 
    mutate(
        total_cash_value_of_cash_for_work_ugx = ifelse(!is.na(total_cash_value_of_cash_for_work_ugx), (total_cash_value_of_cash_for_work_ugx/currency_conversion_factor), NA)
    )%>% 
    filter(!is.na(select_quarter))

seo_df_data <- df_emergency_livelihood_support %>% 
    separate(select_quarter, c("Quarter", "Year"), " ", remove= FALSE, extra = "drop")

seo_beneficiary_types <- seo_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()

# Increased Access Productive Assets
df_access_productive_assets <- read_csv("data/Increased_access_to_productive_assets.csv")
df_access_productive_assets <- janitor::clean_names(df_access_productive_assets) %>% 
    mutate(
        total_cash_value_of_grants_distributed_for_productive_assets_ugx = ifelse(!is.na(total_cash_value_of_grants_distributed_for_productive_assets_ugx), (total_cash_value_of_grants_distributed_for_productive_assets_ugx/currency_conversion_factor), NA)
    )%>% 
    filter(!is.na(select_quarter))

apa_df_data <- df_access_productive_assets %>% 
    separate(select_quarter, c("Quarter", "Year"), " ", remove= FALSE, extra = "drop")

apa_beneficiary_types <- apa_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()


# Environment Protection data
df_environment_protection_restoration <- read_csv("data/Forests_wetlands_shorelines_protected_and_restored.csv")
df_environment_protection_restoration <- janitor::clean_names(df_environment_protection_restoration) %>% 
    mutate(
        total_cash_value_of_cash_for_work_ugx = ifelse(!is.na(total_cash_value_of_cash_for_work_ugx), (total_cash_value_of_cash_for_work_ugx/currency_conversion_factor), NA)
    ) %>% 
    rename(location_district = district_name) %>% 
    filter(!is.na(select_quarter))

epr_df_data <- df_environment_protection_restoration %>% 
    separate(select_quarter, c("Quarter", "Year"), " ", remove= FALSE, extra = "drop")

epr_beneficiary_types <- epr_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()

# saving several data objects into an RDS object
data_for_saving <- list()
data_for_saving$cbi_df_data <- cbi_df_data
data_for_saving$df_shape <- df_shape
data_for_saving$fs_df_data <- fs_df_data
data_for_saving$seo_df_data <- seo_df_data
data_for_saving$apa_df_data <- apa_df_data
data_for_saving$epr_df_data <- epr_df_data

saveRDS(data_for_saving, file = "data/new_data.rds")

get_new_dat <- read_rds(file = "data/new_data.rds")

get_new_dat$seo_df_data %>% view()
