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
# add data
df_data <- read_csv(file = "data/RRP_5W_CBI_for_basic_needs_20210305_055004_UTC.csv") %>% 
    rename_all(~str_replace_all(., "\\s+|\\(|\\)", "_")) %>% 
    separate(Select_Month, c("Month", "Year"), "-", remove= FALSE, extra = "drop") %>% 
    mutate(
        Total_amount_of_cash_transfers = ifelse(!is.na(Total_amount_of_cash_transfers), (Total_amount_of_cash_transfers/currency_conversion_factor), Total_amount_of_cash_transfers) ,
        Quarter = case_when(Month %in% c("Jan", "Feb", "Mar")~"Q1",
                            Month %in% c("Apr", "May", "Jun")~"Q2",
                            Month %in% c("Jul", "Aug", "Sep")~"Q3",
                            Month %in% c("Oct", "Nov", "Dec")~"Q4"  ),
        Date = my(Select_Month),
        Year = paste0("20",Year)
    ) %>% 
    rowwise() %>%
    mutate(
        i.hh_receiving_any_form_of_cash = sum(Households_receiving_cash_assistance_for_basic_needs, Households_receiving_voucher_assistance_for_basic_needs, na.rm = T),
        i.psn_hh_receiving_any_form_of_cash = sum(c_across(PSN_households_receiving_cash_assistance_for_basic_needs__total_:PSN_households_receiving_voucher_assistance_for_basic_needs__Woman_at_risk_), na.rm = T)
    ) %>% 
    ungroup() %>% 
    arrange(desc(Year),desc(Quarter))

beneficiary_types <- df_data %>% 
    filter(!is.na(Select_Beneficiary_Type)) %>% pull(Select_Beneficiary_Type) %>% unique()

df_shape <- st_read("data/UGA_Admin/UGA_Admin_2_Districts_2020.shp", crs=4326 ) %>% 
    mutate(ADM2_EN = toupper(ADM2_EN))

df_shape_data <- df_shape%>% 
    left_join(df_data, by = c("ADM2_EN"="Location_District")) 

districts_assessed<-df_shape_data %>% 
    filter(!is.na(Partner_Name)) %>% pull(ADM2_EN) %>% unique()

# Food security data
df_food_security <- read_csv("data/Food_Security.csv")
df_food_security <- janitor::clean_names(df_food_security) %>% 
    mutate(
    fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers = ifelse(!is.na(fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers), (fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers/currency_conversion_factor), NA)
    )

fs_df_data <- df_food_security %>% 
    separate(select_quarter, c("Quarter", "Year"), " ", remove= FALSE, extra = "drop")

fs_beneficiary_types <- fs_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()

# Livelihoods data
df_emergency_livelihood_support <- read_csv("data/ELS_Increased_access_to_short-term_employment_opportunities.csv")
df_emergency_livelihood_support <- janitor::clean_names(df_emergency_livelihood_support) %>% 
    mutate(
        total_cash_value_of_cash_for_work_ugx = ifelse(!is.na(total_cash_value_of_cash_for_work_ugx), (total_cash_value_of_cash_for_work_ugx/currency_conversion_factor), NA)
    )

els_df_data <- df_emergency_livelihood_support %>% 
    separate(select_quarter, c("Quarter", "Year"), " ", remove= FALSE, extra = "drop")

els_beneficiary_types <- els_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()


# Environment Protection data
df_environment_protection_restoration <- read_csv("data/Forests_wetlands_shorelines_protected_and_restored.csv")
df_environment_protection_restoration <- janitor::clean_names(df_environment_protection_restoration) %>% 
    mutate(
        total_cash_value_of_cash_for_work_ugx = ifelse(!is.na(total_cash_value_of_cash_for_work_ugx), (total_cash_value_of_cash_for_work_ugx/currency_conversion_factor), NA)
    ) %>% 
    rename(location_district = district_name)

epr_df_data <- df_environment_protection_restoration %>% 
    separate(select_quarter, c("Quarter", "Year"), " ", remove= FALSE, extra = "drop")

epr_beneficiary_types <- epr_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()

?rename
# saving several data objects into an RDS object
data_for_saving <- list()
data_for_saving$df_data <- df_data
data_for_saving$df_shape <- df_shape
data_for_saving$fs_df_data <- fs_df_data
data_for_saving$els_df_data <- els_df_data

saveRDS(data_for_saving, file = "data/new_data.rds")

get_new_dat <- read_rds(file = "data/new_data.rds")

get_new_dat$els_df_data
