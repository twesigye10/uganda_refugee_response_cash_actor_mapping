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


# Data --------------------------------------------------------------------

# currency conversion
currency_conversion_factor <- 3650
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

saveRDS(df_data,  file = "data/cbi_project_df_data.RDS")
saveRDS(df_shape,  file = "data/cbi_project_df_shape.RDS")
saveRDS(df_shape_data,  file = "data/cbi_project_df_shape_data.RDS")
