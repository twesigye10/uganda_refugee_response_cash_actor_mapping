#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load packages
library(sf)
library(tidyverse)
library(lubridate)
library(janitor)

# Data --------------------------------------------------------------------

# currency conversion
currency_conversion_factor <- 1000
display_in_title <- " For All districts"


# shapefile ---------------------------------------------------------------

df_shape <- st_read("support_data/data/UGA_Admin/UGA_Admin_2_Districts_2020.shp", crs=4326 ) %>% 
    mutate(ADM2_EN = toupper(ADM2_EN))%>% 
    select(ADM2_EN) %>% st_transform(crs = 32636) %>% 
    st_simplify(dTolerance = 800) %>% st_transform(crs=4326)


# CBI data ----------------------------------------------------------------

cbi_df_data <- read_csv(file = "support_data/data/new_quarter_data/api_cbi_CBI_for_basic_needs.csv")
cbi_df_data <- janitor::clean_names(cbi_df_data) %>% 
    mutate(
        select_month = ifelse(!is.na(reporting_month), paste0(month(ym(reporting_month), label = T, abbr = T), "-", substr( reporting_month, 3, 4)), select_month)
    ) %>% 
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
    arrange(desc(Year),desc(Quarter)) %>% 
    filter(!is.na(select_month) & !is.na(select_delivery_mechanism) & !is.na(partner_name)) %>%  
    filter(
        !grepl("^[0-9]", select_delivery_mechanism,  ignore.case = FALSE) & 
            !grepl("^[0-9]", partner_name,  ignore.case = FALSE) & 
            total_amount_of_cash_transfers > 0
    )

cbi_beneficiary_types <- cbi_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()


# Food security data ------------------------------------------------------

df_food_security <- read_csv("support_data/data/new_quarter_data/api_fs_Food_Security.csv")
df_food_security <- janitor::clean_names(df_food_security) %>% 
    mutate(
    fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers = ifelse(!is.na(fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers), (fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers/currency_conversion_factor), NA)
    )%>% 
    filter(!is.na(select_quarter) & !is.na(select_delivery_mechanism) & !is.na(partner_name)) %>% 
    filter(
        !grepl("^[0-9]", select_delivery_mechanism,  ignore.case = FALSE) & 
            !grepl("^[0-9]", partner_name,  ignore.case = FALSE) & 
            fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers > 0
    )

fs_df_data <- df_food_security %>% 
    separate(select_quarter, c("Quarter", "Year"), " ", remove= FALSE, extra = "drop")

fs_beneficiary_types <- fs_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()


# Emergency Livelihood Support --------------------------------------------

# Increased access to short-term employment opportunities data
seo_df_data <- read_csv("support_data/data/new_quarter_data/api_live_ELS_Increased_access_to_short_term_employment_opportunities.csv")
seo_df_data <- janitor::clean_names(seo_df_data) %>% 
    mutate(
        total_cash_value_of_cash_for_work_ugx = ifelse(!is.na(total_cash_value_of_cash_for_work_ugx), (total_cash_value_of_cash_for_work_ugx/currency_conversion_factor), NA)
    )%>% 
    filter(!is.na(select_quarter) & !is.na(delivery_mechanism) & !is.na(partner_name)) %>% 
    filter(
        !grepl("^[0-9]", delivery_mechanism,  ignore.case = FALSE) & 
            !grepl("^[0-9]", partner_name,  ignore.case = FALSE) & 
            total_cash_value_of_cash_for_work_ugx > 0
    )

seo_df_data <- seo_df_data %>% 
    separate(select_quarter, c("Quarter", "Year"), " ", remove= FALSE, extra = "drop")

seo_beneficiary_types <- seo_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()

# Increased Access Productive Assets
df_access_productive_assets <- read_csv("support_data/data/new_quarter_data/api_live_ELS_Increased_access_to_productive_assetss.csv")
df_access_productive_assets <- janitor::clean_names(df_access_productive_assets) %>% 
    mutate(
        total_cash_value_of_grants_distributed_for_productive_assets_ugx = ifelse(!is.na(total_cash_value_of_grants_distributed_for_productive_assets_ugx), (total_cash_value_of_grants_distributed_for_productive_assets_ugx/currency_conversion_factor), NA)
    )%>% 
    filter(!is.na(select_quarter) & !is.na(delivery_mechanism) & !is.na(partner_name)) %>% 
    filter(
        !grepl("^[0-9]", delivery_mechanism,  ignore.case = FALSE) & 
            !grepl("^[0-9]", partner_name,  ignore.case = FALSE) & 
            total_cash_value_of_grants_distributed_for_productive_assets_ugx > 0
    )

apa_df_data <- df_access_productive_assets %>% 
    separate(select_quarter, c("Quarter", "Year"), " ", remove= FALSE, extra = "drop")

apa_beneficiary_types <- apa_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()



# Environment Protection data ---------------------------------------------

df_environment_protection_restoration <- read_csv("support_data/data/new_quarter_data/api_env_Forests_wetlands_shorelines_protected_and_restored.csv")
df_environment_protection_restoration <- janitor::clean_names(df_environment_protection_restoration) %>% 
    mutate(
        total_cash_value_of_cash_for_work_ugx = ifelse(!is.na(total_cash_value_of_cash_for_work_ugx), (total_cash_value_of_cash_for_work_ugx/currency_conversion_factor), NA),
        select_quarter = ifelse(is.na(select_quarter), case_when(substr(select_month, 1, 3) %in% c("Jan", "Feb", "Mar") ~ paste0("Q1 20", substr(select_month, 5, 6)),
                                                                 substr(select_month, 1, 3) %in% c("Apr", "May", "Jun")~ paste0("Q2 20", substr(select_month, 5, 6)),
                                                                 substr(select_month, 1, 3) %in% c("Jul", "Aug", "Sep")~ paste0("Q3 20", substr(select_month, 5, 6)),
                                                                 substr(select_month, 1, 3) %in% c("Oct", "Nov", "Dec")~ paste0("Q4 20", substr(select_month, 5, 6))  ), select_quarter)
    ) %>% 
    rename(location_district = district_name) %>% 
    filter(!is.na(select_quarter) & !is.na(delivery_mechanism) & !is.na(partner_name))%>% 
    filter(
        !grepl("^[0-9]", delivery_mechanism,  ignore.case = FALSE) & 
            !grepl("^[0-9]", partner_name,  ignore.case = FALSE) & 
            total_cash_value_of_cash_for_work_ugx > 0
    )

epr_df_data <- df_environment_protection_restoration %>% 
    separate(select_quarter, c("Quarter", "Year"), " ", remove= FALSE, extra = "drop")

epr_beneficiary_types <- epr_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()

# households_using_alternative_and_or_renewable_energy
df_alternative_and_or_renewable_energy <- read_csv("support_data/data/new_quarter_data/api_env_households_using_alternative_and_or_renewable_energy.csv")
df_alternative_and_or_renewable_energy <- janitor::clean_names(df_alternative_and_or_renewable_energy) %>% 
    mutate(
        total_cash_value_of_cash_for_work_ugx = ifelse(!is.na(total_cash_value_of_cash_for_work_ugx), (total_cash_value_of_cash_for_work_ugx/currency_conversion_factor), NA),
        select_quarter = ifelse(is.na(select_quarter), case_when(substr(select_month, 1, 3) %in% c("Jan", "Feb", "Mar") ~ paste0("Q1 20", substr(select_month, 5, 6)),
                                                                 substr(select_month, 1, 3) %in% c("Apr", "May", "Jun")~ paste0("Q2 20", substr(select_month, 5, 6)),
                                                                 substr(select_month, 1, 3) %in% c("Jul", "Aug", "Sep")~ paste0("Q3 20", substr(select_month, 5, 6)),
                                                                 substr(select_month, 1, 3) %in% c("Oct", "Nov", "Dec")~ paste0("Q4 20", substr(select_month, 5, 6))  ), select_quarter)
    ) %>% 
    # rename(location_district = district_name) %>% 
    filter(!is.na(select_quarter) & !is.na(delivery_mechanism) & !is.na(partner_name))%>% 
    filter(
        !grepl("^[0-9]", delivery_mechanism,  ignore.case = FALSE) & 
            !grepl("^[0-9]", partner_name,  ignore.case = FALSE) & 
            total_cash_value_of_cash_for_work_ugx > 0
    )

aor_df_data <- df_alternative_and_or_renewable_energy %>% 
    separate(select_quarter, c("Quarter", "Year"), " ", remove= FALSE, extra = "drop")

aor_beneficiary_types <- aor_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()

# using_fuel_efficient_cook_stove
df_fuel_efficient_cook_stove <- read_csv("support_data/data/new_quarter_data/api_env_households_that_self_report_using_fuel_efficient_cook_stove_to_cook_the_main_meal.csv")
df_fuel_efficient_cook_stove <- janitor::clean_names(df_fuel_efficient_cook_stove) %>% 
    mutate(
        total_cash_value_of_cash_for_work_ugx = ifelse(!is.na(total_cash_value_of_cash_for_work_ugx), (total_cash_value_of_cash_for_work_ugx/currency_conversion_factor), NA),
        select_quarter = ifelse(is.na(select_quarter), case_when(substr(select_month, 1, 3) %in% c("Jan", "Feb", "Mar") ~ paste0("Q1 20", substr(select_month, 5, 6)),
                                                                 substr(select_month, 1, 3) %in% c("Apr", "May", "Jun")~ paste0("Q2 20", substr(select_month, 5, 6)),
                                                                 substr(select_month, 1, 3) %in% c("Jul", "Aug", "Sep")~ paste0("Q3 20", substr(select_month, 5, 6)),
                                                                 substr(select_month, 1, 3) %in% c("Oct", "Nov", "Dec")~ paste0("Q4 20", substr(select_month, 5, 6))  ), select_quarter)
    ) %>% 
    # rename(location_district = district_name) %>% 
    filter(!is.na(select_quarter) & !is.na(delivery_mechanism) & !is.na(partner_name))%>% 
    filter(
        !grepl("^[0-9]", delivery_mechanism,  ignore.case = FALSE) & 
            !grepl("^[0-9]", partner_name,  ignore.case = FALSE) & 
            total_cash_value_of_cash_for_work_ugx > 0
    )

ecs_df_data <- df_fuel_efficient_cook_stove %>% 
    separate(select_quarter, c("Quarter", "Year"), " ", remove= FALSE, extra = "drop")

ecs_beneficiary_types <- ecs_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()



# WASH data ---------------------------------------------------------------

# CBI approach in sanitation services data
ss_df_data <- read_csv("support_data/data/new_quarter_data/api_wash_CBI_approach_in_sanitation_services.csv")
ss_df_data <- janitor::clean_names(ss_df_data) %>% 
    separate(end_date, c("Year", "Month"), "-", remove= FALSE, extra = "drop") %>% 
    mutate(
        total_cash_value_of_cash_grants_ugx = ifelse(!is.na(total_cash_value_of_cash_grants_ugx), (total_cash_value_of_cash_grants_ugx/currency_conversion_factor), NA),
        Quarter = case_when(Month %in% c("01", "02", "03")~"Q1",
                            Month %in% c("04", "05", "06")~"Q2",
                            Month %in% c("07", "08", "09")~"Q3",
                            Month %in% c("10", "11", "12")~"Q4"  ),
        select_quarter = paste(Quarter," ",Year)
    )%>% 
    filter(!is.na(end_date) & !is.na(delivery_mechanism) & !is.na(partner_name)) %>% 
    filter(
        !grepl("^[0-9]", delivery_mechanism,  ignore.case = FALSE) & 
            !grepl("^[0-9]", partner_name,  ignore.case = FALSE) & 
            total_cash_value_of_cash_grants_ugx > 0
    )

ss_beneficiary_types <- ss_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()

# CBI approach in WASH NFI data
wn_df_data <- read_csv("support_data/data/new_quarter_data/api_wash_CBI_approach_in_WASH_NFI.csv")
wn_df_data <- janitor::clean_names(wn_df_data) %>% 
    separate(end_date, c("Year", "Month"), "-", remove= FALSE, extra = "drop") %>% 
    mutate(
        total_cash_value_of_cash_grants_ugx = ifelse(!is.na(total_cash_value_of_cash_grants_ugx), (total_cash_value_of_cash_grants_ugx/currency_conversion_factor), NA),
        Quarter = case_when(Month %in% c("01", "02", "03")~"Q1",
                            Month %in% c("04", "05", "06")~"Q2",
                            Month %in% c("07", "08", "09")~"Q3",
                            Month %in% c("10", "11", "12")~"Q4"  ),
        select_quarter = paste(Quarter," ",Year)
    )%>% 
    filter(!is.na(end_date) & !is.na(delivery_mechanism) & !is.na(partner_name)) %>% 
    filter(
        !grepl("^[0-9]", delivery_mechanism,  ignore.case = FALSE) & 
            !grepl("^[0-9]", partner_name,  ignore.case = FALSE) & 
            total_cash_value_of_cash_grants_ugx > 0
    )

wn_beneficiary_types <- wn_df_data %>% 
    filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()

# saving several data objects into an RDS object --------------------------

data_for_saving <- list()
data_for_saving$df_shape <- df_shape
data_for_saving$cbi_df_data <- cbi_df_data
data_for_saving$fs_df_data <- fs_df_data
data_for_saving$seo_df_data <- seo_df_data
data_for_saving$apa_df_data <- apa_df_data
data_for_saving$epr_df_data <- epr_df_data
data_for_saving$aor_df_data <- aor_df_data
data_for_saving$ecs_df_data <- ecs_df_data
data_for_saving$ss_df_data <- ss_df_data
data_for_saving$wn_df_data <- wn_df_data

saveRDS(data_for_saving, file = "support_data/data/new_quarter_data/data.rds")

get_new_dat <- read_rds(file = "support_data/data/new_quarter_data/data.rds")

get_new_dat$wn_df_data %>% view()
