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

display_in_title <- " for all Districts"
# add data CBI
dat<-read_rds(file = "data/data.rds")
df_data<- dat$df_data
df_shape<- dat$df_shape

beneficiary_types <- df_data %>% 
  filter(!is.na(Select_Beneficiary_Type)) %>% pull(Select_Beneficiary_Type) %>% unique() %>% sort()

# add data food security
fs_df_data <-dat$fs_df_data
fs_beneficiary_types <- fs_df_data %>% 
  filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique() %>% sort()