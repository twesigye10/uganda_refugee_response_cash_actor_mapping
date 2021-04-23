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

els_df_data <-dat$els_df_data
els_beneficiary_types <- els_df_data %>% 
  filter(!is.na(select_beneficiary_type)) %>% pull(select_beneficiary_type) %>% unique()

# UI module -------------------------------------------------------------

tabPageUI <- function(
  id, label, inp_yearperiod, inp_yearperiod_choices,
  inp_quarterperiod, inp_mapreset, inp_selecteddistrict, outp_hhreceivingcash,
  outp_plotcashquarter, outp_map, outp_plotdeliverymechanism, outp_plotcashpartner){
  ns <- NS(id)
  
  tabPanel( label,
            # Sidebar
            sidebarLayout(
              # side panel
              sidebarPanel(
                fluidRow(
                  column(width = 4,
                         selectInput(inp_yearperiod, 
                                     "Select Year", 
                                     choices = c("All", unique(as.character(inp_yearperiod_choices))),
                                     selected = "All"
                         )
                  ),
                  column(width = 4,
                         selectInput(inp_quarterperiod, 
                                     "Select Quarter", 
                                     choices = c("All"),
                                     selected = "All"
                         )
                  ),
                  column(width = 4,
                         actionButton(inp_mapreset, "Reset Map"),
                         textOutput(inp_selecteddistrict)
                  ),
                  
                ),
                billboarderOutput(outp_hhreceivingcash ),
                highchartOutput(outp_plotcashquarter)
              ),
              # end side panel
              
              # main panel
              mainPanel(
                
                # map
                leafletOutput(outp_map, height = "60%"),
                
                fluidRow(
                  column(width = 6,
                         # Select Delivery Mechanism
                         highchartOutput(outp_plotdeliverymechanism, )
                  ),
                  column(width = 6,
                         highchartOutput(outp_plotcashpartner)
                  )
                )
              )
              # end main panel
            )
            # end sidebar layout
  )
  
}


# Server modules ----------------------------------------------------------

# filter cash data
filterCashData <- function(id, input_df, inp_id_yearperiod, inp_field_year, 
                           inp_id_quarterperiod, inp_field_quarter){
  moduleServer(id, function(input, output, session){
    # defaultly display all data from all districts, years and all quarters
    if (inp_id_yearperiod == "All" & inp_id_quarterperiod == "All"){
      input_df
    }else if(inp_id_yearperiod == "All" & inp_id_quarterperiod != "All"){
      input_df %>%
        filter(inp_field_quarter == inp_id_quarterperiod )
    }else if(inp_id_yearperiod != "All" & inp_id_quarterperiod == "All"){
      input_df %>%
        filter(inp_field_year == inp_id_yearperiod)
    } else{
      input_df %>%
        filter(inp_field_year == inp_id_yearperiod, 
               inp_field_quarter == inp_id_quarterperiod )
    }
    
    return(input_df)
  })
  
}

# filter cash data by district
filterCashDataByDistrict <- function(id, input_df, inp_field_district, input_district_click){
  moduleServer(id, function(input, output, session){
    input_df %>% 
      filter(inp_field_district == input_district_click )
    
    return(input_df)
  })
  
}


# donut chart module ------------------------------------------------------

donutChartCashBeneficiary <- function(id, input_data, input_field_group,
                                      input_field_analysis, input_title, input_beneficiary_vector){
  moduleServer(id, function(input, output, session){
    output$hhreceivingcash <-  renderBillboarder({
      
      df_billb_data <- input_data %>% 
        group_by({{input_field_group}} ) %>% 
        summarise(
          cash_assistance_by_beneficiary_type = sum({{input_field_analysis}}, na.rm = T)
        ) 
      
      billboarder(data = df_billb_data) %>%
        bb_donutchart() %>% 
        bb_legend(position = 'right') %>%
        bb_donut(title = input_title, width = 70) %>% 
        bb_colors_manual(
          setNames(c('#E58606','#5D69B1','#52BCA3','#99C945'), c(input_beneficiary_vector))
        )
    })
    
  })
}