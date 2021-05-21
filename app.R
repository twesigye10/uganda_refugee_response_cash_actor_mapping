
# load packages
library(shiny)
library(shinybusy)
library(sf)
library(tidyverse)
library(lubridate)
library(leaflet)
library(bslib)
library(ggreach)
library(highcharter)
library(billboarder)
library(glue)

# load scripts
list.files("modules") %>%
    purrr::map(~ source(paste0("modules/", .)))

reach_theme <- bs_theme(
    bg = ggreach::reach_cols("lightgrey"), 
    fg = ggreach::reach_cols("medred"),
    primary = ggreach::reach_cols("medbeige"),
    base_font = "Arial"
)

# Define UI for application -----------------------------------------------

ui <- navbarPage(
    title = div(class = "page-header", img(height = 40, width = 100, src="reach-logo-informing.png"), "Cash-Based Interventions. Uganda RRP") ,

    id = "tab_being_displayed",
        # CBI for Basic Needs -----------------------------------------------------
        tabPageCBIUI(
            "cbipagetab", "CBI for Basic Needs", "yearperiod", cbi_df_data$Year,
            "quarterperiod", "mapreset", "selecteddistrict", "hhreceivingcash",
            "plotcashquarter", "map", "psndata", "plotdeliverymechanism", "plotcashpartner"
        ),
        # Environmental Protection --------------------------------------------------------------
        tabPageUI(
            "eprpagetab", "Environmental Protection", "epr_yearperiod", epr_df_data$Year,
            "epr_quarterperiod", "epr_mapreset", "epr_selecteddistrict", "epr_hhreceivingcash",
            "epr_plotcashquarter", "epr_map", "epr_plotdeliverymechanism", "epr_plotcashpartner"
        ),
        # combine livelihood components
        tabPanel("Emergency Livelihood Support",
                 tabsetPanel(
                     id = "tabs",
                     # Short term Employment --------------------------------------------------------------
                     tabPageSEOUI(
                         "seopagetab", "Short term Employment", "seo_yearperiod", seo_df_data$Year,
                         "seo_quarterperiod", "seo_mapreset", "seo_selecteddistrict", "seo_hhreceivingcash",
                         "seo_plotcashquarter", "seo_map", "seotable", "seocvpdtable", "seo_plotdeliverymechanism", "seo_plotcashpartner"
                     ),
                     # Access to Productive Assets --------------------------------------------------------------
                     tabPageUI(
                         "apapagetab", "Access to Productive Assets", "apa_yearperiod", apa_df_data$Year,
                         "apa_quarterperiod", "apa_mapreset", "apa_selecteddistrict", "apa_hhreceivingcash",
                         "apa_plotcashquarter", "apa_map", "apa_plotdeliverymechanism", "apa_plotcashpartner"
                     ) 
                 )
        ),
        # Food Security -----------------------------------------------------------
        tabPageUI(
            "fspagetab", "Food Security", "fs_yearperiod", fs_df_data$Year,
            "fs_quarterperiod", "fs_mapreset", "fs_selecteddistrict", "fs_hhreceivingcash",
            "fs_plotcashquarter", "fs_map", "fs_plotdeliverymechanism", "fs_plotcashpartner"
        ),
        # combine WASH components
        tabPanel("WASH",
                 tabsetPanel(
                     id = "wash_tabs",
                     # Short term Employment --------------------------------------------------------------
                     tabPageUI(
                         "sspagetab", "CBI Approach in Sanitation Services", "ss_yearperiod", ss_df_data$Year,
                         "ss_quarterperiod", "ss_mapreset", "ss_selecteddistrict", "ss_hhreceivingcash",
                         "ss_plotcashquarter", "ss_map", "ss_plotdeliverymechanism", "ss_plotcashpartner"
                     ),
                     # Access to Productive Assets --------------------------------------------------------------
                     tabPageUI(
                         "wnpagetab", "CBI approach in WASH NFI", "wn_yearperiod", wn_df_data$Year,
                         "wn_quarterperiod", "wn_mapreset", "wn_selecteddistrict", "wn_hhreceivingcash",
                         "wn_plotcashquarter", "wn_map", "wn_plotdeliverymechanism", "wn_plotcashpartner"
                     ) 
                 )
    ),
    # tabPanel("About"),
    tabPanel("Read Me", readmeUI("rdmpagetab")),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    windowTitle = "Cash Based Interventions",
    header = busy_start_up(
        loader = spin_epic("orbit", color = "#FFF"),
        text = "Loading...",
        timeout = 1500,
        color = "#FFF",
        background = "#3E3E3F"
    ),
    
    theme= reach_theme
)

# Define server logic required --------------------------------------------

server <- function(input, output, session) {
    showModal(modalDialog(
        title = NULL,
        size = "l",
        easyClose = TRUE,
        fade = TRUE,
        div(class="read-me-div",
            readmeUI("rdmpagetab")
            
        )
    ))
    # Cash Based Intervention  -----------------------------------------------------------
    cbi_year <- cbiYearValueServer("cbipagetab")
    cbi_quarter <- cbiQuarterValueServer("cbipagetab")
    # default leaflet map  ------------------------------------------------------
    cbiDefaultMap("cbipagetab")
    # dynamic charts and map --------------------------------------------------
    observe({
        req(input$tab_being_displayed == "CBI for Basic Needs")
        # UI selectors to filter shape data
        df_by_district_cash_data <- reactive({filterCashData("cbipagetab", cbi_df_data, cbi_year(), Year, cbi_quarter(), Quarter )})
        df_shape_data <- dfShapeDefault("cbipagetab", df_shape, df_by_district_cash_data(), location_district, total_amount_of_cash_transfers, "location_district")
        
        df_point_data <- df_shape_data %>% filter(ADM2_EN %in% refugee_districts) %>% sf::st_transform(crs = 32636 ) %>%
            sf::st_centroid() %>% sf::st_transform(4326) %>%
            mutate( lat = sf::st_coordinates(.)[,1],  lon = sf::st_coordinates(.)[,2] )
        
        df_shape_data_map <- df_shape_data %>% filter(cash_transfers_by_district > 0)
        refugee_districts_cash <-  df_shape_data_map %>% pull(ADM2_EN)
        
        df_other_refugee_host_dist <- df_shape_data %>%
            filter(!(ADM2_EN %in% refugee_districts_cash) )%>% 
            mutate(col_legenend_factor = ifelse(ADM2_EN %in% refugee_districts, "Host", "None Host" ) )
        
        ## create all the charts
        dynamicMapLayer("cbipagetab", "map", df_shape_data_map)
        refugeeHostLayer("cbipagetab", "map",df_other_refugee_host_dist)
        dynamicMapLabels("cbipagetab", "map", df_point_data)
        
        cbiDataForPSN ("cbipagetab", df_by_district_cash_data())
        
        cbiDonutChartCashBeneficiary ("cbipagetab",
                                      df_by_district_cash_data(),
                                      select_beneficiary_type,
                                      total_amount_of_cash_transfers,
                                      "% of Total \nCash Transfer\n by Beneficiary Type",
                                      cbi_beneficiary_types)
        
        cbiLineChartTotalCashQuarter ("cbipagetab", df_by_district_cash_data(), 
                                      total_amount_of_cash_transfers, Year, Quarter, select_month, 
                                      Date, "select_month",  glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        
        cbiBarChartDeliveryMechanism ("cbipagetab", df_by_district_cash_data(),
                                      select_delivery_mechanism,
                                      total_amount_of_cash_transfers,
                                      glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        
        cbiBarChartCashByPartner ("cbipagetab", df_by_district_cash_data(), partner_name,
                                  total_amount_of_cash_transfers,
                                  glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        
        
    })
    
    # observe year change to update quarter -----------------------------------
    observe({
        if(cbi_year() != "All"){
            selected_year <- cbi_year()
            filter_cash_data_quarter <- filterYearForQuarters("cbipagetab", cbi_df_data, Year, selected_year ) 
            # update quarter selection
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(cbi_quarter() %in% available_quarter_choices){
                cbiUpdateQuarter("cbipagetab", available_quarter_choices, cbi_quarter())
            }else{
                cbiUpdateQuarter("cbipagetab", available_quarter_choices, "All")
            }
        }else{
            cbiUpdateQuarter("cbipagetab", "All", "All")
        }
    })
    
    # Charts listen to map click ----------------------------------------------
    observeEvent(cbiClickedDistrictValueServer("cbipagetab"),{
        click_district <- cbiClickedDistrictValueServer("cbipagetab")
        display_in_title <<- paste(" for ", stringr::str_to_title(click_district))
        
        filter_cash_data_based_on_map <- filterCashDataByDistrict("cbipagetab", cbi_df_data, location_district, click_district)
        
        cbiDataForPSN ("cbipagetab", filter_cash_data_based_on_map)
        
        # create all the charts
        cbiDonutChartCashBeneficiary ("cbipagetab",
                                      filter_cash_data_based_on_map,
                                      select_beneficiary_type,
                                      total_amount_of_cash_transfers,
                                      "% of Total \nCash Transfer\n by Beneficiary Type",
                                      cbi_beneficiary_types)
        cbiLineChartTotalCashQuarter ("cbipagetab", filter_cash_data_based_on_map, 
                                      total_amount_of_cash_transfers, Year, Quarter, select_month, 
                                      Date, "select_month",  glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        cbiBarChartDeliveryMechanism ("cbipagetab", filter_cash_data_based_on_map,
                                      select_delivery_mechanism,
                                      total_amount_of_cash_transfers,
                                      glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        cbiBarChartCashByPartner ("cbipagetab", filter_cash_data_based_on_map, partner_name,
                                  total_amount_of_cash_transfers,
                                  glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        cbiTextSelectedDistrict("cbipagetab", click_district)
        

        # update year selection
        filter_original_cash_data <- filter_cash_data_based_on_map
        available_year_choices <- unique(as.character(filter_original_cash_data$Year))
        if (cbi_year() %in% available_year_choices){
            cbiUpdateYear("cbipagetab", available_year_choices, cbi_year())
        }else{
            cbiUpdateYear("cbipagetab", available_year_choices, "All")
        }
        # update quarter selection based on year and district
        if(cbi_year() != "All"){
            selected_year <- cbi_year()
            filter_cash_data_quarter <- filterYearDistrictForQuarters ("cbipagetab", cbi_df_data, Year, selected_year,
                                                                       location_district, click_district )
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(cbi_quarter() %in% available_quarter_choices){
                cbiUpdateQuarter("cbipagetab", available_quarter_choices, cbi_quarter())
            }else{
                cbiUpdateQuarter("cbipagetab", available_quarter_choices, "All")
            }
        }
    })
    
    # Map reset button --------------------------------------------------------
    observeEvent(cbiResetMapServer("cbipagetab"),{
        
        display_in_title <<- " across all Districts"
        
        cbiUpdateYear("cbipagetab", unique(as.character(cbi_df_data$Year)), "All")
        cbiUpdateQuarter("cbipagetab", "All", "All")
        
        filter_cash_data_based_on_map <- cbi_df_data
        
        cbiDataForPSN ("cbipagetab", filter_cash_data_based_on_map)
        
        cbiDonutChartCashBeneficiary ("cbipagetab",
                                      filter_cash_data_based_on_map,
                                      select_beneficiary_type,
                                      total_amount_of_cash_transfers,
                                      "% of Total \nCash Transfer\n by Beneficiary Type",
                                      cbi_beneficiary_types)
        cbiLineChartTotalCashQuarter ("cbipagetab", filter_cash_data_based_on_map, 
                                      total_amount_of_cash_transfers, Year, Quarter, select_month, 
                                      Date, "select_month",  glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        cbiBarChartDeliveryMechanism ("cbipagetab", filter_cash_data_based_on_map,
                                      select_delivery_mechanism,
                                      total_amount_of_cash_transfers,
                                      glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        cbiBarChartCashByPartner ("cbipagetab", filter_cash_data_based_on_map, partner_name,
                                  total_amount_of_cash_transfers,
                                  glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        cbiTextSelectedDistrict("cbipagetab", "")
        
        
        
    })
    
    # Food Security -----------------------------------------------------------
    
    fs_year <- fsYearValueServer("fspagetab")
    fs_quarter <- fsQuarterValueServer("fspagetab")
    fsDefaultMap("fspagetab")
    # dynamic charts and map --------------------------------------------------
    observe({
        req(input$tab_being_displayed == "Food Security")
        # UI selectors to filter shape data
        df_by_district_cash_data <- reactive({filterCashData("fspagetab", fs_df_data, fs_year(), Year, fs_quarter(), Quarter )})
        df_shape_data <- dfShapeDefault("fspagetab", df_shape, df_by_district_cash_data(), location_district, fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers, "location_district")
        df_point_data <- df_shape_data %>%filter(ADM2_EN %in% refugee_districts) %>% sf::st_transform(crs = 32636 ) %>%
            sf::st_centroid() %>% sf::st_transform(4326) %>%
            mutate( lat = sf::st_coordinates(.)[,1],  lon = sf::st_coordinates(.)[,2] )
        
        df_shape_data_map <- df_shape_data %>% filter(cash_transfers_by_district > 0)
        refugee_districts_cash <-  df_shape_data_map %>% pull(ADM2_EN)
        
        df_other_refugee_host_dist <- df_shape_data %>%
            filter(!(ADM2_EN %in% refugee_districts_cash) )%>% 
            mutate(col_legenend_factor = ifelse(ADM2_EN %in% refugee_districts, "Host", "None Host" ) )
        
        ## create all the charts
        dynamicMapLayer("fspagetab", "fs_map", df_shape_data_map)
        refugeeHostLayer("fspagetab", "fs_map",df_other_refugee_host_dist)
        dynamicMapLabels("fspagetab", "fs_map", df_point_data)
        
        fsDonutChartCashBeneficiary ("fspagetab",
                                     df_by_district_cash_data(),
                                     select_beneficiary_type,
                                     fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers,
                                     "% of Total \nCash Transfer\n by Beneficiary Type",
                                     fs_beneficiary_types)
        fsLineChartTotalCashQuarter ("fspagetab", df_by_district_cash_data(), 
                                     fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers, Year, Quarter, select_quarter, 
                                     glue("Total cash distributed per month{display_in_title}  (UGX '000)"))
        fsBarChartDeliveryMechanism ("fspagetab", df_by_district_cash_data(),
                                     select_delivery_mechanism,
                                     fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers,
                                     glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        fsBarChartCashByPartner ("fspagetab", df_by_district_cash_data(), partner_name,
                                 fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers,
                                 glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        
    })
    
    # observe year change to update quarter -----------------------------------
    observe({
        if(fs_year() != "All"){
            selected_year <- fs_year()
            filter_cash_data_quarter <- filterYearForQuarters("fspagetab", fs_df_data, Year, selected_year ) 
            # update quarter selection
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(fs_quarter() %in% available_quarter_choices){
                fsUpdateQuarter("fspagetab", available_quarter_choices, fs_quarter())
            }else{
                fsUpdateQuarter("fspagetab", available_quarter_choices, "All")
            }
        }else{
            fsUpdateQuarter("fspagetab", "All", "All")
        }
    })
    
    # Charts listen to map click ----------------------------------------------
    observeEvent(fsClickedDistrictValueServer("fspagetab"),{
        click_district <- fsClickedDistrictValueServer("fspagetab")
        display_in_title <<- paste(" for ", stringr::str_to_title(click_district))
        filter_cash_data_based_on_map <- filterCashDataByDistrict("fspagetab", fs_df_data, location_district, click_district)
        # create all the charts
        fsDonutChartCashBeneficiary ("fspagetab",
                                     filter_cash_data_based_on_map,
                                     select_beneficiary_type,
                                     fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers,
                                     "% of Total \nCash Transfer\n by Beneficiary Type",
                                     fs_beneficiary_types)
        fsLineChartTotalCashQuarter ("fspagetab", filter_cash_data_based_on_map, 
                                     fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers, Year, Quarter, select_quarter, 
                                     glue("Total cash distributed per month{display_in_title}  (UGX '000)"))
        fsBarChartDeliveryMechanism ("fspagetab", filter_cash_data_based_on_map,
                                     select_delivery_mechanism,
                                     fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers,
                                     glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        fsBarChartCashByPartner ("fspagetab", filter_cash_data_based_on_map, partner_name,
                                 fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers,
                                 glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        fsTextSelectedDistrict("fspagetab", click_district)
        # update year selection
        filter_original_cash_data <- filter_cash_data_based_on_map
        available_year_choices <- unique(as.character(filter_original_cash_data$Year))
        if (fs_year() %in% available_year_choices){
            fsUpdateYear("fspagetab", available_year_choices, fs_year())
        }else{
            fsUpdateYear("fspagetab", available_year_choices, "All")
        }
        # update quarter selection based on year and district
        if(fs_year() != "All"){
            selected_year <- fs_year()
            filter_cash_data_quarter <- filterYearDistrictForQuarters ("fspagetab", fs_df_data, Year, selected_year,
                                                                       location_district, click_district )
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(fs_quarter() %in% available_quarter_choices){
                fsUpdateQuarter("fspagetab", available_quarter_choices, fs_quarter())
            }else{
                fsUpdateQuarter("fspagetab", available_quarter_choices, "All")
            }
        }
    })
    
    # Map reset button --------------------------------------------------------
    observeEvent(fsResetMapServer("fspagetab"),{
        
        display_in_title <<- " across all Districts"
        
        fsUpdateYear("fspagetab", unique(as.character(fs_df_data$Year)), "All")
        fsUpdateQuarter("fspagetab", "All", "All")
        
        filter_cash_data_based_on_map <- fs_df_data
        
        fsDonutChartCashBeneficiary ("fspagetab",
                                     filter_cash_data_based_on_map,
                                     select_beneficiary_type,
                                     fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers,
                                     "% of Total \nCash Transfer\n by Beneficiary Type",
                                     fs_beneficiary_types)
        fsLineChartTotalCashQuarter ("fspagetab", filter_cash_data_based_on_map, 
                                     fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers, Year, Quarter, select_quarter, 
                                     glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        fsBarChartDeliveryMechanism ("fspagetab", filter_cash_data_based_on_map,
                                     select_delivery_mechanism,
                                     fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers,
                                     glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        fsBarChartCashByPartner ("fspagetab", filter_cash_data_based_on_map, partner_name,
                                 fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers,
                                 glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        fsTextSelectedDistrict("fspagetab", "")
    })
    
    
    
    
    # Short term Employment -----------------------------------------------------------
    
    seo_year <- seoYearValueServer("seopagetab")
    seo_quarter <- seoQuarterValueServer("seopagetab")
    seoDefaultMap("seopagetab")
    # dynamic charts and map --------------------------------------------------
    observe({
        req(input$tab_being_displayed == "Emergency Livelihood Support")
        req(input$tabs == "Short term Employment")
        # UI selectors to filter shape data
        df_by_district_cash_data <- reactive({filterCashData("seopagetab", seo_df_data, seo_year(), Year, seo_quarter(), Quarter )})
        df_shape_data <- dfShapeDefault("seopagetab", df_shape, df_by_district_cash_data(), location_district, total_cash_value_of_cash_for_work_ugx, "location_district")
        df_point_data <- df_shape_data %>% filter(ADM2_EN %in% refugee_districts) %>% sf::st_transform(crs = 32636 ) %>%
            sf::st_centroid() %>% sf::st_transform(4326) %>%
            mutate( lat = sf::st_coordinates(.)[,1],  lon = sf::st_coordinates(.)[,2] )
       
        df_shape_data_map <- df_shape_data %>% filter(cash_transfers_by_district > 0)
        refugee_districts_cash <-  df_shape_data_map %>% pull(ADM2_EN)
        
        df_other_refugee_host_dist <- df_shape_data %>%
            filter(!(ADM2_EN %in% refugee_districts_cash) )%>% 
            mutate(col_legenend_factor = ifelse(ADM2_EN %in% refugee_districts, "Host", "None Host" ) )
        
        ## create all the charts
        dynamicMapLayer("seopagetab", "seo_map", df_shape_data_map)
        refugeeHostLayer("seopagetab", "seo_map",df_other_refugee_host_dist)
        dynamicMapLabels("seopagetab", "seo_map", df_point_data)
        
        seoTableForEmploy("seopagetab", df_by_district_cash_data())
        
        seoTableForCVPD("seopagetab", df_by_district_cash_data())
        
        seoDonutChartCashBeneficiary ("seopagetab",
                                      df_by_district_cash_data(),
                                      select_beneficiary_type,
                                      total_cash_value_of_cash_for_work_ugx,
                                      "% of Total \nCash Transfer\n by Beneficiary Type",
                                      seo_beneficiary_types)
        seoLineChartTotalCashQuarter ("seopagetab", df_by_district_cash_data(), 
                                      total_cash_value_of_cash_for_work_ugx, Year, Quarter, select_quarter, 
                                      glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        seoBarChartDeliveryMechanism ("seopagetab", df_by_district_cash_data(),
                                      delivery_mechanism,
                                      total_cash_value_of_cash_for_work_ugx,
                                      glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        seoBarChartCashByPartner ("seopagetab", df_by_district_cash_data(), partner_name,
                                  total_cash_value_of_cash_for_work_ugx,
                                  glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        
    })
    
    # observe year change to update quarter -----------------------------------
    observe({
        if(seo_year() != "All"){
            selected_year <- seo_year()
            filter_cash_data_quarter <- filterYearForQuarters("seopagetab", seo_df_data, Year, selected_year ) 
            # update quarter selection
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(seo_quarter() %in% available_quarter_choices){
                seoUpdateQuarter("seopagetab", available_quarter_choices, seo_quarter())
            }else{
                seoUpdateQuarter("seopagetab", available_quarter_choices, "All")
            }
        }else{
            seoUpdateQuarter("seopagetab", "All", "All")
        }
    })
    
    # Charts listen to map click ----------------------------------------------
    observeEvent(seoClickedDistrictValueServer("seopagetab"),{
        click_district <- seoClickedDistrictValueServer("seopagetab")
        display_in_title <<- paste(" for ", stringr::str_to_title(click_district))
        filter_cash_data_based_on_map <- filterCashDataByDistrict("seopagetab", seo_df_data, location_district, click_district)
        # create all the charts
        seoTableForEmploy("seopagetab", filter_cash_data_based_on_map)
        
        seoTableForCVPD("seopagetab", filter_cash_data_based_on_map)
        
        seoDonutChartCashBeneficiary ("seopagetab",
                                      filter_cash_data_based_on_map,
                                      select_beneficiary_type,
                                      total_cash_value_of_cash_for_work_ugx,
                                      "% of Total \nCash Transfer\n by Beneficiary Type",
                                      seo_beneficiary_types)
        seoLineChartTotalCashQuarter ("seopagetab", filter_cash_data_based_on_map, 
                                      total_cash_value_of_cash_for_work_ugx, Year, Quarter, select_quarter, 
                                      glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        seoBarChartDeliveryMechanism ("seopagetab", filter_cash_data_based_on_map,
                                      delivery_mechanism,
                                      total_cash_value_of_cash_for_work_ugx,
                                      glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        seoBarChartCashByPartner ("seopagetab", filter_cash_data_based_on_map, partner_name,
                                  total_cash_value_of_cash_for_work_ugx,
                                  glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        seoTextSelectedDistrict("seopagetab", click_district)
        
        
        # update year selection
        filter_original_cash_data <- filter_cash_data_based_on_map
        available_year_choices <- unique(as.character(filter_original_cash_data$Year))
        if (seo_year() %in% available_year_choices){
            seoUpdateYear("seopagetab", available_year_choices, seo_year())
        }else{
            seoUpdateYear("seopagetab", available_year_choices, "All")
        }
        # update quarter selection based on year and district
        if(seo_year() != "All"){
            selected_year <- seo_year()
            filter_cash_data_quarter <- filterYearDistrictForQuarters ("seopagetab", seo_df_data, Year, selected_year,
                                                                       location_district, click_district )
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(seo_quarter() %in% available_quarter_choices){
                seoUpdateQuarter("seopagetab", available_quarter_choices, seo_quarter())
            }else{
                seoUpdateQuarter("seopagetab", available_quarter_choices, "All")
            }
        }
    })
    
    # Map reset button --------------------------------------------------------
    observeEvent(seoResetMapServer("seopagetab"),{
        display_in_title <<- " across all Districts"
        
        seoUpdateYear("seopagetab", unique(as.character(seo_df_data$Year)), "All")
        seoUpdateQuarter("seopagetab", "All", "All")
        
        filter_cash_data_based_on_map <- seo_df_data
        
        seoTableForEmploy("seopagetab", filter_cash_data_based_on_map)
        
        seoTableForCVPD("seopagetab", filter_cash_data_based_on_map)
        
        seoDonutChartCashBeneficiary ("seopagetab",
                                      filter_cash_data_based_on_map,
                                      select_beneficiary_type,
                                      total_cash_value_of_cash_for_work_ugx,
                                      "% of Total \nCash Transfer\n by Beneficiary Type",
                                      seo_beneficiary_types)
        seoLineChartTotalCashQuarter ("seopagetab", filter_cash_data_based_on_map, 
                                      total_cash_value_of_cash_for_work_ugx, Year, Quarter, select_quarter, 
                                      glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        seoBarChartDeliveryMechanism ("seopagetab", filter_cash_data_based_on_map,
                                      delivery_mechanism,
                                      total_cash_value_of_cash_for_work_ugx,
                                      glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        seoBarChartCashByPartner ("seopagetab", filter_cash_data_based_on_map, partner_name,
                                  total_cash_value_of_cash_for_work_ugx,
                                  glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        seoTextSelectedDistrict("seopagetab", "")
        
        
    })
    
    
    # Environmental Protection Restoration ------------------------------------
    
    epr_year <- eprYearValueServer("eprpagetab")
    epr_quarter <- eprQuarterValueServer("eprpagetab")
    eprDefaultMap("eprpagetab")
    # dynamic charts and map --------------------------------------------------
    observe({
        req(input$tab_being_displayed == "Environmental Protection")
        # UI selectors to filter shape data
        df_by_district_cash_data <- reactive({filterCashData("eprpagetab", epr_df_data, epr_year(), Year, epr_quarter(), Quarter )})
        df_shape_data <- dfShapeDefault("eprpagetab", df_shape, df_by_district_cash_data(), location_district, total_cash_value_of_cash_for_work_ugx, "location_district")
        df_point_data <- df_shape_data %>% filter(ADM2_EN %in% refugee_districts) %>% sf::st_transform(crs = 32636 ) %>%
            sf::st_centroid() %>% sf::st_transform(4326) %>%
            mutate( lat = sf::st_coordinates(.)[,1],  lon = sf::st_coordinates(.)[,2] )
        
        df_shape_data_map <- df_shape_data %>% filter(cash_transfers_by_district > 0)
        refugee_districts_cash <-  df_shape_data_map %>% pull(ADM2_EN)
        
        df_other_refugee_host_dist <- df_shape_data %>%
            filter(!(ADM2_EN %in% refugee_districts_cash) )%>% 
            mutate(col_legenend_factor = ifelse(ADM2_EN %in% refugee_districts, "Host", "None Host" ) )
        
        ## create all the charts
        dynamicMapLayer("eprpagetab", "epr_map", df_shape_data_map)
        refugeeHostLayer("eprpagetab", "epr_map",df_other_refugee_host_dist)
        dynamicMapLabels("eprpagetab", "epr_map", df_point_data)
        
        eprDonutChartCashBeneficiary ("eprpagetab",
                                      df_by_district_cash_data(),
                                      select_beneficiary_type,
                                      total_cash_value_of_cash_for_work_ugx,
                                      "% of Total \nCash Transfer\n by Beneficiary Type",
                                      epr_beneficiary_types)
        eprLineChartTotalCashQuarter ("eprpagetab", df_by_district_cash_data(), 
                                      total_cash_value_of_cash_for_work_ugx, Year, Quarter, select_quarter, 
                                      glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        eprBarChartDeliveryMechanism ("eprpagetab", df_by_district_cash_data(),
                                      delivery_mechanism,
                                      total_cash_value_of_cash_for_work_ugx,
                                      glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        eprBarChartCashByPartner ("eprpagetab", df_by_district_cash_data(), partner_name,
                                  total_cash_value_of_cash_for_work_ugx,
                                  glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        
    })
    
    # observe year change to update quarter -----------------------------------
    observe({
        if(epr_year() != "All"){
            selected_year <- epr_year()
            filter_cash_data_quarter <- filterYearForQuarters("eprpagetab", epr_df_data, Year, selected_year ) 
            # update quarter selection
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(epr_quarter() %in% available_quarter_choices){
                eprUpdateQuarter("eprpagetab", available_quarter_choices, epr_quarter())
            }else{
                eprUpdateQuarter("eprpagetab", available_quarter_choices, "All")
            }
        }else{
            eprUpdateQuarter("eprpagetab", "All", "All")
        }
    })
    
    # Charts listen to map click ----------------------------------------------
    observeEvent(eprClickedDistrictValueServer("eprpagetab"),{
        click_district <- eprClickedDistrictValueServer("eprpagetab")
        display_in_title <<- paste(" for ", stringr::str_to_title(click_district))
        filter_cash_data_based_on_map <- filterCashDataByDistrict("eprpagetab", epr_df_data, location_district, click_district)
        # create all the charts
        eprDonutChartCashBeneficiary ("eprpagetab",
                                      filter_cash_data_based_on_map,
                                      select_beneficiary_type,
                                      total_cash_value_of_cash_for_work_ugx,
                                      "% of Total \nCash Transfer\n by Beneficiary Type",
                                      epr_beneficiary_types)
        eprLineChartTotalCashQuarter ("eprpagetab", filter_cash_data_based_on_map, 
                                      total_cash_value_of_cash_for_work_ugx, Year, Quarter, select_quarter, 
                                      glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        eprBarChartDeliveryMechanism ("eprpagetab", filter_cash_data_based_on_map,
                                      delivery_mechanism,
                                      total_cash_value_of_cash_for_work_ugx,
                                      glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        eprBarChartCashByPartner ("eprpagetab", filter_cash_data_based_on_map, partner_name,
                                  total_cash_value_of_cash_for_work_ugx,
                                  glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        eprTextSelectedDistrict("eprpagetab", click_district)
        # update year selection
        filter_original_cash_data <- filter_cash_data_based_on_map
        available_year_choices <- unique(as.character(filter_original_cash_data$Year))
        if (epr_year() %in% available_year_choices){
            eprUpdateYear("eprpagetab", available_year_choices, epr_year())
        }else{
            eprUpdateYear("eprpagetab", available_year_choices, "All")
        }
        # update quarter selection based on year and district
        if(epr_year() != "All"){
            selected_year <- epr_year()
            filter_cash_data_quarter <- filterYearDistrictForQuarters ("eprpagetab", epr_df_data, Year, selected_year,
                                                                       location_district, click_district )
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(epr_quarter() %in% available_quarter_choices){
                eprUpdateQuarter("eprpagetab", available_quarter_choices, epr_quarter())
            }else{
                eprUpdateQuarter("eprpagetab", available_quarter_choices, "All")
            }
        }
    })
    
    # Map reset button --------------------------------------------------------
    observeEvent(eprResetMapServer("eprpagetab"),{
        display_in_title <<- " across all Districts"
        
        eprUpdateYear("eprpagetab", unique(as.character(epr_df_data$Year)), "All")
        eprUpdateQuarter("eprpagetab", "All", "All")
        
        filter_cash_data_based_on_map <- epr_df_data
        
        eprDonutChartCashBeneficiary ("eprpagetab",
                                      filter_cash_data_based_on_map,
                                      select_beneficiary_type,
                                      total_cash_value_of_cash_for_work_ugx,
                                      "% of Total \nCash Transfer\n by Beneficiary Type",
                                      epr_beneficiary_types)
        eprLineChartTotalCashQuarter ("eprpagetab", filter_cash_data_based_on_map, 
                                      total_cash_value_of_cash_for_work_ugx, Year, Quarter, select_quarter, 
                                      glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        eprBarChartDeliveryMechanism ("eprpagetab", filter_cash_data_based_on_map,
                                      delivery_mechanism,
                                      total_cash_value_of_cash_for_work_ugx,
                                      glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        eprBarChartCashByPartner ("eprpagetab", filter_cash_data_based_on_map, partner_name,
                                  total_cash_value_of_cash_for_work_ugx,
                                  glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        eprTextSelectedDistrict("eprpagetab", "")
        
    })
    
    
    # Access to Productive Assets ---------------------------------------------
    
    apa_year <- apaYearValueServer("apapagetab")
    apa_quarter <- apaQuarterValueServer("apapagetab")
    apaDefaultMap("apapagetab")
    # dynamic charts and map --------------------------------------------------
    observe({
        req(input$tab_being_displayed == "Emergency Livelihood Support")
        req(input$tabs == "Access to Productive Assets")
        # UI selectors to filter shape data
        df_by_district_cash_data <- reactive({filterCashData("apapagetab", apa_df_data, apa_year(), Year, apa_quarter(), Quarter )})
        df_shape_data <- dfShapeDefault("apapagetab", df_shape, df_by_district_cash_data(), location_district, total_cash_value_of_grants_distributed_for_productive_assets_ugx, "location_district")
        df_point_data <- df_shape_data %>% filter(ADM2_EN %in% refugee_districts) %>% sf::st_transform(crs = 32636 ) %>%
            sf::st_centroid() %>% sf::st_transform(4326) %>%
            mutate( lat = sf::st_coordinates(.)[,1],  lon = sf::st_coordinates(.)[,2] )
        
        df_shape_data_map <- df_shape_data %>% filter(cash_transfers_by_district > 0)
        refugee_districts_cash <-  df_shape_data_map %>% pull(ADM2_EN)
        
        df_other_refugee_host_dist <- df_shape_data %>%
            filter(!(ADM2_EN %in% refugee_districts_cash) )%>% 
            mutate(col_legenend_factor = ifelse(ADM2_EN %in% refugee_districts, "Host", "None Host" ) )
        
        ## create all the charts
        dynamicMapLayer("apapagetab", "apa_map", df_shape_data_map)
        refugeeHostLayer("apapagetab", "apa_map",df_other_refugee_host_dist)
        dynamicMapLabels("apapagetab", "apa_map", df_point_data)
        
        apaDonutChartCashBeneficiary ("apapagetab",
                                      df_by_district_cash_data(),
                                      select_beneficiary_type,
                                      total_cash_value_of_grants_distributed_for_productive_assets_ugx,
                                      "% of Total \nCash Transfer\n by Beneficiary Type",
                                      apa_beneficiary_types)
        apaLineChartTotalCashQuarter ("apapagetab", df_by_district_cash_data(), 
                                      total_cash_value_of_grants_distributed_for_productive_assets_ugx, Year, Quarter, select_quarter, 
                                      glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        apaBarChartDeliveryMechanism ("apapagetab", df_by_district_cash_data(),
                                      delivery_mechanism,
                                      total_cash_value_of_grants_distributed_for_productive_assets_ugx,
                                      glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        apaBarChartCashByPartner ("apapagetab", df_by_district_cash_data(), partner_name,
                                  total_cash_value_of_grants_distributed_for_productive_assets_ugx,
                                  glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        
    })
    
    # observe year change to update quarter -----------------------------------
    observe({
        if(apa_year() != "All"){
            selected_year <- apa_year()
            filter_cash_data_quarter <- filterYearForQuarters("apapagetab", apa_df_data, Year, selected_year ) 
            # update quarter selection
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(apa_quarter() %in% available_quarter_choices){
                apaUpdateQuarter("apapagetab", available_quarter_choices, apa_quarter())
            }else{
                apaUpdateQuarter("apapagetab", available_quarter_choices, "All")
            }
        }else{
            apaUpdateQuarter("apapagetab", "All", "All")
        }
    })
    
    # Charts listen to map click ----------------------------------------------
    observeEvent(apaClickedDistrictValueServer("apapagetab"),{
        click_district <- apaClickedDistrictValueServer("apapagetab")
        display_in_title <<- paste(" for ", stringr::str_to_title(click_district))
        filter_cash_data_based_on_map <- filterCashDataByDistrict("apapagetab", apa_df_data, location_district, click_district)
        # create all the charts
        apaDonutChartCashBeneficiary ("apapagetab",
                                      filter_cash_data_based_on_map,
                                      select_beneficiary_type,
                                      total_cash_value_of_grants_distributed_for_productive_assets_ugx,
                                      "% of Total \nCash Transfer\n by Beneficiary Type",
                                      apa_beneficiary_types)
        apaLineChartTotalCashQuarter ("apapagetab", filter_cash_data_based_on_map, 
                                      total_cash_value_of_grants_distributed_for_productive_assets_ugx, Year, Quarter, select_quarter, 
                                      glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        apaBarChartDeliveryMechanism ("apapagetab", filter_cash_data_based_on_map,
                                      delivery_mechanism,
                                      total_cash_value_of_grants_distributed_for_productive_assets_ugx,
                                      glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        apaBarChartCashByPartner ("apapagetab", filter_cash_data_based_on_map, partner_name,
                                  total_cash_value_of_grants_distributed_for_productive_assets_ugx,
                                  glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        apaTextSelectedDistrict("apapagetab", click_district)
        # update year selection
        filter_original_cash_data <- filter_cash_data_based_on_map
        available_year_choices <- unique(as.character(filter_original_cash_data$Year))
        if (apa_year() %in% available_year_choices){
            apaUpdateYear("apapagetab", available_year_choices, apa_year())
        }else{
            apaUpdateYear("apapagetab", available_year_choices, "All")
        }
        # update quarter selection based on year and district
        if(apa_year() != "All"){
            selected_year <- apa_year()
            filter_cash_data_quarter <- filterYearDistrictForQuarters ("apapagetab", apa_df_data, Year, selected_year,
                                                                       location_district, click_district )
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(apa_quarter() %in% available_quarter_choices){
                apaUpdateQuarter("apapagetab", available_quarter_choices, apa_quarter())
            }else{
                apaUpdateQuarter("apapagetab", available_quarter_choices, "All")
            }
        }
    })
    
    # Map reset button --------------------------------------------------------
    observeEvent(apaResetMapServer("apapagetab"),{
        display_in_title <<- " across all Districts"
        
        apaUpdateYear("apapagetab", unique(as.character(apa_df_data$Year)), "All")
        apaUpdateQuarter("apapagetab", "All", "All")
        
        filter_cash_data_based_on_map <- apa_df_data
        
        apaDonutChartCashBeneficiary ("apapagetab",
                                      filter_cash_data_based_on_map,
                                      select_beneficiary_type,
                                      total_cash_value_of_grants_distributed_for_productive_assets_ugx,
                                      "% of Total \nCash Transfer\n by Beneficiary Type",
                                      apa_beneficiary_types)
        apaLineChartTotalCashQuarter ("apapagetab", filter_cash_data_based_on_map, 
                                      total_cash_value_of_grants_distributed_for_productive_assets_ugx, Year, Quarter, select_quarter, 
                                      glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        apaBarChartDeliveryMechanism ("apapagetab", filter_cash_data_based_on_map,
                                      delivery_mechanism,
                                      total_cash_value_of_grants_distributed_for_productive_assets_ugx,
                                      glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        apaBarChartCashByPartner ("apapagetab", filter_cash_data_based_on_map, partner_name,
                                  total_cash_value_of_grants_distributed_for_productive_assets_ugx,
                                  glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        apaTextSelectedDistrict("apapagetab", "")
        
    })
    
    # CBI Approach in Sanitation Services -----------------------------------------------------------
    
    ss_year <- ssYearValueServer("sspagetab")
    ss_quarter <- ssQuarterValueServer("sspagetab")
    ssDefaultMap("sspagetab")
    # dynamic charts and map --------------------------------------------------
    observe({
        req(input$tab_being_displayed == "WASH")
        req(input$wash_tabs == "CBI Approach in Sanitation Services")
        # UI selectors to filter shape data
        df_by_district_cash_data <- reactive({filterCashData("sspagetab", ss_df_data, ss_year(), Year, ss_quarter(), Quarter )})
        df_shape_data <- dfShapeDefault("sspagetab", df_shape, df_by_district_cash_data(), location_district, total_cash_value_of_cash_grants_ugx, "location_district")
        df_point_data <- df_shape_data %>% filter(ADM2_EN %in% refugee_districts) %>% sf::st_transform(crs = 32636 ) %>%
            sf::st_centroid() %>% sf::st_transform(4326) %>%
            mutate( lat = sf::st_coordinates(.)[,1],  lon = sf::st_coordinates(.)[,2] )
        
        df_shape_data_map <- df_shape_data %>% filter(cash_transfers_by_district > 0)
        refugee_districts_cash <-  df_shape_data_map %>% pull(ADM2_EN)
        
        df_other_refugee_host_dist <- df_shape_data %>%
            filter(!(ADM2_EN %in% refugee_districts_cash) )%>% 
            mutate(col_legenend_factor = ifelse(ADM2_EN %in% refugee_districts, "Host", "None Host" ) )
        
        ## create all the charts
        dynamicMapLayer("sspagetab", "ss_map", df_shape_data_map)
        refugeeHostLayer("sspagetab", "ss_map",df_other_refugee_host_dist)
        dynamicMapLabels("sspagetab", "ss_map", df_point_data)
        
        ssDonutChartCashBeneficiary ("sspagetab",
                                     df_by_district_cash_data(),
                                     select_beneficiary_type,
                                     total_cash_value_of_cash_grants_ugx,
                                     "% of Total \nCash Transfer\n by Beneficiary Type",
                                     ss_beneficiary_types)
        ssLineChartTotalCashQuarter ("sspagetab", df_by_district_cash_data(), 
                                     total_cash_value_of_cash_grants_ugx, Year, Quarter, select_quarter, 
                                     glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        ssBarChartDeliveryMechanism ("sspagetab", df_by_district_cash_data(),
                                     delivery_mechanism,
                                     total_cash_value_of_cash_grants_ugx,
                                     glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        ssBarChartCashByPartner ("sspagetab", df_by_district_cash_data(), partner_name,
                                 total_cash_value_of_cash_grants_ugx,
                                 glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        
    })
    
    # observe year change to update quarter -----------------------------------
    observe({
        if(ss_year() != "All"){
            selected_year <- ss_year()
            filter_cash_data_quarter <- filterYearForQuarters("sspagetab", ss_df_data, Year, selected_year ) 
            # update quarter selection
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(ss_quarter() %in% available_quarter_choices){
                ssUpdateQuarter("sspagetab", available_quarter_choices, ss_quarter())
            }else{
                ssUpdateQuarter("sspagetab", available_quarter_choices, "All")
            }
        }else{
            ssUpdateQuarter("sspagetab", "All", "All")
        }
    })
    
    # Charts listen to map click ----------------------------------------------
    observeEvent(ssClickedDistrictValueServer("sspagetab"),{
        click_district <- ssClickedDistrictValueServer("sspagetab")
        display_in_title <<- paste(" for ", stringr::str_to_title(click_district))
        filter_cash_data_based_on_map <- filterCashDataByDistrict("sspagetab", ss_df_data, location_district, click_district)
        # create all the charts
        ssDonutChartCashBeneficiary ("sspagetab",
                                     filter_cash_data_based_on_map,
                                     select_beneficiary_type,
                                     total_cash_value_of_cash_grants_ugx,
                                     "% of Total \nCash Transfer\n by Beneficiary Type",
                                     ss_beneficiary_types)
        ssLineChartTotalCashQuarter ("sspagetab", filter_cash_data_based_on_map, 
                                     total_cash_value_of_cash_grants_ugx, Year, Quarter, select_quarter, 
                                     glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        ssBarChartDeliveryMechanism ("sspagetab", filter_cash_data_based_on_map,
                                     delivery_mechanism,
                                     total_cash_value_of_cash_grants_ugx,
                                     glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        ssBarChartCashByPartner ("sspagetab", filter_cash_data_based_on_map, partner_name,
                                 total_cash_value_of_cash_grants_ugx,
                                 glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        ssTextSelectedDistrict("sspagetab", click_district)
        # update year selection
        filter_original_cash_data <- filter_cash_data_based_on_map
        available_year_choices <- unique(as.character(filter_original_cash_data$Year))
        if (ss_year() %in% available_year_choices){
            ssUpdateYear("sspagetab", available_year_choices, ss_year())
        }else{
            ssUpdateYear("sspagetab", available_year_choices, "All")
        }
        # update quarter selection based on year and district
        if(ss_year() != "All"){
            selected_year <- ss_year()
            filter_cash_data_quarter <- filterYearDistrictForQuarters ("sspagetab", ss_df_data, Year, selected_year,
                                                                       location_district, click_district )
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(ss_quarter() %in% available_quarter_choices){
                ssUpdateQuarter("sspagetab", available_quarter_choices, ss_quarter())
            }else{
                ssUpdateQuarter("sspagetab", available_quarter_choices, "All")
            }
        }
    })
    
    # Map reset button --------------------------------------------------------
    observeEvent(ssResetMapServer("sspagetab"),{
        
        display_in_title <<- " across all Districts"
        
        ssUpdateYear("sspagetab", unique(as.character(ss_df_data$Year)), "All")
        ssUpdateQuarter("sspagetab", "All", "All")
        
        filter_cash_data_based_on_map <- ss_df_data
        
        ssDonutChartCashBeneficiary ("sspagetab",
                                     filter_cash_data_based_on_map,
                                     select_beneficiary_type,
                                     total_cash_value_of_cash_grants_ugx,
                                     "% of Total \nCash Transfer\n by Beneficiary Type",
                                     ss_beneficiary_types)
        ssLineChartTotalCashQuarter ("sspagetab", filter_cash_data_based_on_map, 
                                     total_cash_value_of_cash_grants_ugx, Year, Quarter, select_quarter, 
                                     glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        ssBarChartDeliveryMechanism ("sspagetab", filter_cash_data_based_on_map,
                                     delivery_mechanism,
                                     total_cash_value_of_cash_grants_ugx,
                                     glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        ssBarChartCashByPartner ("sspagetab", filter_cash_data_based_on_map, partner_name,
                                 total_cash_value_of_cash_grants_ugx,
                                 glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        ssTextSelectedDistrict("sspagetab", "")
    })
    
    # CBI approach in WASH NFI -----------------------------------------------------------
    
    wn_year <- wnYearValueServer("wnpagetab")
    wn_quarter <- wnQuarterValueServer("wnpagetab")
    wnDefaultMap("wnpagetab")
    # dynamic charts and map --------------------------------------------------
    observe({
        req(input$tab_being_displayed == "WASH")
        req(input$wash_tabs == "CBI approach in WASH NFI")
        # UI selectors to filter shape data
        df_by_district_cash_data <- reactive({filterCashData("wnpagetab", wn_df_data, wn_year(), Year, wn_quarter(), Quarter )})
        df_shape_data <- dfShapeDefault("wnpagetab", df_shape, df_by_district_cash_data(), location_district, total_cash_value_of_cash_grants_ugx, "location_district")
        df_point_data <- df_shape_data %>% filter(ADM2_EN %in% refugee_districts) %>% sf::st_transform(crs = 32636 ) %>%
            sf::st_centroid() %>% sf::st_transform(4326) %>%
            mutate( lat = sf::st_coordinates(.)[,1],  lon = sf::st_coordinates(.)[,2] )
        
        df_shape_data_map <- df_shape_data %>% filter(cash_transfers_by_district > 0)
        refugee_districts_cash <-  df_shape_data_map %>% pull(ADM2_EN)
        
        df_other_refugee_host_dist <- df_shape_data %>%
            filter(!(ADM2_EN %in% refugee_districts_cash) )%>% 
            mutate(col_legenend_factor = ifelse(ADM2_EN %in% refugee_districts, "Host", "None Host" ) )
        
        ## create all the charts
        dynamicMapLayer("wnpagetab", "wn_map", df_shape_data_map)
        refugeeHostLayer("wnpagetab", "wn_map",df_other_refugee_host_dist)
        dynamicMapLabels("wnpagetab", "wn_map", df_point_data)
        
        wnDonutChartCashBeneficiary ("wnpagetab",
                                     df_by_district_cash_data(),
                                     select_beneficiary_type,
                                     total_cash_value_of_cash_grants_ugx,
                                     "% of Total \nCash Transfer\n by Beneficiary Type",
                                     wn_beneficiary_types)
        wnLineChartTotalCashQuarter ("wnpagetab", df_by_district_cash_data(), 
                                     total_cash_value_of_cash_grants_ugx, Year, Quarter, select_quarter, 
                                     glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        wnBarChartDeliveryMechanism ("wnpagetab", df_by_district_cash_data(),
                                     delivery_mechanism,
                                     total_cash_value_of_cash_grants_ugx,
                                     glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        wnBarChartCashByPartner ("wnpagetab", df_by_district_cash_data(), partner_name,
                                 total_cash_value_of_cash_grants_ugx,
                                 glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        
    })
    
    # observe year change to update quarter -----------------------------------
    observe({
        if(wn_year() != "All"){
            selected_year <- wn_year()
            filter_cash_data_quarter <- filterYearForQuarters("wnpagetab", wn_df_data, Year, selected_year ) 
            # update quarter selection
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(wn_quarter() %in% available_quarter_choices){
                wnUpdateQuarter("wnpagetab", available_quarter_choices, wn_quarter())
            }else{
                wnUpdateQuarter("wnpagetab", available_quarter_choices, "All")
            }
        }else{
            wnUpdateQuarter("wnpagetab", "All", "All")
        }
    })
    
    # Charts listen to map click ----------------------------------------------
    observeEvent(wnClickedDistrictValueServer("wnpagetab"),{
        click_district <- wnClickedDistrictValueServer("wnpagetab")
        display_in_title <<- paste(" for ", stringr::str_to_title(click_district))
        filter_cash_data_based_on_map <- filterCashDataByDistrict("wnpagetab", wn_df_data, location_district, click_district)
        # create all the charts
        wnDonutChartCashBeneficiary ("wnpagetab",
                                     filter_cash_data_based_on_map,
                                     select_beneficiary_type,
                                     total_cash_value_of_cash_grants_ugx,
                                     "% of Total \nCash Transfer\n by Beneficiary Type",
                                     wn_beneficiary_types)
        wnLineChartTotalCashQuarter ("wnpagetab", filter_cash_data_based_on_map, 
                                     total_cash_value_of_cash_grants_ugx, Year, Quarter, select_quarter, 
                                     glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        wnBarChartDeliveryMechanism ("wnpagetab", filter_cash_data_based_on_map,
                                     delivery_mechanism,
                                     total_cash_value_of_cash_grants_ugx,
                                     glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        wnBarChartCashByPartner ("wnpagetab", filter_cash_data_based_on_map, partner_name,
                                 total_cash_value_of_cash_grants_ugx,
                                 glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        wnTextSelectedDistrict("wnpagetab", click_district)
        # update year selection
        filter_original_cash_data <- filter_cash_data_based_on_map
        available_year_choices <- unique(as.character(filter_original_cash_data$Year))
        if (wn_year() %in% available_year_choices){
            wnUpdateYear("wnpagetab", available_year_choices, wn_year())
        }else{
            wnUpdateYear("wnpagetab", available_year_choices, "All")
        }
        # update quarter selection based on year and district
        if(wn_year() != "All"){
            selected_year <- wn_year()
            filter_cash_data_quarter <- filterYearDistrictForQuarters ("wnpagetab", wn_df_data, Year, selected_year,
                                                                       location_district, click_district )
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(wn_quarter() %in% available_quarter_choices){
                wnUpdateQuarter("wnpagetab", available_quarter_choices, wn_quarter())
            }else{
                wnUpdateQuarter("wnpagetab", available_quarter_choices, "All")
            }
        }
    })
    
    # Map reset button --------------------------------------------------------
    observeEvent(wnResetMapServer("wnpagetab"),{
        
        display_in_title <<- " across all Districts"
        
        wnUpdateYear("wnpagetab", unique(as.character(wn_df_data$Year)), "All")
        wnUpdateQuarter("wnpagetab", "All", "All")
        
        filter_cash_data_based_on_map <- wn_df_data
        
        wnDonutChartCashBeneficiary ("wnpagetab",
                                     filter_cash_data_based_on_map,
                                     select_beneficiary_type,
                                     total_cash_value_of_cash_grants_ugx,
                                     "% of Total \nCash Transfer\n by Beneficiary Type",
                                     wn_beneficiary_types)
        wnLineChartTotalCashQuarter ("wnpagetab", filter_cash_data_based_on_map, 
                                     total_cash_value_of_cash_grants_ugx, Year, Quarter, select_quarter, 
                                     glue("Total cash distributed per month{display_in_title} (UGX '000)"))
        wnBarChartDeliveryMechanism ("wnpagetab", filter_cash_data_based_on_map,
                                     delivery_mechanism,
                                     total_cash_value_of_cash_grants_ugx,
                                     glue("Total cash transfer value by delivery mechanism{display_in_title}"))
        wnBarChartCashByPartner ("wnpagetab", filter_cash_data_based_on_map, partner_name,
                                 total_cash_value_of_cash_grants_ugx,
                                 glue("Total cash transfer value by partner{display_in_title} (UGX '000)"))
        wnTextSelectedDistrict("wnpagetab", "")
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
