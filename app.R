
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

ui <- fluidPage(
    # theme
    # theme = bslib::bs_theme(bootswatch = "darkly"),
    # theme = bslib::bs_theme(bootswatch = "cyborg"),
    theme= reach_theme,
    # Application title
    titlePanel(p("Cash-Based Interventions. Uganda Refugee Response Plan (RRP)", style = "color:#3474A7"), windowTitle = "Cash Based Interventions"),
    p( "The response seeks to explore opportunities to transition from in-kind to cash-based assistance. The injection of cash, through unconditional multi-purpose, and conditional cash-based interventions will have 
multiplier effects on food security, social cohesion, reduction of aid dependency, and productive engagement of the youth, among others. The established reference Minimum Expenditure Basket (MEB) tool will ultimately support the cost efficiency and cost effectiveness, and pave the way for coherent multi-purpose cash programming and delivery. Partners continue efforts to establish a common platform for cash transfers. The information is collected through the Activity Info platform." ),
    
    tabsetPanel( 
        id = "tab_being_displayed",
        # CBI for Basic Needs -----------------------------------------------------
        tabPageUI(
            "cbipagetab", "CBI for Basic Needs", "yearperiod", df_data$Year,
            "quarterperiod", "mapreset", "selecteddistrict", "hhreceivingcash",
            "plotcashquarter", "map", "plotdeliverymechanism", "plotcashpartner"
        ),
        
        # Food Security -----------------------------------------------------------
        tabPageUI(
            "fspagetab", "Food Security", "fs_yearperiod", fs_df_data$Year,
            "fs_quarterperiod", "fs_mapreset", "fs_selecteddistrict", "fs_hhreceivingcash",
            "fs_plotcashquarter", "fs_map", "fs_plotdeliverymechanism", "fs_plotcashpartner"
        ),
        
        # Livelihood --------------------------------------------------------------
        tabPageUI(
            "elspagetab", "Emergency Livelihood Support", "els_yearperiod", els_df_data$Year,
            "els_quarterperiod", "els_mapreset", "els_selecteddistrict", "els_hhreceivingcash",
            "els_plotcashquarter", "els_map", "els_plotdeliverymechanism", "els_plotcashpartner"
        ),
        
        # Environmental Protection --------------------------------------------------------------
        tabPageUI(
            "eprpagetab", "Environmental Protection", "epr_yearperiod", epr_df_data$Year,
            "epr_quarterperiod", "epr_mapreset", "epr_selecteddistrict", "epr_hhreceivingcash",
            "epr_plotcashquarter", "epr_map", "epr_plotdeliverymechanism", "epr_plotcashpartner"
        )
    )
)

# Define server logic required --------------------------------------------

server <- function(input, output, session) {
    # Cash Based Intervention  -----------------------------------------------------------
    cbi_year <- cbiYearValueServer("cbipagetab")
    cbi_quarter <- cbiQuarterValueServer("cbipagetab")
    # default leaflet map  ------------------------------------------------------
    cbiDefaultMap("cbipagetab")
    # dynamic charts and map --------------------------------------------------
    observe({
        # UI selectors to filter shape data
        df_by_district_cash_data <- reactive({filterCashData("cbipagetab", df_data, cbi_year(), Year, cbi_quarter(), Quarter )})
        df_shape_data <- dfShapeDefault("cbipagetab", df_shape, df_by_district_cash_data(), Location_District, Total_amount_of_cash_transfers, "Location_District")
        
        df_point_data <- df_shape_data %>% sf::st_transform(crs = 32636 ) %>%
            sf::st_centroid() %>% sf::st_transform(4326) %>%
            mutate( lat = sf::st_coordinates(.)[,1],  lon = sf::st_coordinates(.)[,2] )
        
        ## create all the charts
        cbiCreatingMap("cbipagetab", df_shape_data)
        cbiMapLabels("cbipagetab", df_point_data)
        cbiDonutChartCashBeneficiary ("cbipagetab", df_by_district_cash_data())
        cbiLineChartTotalCashQuarter ("cbipagetab", df_by_district_cash_data())
        cbiBarChartDeliveryMechanism ("cbipagetab", df_by_district_cash_data())
        cbiBarChartCashByPartner ("cbipagetab", df_by_district_cash_data())
    })
    
    # observe year change to update quarter -----------------------------------
    observe({
        if(cbi_year() != "All"){
            selected_year <- cbi_year()
            filter_cash_data_quarter <- filterYearForQuarters("cbipagetab", df_data, Year, selected_year ) 
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
        filter_cash_data_based_on_map <- filterCashDataByDistrict("cbipagetab", df_data, Location_District, click_district)
        # create all the charts
        cbiDonutChartCashBeneficiary ("cbipagetab", filter_cash_data_based_on_map)
        cbiLineChartTotalCashQuarter ("cbipagetab", filter_cash_data_based_on_map)
        cbiBarChartDeliveryMechanism ("cbipagetab", filter_cash_data_based_on_map)
        cbiBarChartCashByPartner ("cbipagetab", filter_cash_data_based_on_map)
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
            filter_cash_data_quarter <- filterYearDistrictForQuarters ("cbipagetab", df_data, Year, selected_year,
                                                                       Location_District, click_district )
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
        display_in_title <<- " for all Districts"
        cbiUpdateYear("cbipagetab", unique(as.character(df_data$Year)), "All")
        cbiUpdateQuarter("cbipagetab", "All", "All")
        filter_cash_data_based_on_map <- df_data
        cbiDonutChartCashBeneficiary ("cbipagetab", filter_cash_data_based_on_map)
        cbiLineChartTotalCashQuarter ("cbipagetab", filter_cash_data_based_on_map)
        cbiBarChartDeliveryMechanism ("cbipagetab", filter_cash_data_based_on_map)
        cbiBarChartCashByPartner ("cbipagetab", filter_cash_data_based_on_map)
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
        df_point_data <- df_shape_data %>% sf::st_transform(crs = 32636 ) %>%
            sf::st_centroid() %>% sf::st_transform(4326) %>%
            mutate( lat = sf::st_coordinates(.)[,1],  lon = sf::st_coordinates(.)[,2] )
        ## create all the charts
        fsCreatingMap("fspagetab", df_shape_data)
        fsMapLabels("fspagetab", df_point_data)
        fsDonutChartCashBeneficiary ("fspagetab", df_by_district_cash_data())
        fsLineChartTotalCashQuarter ("fspagetab", df_by_district_cash_data())
        fsBarChartDeliveryMechanism ("fspagetab", df_by_district_cash_data())
        fsBarChartCashByPartner ("fspagetab", df_by_district_cash_data())
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
        fsDonutChartCashBeneficiary ("fspagetab", filter_cash_data_based_on_map)
        fsLineChartTotalCashQuarter ("fspagetab", filter_cash_data_based_on_map)
        fsBarChartDeliveryMechanism ("fspagetab", filter_cash_data_based_on_map)
        fsBarChartCashByPartner ("fspagetab", filter_cash_data_based_on_map)
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
        display_in_title <<- " for all Districts"
        fsUpdateYear("fspagetab", unique(as.character(fs_df_data$Year)), "All")
        fsUpdateQuarter("fspagetab", "All", "All")
        filter_cash_data_based_on_map <- fs_df_data
        fsDonutChartCashBeneficiary ("fspagetab", filter_cash_data_based_on_map)
        fsLineChartTotalCashQuarter ("fspagetab", filter_cash_data_based_on_map)
        fsBarChartDeliveryMechanism ("fspagetab", filter_cash_data_based_on_map)
        fsBarChartCashByPartner ("fspagetab", filter_cash_data_based_on_map)
        fsTextSelectedDistrict("fspagetab", "")
    })
    
    
    # Emergency Livelihood Support -----------------------------------------------------------
    
    els_year <- elsYearValueServer("elspagetab")
    els_quarter <- elsQuarterValueServer("elspagetab")
    elsDefaultMap("elspagetab")
    # dynamic charts and map --------------------------------------------------
    observe({
        req(input$tab_being_displayed == "Emergency Livelihood Support")
        # UI selectors to filter shape data
        df_by_district_cash_data <- reactive({filterCashData("elspagetab", els_df_data, els_year(), Year, els_quarter(), Quarter )})
        df_shape_data <- dfShapeDefault("elspagetab", df_shape, df_by_district_cash_data(), location_district, total_cash_value_of_cash_for_work_ugx, "location_district")
        df_point_data <- df_shape_data %>% sf::st_transform(crs = 32636 ) %>%
            sf::st_centroid() %>% sf::st_transform(4326) %>%
            mutate( lat = sf::st_coordinates(.)[,1],  lon = sf::st_coordinates(.)[,2] )
        ## create all the charts
        elsCreatingMap("elspagetab", df_shape_data)
        elsMapLabels("elspagetab", df_point_data)
        elsDonutChartCashBeneficiary ("elspagetab", df_by_district_cash_data())
        elsLineChartTotalCashQuarter ("elspagetab", df_by_district_cash_data())
        elsBarChartDeliveryMechanism ("elspagetab", df_by_district_cash_data())
        elsBarChartCashByPartner ("elspagetab", df_by_district_cash_data())
    })
    
    # observe year change to update quarter -----------------------------------
    observe({
        if(els_year() != "All"){
            selected_year <- els_year()
            filter_cash_data_quarter <- filterYearForQuarters("elspagetab", els_df_data, Year, selected_year ) 
            # update quarter selection
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(els_quarter() %in% available_quarter_choices){
                elsUpdateQuarter("elspagetab", available_quarter_choices, els_quarter())
            }else{
                elsUpdateQuarter("elspagetab", available_quarter_choices, "All")
            }
        }else{
            elsUpdateQuarter("elspagetab", "All", "All")
        }
    })
    
    # Charts listen to map click ----------------------------------------------
    observeEvent(elsClickedDistrictValueServer("elspagetab"),{
        click_district <- elsClickedDistrictValueServer("elspagetab")
        display_in_title <<- paste(" for ", stringr::str_to_title(click_district))
        filter_cash_data_based_on_map <- filterCashDataByDistrict("elspagetab", els_df_data, location_district, click_district)
        # create all the charts
        elsDonutChartCashBeneficiary ("elspagetab", filter_cash_data_based_on_map)
        elsLineChartTotalCashQuarter ("elspagetab", filter_cash_data_based_on_map)
        elsBarChartDeliveryMechanism ("elspagetab", filter_cash_data_based_on_map)
        elsBarChartCashByPartner ("elspagetab", filter_cash_data_based_on_map)
        elsTextSelectedDistrict("elspagetab", click_district)
        # update year selection
        filter_original_cash_data <- filter_cash_data_based_on_map
        available_year_choices <- unique(as.character(filter_original_cash_data$Year))
        if (els_year() %in% available_year_choices){
            elsUpdateYear("elspagetab", available_year_choices, els_year())
        }else{
            elsUpdateYear("elspagetab", available_year_choices, "All")
        }
        # update quarter selection based on year and district
        if(els_year() != "All"){
            selected_year <- els_year()
            filter_cash_data_quarter <- filterYearDistrictForQuarters ("elspagetab", els_df_data, Year, selected_year,
                                                                       location_district, click_district )
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(els_quarter() %in% available_quarter_choices){
                elsUpdateQuarter("elspagetab", available_quarter_choices, els_quarter())
            }else{
                elsUpdateQuarter("elspagetab", available_quarter_choices, "All")
            }
        }
    })
    
    # Map reset button --------------------------------------------------------
    observeEvent(elsResetMapServer("elspagetab"),{
        display_in_title <<- " for all Districts"
        elsUpdateYear("elspagetab", unique(as.character(els_df_data$Year)), "All")
        elsUpdateQuarter("elspagetab", "All", "All")
        filter_cash_data_based_on_map <- els_df_data
        elsDonutChartCashBeneficiary ("elspagetab", filter_cash_data_based_on_map)
        elsLineChartTotalCashQuarter ("elspagetab", filter_cash_data_based_on_map)
        elsBarChartDeliveryMechanism ("elspagetab", filter_cash_data_based_on_map)
        elsBarChartCashByPartner ("elspagetab", filter_cash_data_based_on_map)
        elsTextSelectedDistrict("elspagetab", "")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
