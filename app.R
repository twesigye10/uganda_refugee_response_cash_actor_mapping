
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
        
        cbiDonutChartCashBeneficiary ("cbipagetab",
                                   df_by_district_cash_data(),
                                   Select_Beneficiary_Type,
                                   Total_amount_of_cash_transfers,
                                   "% of Total \nCash Transfer\n by Beneficiary Type",
                                   beneficiary_types)
        
        cbiLineChartTotalCashQuarter ("cbipagetab", df_by_district_cash_data(), 
                                   Total_amount_of_cash_transfers, Year, Quarter, Select_Month, 
                                   Date, "Select_Month",  glue("Total Cash Distributed{display_in_title}"))
        
        cbiBarChartDeliveryMechanism ("cbipagetab", df_by_district_cash_data(),
                                   Select_Delivery_Mechanism,
                                   Total_amount_of_cash_transfers,
                                   glue("Total Cash by Delivery Mechanism{display_in_title}"))
        
        cbiBarChartCashByPartner ("cbipagetab", df_by_district_cash_data(), Partner_Name,
                               Total_amount_of_cash_transfers,
                               glue("Total cash Transfers by Partner{display_in_title}"))
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
        cbiDonutChartCashBeneficiary ("cbipagetab",
                                   filter_cash_data_based_on_map,
                                   Select_Beneficiary_Type,
                                   Total_amount_of_cash_transfers,
                                   "% of Total \nCash Transfer\n by Beneficiary Type",
                                   beneficiary_types)
        cbiLineChartTotalCashQuarter ("cbipagetab", filter_cash_data_based_on_map, 
                                   Total_amount_of_cash_transfers, Year, Quarter, Select_Month, 
                                   Date, "Select_Month",  glue("Total Cash Distributed{display_in_title}"))
        cbiBarChartDeliveryMechanism ("cbipagetab", filter_cash_data_based_on_map,
                                   Select_Delivery_Mechanism,
                                   Total_amount_of_cash_transfers,
                                   glue("Total Cash by Delivery Mechanism{display_in_title}"))
        cbiBarChartCashByPartner ("cbipagetab", filter_cash_data_based_on_map, Partner_Name,
                               Total_amount_of_cash_transfers,
                               glue("Total cash Transfers by Partner{display_in_title}"))
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
    cbiResetMapServer("cbipagetab")
    
    # Food Security -----------------------------------------------------------
     
    fs_year <- fsYearValueServer("fs_pagetab")
    fs_quarter <- fsQuarterValueServer("fs_pagetab")
    fsDefaultMap("fs_pagetab")
    # dynamic charts and map --------------------------------------------------
    observe({
        req(input$tab_being_displayed == "Food Security")
        # UI selectors to filter shape data
        df_by_district_cash_data <- reactive({filterCashData("fs_pagetab", fs_df_data, fs_year(), Year, fs_quarter(), Quarter )})
        df_shape_data <- dfShapeDefault("fs_pagetab", df_shape, df_by_district_cash_data(), location_district, fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers, "location_district")
        df_point_data <- df_shape_data %>% sf::st_transform(crs = 32636 ) %>%
            sf::st_centroid() %>% sf::st_transform(4326) %>%
            mutate( lat = sf::st_coordinates(.)[,1],  lon = sf::st_coordinates(.)[,2] )
        ## create all the charts
        fsCreatingMap("fs_pagetab", df_shape_data)
        fsMapLabels("fs_pagetab", df_point_data)
        fsDonutChartCashBeneficiary ("fs_pagetab",
                                     df_by_district_cash_data(),
                                     select_beneficiary_type,
                                     fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers,
                                     "% of Total \nCash Transfer\n by Beneficiary Type",
                                     fs_beneficiary_types)
        fsLineChartTotalCashQuarter ("fs_pagetab", df_by_district_cash_data(), 
                                     fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers, Year, Quarter, select_quarter, 
                                     "select_quarter",  glue("Total Cash Distributed{display_in_title}"))
        fsBarChartDeliveryMechanism ("fs_pagetab", df_by_district_cash_data(),
                                     select_delivery_mechanism,
                                     fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers,
                                     glue("Total Cash by Delivery Mechanism{display_in_title}"))
        fsBarChartCashByPartner ("fs_pagetab", df_by_district_cash_data(), partner_name,
                                 fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers,
                                 glue("Total cash Transfers by Partner{display_in_title}"))
        
    })
    
    # observe year change to update quarter -----------------------------------
    observe({
        if(fs_year() != "All"){
            selected_year <- fs_year()
            filter_cash_data_quarter <- filterYearForQuarters("fs_pagetab", fs_df_data, Year, selected_year ) 
            # update quarter selection
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(fs_quarter() %in% available_quarter_choices){
                fsUpdateQuarter("fs_pagetab", available_quarter_choices, fs_quarter())
            }else{
                fsUpdateQuarter("fs_pagetab", available_quarter_choices, "All")
            }
        }else{
            fsUpdateQuarter("fs_pagetab", "All", "All")
        }
    })
    
    # Charts listen to map click ----------------------------------------------
    observeEvent(fsClickedDistrictValueServer("fs_pagetab"),{
        click_district <- fsClickedDistrictValueServer("fs_pagetab")
        display_in_title <<- paste(" for ", stringr::str_to_title(click_district))
        filter_cash_data_based_on_map <- filterCashDataByDistrict("fs_pagetab", fs_df_data, location_district, click_district)
        # create all the charts
        fsDonutChartCashBeneficiary ("fs_pagetab",
                                     filter_cash_data_based_on_map,
                                     select_beneficiary_type,
                                     fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers,
                                     "% of Total \nCash Transfer\n by Beneficiary Type",
                                     beneficiary_types)
        fsLineChartTotalCashQuarter ("fs_pagetab", filter_cash_data_based_on_map, 
                                     fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers, Year, Quarter, select_quarter, 
                                     Date, "select_quarter",  glue("Total Cash Distributed{display_in_title}"))
        fsBarChartDeliveryMechanism ("fs_pagetab", filter_cash_data_based_on_map,
                                     select_delivery_mechanism,
                                     fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers,
                                     glue("Total Cash by Delivery Mechanism{display_in_title}"))
        fsBarChartCashByPartner ("fs_pagetab", filter_cash_data_based_on_map, partner_name,
                                 fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers,
                                 glue("Total cash Transfers by Partner{display_in_title}"))
        fsTextSelectedDistrict("fs_pagetab", click_district)
        # update year selection
        filter_original_cash_data <- filter_cash_data_based_on_map
        available_year_choices <- unique(as.character(filter_original_cash_data$Year))
        if (fs_year() %in% available_year_choices){
            fsUpdateYear("fs_pagetab", available_year_choices, fs_year())
        }else{
            fsUpdateYear("fs_pagetab", available_year_choices, "All")
        }
        # update quarter selection based on year and district
        if(fs_year() != "All"){
            selected_year <- fs_year()
            filter_cash_data_quarter <- filterYearDistrictForQuarters ("fs_pagetab", fs_df_data, Year, selected_year,
                                                                       location_district, click_district )
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(fs_quarter() %in% available_quarter_choices){
                fsUpdateQuarter("fs_pagetab", available_quarter_choices, fs_quarter())
            }else{
                fsUpdateQuarter("fs_pagetab", available_quarter_choices, "All")
            }
        }
    })
    
    # Map reset button --------------------------------------------------------
    fsResetMapServer("fs_pagetab")
    
    
    
    # # Emergency Livelihood Support -----------------------------------------------------------
    # 
    # # Charting functions ------------------------------------------------------
    # 
    # # cash for work by beneficiary type
    # els_draw_chart_receiving_cash <- function( input_data ){
    #     output$els_hhreceivingcash <-  renderBillboarder({
    #         
    #         df_billb_data <- input_data %>% 
    #             group_by(select_beneficiary_type ) %>% 
    #             summarise(
    #                 total_cash_for_work_ugx = sum(total_cash_value_of_cash_for_work_ugx, na.rm = T)
    #             ) 
    #         
    #         billboarder(data = df_billb_data) %>%
    #             bb_donutchart() %>% 
    #             bb_legend(position = 'right') %>%
    #             bb_donut(title = "% Total Cash for Work \n by Beneficiary Type", width = 70) %>% 
    #             bb_colors_manual(
    #                 setNames(c('#5D69B1','#52BCA3','#99C945'), c(els_beneficiary_types))
    #             )
    #     })
    # }
    # 
    # # cash quarter
    # els_draw_chart_total_Cash_distributed <- function(input_data){
    #     output$els_plotcashquarter <-  renderHighchart({
    #         input_data %>%
    #             group_by(Year, Quarter, select_quarter ) %>%
    #             summarise(
    #                 total_amount_of_cash_by_quarter = sum(total_cash_value_of_cash_for_work_ugx, na.rm = T)
    #             ) %>%
    #             hchart(type = "line",
    #                    hcaes(x = select_quarter, y = total_amount_of_cash_by_quarter)) %>%  
    #             hc_title( text = glue("Total Cash for Work{display_in_title}"), margin = 5, align = "left" )%>% 
    #             hc_xAxis( title = list(text = "Quarter") ) %>% 
    #             hc_yAxis(title = list(text = "Total Cash")) 
    #     })
    # }
    # 
    # # delivery mechanism
    # els_draw_chart_assistance_deliverymechanism <- function(input_data ){
    #     output$els_plotdeliverymechanism <-  renderHighchart ({
    #         input_data %>% 
    #             group_by(delivery_mechanism ) %>% 
    #             summarise(
    #                 chash_for_work_by_delivery_mechanism = sum(total_cash_value_of_cash_for_work_ugx, na.rm = T)
    #             ) %>% 
    #             arrange(-chash_for_work_by_delivery_mechanism) %>% 
    #             hchart(type = "bar",
    #                    hcaes(x = delivery_mechanism, y = chash_for_work_by_delivery_mechanism)) %>%  
    #             hc_title( text = glue("Total Cash for Work by Delivery Mechanism{display_in_title}"), margin = 5, align = "left" )%>% 
    #             hc_xAxis( title = list(text = "Delivery Mechanism") ) %>% 
    #             hc_yAxis(title = list(text = "Cash for Work by Delivery Mechanism"))  
    #     })
    # }
    # 
    # # cash transfer by partner
    # els_draw_chart_cash_transfers_by_partner <- function( input_data){
    #     output$els_plotcashpartner <-  renderHighchart({
    #         
    #         input_data %>% 
    #             group_by(partner_name ) %>% 
    #             summarise(
    #                 total_cash_by_parter = sum(total_cash_value_of_cash_for_work_ugx, na.rm = T)
    #             ) %>% 
    #             arrange(-total_cash_by_parter) %>%
    #             hchart(type = "bar",
    #                    hcaes(x = partner_name, y = total_cash_by_parter)) %>% 
    #             hc_title( text = glue("Total Cash for Work by Partner{display_in_title}"), margin = 5, align = "left" )%>% 
    #             hc_xAxis( title = list(text = "Partner") ) %>% 
    #             hc_yAxis(title = list(text = "Total Cash for Work") ) 
    #     })
    # }
    # 
    # # handle text
    # els_text_selected_district <- function(input_text){
    #     output$els_selecteddistrict <- renderText({
    #         if(str_length(input_text) < 1){
    #             paste("")
    #         }
    #         else{
    #             paste("Selected District: ", stringr::str_to_title(input_text))
    #         }
    #         
    #     })
    # }
    # 
    # # function for adding polgons to map
    # els_creating_map <- function(input_data){
    #     # Create a continuous palette function
    #     pal <- colorNumeric(
    #         palette = "Reds",
    #         domain = input_data$cash_transfers_by_district,
    #         na.color = "#b6b6b7"
    #     )
    #     # label districts in the map
    #     labels_v1 <- ~sprintf(
    #         "<strong>%s</strong><br/>Cash Transfers : %s ",
    #         stringr::str_to_title(ADM2_EN), cash_transfers_by_district
    #     ) %>% 
    #         lapply(htmltools::HTML)
    #     
    #     labels_district <- ~sprintf(
    #         "<strong>%s</strong>",
    #         ifelse(!is.na(cash_transfers_by_district), ADM2_EN, "" ) 
    #     ) %>% 
    #         lapply(htmltools::HTML)
    #     
    #     # construct the dynamic map
    #     proxy = leafletProxy("els_map", data = input_data) #%>% 
    #     # clearShapes()
    #     
    #     proxy %>% 
    #         clearControls() %>% 
    #         addPolygons(
    #             color = "white",
    #             options = pathOptions(
    #                 clickable = ~ifelse(!is.na(cash_transfers_by_district), TRUE, FALSE)),
    #             fillColor = ~pal(cash_transfers_by_district),
    #             fillOpacity = ~ifelse(is.na(cash_transfers_by_district), 0.7, 1),
    #             weight = 1,
    #             opacity = 1,
    #             label = labels_v1,
    #             labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
    #                                         textsize = "15px",
    #                                         direction = "auto", opacity =0.75),
    #             layerId = ~ADM2_EN,
    #             dashArray = "3",
    #             highlight = highlightOptions(weight = 3,
    #                                          color = "#666",
    #                                          dashArray = "",
    #                                          fillOpacity = 0.7,
    #                                          bringToFront = TRUE),
    #             group="Districts Assessed"
    #         ) %>% 
    #         addLegend(position ="bottomright", 
    #                   pal = pal, 
    #                   values = ~cash_transfers_by_district,
    #                   title = "Total cash<br>(UGX '000)",
    #                   opacity  = 1,
    #                   na.label = "Not Assessed"
    #         )%>% 
    #         addLayersControl(
    #             baseGroups = c("Esri Gray Canvas", "Stamen Toner", "CartoDB Voyager"),
    #             overlayGroups = c("Districts Assessed"),
    #             options = layersControlOptions(collapsed = FALSE)
    #         )
    # }
    # 
    # # function for adding labels to map
    # els_creating_map_labels <- function(input_data){
    #     # label districts in the map
    #     labels_district <- ~sprintf(
    #         "<strong>%s</strong>",
    #         ifelse(!is.na(cash_transfers_by_district),  stringr::str_to_title(ADM2_EN), "" ) 
    #     ) %>% 
    #         lapply(htmltools::HTML)
    #     # add labels on the map
    #     proxy = leafletProxy("els_map", data=input_data ) 
    #     proxy %>%
    #         clearMarkers() %>%
    #         addLabelOnlyMarkers( label = labels_district, 
    #                              labelOptions = labelOptions(noHide = T, textOnly = TRUE)
    #         )
    # }
    # 
    # # default polgons data
    # els_df_shape_default <- function(input_shape_data, input_cash_data){
    #     # UI selectors to filter shape data
    #     df_by_district_cash_data <- input_cash_data %>% 
    #         select(location_district, total_cash_value_of_cash_for_work_ugx) %>% 
    #         group_by(location_district) %>% 
    #         summarise(cash_transfers_by_district = sum(total_cash_value_of_cash_for_work_ugx, na.rm = T)) %>% 
    #         filter(cash_transfers_by_district > 0)
    #     
    #     df_shape_data <- input_shape_data%>% 
    #         left_join(df_by_district_cash_data, by = c("ADM2_EN"="location_district"))
    #     return(
    #         df_shape_data
    #     )
    # }
    # 
    # 
    # # Map ---------------------------------------------------------------------
    # 
    # # contents on the map that do not change
    # output$els_map  <-  renderLeaflet({
    #     leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>% 
    #         addProviderTiles(providers$Esri.WorldGrayCanvas, 
    #                          options = providerTileOptions(minZoom = 5, maxZoom = 10), 
    #                          group="Esri Gray Canvas") %>% 
    #         addProviderTiles(providers$Stamen.Toner, 
    #                          options = providerTileOptions(minZoom = 5, maxZoom = 10), 
    #                          group="Stamen Toner") %>% 
    #         addProviderTiles(providers$CartoDB.Voyager, 
    #                          options = providerTileOptions(minZoom = 5, maxZoom = 10), 
    #                          group="CartoDB Voyager") %>% 
    #         setView(lng = 32.2903, lat= 1.3733, zoom = 7.25) %>% 
    #         addMiniMap( width = 100, height = 100, position = "bottomleft", zoomAnimation = TRUE,  toggleDisplay = TRUE) %>% 
    #         addEasyButton(easyButton(
    #             icon="fa-globe", title="Home",
    #             onClick=JS("function(btn, fs_map){ fs_map.setView(new L.LatLng(1.3733,32.2903), 7.25) }")))
    # })
    # 
    # 
    # # handle changes on the map data through proxy
    # observe({
    #     req(input$tab_being_displayed == "Emergency Livelihood Support")
    #     # UI selectors to filter shape data
    #     df_by_district_cash_data <- els_filter_cash_data(els_df_data)
    #     
    #     df_shape_data <- els_df_shape_default(df_shape, df_by_district_cash_data)
    #     
    #     df_point_data <- df_shape_data %>% sf::st_transform(crs = 32636 ) %>%
    #         sf::st_centroid() %>% sf::st_transform(4326) %>%
    #         mutate(
    #             lat = sf::st_coordinates(.)[,1],
    #             lon = sf::st_coordinates(.)[,2]
    #         )
    #     
    #     # add polygon shapes to the map
    #     els_creating_map(df_shape_data)
    #     els_creating_map_labels(df_point_data)
    #     
    #     # # create all the charts
    #     els_draw_chart_receiving_cash(df_by_district_cash_data)
    #     els_draw_chart_total_Cash_distributed(df_by_district_cash_data)
    #     els_draw_chart_assistance_deliverymechanism(df_by_district_cash_data)
    #     els_draw_chart_cash_transfers_by_partner(df_by_district_cash_data)
    #     
    # })
    # 
    # # observe year change to update quarter
    # observe({
    #     if(input$els_yearperiod != "All"){
    #         selected_year <- input$els_yearperiod
    #         click = input$els_map_shape_click
    #         click_district <- click$id
    #         
    #         if (!is.null(click)){
    #             els_filter_cash_data_quarter <- els_df_data %>%
    #                 filter(Year == selected_year, location_district == click_district )
    #         }else{
    #             els_filter_cash_data_quarter <- els_df_data %>%
    #                 filter(Year == selected_year )
    #         }
    #         # update quarter selection
    #         els_available_quarter_choices <- unique(as.character(els_filter_cash_data_quarter$Quarter))
    #         if(input$els_quarterperiod %in% els_available_quarter_choices){
    #             updateSelectInput(session, "els_quarterperiod",
    #                               label = "Select Quarter",
    #                               choices = c("All", els_available_quarter_choices),
    #                               selected = input$els_quarterperiod
    #             )
    #         }else{
    #             updateSelectInput(session, "els_quarterperiod",
    #                               label = "Select Quarter",
    #                               choices = c("All", els_available_quarter_choices),
    #                               selected = "All"
    #             )
    #         }
    #         
    #     }else{
    #         updateSelectInput(session, "els_quarterperiod",
    #                           label = "Select Quarter",
    #                           choices = c("All"),
    #                           selected = "All"
    #         )
    #     }
    #     
    # })
    # 
    # # Charts listen to map click ----------------------------------------------
    # 
    # observeEvent(input$els_map_shape_click,{
    #     click = input$els_map_shape_click
    #     click_district <- click$id
    #     display_in_title <<- paste(" for ", stringr::str_to_title(click_district))
    #     
    #     if(is.null(click)){
    #         els_filter_cash_data_based_on_map <- els_filter_cash_data(els_df_data) 
    #     }else{
    #         els_filter_cash_data_based_on_map <- els_filter_cash_data(els_df_data)  %>%
    #             filter(location_district ==  click_district)}
    #     
    #     # create all the charts
    #     els_draw_chart_receiving_cash(els_filter_cash_data_based_on_map)
    #     els_draw_chart_total_Cash_distributed(els_filter_cash_data_based_on_map)
    #     els_draw_chart_assistance_deliverymechanism(els_filter_cash_data_based_on_map)
    #     els_draw_chart_cash_transfers_by_partner(els_filter_cash_data_based_on_map)
    #     
    #     if(!is.null(click)){
    #         els_text_selected_district(click_district)
    #         
    #         # update year selection
    #         els_filter_original_cash_data <- els_filter_cash_data_by_district(els_df_data, click_district)
    #         els_available_year_choices <- unique(as.character(els_filter_original_cash_data$Year))
    #         if (input$els_yearperiod %in% els_available_year_choices){
    #             # print(paste("District", click_district, "Current selected year is:",input$yearperiod, " And available choices: ", available_year_choices ))
    #             updateSelectInput(session, "els_yearperiod",
    #                               label = "els_Select Year",
    #                               choices = c("All", els_available_year_choices),
    #                               selected = input$els_yearperiod
    #             )
    #         }else{
    #             updateSelectInput(session, "els_yearperiod",
    #                               label = "Select Year",
    #                               choices = c("All", els_available_year_choices),
    #                               selected = "All"
    #             )
    #         }
    #         
    #         
    #         if(input$els_yearperiod != "All"){
    #             selected_year <- input$els_yearperiod
    #             fs_filter_cash_data_quarter <- els_df_data %>%
    #                 filter(Year == selected_year, location_district == click_district )
    #             
    #             # update quarter selection
    #             els_available_quarter_choices <- unique(as.character(els_filter_cash_data_quarter$Quarter))
    #             if(input$els_quarterperiod %in% els_available_quarter_choices){
    #                 updateSelectInput(session, "els_quarterperiod",
    #                                   label = "Select Quarter",
    #                                   choices = c("All", els_available_quarter_choices),
    #                                   selected = input$els_quarterperiod
    #                 )
    #             }else{
    #                 updateSelectInput(session, "els_quarterperiod",
    #                                   label = "Select Quarter",
    #                                   choices = c("All", els_available_quarter_choices),
    #                                   selected = "All"
    #                 )
    #             }
    #             
    #         }
    #     }
    #     
    #     
    #     
    # })
    # 
    # 
    # # Map reset button --------------------------------------------------------
    # 
    # 
    # observeEvent(input$els_mapreset, {
    #     
    #     if (!is.null(input$els_mapreset)){
    #         display_in_title <<- " for all Districts"
    #         els_filter_cash_data_based_on_map <- els_filter_cash_data(els_df_data) 
    #         # create all the charts
    #         els_draw_chart_receiving_cash(els_filter_cash_data_based_on_map)
    #         els_draw_chart_total_Cash_distributed(els_filter_cash_data_based_on_map)
    #         els_draw_chart_assistance_deliverymechanism(els_filter_cash_data_based_on_map)
    #         els_draw_chart_cash_transfers_by_partner(els_filter_cash_data_based_on_map)
    #         # update button
    #         updateActionButton(session, "els_mapreset", "Reset Map")
    #         # update text
    #         els_text_selected_district("")
    #         # update year selection
    #         updateSelectInput(session, "els_yearperiod",
    #                           label = "Select Year",
    #                           choices = c("All", unique(as.character(els_df_data$Year))),
    #                           selected = "All"
    #         )
    #         
    #         # update Quarter selection
    #         updateSelectInput(session, "els_quarterperiod",
    #                           label = "Select Quarter",
    #                           choices = c("All"),
    #                           selected = "All"
    #         )
    #     }
    #     
    # })
    # 
    # 
    # 
    # 
}

# Run the application 
shinyApp(ui = ui, server = server)
