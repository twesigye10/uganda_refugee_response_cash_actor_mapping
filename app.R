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


# load scripts
source("global.R")

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
            "lrpagetab", "Livelihoods & Resilience", "lr_yearperiod", fs_df_data$Year,
            "lr_quarterperiod", "lr_mapreset", "lr_selecteddistrict", "lr_hhreceivingcash",
            "lr_plotcashquarter", "lr_map", "lr_plotdeliverymechanism", "lr_plotcashpartner"
        )
        
    )
)


# Define server logic required --------------------------------------------


server <- function(input, output, session) {
    
    
    # filter cash data
    filter_cash_data <- function(input_df){
        # defaultly display all data from all districts, years and all quarters
        if (input$yearperiod == "All" & input$quarterperiod == "All"){
            input_df
        }else if(input$yearperiod == "All" & input$quarterperiod != "All"){
            input_df %>%
                filter(Quarter == input$quarterperiod )
        }else if(input$yearperiod != "All" & input$quarterperiod == "All"){
            input_df %>%
                filter(Year == input$yearperiod)
        } else{
            input_df %>%
                filter(Year == input$yearperiod, Quarter == input$quarterperiod )
        }
    }
    
    fs_filter_cash_data <- function(input_df){
        # defaultly display all data from all districts, years and all quarters
        if (input$fs_yearperiod == "All" & input$fs_quarterperiod == "All"){
            input_df
        }else if(input$fs_yearperiod == "All" & input$fs_quarterperiod != "All"){
            input_df %>%
                filter(Quarter == input$fs_quarterperiod )
        }else if(input$fs_yearperiod != "All" & input$fs_quarterperiod == "All"){
            input_df %>%
                filter(Year == input$fs_yearperiod)
        } else{
            input_df %>%
                filter(Year == input$fs_yearperiod, Quarter == input$fs_quarterperiod )
        }
    }
    
    # filter cash data by district
    filter_cash_data_by_district <- function(input_df, input_district_click){
        input_df %>% 
            filter(Location_District == input_district_click )
    }
    
    # filter cash data by district
    fs_filter_cash_data_by_district <- function(input_df, input_district_click){
        input_df %>% 
            filter(location_district == input_district_click )
    }
    
    # Charting functions ------------------------------------------------------
    
    # household receive cash
    draw_chart_receiving_cash <- function( input_data ){
        output$hhreceivingcash <-  renderBillboarder({
            
            df_billb_data <- input_data %>% 
                group_by(Select_Beneficiary_Type ) %>% 
                summarise(
                    count_hh_receive_cash_assistance = sum(Total_amount_of_cash_transfers, na.rm = T)
                ) 
            
            billboarder(data = df_billb_data) %>%
                bb_donutchart() %>% 
                bb_legend(position = 'right') %>%
                bb_donut(title = "% of Total \nCash Transfer\n by Beneficiary Type", width = 70) %>% 
                bb_colors_manual(
                    setNames(c('#E58606','#5D69B1','#52BCA3','#99C945'), c(beneficiary_types))
                )
        })
    }
    
    # cash quarter
    draw_chart_total_Cash_distributed <- function(input_data){
        output$plotcashquarter <-  renderHighchart({
            input_data %>%
                group_by(Year, Quarter, Select_Month, Date ) %>%
                summarise(
                    total_amount_of_cash_by_quarter = sum(Total_amount_of_cash_transfers, na.rm = T)
                ) %>%
                arrange(Date) %>% 
                hchart(type = "line",
                       hcaes(x = Select_Month, y = total_amount_of_cash_by_quarter)) %>%  
                hc_title( text = glue("Total Cash Distributed{display_in_title}"), margin = 5, align = "left" )%>% 
                hc_xAxis( title = list(text = "Month") ) %>% 
                hc_yAxis(title = list(text = "Total Cash")) 
        })
    }
    
    # delivery mechanism
    draw_chart_assistance_deliverymechanism <- function(input_data ){
        output$plotdeliverymechanism <-  renderHighchart ({
            input_data %>% 
                group_by(Select_Delivery_Mechanism ) %>% 
                summarise(
                    cash_transfer_by_delivery_mechanism = sum(Total_amount_of_cash_transfers, na.rm = T)
                ) %>% 
                arrange(-cash_transfer_by_delivery_mechanism) %>% 
                hchart(type = "bar",
                       hcaes(x = Select_Delivery_Mechanism, y = cash_transfer_by_delivery_mechanism)) %>%  
                hc_title( text = glue("Total Cash by Delivery Mechanism{display_in_title}"), margin = 5, align = "left" )%>% 
                hc_xAxis( title = list(text = "Delivery Mechanism") ) %>% 
                hc_yAxis(title = list(text = "Cash Transfer by Delivery Mechanism"))  
        })
    }
    
    # cash transfer by partner
    draw_chart_cash_transfers_by_partner <- function( input_data){
        output$plotcashpartner <-  renderHighchart({
            
            input_data %>% 
                group_by(Partner_Name ) %>% 
                summarise(
                    total_cash_by_parter = sum(Total_amount_of_cash_transfers, na.rm = T)
                ) %>% 
                arrange(-total_cash_by_parter) %>%
                hchart(type = "bar",
                       hcaes(x = Partner_Name, y = total_cash_by_parter)) %>% 
                hc_title( text = glue("Total cash Transfers by Partner{display_in_title}"), margin = 5, align = "left" )%>% 
                hc_xAxis( title = list(text = "Partner") ) %>% 
                hc_yAxis(title = list(text = "Total cash Transfers") ) 
        })
    }
    
    # handle text
    text_selected_district <- function(input_text){
        output$selecteddistrict <- renderText({
            if(str_length(input_text) < 1){
                paste("")
            }
            else{
                paste("Selected District: ", stringr::str_to_title(input_text))
            }
            
        })
    }
    
    # function for adding polgons to map
    creating_map <- function(input_data){
        # Create a continuous palette function
        pal <- colorNumeric(
            palette = "Reds",
            domain = input_data$cash_transfers_by_district,
            na.color = "#b6b6b7"
        )
        # label districts in the map
        labels_v1 <- ~sprintf(
            "<strong>%s</strong><br/>Cash Transfers : %s ",
            stringr::str_to_title(ADM2_EN), cash_transfers_by_district
        ) %>% 
            lapply(htmltools::HTML)
        
        labels_district <- ~sprintf(
            "<strong>%s</strong>",
            ifelse(!is.na(cash_transfers_by_district), ADM2_EN, "" ) 
        ) %>% 
            lapply(htmltools::HTML)
        
        # construct the dynamic map
        proxy = leafletProxy("map", data = input_data) #%>% 
        # clearShapes()
        
        proxy %>% 
            clearControls() %>% 
            addPolygons(
                color = "white",
                options = pathOptions(
                    clickable = ~ifelse(!is.na(cash_transfers_by_district), TRUE, FALSE)),
                fillColor = ~pal(cash_transfers_by_district),
                fillOpacity = ~ifelse(is.na(cash_transfers_by_district), 0.7, 1),
                weight = 1,
                opacity = 1,
                label = labels_v1,
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto", opacity =0.75),
                layerId = ~ADM2_EN,
                dashArray = "3",
                highlight = highlightOptions(weight = 3,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.7,
                                             bringToFront = TRUE),
                group="Districts Assessed"
            ) %>% 
            addLegend(position ="bottomright", 
                      pal = pal, 
                      values = ~cash_transfers_by_district,
                      title = "Total cash<br>(UGX '000)",
                      opacity  = 1,
                      na.label = "Not Assessed"
            )%>% 
            leafem::addStaticLabels( label=ifelse(!is.na(input_data$cash_transfers_by_district), stringr::str_to_title(input_data$ADM2_EN), "" )) %>% 
            addLayersControl(
                baseGroups = c("Esri Gray Canvas", "Stamen Toner", "CartoDB Voyager"),
                overlayGroups = c("Districts Assessed"),
                options = layersControlOptions(collapsed = FALSE)
            )
    }
    
    # function for adding labels to map
    creating_map_labels <- function(input_data){
        # label districts in the map
        labels_district <- ~sprintf(
            "<strong>%s</strong>",
            ifelse(!is.na(cash_transfers_by_district),  stringr::str_to_title(ADM2_EN), "" ) 
        ) %>% 
            lapply(htmltools::HTML)
        # add labels on the map
        proxy = leafletProxy("map", data=input_data ) 
        proxy %>%
            clearMarkers() %>%
            addLabelOnlyMarkers( label = labels_district, 
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE)
            )
    }
    
    # default polgons data
    df_shape_default <- function(input_shape_data, input_cash_data){
        # UI selectors to filter shape data
        df_by_district_cash_data <- input_cash_data %>% 
            select(Location_District, Total_amount_of_cash_transfers) %>% 
            group_by(Location_District) %>% 
            summarise(cash_transfers_by_district = sum(Total_amount_of_cash_transfers, na.rm = T)) %>% 
            filter(cash_transfers_by_district > 0)
        
        df_shape_data <- input_shape_data%>% 
            left_join(df_by_district_cash_data, by = c("ADM2_EN"="Location_District"))
        return(
            df_shape_data
        )
    }
    
    
    # Map ---------------------------------------------------------------------
    
    # contents on the map that do not change
    output$map  <-  renderLeaflet({
        leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>% 
            addProviderTiles(providers$Esri.WorldGrayCanvas, 
                             options = providerTileOptions(minZoom = 5, maxZoom = 10), 
                             group="Esri Gray Canvas") %>% 
            addProviderTiles(providers$Stamen.Toner, 
                             options = providerTileOptions(minZoom = 5, maxZoom = 10), 
                             group="Stamen Toner") %>% 
            addProviderTiles(providers$CartoDB.Voyager, 
                             options = providerTileOptions(minZoom = 5, maxZoom = 10), 
                             group="CartoDB Voyager") %>% 
            setView(lng = 32.2903, lat= 1.3733, zoom = 7.25) %>% 
            addMiniMap( width = 100, height = 100, position = "bottomleft", zoomAnimation = TRUE,  toggleDisplay = TRUE) %>% 
            addEasyButton(easyButton(
                icon="fa-globe", title="Home",
                onClick=JS("function(btn, map){ map.setView(new L.LatLng(1.3733,32.2903), 7.25) }")))
    })
    
    
    # handle changes on the map data through proxy
    observe({
        # UI selectors to filter shape data
        df_by_district_cash_data <- filterCashData("filter_cbi_cash_data", df_data, input$yearperiod, "Year", input$quarterperiod, "Quarter") 
        
        df_shape_data <- df_shape_default(df_shape, df_by_district_cash_data)
        
        df_point_data <- df_shape_data %>% sf::st_transform(crs = 32636 ) %>%
            sf::st_centroid() %>% sf::st_transform(4326) %>%
            mutate(
                lat = sf::st_coordinates(.)[,1],
                lon = sf::st_coordinates(.)[,2]
            )
        
        # add polygon shapes to the map
        creating_map(df_shape_data)
        # add labels to the map
        creating_map_labels(df_point_data)
        
        # create all the charts
        draw_chart_receiving_cash(df_by_district_cash_data)
        draw_chart_total_Cash_distributed(df_by_district_cash_data)
        draw_chart_assistance_deliverymechanism(df_by_district_cash_data)
        draw_chart_cash_transfers_by_partner(df_by_district_cash_data)
        
    })
    
    # observe year change to update quarter
    observe({
        if(input$yearperiod != "All"){
            selected_year <- input$yearperiod
            click = input$map_shape_click
            click_district <- click$id
            
            if (!is.null(click)){
                filter_cash_data_quarter <- df_data %>% 
                    filter(Year == selected_year, Location_District == click_district )
            }else{
                filter_cash_data_quarter <- df_data %>% 
                    filter(Year == selected_year )
            }
            # update quarter selection
            available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
            if(input$quarterperiod %in% available_quarter_choices){
                updateSelectInput(session, "quarterperiod", 
                                  label = "Select Quarter", 
                                  choices = c("All", available_quarter_choices),
                                  selected = input$quarterperiod
                )
            }else{
                updateSelectInput(session, "quarterperiod", 
                                  label = "Select Quarter", 
                                  choices = c("All", available_quarter_choices),
                                  selected = "All"
                )
            }
            
        }else{
            updateSelectInput(session, "quarterperiod", 
                              label = "Select Quarter", 
                              choices = c("All"),
                              selected = "All"
            )
        }
        
    })
    
    # Charts listen to map click ----------------------------------------------
    
    observeEvent(input$map_shape_click,{
        click = input$map_shape_click
        click_district <- click$id
        display_in_title <<- paste(" for ", stringr::str_to_title(click_district))
        
        if(is.null(click)){
            filter_cash_data_based_on_map <- filterCashData("filter_cbi_cash_data", df_data, input$yearperiod, "Year", input$quarterperiod, "Quarter")
        }else{
            filter_cash_data_based_on_map <- filterCashData("filter_cbi_cash_data", df_data, input$yearperiod, "Year", input$quarterperiod, "Quarter") %>% 
                filter(Location_District ==  click_district)}
        
        # create all the charts
        draw_chart_receiving_cash(filter_cash_data_based_on_map)
        draw_chart_total_Cash_distributed(filter_cash_data_based_on_map)
        draw_chart_assistance_deliverymechanism(filter_cash_data_based_on_map)
        draw_chart_cash_transfers_by_partner(filter_cash_data_based_on_map)
        
        if(!is.null(click)){
            text_selected_district(click_district)
            
            # update year selection
            filter_original_cash_data <- filter_cash_data_by_district(df_data, click_district)
            available_year_choices <- unique(as.character(filter_original_cash_data$Year))
            if (input$yearperiod %in% available_year_choices){
                # print(paste("District", click_district, "Current selected year is:",input$yearperiod, " And available choices: ", available_year_choices ))
                updateSelectInput(session, "yearperiod", 
                                  label = "Select Year", 
                                  choices = c("All", available_year_choices),
                                  selected = input$yearperiod
                )
            }else{
                updateSelectInput(session, "yearperiod", 
                                  label = "Select Year", 
                                  choices = c("All", available_year_choices),
                                  selected = "All"
                )
            }
            
            
            if(input$yearperiod != "All"){
                selected_year <- input$yearperiod
                filter_cash_data_quarter <- df_data %>% 
                    filter(Year == selected_year, Location_District == click_district )
                
                # update quarter selection
                available_quarter_choices <- unique(as.character(filter_cash_data_quarter$Quarter))
                if(input$quarterperiod %in% available_quarter_choices){
                    updateSelectInput(session, "quarterperiod", 
                                      label = "Select Quarter", 
                                      choices = c("All", available_quarter_choices),
                                      selected = input$quarterperiod
                    )
                }else{
                    updateSelectInput(session, "quarterperiod", 
                                      label = "Select Quarter", 
                                      choices = c("All", available_quarter_choices),
                                      selected = "All"
                    )
                }
                
            }
        }
        
        
        
    })
    
    
    # Map reset button --------------------------------------------------------
    
    
    observeEvent(input$mapreset, {
        
        if (!is.null(input$mapreset)){
            display_in_title <<- " for all Districts"
            filter_cash_data_based_on_map <- filterCashData("filter_cbi_cash_data", df_data, input$yearperiod, "Year", input$quarterperiod, "Quarter")
            # create all the charts
            draw_chart_receiving_cash(filter_cash_data_based_on_map)
            draw_chart_total_Cash_distributed(filter_cash_data_based_on_map)
            draw_chart_assistance_deliverymechanism(filter_cash_data_based_on_map)
            draw_chart_cash_transfers_by_partner(filter_cash_data_based_on_map)
            # update button
            updateActionButton(session, "mapreset", "Reset Map")
            # update text
            text_selected_district("")
            # update year selection
            updateSelectInput(session, "yearperiod", 
                              label = "Select Year", 
                              choices = c("All", unique(as.character(df_data$Year))),
                              selected = "All"
            )
            
            # update Quarter selection
            updateSelectInput(session, "quarterperiod", 
                              label = "Select Quarter", 
                              choices = c("All"),
                              selected = "All"
            )
        }
        
    })
    
    
    # Food Security -----------------------------------------------------------
    
    # Charting functions ------------------------------------------------------
    
    # household receive cash
    fs_draw_chart_receiving_cash <- function( input_data ){
        output$fs_hhreceivingcash <-  renderBillboarder({
            
            df_billb_data <- input_data %>% 
                group_by(select_beneficiary_type ) %>% 
                summarise(
                    total_cash_transfer_by_beneficiary_type = sum(fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers, na.rm = T)
                ) 
            
            billboarder(data = df_billb_data) %>%
                bb_donutchart() %>% 
                bb_legend(position = 'right') %>%
                bb_donut(title = "% total cash transfer \n by Beneficiary Type", width = 70) %>% 
                bb_colors_manual(
                    setNames(c('#E58606','#5D69B1','#52BCA3','#99C945'), c(fs_beneficiary_types))
                )
        })
    }
    
    # cash quarter
    fs_draw_chart_total_Cash_distributed <- function(input_data){
        output$fs_plotcashquarter <-  renderHighchart({
            input_data %>%
                group_by(Year, Quarter, select_quarter ) %>%
                summarise(
                    total_amount_of_cash_by_quarter = sum(fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers, na.rm = T)
                ) %>%
                # arrange(select_quarter) %>% 
                hchart(type = "line",
                       hcaes(x = select_quarter, y = total_amount_of_cash_by_quarter)) %>%  
                hc_title( text = glue("Total Cash Transfer{display_in_title}"), margin = 5, align = "left" )%>% 
                hc_xAxis( title = list(text = "Quarter") ) %>% 
                hc_yAxis(title = list(text = "Total Cash")) 
        })
    }
    
    # delivery mechanism
    fs_draw_chart_assistance_deliverymechanism <- function(input_data ){
        output$fs_plotdeliverymechanism <-  renderHighchart ({
            input_data %>% 
                group_by(select_delivery_mechanism ) %>% 
                summarise(
                    chash_transfer_by_delivery_mechanism = sum(fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers, na.rm = T)
                ) %>% 
                arrange(-chash_transfer_by_delivery_mechanism) %>% 
                hchart(type = "bar",
                       hcaes(x = select_delivery_mechanism, y = chash_transfer_by_delivery_mechanism)) %>%  
                hc_title( text = glue("Total Cash Transfer by Delivery Mechanism{display_in_title}"), margin = 5, align = "left" )%>% 
                hc_xAxis( title = list(text = "Delivery Mechanism") ) %>% 
                hc_yAxis(title = list(text = "Cash Transfer by Delivery Mechanismt"))  
        })
    }
    
    # cash transfer by partner
    fs_draw_chart_cash_transfers_by_partner <- function( input_data){
        output$fs_plotcashpartner <-  renderHighchart({
            
            input_data %>% 
                group_by(partner_name ) %>% 
                summarise(
                    total_cash_by_parter = sum(fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers, na.rm = T)
                ) %>% 
                arrange(-total_cash_by_parter) %>%
                hchart(type = "bar",
                       hcaes(x = partner_name, y = total_cash_by_parter)) %>% 
                hc_title( text = glue("Total cash Transfers by Partner{display_in_title}"), margin = 5, align = "left" )%>% 
                hc_xAxis( title = list(text = "Partner") ) %>% 
                hc_yAxis(title = list(text = "Total cash Transfers") ) 
        })
    }
    
    # handle text
    fs_text_selected_district <- function(input_text){
        output$fs_selecteddistrict <- renderText({
            if(str_length(input_text) < 1){
                paste("")
            }
            else{
                paste("Selected District: ", stringr::str_to_title(input_text))
            }
            
        })
    }
    
    # function for adding polgons to map
    fs_creating_map <- function(input_data){
        # Create a continuous palette function
        pal <- colorNumeric(
            palette = "Reds",
            domain = input_data$cash_transfers_by_district,
            na.color = "#b6b6b7"
        )
        # label districts in the map
        labels_v1 <- ~sprintf(
            "<strong>%s</strong><br/>Cash Transfers : %s ",
            stringr::str_to_title(ADM2_EN), cash_transfers_by_district
        ) %>% 
            lapply(htmltools::HTML)
        
        labels_district <- ~sprintf(
            "<strong>%s</strong>",
            ifelse(!is.na(cash_transfers_by_district), ADM2_EN, "" ) 
        ) %>% 
            lapply(htmltools::HTML)
        
        # construct the dynamic map
        proxy = leafletProxy("fs_map", data = input_data) #%>% 
        # clearShapes()
        
        proxy %>% 
            clearControls() %>% 
            addPolygons(
                color = "white",
                options = pathOptions(
                    clickable = ~ifelse(!is.na(cash_transfers_by_district), TRUE, FALSE)),
                fillColor = ~pal(cash_transfers_by_district),
                fillOpacity = ~ifelse(is.na(cash_transfers_by_district), 0.7, 1),
                weight = 1,
                opacity = 1,
                label = labels_v1,
                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto", opacity =0.75),
                layerId = ~ADM2_EN,
                dashArray = "3",
                highlight = highlightOptions(weight = 3,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.7,
                                             bringToFront = TRUE),
                group="Districts Assessed"
            ) %>% 
            addLegend(position ="bottomright", 
                      pal = pal, 
                      values = ~cash_transfers_by_district,
                      title = "Total cash<br>(UGX '000)",
                      opacity  = 1,
                      na.label = "Not Assessed"
            )%>% 
            addLayersControl(
                baseGroups = c("Esri Gray Canvas", "Stamen Toner", "CartoDB Voyager"),
                overlayGroups = c("Districts Assessed"),
                options = layersControlOptions(collapsed = FALSE)
            )
    }
    
    # function for adding labels to map
    fs_creating_map_labels <- function(input_data){
        # label districts in the map
        labels_district <- ~sprintf(
            "<strong>%s</strong>",
            ifelse(!is.na(cash_transfers_by_district),  stringr::str_to_title(ADM2_EN), "" ) 
        ) %>% 
            lapply(htmltools::HTML)
        # add labels on the map
        proxy = leafletProxy("fs_map", data=input_data ) 
        proxy %>%
            clearMarkers() %>%
            addLabelOnlyMarkers( label = labels_district, 
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE)
            )
    }
    
    # default polgons data
    fs_df_shape_default <- function(input_shape_data, input_cash_data){
        # UI selectors to filter shape data
        df_by_district_cash_data <- input_cash_data %>% 
            select(location_district, fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers) %>% 
            group_by(location_district) %>% 
            summarise(cash_transfers_by_district = sum(fs_i_1_2_refugees_receiving_cash_total_amount_of_cash_transfers, na.rm = T)) %>% 
            filter(cash_transfers_by_district > 0)
        
        df_shape_data <- input_shape_data%>% 
            left_join(df_by_district_cash_data, by = c("ADM2_EN"="location_district"))
        return(
            df_shape_data
        )
    }
    
    
    # Map ---------------------------------------------------------------------
    
    # contents on the map that do not change
    output$fs_map  <-  renderLeaflet({
        leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>% 
            addProviderTiles(providers$Esri.WorldGrayCanvas, 
                             options = providerTileOptions(minZoom = 5, maxZoom = 10), 
                             group="Esri Gray Canvas") %>% 
            addProviderTiles(providers$Stamen.Toner, 
                             options = providerTileOptions(minZoom = 5, maxZoom = 10), 
                             group="Stamen Toner") %>% 
            addProviderTiles(providers$CartoDB.Voyager, 
                             options = providerTileOptions(minZoom = 5, maxZoom = 10), 
                             group="CartoDB Voyager") %>% 
            setView(lng = 32.2903, lat= 1.3733, zoom = 7.25) %>% 
            addMiniMap( width = 100, height = 100, position = "bottomleft", zoomAnimation = TRUE,  toggleDisplay = TRUE) %>% 
            addEasyButton(easyButton(
                icon="fa-globe", title="Home",
                onClick=JS("function(btn, fs_map){ fs_map.setView(new L.LatLng(1.3733,32.2903), 7.25) }")))
    })
    
    
    # handle changes on the map data through proxy
    observe({
        req(input$tab_being_displayed == "Food Security")
        # UI selectors to filter shape data
        df_by_district_cash_data <- fs_filter_cash_data(fs_df_data) 
        
        df_shape_data <- fs_df_shape_default(df_shape, df_by_district_cash_data)
        
        df_point_data <- df_shape_data %>% sf::st_transform(crs = 32636 ) %>%
            sf::st_centroid() %>% sf::st_transform(4326) %>%
            mutate(
                lat = sf::st_coordinates(.)[,1],
                lon = sf::st_coordinates(.)[,2]
            )
        
        # add polygon shapes to the map
        fs_creating_map(df_shape_data)
        fs_creating_map_labels(df_point_data)
        
        # # create all the charts
        fs_draw_chart_receiving_cash(df_by_district_cash_data)
        fs_draw_chart_total_Cash_distributed(df_by_district_cash_data)
        fs_draw_chart_assistance_deliverymechanism(df_by_district_cash_data)
        fs_draw_chart_cash_transfers_by_partner(df_by_district_cash_data)
        
    })
    
    # observe year change to update quarter
    observe({
        if(input$fs_yearperiod != "All"){
            selected_year <- input$fs_yearperiod
            click = input$fs_map_shape_click
            click_district <- click$id
            
            if (!is.null(click)){
                fs_filter_cash_data_quarter <- fs_df_data %>%
                    filter(Year == selected_year, location_district == click_district )
            }else{
                fs_filter_cash_data_quarter <- fs_df_data %>%
                    filter(Year == selected_year )
            }
            # update quarter selection
            fs_available_quarter_choices <- unique(as.character(fs_filter_cash_data_quarter$Quarter))
            if(input$fs_quarterperiod %in% fs_available_quarter_choices){
                updateSelectInput(session, "fs_quarterperiod",
                                  label = "Select Quarter",
                                  choices = c("All", fs_available_quarter_choices),
                                  selected = input$fs_quarterperiod
                )
            }else{
                updateSelectInput(session, "fs_quarterperiod",
                                  label = "Select Quarter",
                                  choices = c("All", fs_available_quarter_choices),
                                  selected = "All"
                )
            }
            
        }else{
            updateSelectInput(session, "fs_quarterperiod",
                              label = "Select Quarter",
                              choices = c("All"),
                              selected = "All"
            )
        }
        
    })
    
    # Charts listen to map click ----------------------------------------------
    
    observeEvent(input$fs_map_shape_click,{
        click = input$fs_map_shape_click
        click_district <- click$id
        display_in_title <<- paste(" for ", stringr::str_to_title(click_district))
        
        if(is.null(click)){
            fs_filter_cash_data_based_on_map <- fs_filter_cash_data(fs_df_data)
        }else{
            fs_filter_cash_data_based_on_map <- fs_filter_cash_data(fs_df_data) %>%
                filter(location_district ==  click_district)}
        
        # create all the charts
        fs_draw_chart_receiving_cash(fs_filter_cash_data_based_on_map)
        fs_draw_chart_total_Cash_distributed(fs_filter_cash_data_based_on_map)
        fs_draw_chart_assistance_deliverymechanism(fs_filter_cash_data_based_on_map)
        fs_draw_chart_cash_transfers_by_partner(fs_filter_cash_data_based_on_map)
        
        if(!is.null(click)){
            fs_text_selected_district(click_district)
            
            # update year selection
            fs_filter_original_cash_data <- fs_filter_cash_data_by_district(fs_df_data, click_district)
            fs_available_year_choices <- unique(as.character(fs_filter_original_cash_data$Year))
            if (input$yearperiod %in% fs_available_year_choices){
                # print(paste("District", click_district, "Current selected year is:",input$yearperiod, " And available choices: ", available_year_choices ))
                updateSelectInput(session, "fs_yearperiod",
                                  label = "fs_Select Year",
                                  choices = c("All", fs_available_year_choices),
                                  selected = input$yearperiod
                )
            }else{
                updateSelectInput(session, "fs_yearperiod",
                                  label = "Select Year",
                                  choices = c("All", fs_available_year_choices),
                                  selected = "All"
                )
            }
            
            
            if(input$fs_yearperiod != "All"){
                selected_year <- input$fs_yearperiod
                fs_filter_cash_data_quarter <- fs_df_data %>%
                    filter(Year == selected_year, location_district == click_district )
                
                # update quarter selection
                fs_available_quarter_choices <- unique(as.character(fs_filter_cash_data_quarter$Quarter))
                if(input$fs_quarterperiod %in% fs_available_quarter_choices){
                    updateSelectInput(session, "fs_quarterperiod",
                                      label = "Select Quarter",
                                      choices = c("All", fs_available_quarter_choices),
                                      selected = input$fs_quarterperiod
                    )
                }else{
                    updateSelectInput(session, "fs_quarterperiod",
                                      label = "Select Quarter",
                                      choices = c("All", fs_available_quarter_choices),
                                      selected = "All"
                    )
                }
                
            }
        }
        
        
        
    })
    
    
    # Map reset button --------------------------------------------------------
    
    
    observeEvent(input$fs_mapreset, {
        
        if (!is.null(input$fs_mapreset)){
            display_in_title <<- " for all Districts"
            fs_filter_cash_data_based_on_map <- fs_filter_cash_data(fs_df_data)
            # create all the charts
            fs_draw_chart_receiving_cash(fs_filter_cash_data_based_on_map)
            fs_draw_chart_total_Cash_distributed(fs_filter_cash_data_based_on_map)
            fs_draw_chart_assistance_deliverymechanism(fs_filter_cash_data_based_on_map)
            fs_draw_chart_cash_transfers_by_partner(fs_filter_cash_data_based_on_map)
            # update button
            updateActionButton(session, "fs_mapreset", "Reset Map")
            # update text
            fs_text_selected_district("")
            # update year selection
            updateSelectInput(session, "fs_yearperiod",
                              label = "Select Year",
                              choices = c("All", unique(as.character(fs_df_data$Year))),
                              selected = "All"
            )
            
            # update Quarter selection
            updateSelectInput(session, "fs_quarterperiod",
                              label = "Select Quarter",
                              choices = c("All"),
                              selected = "All"
            )
        }
        
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
