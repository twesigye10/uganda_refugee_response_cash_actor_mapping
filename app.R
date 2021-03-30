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



# Data --------------------------------------------------------------------

# currency conversion
currency_conversion_factor <- 3650

# this is the development branch
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


df_shape <- st_read("data/UGA_Admin/UGA_Admin_2_Districts_2020.shp", crs=4326 ) %>% 
    mutate(ADM2_EN = toupper(ADM2_EN))

df_shape_data <- df_shape%>% 
    left_join(df_data, by = c("ADM2_EN"="Location_District")) 

districts_assessed<-df_shape_data %>% 
    filter(!is.na(Partner_Name)) %>% pull(ADM2_EN) %>% unique()

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
    titlePanel(p("Cash-Based Interventions. Uganda Refugee Response Plan (RRP) 2020-2021", style = "color:#3474A7"), windowTitle = "Cash Based Interventions"),
    p( "The response seeks to explore opportunities to transition from in-kind to cash-based assistance. The injection of cash, through unconditional multi-purpose, and conditional cash-based interventions will have 
multiplier effects on food security, social cohesion, reduction of aid dependency, and productive engagement of the youth, among others. The established reference Minimum Expenditure Basket (MEB) tool will ultimately support the cost efficiency and cost effectiveness, and pave the way for coherent multi-purpose cash programming and delivery. Partners continue efforts to establish a common platform for cash transfers. The information is collected through the Activity Info platform." ),
    # Sidebar
    sidebarLayout(
        # side panel
        sidebarPanel(
            fluidRow(
                column(width = 4,
                       selectInput("yearperiod", 
                                   "Select Year", 
                                   choices = c("All", unique(as.character(df_data$Year))),
                                   selected = "All"
                       )
                ),
                column(width = 4,
                       selectInput("quarterperiod", 
                                   "Select Quarter", 
                                   choices = c("All"),
                                   selected = "All"
                       )
                ),
                column(width = 4,
                       actionButton("mapreset", "Reset Map"),
                       textOutput("selecteddistrict")
                ),
                
            ),
            billboarderOutput("hhreceivingcash" ),
            highchartOutput("plotcashquarter")
        ),
        # end side panel
        
        
        # main panel
        mainPanel(
            # map
            leafletOutput("map"),
            
            
            fluidRow(
                column(width = 6,
                       # Select Delivery Mechanism
                       highchartOutput("plotdeliverymechanism", )
                ),
                column(width = 6,
                       highchartOutput("plotcashpartner")
                )
            )
            
        )
        # end main panel
        
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
        # if (input$yearperiod == "All" ){
        #     input_df
        # } else{
        #     input_df %>% 
        #         filter(Year == input$yearperiod )
        # }
        
    }
    
    # filter cash data by district
    filter_cash_data_by_district <- function(input_df, input_district_click){
        input_df %>% 
            filter(Location_District == input_district_click )
    }
    
    # Charting functions ------------------------------------------------------
    
    # household receive cash
    draw_chart_receiving_cash <- function( input_data ){
        output$hhreceivingcash <-  renderBillboarder({
            
            df_billb_data <- input_data %>% 
                group_by(Select_Beneficiary_Type ) %>% 
                summarise(
                    count_hh_receive_cash_assistance = sum(i.hh_receiving_any_form_of_cash, na.rm = T)
                ) 
            
            billboarder(data = df_billb_data) %>%
                bb_donutchart() %>% 
                bb_legend(position = 'right') %>%
                bb_donut(title = "% of HH receiving cash \nfor Basic Needs\n by Beneficiary Type", width = 70) %>% 
                bb_color(palette = c('#E58606','#5D69B1','#52BCA3','#99C945','#CC61B0'))
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
                hc_title( text = "Total Cash Distributed", margin = 5, align = "left" )%>% 
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
                    count_by_delivery_mechanism = n(),
                    percentage_by_delivery_mechanism = (count_by_delivery_mechanism/nrow(.))*100
                ) %>% 
                arrange(-percentage_by_delivery_mechanism) %>% 
                hchart(type = "bar",
                       hcaes(x = Select_Delivery_Mechanism, y = percentage_by_delivery_mechanism)) %>%  
                hc_title( text = "Percentage of Assistance by Delivery Mechanism", margin = 5, align = "left" )%>% 
                hc_xAxis( title = list(text = "Delivery Mechanism") ) %>% 
                hc_yAxis(title = list(text = "% Assistance by Delivery Mechanismt"))  
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
                hc_title( text = "Total cash Transfers by Partner", margin = 5, align = "left" )%>% 
                hc_xAxis( title = list(text = "Partner") ) %>% 
                hc_yAxis(title = list(text = "Total cash Transfers") ) 
        })
    }
    
    # handle text
    text_selected_district <- function(input_text, input_assessed_districts){
        output$selecteddistrict <- renderText({
            if (input_text %in% input_assessed_districts){
                paste("Selected District: ", input_text)
            }else{
                paste("Selected District: ", "All")
            }
            
        })
    }
    
    # function for adding polgons to map
    creating_map <- function(input_data){
        # Create a continuous palette function
        pal <- colorNumeric(
            palette = "Reds",
            domain = input_data$cash_transfers_by_district)
        # label districts in the map
        labels_v1 <- ~sprintf(
            "<strong>%s</strong><br/>Cash Transfers : %g ",
            ADM2_EN, cash_transfers_by_district
        ) %>% 
            lapply(htmltools::HTML)
        
        labels_district <- ~sprintf(
            "<strong>%s</strong>",
            ifelse(ADM2_EN %in% districts_assessed, ADM2_EN, "" ) 
        ) %>% 
            lapply(htmltools::HTML)
        
        # construct the dynamic map
        proxy = leafletProxy("map", data = input_data) %>% 
            clearShapes()
        
        proxy %>% 
            clearControls() %>% 
            addPolygons(
                color = "white",
                fillColor = ~pal(cash_transfers_by_district),
                fillOpacity = ~ifelse(is.na(cash_transfers_by_district), 0.4, 1),
                weight = 1,
                opacity = 1,
                label = labels_district,
                labelOptions = labelOptions(noHide = T, textOnly = TRUE),
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
                      title = "Total cash",
                      labFormat = labelFormat(prefix = "USD "),
                      opacity  = 1,
                      na.label = "Not Assessed"
            )%>% 
            addLayersControl(
                baseGroups = c("Esri Gray Canvas", "Stamen Toner", "CartoDB Voyager"),
                overlayGroups = c("Districts Assessed"),
                options = layersControlOptions(collapsed = FALSE)
            )
    }
    
    # default polgons data
    df_shape_default <- function(input_shape_data, input_cash_data){
        # UI selectors to filter shape data
        df_by_district_cash_data <- input_cash_data %>% 
            select(Location_District, Total_amount_of_cash_transfers) %>% 
            group_by(Location_District) %>% 
            summarise(cash_transfers_by_district = sum(Total_amount_of_cash_transfers, na.rm = T))
        
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
            setView(lng = 32.2903, lat= 1.3733, zoom = 6.5) %>% 
            addMiniMap( width = 100, height = 100, position = "bottomleft", zoomAnimation = TRUE,  toggleDisplay = TRUE) %>% 
            addEasyButton(easyButton(
                icon="fa-globe", title="Home",
                onClick=JS("function(btn, map){ map.setView(new L.LatLng(1.3733,32.2903), 6.5) }")))
    })
    
    
    # handle changes on the map data through proxy
    observe({
        # UI selectors to filter shape data
        df_by_district_cash_data <- filter_cash_data(df_data) 
        
        df_shape_data <- df_shape_default(df_shape, df_by_district_cash_data)
        
        # add polygon shapes to the map
        creating_map(df_shape_data)
        
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
        
        if(is.null(click)){
            filter_cash_data_based_on_map <- filter_cash_data(df_data)
        }
        else if((!click_district %in% districts_assessed)){
            filter_cash_data_based_on_map <- filter_cash_data(df_data)
        }else{
            filter_cash_data_based_on_map <- filter_cash_data(df_data) %>% 
                filter(Location_District ==  click_district)}
        
        # create all the charts
        draw_chart_receiving_cash(filter_cash_data_based_on_map)
        draw_chart_total_Cash_distributed(filter_cash_data_based_on_map)
        draw_chart_assistance_deliverymechanism(filter_cash_data_based_on_map)
        draw_chart_cash_transfers_by_partner(filter_cash_data_based_on_map)
        
        if(!is.null(click)){
            text_selected_district(click_district, districts_assessed)
            
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
            filter_cash_data_based_on_map <- filter_cash_data(df_data)
            # create all the charts
            draw_chart_receiving_cash(filter_cash_data_based_on_map)
            draw_chart_total_Cash_distributed(filter_cash_data_based_on_map)
            draw_chart_assistance_deliverymechanism(filter_cash_data_based_on_map)
            draw_chart_cash_transfers_by_partner(filter_cash_data_based_on_map)
            # update button
            updateActionButton(session, "mapreset", "Reset Map")
            # update text
            text_selected_district("All", districts_assessed)
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
            
            leafletProxy("mmap") %>%
                clearShapes() %>% 
                clearControls()
        }
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
