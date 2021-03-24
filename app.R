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

# data
# this is the development branch
df_data <- read_csv(file = "data/RRP_5W_CBI_for_basic_needs_20210305_055004_UTC.csv") %>% 
    rename_all(~str_replace_all(., "\\s+|\\(|\\)", "_")) %>% 
    separate(Select_Month, c("Month", "Year"), "-", remove= FALSE, extra = "drop") %>% 
    mutate(
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


df_shape <- st_read("data/UGA_Admin/UGA_Admin_2_Districts_2018.shp", crs=32636 ) %>%
    st_transform( crs = 4326)


reach_theme <- bs_theme(
    bg = ggreach::reach_cols("lightgrey"), 
    fg = ggreach::reach_cols("medred"),
    primary = ggreach::reach_cols("medbeige"),
    base_font = "Arial"
)
# Define UI for application
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
                column(width = 6,
                       selectInput("yearperiod", 
                                   "Select Year", 
                                   choices = c("All", unique(as.character(df_data$Year))),
                                   selected = "All"
                                   )
                       ),
                column(width = 6,
                       selectInput("quarterperiod", 
                                   "Select Quarter", 
                                   choices = c("All", unique(as.character(df_data$Quarter))),
                                   selected = "All"
                                   )
                       )
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

# Define server logic required
server <- function(input, output) {

    # filter cash data
    filter_cash_data <- reactive({
        # defaultly display all data from all districts, years and all quarters
        if (input$yearperiod == "All" & input$quarterperiod == "All"){
            df_data
        }else if(input$yearperiod == "All" & input$quarterperiod != "All"){
            df_data %>% 
                filter(Quarter == input$quarterperiod )
        }else if(input$yearperiod != "All" & input$quarterperiod == "All"){
            df_data %>% 
                filter(Year == input$yearperiod)
        } else{
            df_data %>% 
                filter(Year == input$yearperiod, Quarter == input$quarterperiod )
        }
    })

    # contents on the map that do not change
    output$map  <-  renderLeaflet({
        leaflet() %>% 
            addProviderTiles(providers$Esri.WorldGrayCanvas, options = providerTileOptions(minZoom = 5, maxZoom = 10)) %>% 
            setView(lng = 32.2903, 1.3733, zoom = 7)
    })
    
    # Create a continuous palette function
    pal <- colorNumeric(
        palette = "Reds",
        domain = df_shape$cash_transfers_by_district)
    # label districts in the map
    labels <- ~sprintf(
        "<strong>%s</strong><br/>Cash Transfers : %g ",
        DNAME2018, cash_transfers_by_district
    ) %>% 
        lapply(htmltools::HTML)
    
    # handle changes on the map data through proxy
    observe({
        # UI selectors to filter shape data
        df_by_district_cash_data <- filter_cash_data() %>% 
            select(Location_District, Total_amount_of_cash_transfers) %>% 
            group_by(Location_District) %>% 
            summarise(cash_transfers_by_district = sum(Total_amount_of_cash_transfers, na.rm = T))
        df_shape <- df_shape%>% 
            left_join(df_by_district_cash_data, by = c("DNAME2018"="Location_District"), ignore_case =TRUE) %>% 
            filter(!is.na(cash_transfers_by_district))
        
        # construct the dynamic map
        proxy = leafletProxy("map", data = df_shape) %>% 
            clearShapes()
        
        proxy %>% 
            clearControls() %>% 
            addPolygons(
                color = "white",
                fillColor = ~pal(cash_transfers_by_district),
                fillOpacity = 0.9,
                weight = 2,
                opacity = 1,
                label = labels,
                layerId = ~DNAME2018,
                dashArray = "3",
                highlight = highlightOptions(weight = 5,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.7,
                                             bringToFront = TRUE)
                ) %>% 
            addLegend(position ="bottomright", 
                      pal = pal, 
                      values = ~cash_transfers_by_district,
                      title = "Total cash",
                      labFormat = labelFormat(prefix = "UGX"),
                      opacity  = 1
                      )
        
    })
    
    # charts listen to map click
    observe({
        click = input$map_shape_click
        click_district <- click$id
        
        if(is.null(click))
            filter_cash_data_based_on_map <- filter_cash_data()
        else
            filter_cash_data_based_on_map <- filter_cash_data() %>% 
            filter(Location_District ==  click_district)
        
        # household receive cash
        output$hhreceivingcash <-  renderBillboarder({
            
            df_billb_data <- filter_cash_data_based_on_map %>% 
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
        
        # cash quarter
        output$plotcashquarter <-  renderHighchart({
            filter_cash_data_based_on_map %>%
                # filter(Location_District ==  click_district) %>% 
                group_by(Year, Quarter, Select_Month, Date ) %>%
                summarise(
                    total_amount_of_cash_by_quarter = sum(Total_amount_of_cash_transfers, na.rm = T)
                ) %>%
                arrange(Date) %>% 
                hchart(type = "line",
                       hcaes(x = Select_Month, y = total_amount_of_cash_by_quarter)) %>%  
                hc_title( text = "Total Cash Distributed", margin = 5, align = "left" )%>% 
                hc_xAxis( title = list(text = "Quarter") ) %>% 
                hc_yAxis(title = list(text = "Total Cash")) 
            
        })
        
        # delivery mechanism
        output$plotdeliverymechanism <-  renderHighchart ({
            filter_cash_data_based_on_map %>% 
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
        
        # cash transfer by partner
        output$plotcashpartner <-  renderHighchart({
            
            filter_cash_data_based_on_map %>% 
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
        
        
    
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
