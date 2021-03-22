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
library(leaflet)
library(plotly)
library(bslib)
library(highcharter)


# data
# this is the development branch
df_data <- read_csv(file = "data/RRP_5W_CBI_for_basic_needs_20210305_055004_UTC.csv") %>% 
    rename_all(~str_replace_all(., "\\s+", "_")) %>% 
    separate(Select_Month, c("Month", "Year"), "-", remove= FALSE, extra = "drop") %>% 
    mutate(
        Quarter = case_when(Month %in% c("Jan", "Feb", "Mar")~paste0("Q1_20",Year),
                            Month %in% c("Apr", "May", "Jun")~paste0("Q2_20",Year),
                            Month %in% c("Jul", "Aug", "Sep")~paste0("Q3_20",Year),
                            Month %in% c("Oct", "Nov", "Dec")~paste0("Q4_20",Year)
        )
    ) %>% 
    arrange(desc(Year),desc(Quarter))

df_by_district_cash_data <- df_data %>% 
    select(Location_District, Total_amount_of_cash_transfers) %>% 
    group_by(Location_District) %>% 
    summarise(cash_transfers_by_district = sum(Total_amount_of_cash_transfers, na.rm = T))


df_shape <- st_read("data/UGA_Admin/UGA_Admin_2_Districts_2018.shp", crs=32636 ) %>%
    st_transform( crs = 4326) %>% 
    left_join(df_by_district_cash_data, by = c("DNAME2018"="Location_District"), ignore_case =TRUE) %>% 
    filter(!is.na(cash_transfers_by_district))



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
    titlePanel(p("Cash-Based Interventions. Uganda Refugee Response Plan (RRP) 2020-2021", style = "color:#3474A7")),
    
    # Sidebar
    sidebarLayout(
        # side panel
        sidebarPanel(
            selectInput("district", 
                        "Select District", 
                        choices = c("All", unique(as.character(df_data$Location_District))),
                        selected = "All"
            ),
            selectInput("quarterperiod", 
                        "Select Quarter", 
                        choices = c("All", unique(as.character(df_data$Quarter))),
                        selected = "All"
            ),
            plotOutput("plotcashquarter")
            
        ),
        # end side panel
        
        # main panel
        mainPanel(
            # map
            leafletOutput("map"),
            
            
            fluidRow(
                column(width = 6,
                       # Select Delivery Mechanism
                       highchartOutput("plotdeliverymechanism",
                       )
                ),
                column(width = 6,
                       plotOutput("plotcashpartner")
                )
            )
            
            
        )
        # end main panel
        
    )
)

# Define server logic required
server <- function(input, output) {
    # filter data
    filter_shape_data  <-  reactive({
        # defaultly display all data from all districts
        if (input$district == "All" ){
            df_shape
        } else{
            df_shape %>% 
                filter( DNAME2018 == input$district )
        }
    })
    
    # filter cash data
    filter_cash_data <- reactive({
        # defaultly display all data from all districts and all quarters
        if (input$district == "All" & input$quarterperiod == "All"){
            df_data
        }else if(input$district == "All" & input$quarterperiod != "All"){
            df_data %>% 
                filter(Quarter == input$quarterperiod )
        }else if(input$district != "All" & input$quarterperiod == "All"){
            df_data %>% 
                filter(Location_District == input$district)
        } else{
            df_data %>% 
                filter(Location_District == input$district 
                       & Quarter == input$quarterperiod )
        }
    })
    
    
    # cash quarter
    output$plotcashquarter <-  renderPlot({
        
        filter_cash_data() %>% 
            group_by(Quarter ) %>% 
            summarise(
                total_amount_of_cash_by_quarter = sum(Total_amount_of_cash_transfers, na.rm = T)
            ) %>% 
            ggplot(
                aes(x = total_amount_of_cash_by_quarter,
                    y =  fct_relevel(Quarter, c("Q1_2019","Q2_2019","Q3_2019","Q4_2019","Q1_2020","Q2_2020","Q3_2020","Q4_2020")) 
                )
            )+
            geom_bar(stat = "identity", fill = "blue", show.legend = FALSE) +
            labs( title = "Total Cash by Quarter",
                  x= "Total Cash",
                  y= "Quarter" )+
            theme_bw() 
        
    })
    
    
    # delivery mechanism
    output$plotdeliverymechanism <-  renderHighchart ({
        filter_cash_data() %>% 
            group_by(Select_Delivery_Mechanism ) %>% 
            summarise(
                count_by_delivery_mechanism = n(),
                percentage_by_delivery_mechanism = (count_by_delivery_mechanism/nrow(.))*100
            ) %>% 
            hchart(type = "bar",
                   hcaes(x = Select_Delivery_Mechanism, y = percentage_by_delivery_mechanism))
        
        # filter_cash_data() %>% 
        #     group_by(Select_Delivery_Mechanism ) %>% 
        #     summarise(
        #         count_by_delivery_mechanism = n(),
        #         percentage_by_delivery_mechanism = (count_by_delivery_mechanism/nrow(.))*100
        #     ) %>% 
        #     ggplot(
        #         aes(x = percentage_by_delivery_mechanism, y = reorder(Select_Delivery_Mechanism, percentage_by_delivery_mechanism)
        #         )
        #     )+
        #     geom_bar(stat = "identity", fill = "blue", show.legend = FALSE) +
        #     labs( title = "Percentage of Assistance by Delivery Mechanism",
        #           x= "% Assistance by Delivery Mechanism",
        #           y= "Delivery Mechanism" )+
        #     theme_bw()
        
    })
    
    # cash transfer by partner
    output$plotcashpartner <-  renderPlot({
        
        filter_cash_data() %>% 
            group_by(Partner_Name ) %>% 
            summarise(
                total_cash_by_parter = sum(Total_amount_of_cash_transfers, na.rm = T)
            ) %>% 
            ggplot(
                aes(x = total_cash_by_parter, y = reorder(Partner_Name, total_cash_by_parter) )
            )+
            geom_bar(stat = "identity", fill = "blue",  show.legend = FALSE) +
            labs(
                title = "Total cash Transfers by Partner",
                x= "Total cash Transfers",
                y= "Partner"
            )+
            theme_bw()
        
    })
    
    # contents on the map that do not change
    output$map  <-  renderLeaflet({
        leaflet() %>% 
            addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
            setView(lng = 32.2903, 1.3733, zoom = 6)
    })
    
    # Create a continuous palette function
    pal <- colorNumeric(
        palette = "Reds",
        domain = df_shape$cash_transfers_by_district)
    
    labels <- ~sprintf(
        "<strong>%s</strong><br/>Cash Transfers : %g ",
        DNAME2018, cash_transfers_by_district
    ) %>% 
        lapply(htmltools::HTML)
    
    # handle changes on the map data through proxy
    observe({
        proxy = leafletProxy("map", data = filter_shape_data()) %>% 
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
                dashArray = "3",
                highlight = highlightOptions(weight = 5,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.7,
                                             bringToFront = TRUE)
            )
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
