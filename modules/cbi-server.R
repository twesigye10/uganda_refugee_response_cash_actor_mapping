# Server modules ----------------------------------------------------------

# get year
cbiYearValueServer <- function(id){
  moduleServer(id, function(input, output, session){
    return(
      reactive({input$yearperiod})
    )
  })
  
}

# get quarter
cbiQuarterValueServer <- function(id){
  moduleServer(id, function(input, output, session){
    return(
      reactive({input$quarterperiod})
    )
  })
}


# reset map
cbiResetMapServer <- function(id){
  moduleServer(id, function(input, output, session){
    observeEvent(input$mapreset,{
      display_in_title <<- " for all Districts"
      cbiUpdateQuarter("cbipagetab", unique(as.character(df_data$Year)), "All")
      textSelectedDistrict("cbipagetab", "")
    })
    
  })
}

# get clicked district
cbiClickedDistrictValueServer <- function(id){
  moduleServer(id, function(input, output, session){
    click = input$map_shape_click
    click_district <- click$id
    return(
      click_district
    )
  })
}

# filter cash data
filterCashData <- function(id, input_df, input_year, year_field, input_quarter, quarter_field ){
  moduleServer(id, function(input, output, session){
    # defaultly display all data from all districts, years and all quarters
    if (input_year == "All" & input_quarter == "All"){
      input_df
    }else if(input_year == "All" & input_quarter != "All"){
      input_df %>%
        filter({{quarter_field}} == input_quarter )
    }else if(input_year != "All" & input_quarter == "All"){
      input_df %>%
        filter({{year_field}} == input_year)
    } else{
      input_df %>%
        filter(Year == input_year, Quarter == input_quarter )
    }
    # removed return value for this type of function. It will not return reactive values
    # return(
    #  input_df
    # )
  })
  
}

# filter cash data by district
filterCashDataByDistrict <- function(id, input_df, inp_field_district, input_district_click){
  moduleServer(id, function(input, output, session){
    input_df %>% 
      filter({{inp_field_district}} == input_district_click )
  })
  
}

# filter year for available quarters
filterYearForQuarters <- function(id, input_df, inp_field_year, input_selected_year){
  moduleServer(id, function(input, output, session){
    input_df %>% 
      filter({{inp_field_year}} == input_selected_year )
  })
  
}

# filter year and district for available quarters
filterYearDistrictForQuarters <- function(id, input_df, inp_field_year, input_selected_year,
                                          inp_field_district, input_district_click
){
  moduleServer(id, function(input, output, session){
    input_df %>% 
      filter({{inp_field_year}} == input_selected_year, {{inp_field_district}} == input_district_click  )
    
    
  })
  
}


# filter cash data by district
dfShapeDefault <- function(id, input_shape_data, input_cash_data){
  moduleServer(id, function(input, output, session){
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

lineChartTotalCashQuarter <- function(id, input_data, input_field_analysis, input_field_year, 
                                      input_field_quarter, input_field_select_Month, 
                                      input_field_date, input_field_x, input_title){
  moduleServer(id, function(input, output, session){
    output$plotcashquarter <-  renderHighchart({
      input_data %>%
        group_by({{input_field_year}}, {{input_field_quarter}}, {{input_field_select_Month}}, {{input_field_date}} ) %>%
        summarise(
          total_amount_of_cash_by_quarter = sum({{input_field_analysis}}, na.rm = T)
        ) %>%
        arrange(Date) %>% 
        hchart(type = "line",
               hcaes(x = Select_Month, y = total_amount_of_cash_by_quarter)) %>%  
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
        hc_xAxis( title = list(text = "Month") ) %>% 
        hc_yAxis(title = list(text = "Total Cash")) 
    })
    
  })
}


barChartDeliveryMechanism <- function(id, input_data, input_field_group, input_field_analysis,  
                                      input_title){
  moduleServer(id, function(input, output, session){
    output$plotdeliverymechanism <-  renderHighchart({
      input_data %>%
        group_by({{input_field_group}} ) %>%
        summarise(
          cash_transfer_by_delivery_mechanism = sum({{input_field_analysis}}, na.rm = T)
        ) %>%
        arrange(-cash_transfer_by_delivery_mechanism) %>% 
        hchart(type = "bar",
               hcaes(x = Select_Delivery_Mechanism, y = cash_transfer_by_delivery_mechanism)) %>%  
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
        hc_xAxis( title = list(text = "Delivery Mechanism") ) %>% 
        hc_yAxis(title = list(text = "Cash Transfer by Delivery Mechanism")) 
    })
    
  })
}


barChartDeliveryMechanism <- function(id, input_data, input_field_group, input_field_analysis,  
                                      input_title){
  moduleServer(id, function(input, output, session){
    output$plotdeliverymechanism <-  renderHighchart({
      input_data %>%
        group_by({{input_field_group}} ) %>%
        summarise(
          cash_transfer_by_delivery_mechanism = sum({{input_field_analysis}}, na.rm = T)
        ) %>%
        arrange(-cash_transfer_by_delivery_mechanism) %>% 
        hchart(type = "bar",
               hcaes(x = Select_Delivery_Mechanism, y = cash_transfer_by_delivery_mechanism)) %>%  
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
        hc_xAxis( title = list(text = "Delivery Mechanism") ) %>% 
        hc_yAxis(title = list(text = "Cash Transfer by Delivery Mechanism")) 
    })
    
  })
}


barChartCashByPartner <- function(id, input_data, input_field_group, input_field_analysis,  
                                  input_title){
  moduleServer(id, function(input, output, session){
    output$plotcashpartner <-  renderHighchart({
      input_data %>%
        group_by({{input_field_group}} ) %>%
        summarise(
          total_cash_by_parter = sum({{input_field_analysis}}, na.rm = T)
        ) %>%
        arrange(-total_cash_by_parter) %>% 
        hchart(type = "bar",
               hcaes(x = Partner_Name, y = total_cash_by_parter)) %>%  
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
        hc_xAxis( title = list(text = "Partner") ) %>% 
        hc_yAxis(title = list(text = "Total cash Transfers")) 
    })
    
  })
}

textSelectedDistrict <- function(id, input_text){
  moduleServer(id, function(input, output, session){
    output$selecteddistrict <- renderText({
      if(str_length(input_text) < 1){ paste("")
      } else{ paste("Selected District: ", stringr::str_to_title(input_text))  }
    })
    
  })
}

# cbi default map module ------------------------------------------------------

cbiDefaultMap <- function(id){
  moduleServer(id, function(input, output, session){
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
    
  })
}

# cbi dynamic map layer module ------------------------------------------------------
cbiCreatingMap <- function(id, input_data){
  moduleServer(id, function(input, output, session){
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
      addLayersControl(
        baseGroups = c("Esri Gray Canvas", "Stamen Toner", "CartoDB Voyager"),
        overlayGroups = c("Districts Assessed"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

# cbi dynamic map labels module ------------------------------------------------------
cbiMapLabels <- function(id, input_data){
  moduleServer(id, function(input, output, session){
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
  })
}

# update quarter module ------------------------------------------------------
cbiUpdateQuarter <- function(id, input_quarter_choices, input_selected){
  moduleServer(id, function(input, output, session){
    updateSelectInput(session, "quarterperiod", 
                      label = "Select Quarter", 
                      choices = c("All", input_quarter_choices),
                      selected = input_selected)
  })
}

# update quarter module ------------------------------------------------------
cbiUpdateYear <- function(id, input_year_choices, input_selected){
  moduleServer(id, function(input, output, session){
    updateSelectInput(session, "quarterperiod", 
                      label = "Select Quarter", 
                      choices = c("All", input_year_choices),
                      selected = input_selected)
  })
}

