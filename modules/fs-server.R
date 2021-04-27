# Server modules ----------------------------------------------------------

# get year
fsYearValueServer <- function(id){
  moduleServer(id, function(input, output, session){
    return( reactive({input$fs_yearperiod})  )
  })
}
# get quarter
fsQuarterValueServer <- function(id){
  moduleServer(id, function(input, output, session){
    return( reactive({input$fs_quarterperiod})  )
  })
}
# reset map
fsResetMapServer <- function(id){
  moduleServer(id, function(input, output, session){
    observeEvent(input$fs_mapreset,{
      display_in_title <<- " for all Districts"
      fsUpdateQuarter("fspagetab", unique(as.character(fs_df_data$Year)), "All")
      textSelectedDistrict("fspagetab", "")
    })
    
  })
}
# get clicked district
fsClickedDistrictValueServer <- function(id){
  moduleServer(id, function(input, output, session){
    click = input$fs_map_shape_click
    click_district <- click$id
    return( click_district )
  })
}
# donut chart module ------------------------------------------------------
fsDonutChartCashBeneficiary <- function(id, input_data, input_field_group,
                                      input_field_analysis, input_title, input_beneficiary_vector){
  moduleServer(id, function(input, output, session){
    output$fs_hhreceivingcash <-  renderBillboarder({
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
# line chart cash transfer module ------------------------------------------------------
fsLineChartTotalCashQuarter <- function(id, input_data, input_field_analysis, input_field_year, 
                                      input_field_quarter, input_field_select_Month, 
                                       input_title){
  moduleServer(id, function(input, output, session){
    output$fs_plotcashquarter <-  renderHighchart({
      input_data %>%
        group_by({{input_field_year}}, {{input_field_quarter}}, {{input_field_select_Month}} ) %>%
        summarise(
          total_amount_of_cash_by_quarter = sum({{input_field_analysis}}, na.rm = T)
        ) %>%
        # arrange(select_quarter) %>% 
        hchart(type = "line",
               hcaes(x = select_quarter, y = total_amount_of_cash_by_quarter)) %>%  
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
        hc_xAxis( title = list(text = "Month") ) %>% 
        hc_yAxis(title = list(text = "Total Cash")) 
    })
  })
}
# bar chart delivery mechanism module ------------------------------------------------------
fsBarChartDeliveryMechanism <- function(id, input_data, input_field_group, input_field_analysis,  
                                      input_title){
  moduleServer(id, function(input, output, session){
    output$fs_plotdeliverymechanism <-  renderHighchart({
      input_data %>%
        group_by({{input_field_group}} ) %>%
        summarise(
          cash_transfer_by_delivery_mechanism = sum({{input_field_analysis}}, na.rm = T)
        ) %>%
        arrange(-cash_transfer_by_delivery_mechanism) %>% 
        hchart(type = "bar",
               hcaes(x = select_delivery_mechanism, y = cash_transfer_by_delivery_mechanism)) %>%  
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
        hc_xAxis( title = list(text = "Delivery Mechanism") ) %>% 
        hc_yAxis(title = list(text = "Cash Transfer by Delivery Mechanism")) 
    })
  })
}
# bar chart cash by partner module ------------------------------------------------------
fsBarChartCashByPartner <- function(id, input_data, input_field_group, input_field_analysis,  
                                  input_title){
  moduleServer(id, function(input, output, session){
    output$fs_plotcashpartner <-  renderHighchart({
      input_data %>%
        group_by({{input_field_group}} ) %>%
        summarise(
          total_cash_by_parter = sum({{input_field_analysis}}, na.rm = T)
        ) %>%
        arrange(-total_cash_by_parter) %>% 
        hchart(type = "bar",
               hcaes(x = partner_name, y = total_cash_by_parter)) %>%  
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
        hc_xAxis( title = list(text = "Partner") ) %>% 
        hc_yAxis(title = list(text = "Total cash Transfers")) 
    })
    
  })
}
# text for selected district on the map chart module ------------------------------------------------------
fsTextSelectedDistrict <- function(id, input_text){
  moduleServer(id, function(input, output, session){
    output$fs_selecteddistrict <- renderText({
      if(str_length(input_text) < 1){ paste("")
      } else{ paste("Selected District: ", stringr::str_to_title(input_text))  }
    })
    
  })
}
# fs default map module ------------------------------------------------------
fsDefaultMap <- function(id){
  moduleServer(id, function(input, output, session){
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
  })
}
# fs dynamic map layer module ------------------------------------------------------
fsCreatingMap <- function(id, input_data){
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
    proxy = leafletProxy("fs_map", data = input_data) #%>% 
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
# fs dynamic map labels module ------------------------------------------------------
fsMapLabels <- function(id, input_data){
  moduleServer(id, function(input, output, session){
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
  })
}
# update quarter module ------------------------------------------------------
fsUpdateQuarter <- function(id, input_quarter_choices, input_selected){
  moduleServer(id, function(input, output, session){
    updateSelectInput(session, "fs_quarterperiod", 
                      label = "Select Quarter", 
                      choices = c("All", input_quarter_choices),
                      selected = input_selected)
  })
}
# update quarter module ------------------------------------------------------
fsUpdateYear <- function(id, input_year_choices, input_selected){
  moduleServer(id, function(input, output, session){
    updateSelectInput(session, "fs_quarterperiod", 
                      label = "Select Quarter", 
                      choices = c("All", input_year_choices),
                      selected = input_selected)
  })
}

