# Server modules ----------------------------------------------------------

# get year
eprYearValueServer <- function(id){
  moduleServer(id, function(input, output, session){
    return( reactive({input$epr_yearperiod})  )
  })
}
# get quarter
eprQuarterValueServer <- function(id){
  moduleServer(id, function(input, output, session){
    return( reactive({input$epr_quarterperiod})  )
  })
}
# reset map
eprResetMapServer <- function(id){
  moduleServer(id, function(input, output, session){
    return(
      input$epr_mapreset
    )
  })
}
# get clicked district
eprClickedDistrictValueServer <- function(id){
  moduleServer(id, function(input, output, session){
    click = input$epr_map_shape_click
    click_district <- click$id
    return( click_district )
  })
}
# donut chart module ------------------------------------------------------
eprDonutChartCashBeneficiary <- function(id, input_data, input_field_group,
                                      input_field_analysis, input_title, input_beneficiary_vector){
  moduleServer(id, function(input, output, session){
    output$epr_hhreceivingcash <-  renderBillboarder({
      df_billb_data <- input_data %>% 
        group_by({{input_field_group}} ) %>% 
        summarise(
          cash_assistance_by_beneficiary_type = sum({{input_field_analysis}}, na.rm = T)
        ) 
      billboarder(data = df_billb_data) %>%
        bb_donutchart() %>% 
        bb_legend(position = 'bottom') %>%
        bb_donut(title = input_title, width = 70) %>% 
        bb_colors_manual(
          setNames(c('#5D69B1','#99C945'), c(input_beneficiary_vector))
        )
    })
  })
}
# line chart cash transfer module ------------------------------------------------------
eprLineChartTotalCashQuarter <- function(id, input_data, input_field_analysis, input_field_year, 
                                      input_field_quarter, input_field_select_Month, 
                                       input_title){
  moduleServer(id, function(input, output, session){
    output$epr_plotcashquarter <-  renderHighchart({
      input_data %>%
        group_by({{input_field_year}}, {{input_field_quarter}}, {{input_field_select_Month}} ) %>%
        summarise(
          total_amount_of_cash_by_quarter = sum({{input_field_analysis}}, na.rm = T)
        ) %>%
        # arrange(select_quarter) %>% 
        hchart(type = "line",
               hcaes(x = select_quarter, y = total_amount_of_cash_by_quarter)) %>%
        hc_tooltip(pointFormat = "<b>{point.total_amount_of_cash_by_quarter:,.0f}</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
        hc_xAxis( title = list(text = "Month") ) %>% 
        hc_yAxis(title = list(text = "Total Cash")) 
    })
  })
}
# bar chart delivery mechanism module ------------------------------------------------------
eprBarChartDeliveryMechanism <- function(id, input_data, input_field_group, input_field_analysis,  
                                      input_title){
  moduleServer(id, function(input, output, session){
    output$epr_plotdeliverymechanism <-  renderHighchart({
      input_data %>%
        group_by({{input_field_group}} ) %>%
        summarise(
          cash_transfer_by_delivery_mechanism = sum({{input_field_analysis}}, na.rm = T)
        ) %>%
        arrange(-cash_transfer_by_delivery_mechanism) %>% 
        hchart(type = "bar",
               hcaes(x = delivery_mechanism, y = cash_transfer_by_delivery_mechanism),
               dataLabels = list(enabled = TRUE, format="{point.cash_transfer_by_delivery_mechanism:,.0f}" )) %>%
        hc_tooltip(pointFormat = "<b>{point.cash_transfer_by_delivery_mechanism:,.0f}</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
        hc_xAxis( title = list(text = "Delivery Mechanism") ) %>% 
        hc_yAxis(title = list(text = "Cash Transfer by Delivery Mechanism"), labels = FALSE ) 
    })
  })
}
# bar chart cash by partner module ------------------------------------------------------
eprBarChartCashByPartner <- function(id, input_data, input_field_group, input_field_analysis,  
                                  input_title){
  moduleServer(id, function(input, output, session){
    output$epr_plotcashpartner <-  renderHighchart({
      input_data %>%
        group_by({{input_field_group}} ) %>%
        summarise(
          total_cash_by_parter = sum({{input_field_analysis}}, na.rm = T)
        ) %>%
        arrange(-total_cash_by_parter) %>% 
        hchart(type = "bar",
               hcaes(x = partner_name, y = total_cash_by_parter),
               dataLabels = list(enabled = TRUE, format="{point.total_cash_by_parter:,.0f}" )) %>%
        hc_tooltip(pointFormat = "<b>{point.total_cash_by_parter:,.0f}</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
        hc_xAxis( title = list(text = "Partner") ) %>% 
        hc_yAxis(title = list(text = "Total cash Transfers"), labels = FALSE ) 
    })
    
  })
}
# text for selected district on the map chart module ------------------------------------------------------
eprTextSelectedDistrict <- function(id, input_text){
  moduleServer(id, function(input, output, session){
    output$epr_selecteddistrict <- renderText({
      if(str_length(input_text) < 1){ paste("")
      } else{ paste("Selected District: ", stringr::str_to_title(input_text))  }
    })
    
  })
}
# epr default map module ------------------------------------------------------
eprDefaultMap <- function(id){
  moduleServer(id, function(input, output, session){
    output$epr_map  <-  renderLeaflet({
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
          onClick=JS("function(btn, epr_map){ epr_map.setView(new L.LatLng(1.3733,32.2903), 7.25) }")))
    })
  })
}
# epr dynamic map layer module ------------------------------------------------------
eprCreatingMap <- function(id, input_data){
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
    proxy = leafletProxy("epr_map", data = input_data) #%>% 
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
# epr dynamic map labels module ------------------------------------------------------
eprMapLabels <- function(id, input_data){
  moduleServer(id, function(input, output, session){
    # label districts in the map
    labels_district <- ~sprintf(
      "<strong>%s</strong>",
      ifelse(!is.na(cash_transfers_by_district),  stringr::str_to_title(ADM2_EN), "" ) 
    ) %>% 
      lapply(htmltools::HTML)
    # add labels on the map
    proxy = leafletProxy("epr_map", data=input_data ) 
    proxy %>%
      clearMarkers() %>%
      addLabelOnlyMarkers( label = labels_district, 
                           labelOptions = labelOptions(noHide = T, textOnly = TRUE)
      )
  })
}
# update quarter module ------------------------------------------------------
eprUpdateQuarter <- function(id, input_quarter_choices, input_selected){
  moduleServer(id, function(input, output, session){
    updateSelectInput(session, "epr_quarterperiod", 
                      label = "Select Quarter", 
                      choices = c("All", input_quarter_choices),
                      selected = input_selected)
  })
}
# update year module ------------------------------------------------------
eprUpdateYear <- function(id, input_year_choices, input_selected){
  moduleServer(id, function(input, output, session){
    updateSelectInput(session, "epr_yearperiod", 
                      label = "Select Year", 
                      choices = c("All", input_year_choices),
                      selected = input_selected)
  })
}

