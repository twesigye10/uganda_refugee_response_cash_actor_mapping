# Server modules ----------------------------------------------------------

# get year
aorYearValueServer <- function(id){
  moduleServer(id, function(input, output, session){
    return( reactive({input$aor_yearperiod})  )
  })
}
# get quarter
aorQuarterValueServer <- function(id){
  moduleServer(id, function(input, output, session){
    return( reactive({input$aor_quarterperiod})  )
  })
}
# reset map
aorResetMapServer <- function(id){
  moduleServer(id, function(input, output, session){
    return(
      input$aor_mapreset
    )
  })
}
# get clicked district
aorClickedDistrictValueServer <- function(id){
  moduleServer(id, function(input, output, session){
    click = input$aor_map_shape_click
    click_district <- click$id
    return( click_district )
  })
}
# donut chart module ------------------------------------------------------
aorDonutChartCashBeneficiary <- function(id, input_data, input_field_group,
                                      input_field_analysis, input_title, input_beneficiary_vector){
  moduleServer(id, function(input, output, session){
    output$aor_hhreceivingcash <-  renderBillboarder({
      df_billb_data <- input_data %>% 
        group_by({{input_field_group}} ) %>% 
        summarise(
          cash_assistance_by_beneficiary_type = sum({{input_field_analysis}}, na.rm = T),
          cash_assistance_by_beneficiary_type = round(cash_assistance_by_beneficiary_type,0)
        ) 
      billboarder(data = df_billb_data) %>%
        bb_donutchart() %>% 
        bb_legend(position = 'right', item = list(onclick = htmlwidgets::JS("function(e) {e.stopPropagation();}"))) %>%
        bb_tooltip(format = list(
          name =  htmlwidgets::JS("function(name, ratio, id, index) {return name;}"),
          value = htmlwidgets::JS("function(value) {return \"UGX('000) \"+ d3.format(',')(value);}")
        )) %>%
        bb_donut(title = input_title) %>% 
        bb_colors_manual(
          setNames(c('#F69E61','#0067A9','#A5C9A1','#72966E'), c(input_beneficiary_vector))
        )
    })
  })
}
# line chart cash transfer module ------------------------------------------------------
aorLineChartTotalCashQuarter <- function(id, input_data, input_field_analysis, input_field_year, 
                                      input_field_quarter, input_field_select_Month, 
                                       input_title){
  moduleServer(id, function(input, output, session){
    output$aor_plotcashquarter <-  renderHighchart({
      input_data %>%
        group_by({{input_field_year}}, {{input_field_quarter}}, {{input_field_select_Month}} ) %>%
        summarise(
          total_amount_of_cash_by_quarter = sum({{input_field_analysis}}, na.rm = T)
        ) %>%
        # arrange(select_quarter) %>% 
        hchart(type = "line",
               hcaes(x = select_quarter, y = total_amount_of_cash_by_quarter), color = "#7cb5ec") %>%
        hc_tooltip(pointFormat = "<b>{point.total_amount_of_cash_by_quarter:,.0f}</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left", style = list(fontSize = "12px", color = "#EE6768", useHTML = TRUE) )%>% 
        hc_xAxis( title = list(text = NULL) ) %>% 
        hc_yAxis(title = list(text = "")) 
    })
  })
}
# bar chart delivery mechanism module ------------------------------------------------------
aorBarChartDeliveryMechanism <- function(id, input_data, input_field_group, input_field_analysis,  
                                      input_title){
  moduleServer(id, function(input, output, session){
    output$aor_plotdeliverymechanism <-  renderHighchart({
      input_data %>%
        mutate(over_all_total_cash = sum({{input_field_analysis}})) %>% 
        group_by({{input_field_group}}, over_all_total_cash ) %>%
        summarise(
          cash_transfer_by_delivery_mechanism = sum({{input_field_analysis}}, na.rm = T),
          p_cash_transfer_by_delivery_mechanism = (cash_transfer_by_delivery_mechanism/over_all_total_cash)*100
        ) %>%
        arrange(-cash_transfer_by_delivery_mechanism) %>% 
        hchart(type = "bar",
               hcaes(x = delivery_mechanism, y = p_cash_transfer_by_delivery_mechanism),
               dataLabels = list(enabled = TRUE, format="{point.p_cash_transfer_by_delivery_mechanism:.1f}%" ), color = "#7cb5ec") %>%
        hc_tooltip(pointFormat = "<b>(UGX'000) {point.cash_transfer_by_delivery_mechanism:,.0f}</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left", style = list(fontSize = "12px", color = "#EE6768", useHTML = TRUE) )%>% 
        hc_xAxis( title = list(text = NULL) ) %>% 
        hc_yAxis(title = list(text = ""), labels = list(format = "{value}%") ) 
    })
  })
}
# bar chart cash by partner module ------------------------------------------------------
aorBarChartCashByPartner <- function(id, input_data, input_field_group, input_field_analysis,  
                                  input_title){
  moduleServer(id, function(input, output, session){
    output$aor_plotcashpartner <-  renderHighchart({
      input_data %>%
        group_by({{input_field_group}} ) %>%
        summarise(
          total_cash_by_parter = sum({{input_field_analysis}}, na.rm = T)
        ) %>%
        arrange(-total_cash_by_parter) %>% 
        hchart(type = "bar",
               hcaes(x = partner_name, y = total_cash_by_parter),
               dataLabels = list(enabled = TRUE, format="{point.total_cash_by_parter:,.0f}" ), color = "#7cb5ec") %>%
        hc_tooltip(pointFormat = "<b>{point.total_cash_by_parter:,.0f}</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left", style = list(fontSize = "12px", color = "#EE6768", useHTML = TRUE) )%>% 
        hc_xAxis( title = list(text = NULL) ) %>% 
        hc_yAxis(title = list(text = "") ) 
    })
    
  })
}
# text for selected district on the map chart module ------------------------------------------------------
aorTextSelectedDistrict <- function(id, input_text){
  moduleServer(id, function(input, output, session){
    output$aor_selecteddistrict <- renderText({
      if(str_length(input_text) < 1){ paste("")
      } else{ paste("Selected District: ", stringr::str_to_title(input_text))  }
    })
    
  })
}
# aor default map module ------------------------------------------------------
aorDefaultMap <- function(id){
  moduleServer(id, function(input, output, session){
    output$aor_map  <-  renderLeaflet({
      leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>% 
        addProviderTiles(providers$Esri.WorldGrayCanvas, 
                         options = providerTileOptions(minZoom = 6.75, maxZoom = 7.5), 
                         group="Esri Gray Canvas") %>% 
        setView(lng = 32.2903, lat= 1.3733, zoom = 6.75) %>% 
        addMiniMap( width = 80, height = 80, position = "bottomleft", zoomAnimation = TRUE,  toggleDisplay = TRUE) %>% 
        addEasyButton(easyButton(
          icon="fa-globe", title="Home",
          onClick=JS("function(btn, aor_map){ aor_map.setView(new L.LatLng(1.3733,32.2903), 6.75) }")))
    })
  })
}
# update quarter module ------------------------------------------------------
aorUpdateQuarter <- function(id, input_quarter_choices, input_selected){
  moduleServer(id, function(input, output, session){
    updateSelectInput(session, "aor_quarterperiod", 
                      label = "Select Quarter", 
                      choices = c("All", input_quarter_choices),
                      selected = input_selected)
  })
}
# update year module ------------------------------------------------------
aorUpdateYear <- function(id, input_year_choices, input_selected){
  moduleServer(id, function(input, output, session){
    updateSelectInput(session, "aor_yearperiod", 
                      label = "Select Year", 
                      choices = c("All", input_year_choices),
                      selected = input_selected)
  })
}

