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
          cash_assistance_by_beneficiary_type = sum({{input_field_analysis}}, na.rm = T),
          cash_assistance_by_beneficiary_type = round(cash_assistance_by_beneficiary_type,0)
        ) 
      billboarder(data = df_billb_data) %>%
        bb_donutchart() %>% 
        bb_legend(position = 'bottom') %>%
        bb_tooltip(format = list(
          name =  htmlwidgets::JS("function(name, ratio, id, index) {return name;}"),
          value = htmlwidgets::JS("function(value, ratio, id, index) {return 'UGX(000) '+value;}")
        )) %>%
        bb_donut(title = input_title, width = 70) %>% 
        bb_colors_manual(
          setNames(c('#F69E61','#0067A9','#A5C9A1','#72966E'), c(input_beneficiary_vector))
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
               hcaes(x = select_quarter, y = total_amount_of_cash_by_quarter), color = "#7cb5ec") %>%
        hc_tooltip(pointFormat = "<b>{point.total_amount_of_cash_by_quarter:,.0f}</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left", style = list(color = "#EE6768", useHTML = TRUE) )%>% 
        hc_xAxis( title = list(text = NULL) ) %>% 
        hc_yAxis(title = list(text = "")) 
    })
  })
}
# bar chart delivery mechanism module ------------------------------------------------------
eprBarChartDeliveryMechanism <- function(id, input_data, input_field_group, input_field_analysis,  
                                      input_title){
  moduleServer(id, function(input, output, session){
    output$epr_plotdeliverymechanism <-  renderHighchart({
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
        hc_tooltip(pointFormat = "<b>{point.cash_transfer_by_delivery_mechanism:,.0f} ({point.p_cash_transfer_by_delivery_mechanism:.1f}%)</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left", style = list(color = "#EE6768", useHTML = TRUE) )%>% 
        hc_xAxis( title = list(text = NULL) ) %>% 
        hc_yAxis(title = list(text = ""), labels = list(format = "{value}%") ) 
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
               dataLabels = list(enabled = TRUE, format="{point.total_cash_by_parter:,.0f}" ), color = "#7cb5ec") %>%
        hc_tooltip(pointFormat = "<b>{point.total_cash_by_parter:,.0f}</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left", style = list(color = "#EE6768", useHTML = TRUE) )%>% 
        hc_xAxis( title = list(text = NULL) ) %>% 
        hc_yAxis(title = list(text = "") ) 
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
                         options = providerTileOptions(minZoom = 7, maxZoom = 8), 
                         group="Esri Gray Canvas") %>% 
        setView(lng = 32.2903, lat= 1.3733, zoom = 7.25) %>% 
        addMiniMap( width = 100, height = 100, position = "bottomleft", zoomAnimation = TRUE,  toggleDisplay = TRUE) %>% 
        addEasyButton(easyButton(
          icon="fa-globe", title="Home",
          onClick=JS("function(btn, epr_map){ epr_map.setView(new L.LatLng(1.3733,32.2903), 7.25) }")))
    })
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

