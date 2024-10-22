# Server modules ----------------------------------------------------------

# get year
seoYearValueServer <- function(id){
  moduleServer(id, function(input, output, session){
    return( reactive({input$seo_yearperiod})  )
  })
}
# get quarter
seoQuarterValueServer <- function(id){
  moduleServer(id, function(input, output, session){
    return( reactive({input$seo_quarterperiod})  )
  })
}
# reset map
seoResetMapServer <- function(id){
  moduleServer(id, function(input, output, session){
    return(
      input$seo_mapreset
    )
  })
}
# get clicked district
seoClickedDistrictValueServer <- function(id){
  moduleServer(id, function(input, output, session){
    click = input$seo_map_shape_click
    click_district <- click$id
    return( click_district )
  })
}
# donut chart module ------------------------------------------------------
seoDonutChartCashBeneficiary <- function(id, input_data, input_field_group,
                                      input_field_analysis, input_title, input_beneficiary_vector){
  moduleServer(id, function(input, output, session){
    output$seo_hhreceivingcash <-  renderBillboarder({
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
        bb_donut(title = input_title, 
                 label = list( format = JS("function(value, ratio, id) { return d3.format('.0%')(ratio);}"))
        ) %>% 
        bb_colors_manual(
          setNames(c('#F69E61','#0067A9','#A5C9A1','#72966E'), c(input_beneficiary_vector))
        )
    })
  })
}
# line chart cash transfer module ------------------------------------------------------
seoLineChartTotalCashQuarter <- function(id, input_data, input_field_analysis, input_field_year, 
                                      input_field_quarter, input_field_select_Month, 
                                       input_title){
  moduleServer(id, function(input, output, session){
    output$seo_plotcashquarter <-  renderHighchart({
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
seoBarChartDeliveryMechanism <- function(id, input_data, input_field_group, input_field_analysis,  
                                      input_title){
  moduleServer(id, function(input, output, session){
    output$seo_plotdeliverymechanism <-  renderHighchart({
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
seoBarChartCashByPartner <- function(id, input_data, input_field_group, input_field_analysis,  
                                  input_title){
  moduleServer(id, function(input, output, session){
    output$seo_plotcashpartner <-  renderHighchart({
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
# table employment module ------------------------------------------------------
seoTableForEmploy <- function(id, input_data ){
  moduleServer(id, function(input, output, session){
    output$seotable <-  renderUI({
      
      df_data_indicators <- input_data %>%
        summarise( 
          em.employ_0_14_m = scales::comma(sum(people_engaged_in_short_term_employment_opportunities_male_0_14, na.rm = T), scale = 1, big.mark = ","),
          em.employ_0_14_f = scales::comma(sum(people_engaged_in_short_term_employment_opportunities_female_0_14, na.rm = T), scale = 1, big.mark = ","),
          em.employ_15_17_m = scales::comma(sum(people_engaged_in_short_term_employment_opportunities_male_15_17, na.rm = T), scale = 1, big.mark = ","),
          em.employ_15_17_f = scales::comma(sum(people_engaged_in_short_term_employment_opportunities_female_15_17, na.rm = T), scale = 1, big.mark = ","),
          em.employ_18_35_m = scales::comma(sum(people_engaged_in_short_term_employment_opportunities_male_18_35, na.rm = T), scale = 1, big.mark = ","),
          em.employ_18_35_f = scales::comma(sum(people_engaged_in_short_term_employment_opportunities_female_18_35, na.rm = T), scale = 1, big.mark = ","),
          em.employ_36_59_m = scales::comma(sum(people_engaged_in_short_term_employment_opportunities_male_36_59, na.rm = T), scale = 1, big.mark = ","),
          em.employ_36_59_f = scales::comma(sum(people_engaged_in_short_term_employment_opportunities_female_36_59, na.rm = T), scale = 1, big.mark = ","),
          em.employ_60_plus_m = scales::comma(sum(people_engaged_in_short_term_employment_opportunities_male_60, na.rm = T), scale = 1, big.mark = ","),
          em.employ_60_plus_f = scales::comma(sum(people_engaged_in_short_term_employment_opportunities_female_60, na.rm = T), scale = 1, big.mark = ",")
        ) %>% 
        select(starts_with("em.")) %>% 
        rename_with(~gsub("em.", "", .x, fixed=TRUE))
      
      div_data <- paste('<div class=\"table\">', glue("<h6> Employment by Age{display_in_title}</h6>"))
      thead_data <- paste('<table class=\"table\"> ')
      
      data_employ_0_14 <- paste('<tr><td>',  '0 - 14' ,'</td> <td>', 'Female: ', df_data_indicators %>% pull(employ_0_14_f) ,'</td> <td>','Male: ', df_data_indicators %>% pull(employ_0_14_m) ,'</td> </tr>')
      data_employ_15_17 <- paste('<tr><td>', '15 - 17'  ,'</td> <td>', 'Female: ', df_data_indicators %>% pull(employ_15_17_f) ,'</td> <td>','Male: ', df_data_indicators %>% pull(employ_15_17_m) ,'</td> </tr>')
      data_employ_18_35 <- paste('<tr><td>', '18 - 35'  ,'</td> <td>', 'Female: ', df_data_indicators %>% pull(employ_18_35_f) ,'</td> <td>','Male: ', df_data_indicators %>% pull(employ_18_35_m) ,'</td> </tr>')
      data_employ_36_59 <- paste('<tr><td>',  '36 - 59' ,'</td> <td>', 'Female: ', df_data_indicators %>% pull(employ_36_59_f) ,'</td> <td>','Male: ', df_data_indicators %>% pull(employ_36_59_m) ,'</td> </tr>')
      data_employ_60_plus <- paste('<tr><td>', '60 +'  ,'</td> <td>', 'Female: ', df_data_indicators %>% pull(employ_60_plus_f) ,'</td> <td>','Male: ', df_data_indicators %>% pull(employ_60_plus_m) ,'</td> </tr>')
      
      tfooter_data <- paste('</table> </div>')
      
      
      df_data_cvpd <- input_data %>%
        summarise( 
          cash_value_transfer_m = scales::comma(median(cash_value_of_transfer_per_individual_per_day_male_ugx, na.rm = T), scale = 1, big.mark = ","),
          cash_value_transfer_f = scales::comma(median(cash_value_of_transfer_per_individual_per_day_female_ugx, na.rm = T), scale = 1, big.mark = ",")
        )
      
      div_data_cvpd <- paste('<div class=\"table\">', glue("<h6> Median cash transfer per individual per day{display_in_title}</h6>"))
      thead_data_cvpd <- paste('<table class=\"table\"> ')
      
      data_cvpd <- paste('<tr><td>', 'Female: ', "UGX ", 
                         df_data_cvpd %>% pull(cash_value_transfer_f),
                         '</td> <td>','Male: ',  "UGX ",
                         df_data_cvpd %>% pull(cash_value_transfer_m),
                         '</td> </tr>')
      
      tfooter_data_cvpd <- paste('</table> </div>')
      
      HTML(paste(div_data_cvpd, thead_data_cvpd, data_cvpd, tfooter_data_cvpd))
      
      HTML(paste(div_data, thead_data, data_employ_0_14, data_employ_15_17, 
                 data_employ_18_35, data_employ_36_59,  data_employ_60_plus, 
                 tfooter_data, div_data_cvpd, thead_data_cvpd, data_cvpd, tfooter_data_cvpd))
      
    })
  })
}
# table Cash value of transfer per individual per day module ------------------------------------------------------
seoTableForCVPD <- function(id, input_data ){
  moduleServer(id, function(input, output, session){
    output$seocvpdtable <-  renderUI({
      
      df_data_cvpd <- input_data %>%
        summarise( 
          cash_value_transfer_m = scales::comma(median(cash_value_of_transfer_per_individual_per_day_male_ugx, na.rm = T), scale = 1, big.mark = ","),
          cash_value_transfer_f = scales::comma(median(cash_value_of_transfer_per_individual_per_day_female_ugx, na.rm = T), scale = 1, big.mark = ",")
        )
      
      div_data <- paste('<div class=\"table\">', glue("<h6> Median cash transfer per individual per day{display_in_title}</h6>"))
      thead_data <- paste('<table class=\"table\"> ')
      
      data_cvpd <- paste('<tr><td>', 'Female: ', "UGX ", 
                         df_data_cvpd %>% pull(cash_value_transfer_f),
                         '</td> <td>','Male: ',  "UGX ",
                         df_data_cvpd %>% pull(cash_value_transfer_m),
                         '</td> </tr>')

      tfooter_data <- paste('</table> </div>')
      
      HTML(paste(div_data, thead_data, data_cvpd, tfooter_data))
      
    })
  })
}
# text for selected district on the map chart module ------------------------------------------------------
seoTextSelectedDistrict <- function(id, input_text){
  moduleServer(id, function(input, output, session){
    output$seo_selecteddistrict <- renderText({
      if(str_length(input_text) < 1){ paste("")
      } else{ paste("Selected District: ", stringr::str_to_title(input_text))  }
    })
    
  })
}
# seo default map module ------------------------------------------------------
seoDefaultMap <- function(id){
  moduleServer(id, function(input, output, session){
    output$seo_map  <-  renderLeaflet({
      leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>% 
        addProviderTiles(providers$Esri.WorldGrayCanvas, 
                         options = providerTileOptions(minZoom = 6.75, maxZoom = 7.5), 
                         group="Esri Gray Canvas") %>% 
        setView(lng = 32.2903, lat= 1.3733, zoom = 6.75) %>% 
        addMiniMap( width = 80, height = 80, position = "bottomleft", zoomAnimation = TRUE,  toggleDisplay = TRUE) %>% 
        addEasyButton(easyButton(
          icon="fa-globe", title="Home",
          onClick=JS("function(btn, seo_map){ seo_map.setView(new L.LatLng(1.3733,32.2903), 6.75) }")))
    })
  })
}
# update quarter module ------------------------------------------------------
seoUpdateQuarter <- function(id, input_quarter_choices, input_selected){
  moduleServer(id, function(input, output, session){
    updateSelectInput(session, "seo_quarterperiod", 
                      label = "Select Quarter", 
                      choices = c("All", input_quarter_choices),
                      selected = input_selected)
  })
}
# update year module ------------------------------------------------------
seoUpdateYear <- function(id, input_year_choices, input_selected){
  moduleServer(id, function(input, output, session){
    updateSelectInput(session, "seo_yearperiod", 
                      label = "Select Year", 
                      choices = c("All", input_year_choices),
                      selected = input_selected)
  })
}

