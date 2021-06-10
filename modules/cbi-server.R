# CBI Server modules ----------------------------------------------------------

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
    return(
      input$mapreset
    )
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

# donut chart module ------------------------------------------------------
cbiDonutChartCashBeneficiary <- function(id, input_data, input_field_group,
                                      input_field_analysis, input_title, input_beneficiary_vector){
  moduleServer(id, function(input, output, session){
    output$hhreceivingcash <-  renderBillboarder({
      df_billb_data <- input_data %>% 
        group_by({{input_field_group}} ) %>% 
        summarise(
          cash_assistance_by_beneficiary_type = sum({{input_field_analysis}}, na.rm = T),
          cash_assistance_by_beneficiary_type = round(cash_assistance_by_beneficiary_type,0)
        ) 
      billboarder(data = df_billb_data) %>%
        bb_donutchart() %>% 
        bb_legend(position = 'right', item = list(onclick = htmlwidgets::JS("function(e) {e.stopPropagation();}")) ) %>%
        bb_tooltip(format = list(
          name =  htmlwidgets::JS("function(name, ratio, id, index) {return name;}"),
          value = htmlwidgets::JS("function(value) {return \"UGX('000) \"+ d3.format(',')(value);}")
        )) %>% 
        bb_donut(title = input_title, padding = list(top = 10, bottom = 10, left = 10, right = 10) , style = list(fontSize = "12px", color = "#EE6768")) %>% 
        bb_colors_manual(
          setNames(c('#F69E61','#0067A9','#A5C9A1','#72966E'), c(input_beneficiary_vector))
        )
    })
  })
}
# line chart cash transfer module ------------------------------------------------------
cbiLineChartTotalCashQuarter <- function(id, input_data, input_field_analysis, input_field_year, 
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
               hcaes(x = select_month, y = total_amount_of_cash_by_quarter), color = "#7cb5ec") %>% 
        hc_tooltip(pointFormat = "<b>{point.total_amount_of_cash_by_quarter:,.0f}</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left", style = list(fontSize = "12px", color = "#EE6768", useHTML = TRUE) )%>% 
        hc_xAxis( title = list(text = NULL) ) %>% 
        hc_yAxis(title = list(text = ""))
    })
    
  })
}
# bar chart delivery mechanism module ------------------------------------------------------
cbiBarChartDeliveryMechanism <- function(id, input_data, input_field_group, input_field_analysis,  
                                      input_title){
  moduleServer(id, function(input, output, session){
    output$plotdeliverymechanism <-  renderHighchart({
      input_data %>%
        mutate(over_all_total_cash = sum({{input_field_analysis}})) %>% 
        group_by({{input_field_group}}, over_all_total_cash ) %>%
        summarise(
          cash_transfer_by_delivery_mechanism = sum({{input_field_analysis}}, na.rm = T),
          p_cash_transfer_by_delivery_mechanism = (cash_transfer_by_delivery_mechanism/over_all_total_cash)*100
        ) %>%
        arrange(-cash_transfer_by_delivery_mechanism) %>% 
        hchart(type = "bar",
               hcaes(x = select_delivery_mechanism, y = p_cash_transfer_by_delivery_mechanism),
               dataLabels = list(enabled = TRUE, format="{point.p_cash_transfer_by_delivery_mechanism:.1f}%" ), color = "#7cb5ec") %>%  
        hc_tooltip(pointFormat = "<b>(UGX'000) {point.cash_transfer_by_delivery_mechanism:,.0f}</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left", style = list(fontSize = "12px", color = "#EE6768", useHTML = TRUE) )%>% 
        hc_xAxis( title = list(text = NULL)) %>% 
        hc_yAxis(title = list(text = ""), labels = list(format = "{value}%") ) 
    })
  })
}
# bar chart cash by partner module ------------------------------------------------------
cbiBarChartCashByPartner <- function(id, input_data, input_field_group, input_field_analysis,  
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
               hcaes(x = partner_name, y = total_cash_by_parter),
               dataLabels = list(enabled = TRUE, format="{point.total_cash_by_parter:,.0f}" ), color = "#7cb5ec") %>%
        hc_tooltip(pointFormat = "<b>{point.total_cash_by_parter:,.0f}</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left", style = list(fontSize = "12px", color = "#EE6768", useHTML = TRUE) )%>% 
        hc_xAxis( title = list(text = NULL) ) %>% 
        hc_yAxis(title = list(text = "") ) 
    })
  })
}
# table PSN module ------------------------------------------------------
cbiDataForPSN <- function(id, input_data ){
  moduleServer(id, function(input, output, session){
    output$psndata <-  renderUI({
      
      df_data_indicators <- input_data %>%
        rowwise() %>%
        mutate( 
          ps.child_at_risk = sum(psn_households_receiving_cash_assistance_for_basic_needs_child_at_risk,
                                 psn_households_receiving_voucher_assistance_for_basic_needs_child_at_risk, na.rm = T),
          ps.disability = sum(psn_households_receiving_cash_assistance_for_basic_needs_disability,   
                              psn_households_receiving_voucher_assistance_for_basic_needs_disability, na.rm = T),
          ps.Older_person_at_risk = sum(psn_households_receiving_cash_assistance_for_basic_needs_older_person_at_risk,                           
                                        psn_households_receiving_voucher_assistance_for_basic_needs_older_person_at_risk, na.rm = T),
          ps.serious_medical_condition = sum(psn_households_receiving_cash_assistance_for_basic_needs_serious_medical_condition,                      
                                             psn_households_receiving_voucher_assistance_for_basic_needs_serious_medical_condition, na.rm = T),
          ps.single_parent_or_caregiver = sum(psn_households_receiving_cash_assistance_for_basic_needs_single_parent_or_caregiver,                     
                                              psn_households_receiving_voucher_assistance_for_basic_needs_single_parent_or_caregiver, na.rm = T),
          ps.specific_legal_and_physical_protection_needs = sum(psn_households_receiving_cash_assistance_for_basic_needs_specific_legal_and_physical_protection_needs,   
                                                                psn_households_receiving_voucher_assistance_for_basic_needs_specific_legal_and_physical_protection_needs, na.rm = T),
          ps.unaccompanied_or_separated_child = sum(psn_households_receiving_cash_assistance_for_basic_needs_unaccompanied_or_separated_child,               
                                                    psn_households_receiving_voucher_assistance_for_basic_needs_unaccompanied_or_separated_child, na.rm = T),
          ps.woman_at_risk = sum(psn_households_receiving_cash_assistance_for_basic_needs_woman_at_risk,
                                 psn_households_receiving_voucher_assistance_for_basic_needs_woman_at_risk, na.rm = T)
        ) %>% 
        ungroup() %>% 
        select(starts_with("ps.")) %>% 
        summarise(
          across(everything(), ~scales::comma(sum(.x, na.rm = TRUE), scale = 1, big.mark = ","))
        ) %>% 
        rename_with(~gsub("ps.", "", .x, fixed=TRUE))
      

      div_data <- paste('<div class=\"table\">', glue("<h6> Persons with Specific Needs (PSN) households reported as receiving some form of cash assistance{display_in_title}</h6>"))
      thead_data <- paste('<table class=\"table\"> ')
      
      data_child_at_risk <- paste('<tr><td>', "Child at Risk"  ,'</td> <td>', df_data_indicators %>% pull(child_at_risk)   ,'</td> </tr>')
      data_disability <- paste('<tr><td>', "Disability"  ,'</td> <td>', df_data_indicators %>% pull(disability)   ,'</td> </tr>')
      data_Older_person_at_risk <- paste('<tr><td>', "Older Person at Risk"  ,'</td> <td>', df_data_indicators %>% pull(Older_person_at_risk)   ,'</td> </tr>')
      data_serious_medical_condition <- paste('<tr><td>', "Serious Medical Condition"  ,'</td> <td>', df_data_indicators %>% pull(serious_medical_condition)   ,'</td> </tr>')
      data_single_parent_or_caregiver <- paste('<tr><td>', "Single Parent or Caregiver"  ,'</td> <td>', df_data_indicators %>% pull(single_parent_or_caregiver)   ,'</td> </tr>')
      data_specific_legal_and_physical_protection_needs <- paste('<tr><td>', "Specific Legal and Physical Protection needs"  ,'</td> <td>', df_data_indicators %>% pull(specific_legal_and_physical_protection_needs)   ,'</td> </tr>')
      data_unaccompanied_or_separated_child <- paste('<tr><td>', "Unaccompanied or Separated Child"  ,'</td> <td>', df_data_indicators %>% pull(unaccompanied_or_separated_child)   ,'</td> </tr>')
      data_woman_at_risk <- paste('<tr><td>', "Woman at Risk"  ,'</td> <td>', df_data_indicators %>% pull(woman_at_risk)   ,'</td> </tr>')
      
      tfooter_data <- paste('</table> </div>')
      
      HTML(paste(div_data, thead_data, data_child_at_risk, data_disability, 
                 data_Older_person_at_risk, 
                 data_serious_medical_condition, 
                 data_single_parent_or_caregiver, 
                 data_specific_legal_and_physical_protection_needs, 
                 data_unaccompanied_or_separated_child, 
                 data_woman_at_risk, tfooter_data))

    })
  })
}
# text for selected district on the map chart module ------------------------------------------------------
cbiTextSelectedDistrict <- function(id, input_text){
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
                         options = providerTileOptions(minZoom = 6.75, maxZoom = 7.5), 
                         group="Esri Gray Canvas") %>% 
        setView(lng = 32.2903, lat= 1.3733, zoom = 6.75) %>% 
        addMiniMap( width = 80, height = 80, position = "bottomleft", zoomAnimation = TRUE,  toggleDisplay = TRUE) %>% 
        addEasyButton(easyButton(
          icon="fa-globe", title="Home",
          onClick=JS("function(btn, map){ map.setView(new L.LatLng(1.3733,32.2903), 6.75) }")))
    })
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
# update year module ------------------------------------------------------
cbiUpdateYear <- function(id, input_year_choices, input_selected){
  moduleServer(id, function(input, output, session){
    updateSelectInput(session, "yearperiod", 
                      label = "Select Year", 
                      choices = c("All", input_year_choices),
                      selected = input_selected)
  })
}

