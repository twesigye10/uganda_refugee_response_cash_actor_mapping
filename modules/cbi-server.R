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

# get psn data
cbiPSNDataServer <- function(id, input_data){
  moduleServer(id, function(input, output, session){
    
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
        across(everything(), ~sum(.x, na.rm = TRUE))
      ) %>% 
      rename_with(~gsub("ps.", "", .x, fixed=TRUE))
    
    return(df_data_indicators)
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
          cash_assistance_by_beneficiary_type = sum({{input_field_analysis}}, na.rm = T)
        ) 
      billboarder(data = df_billb_data) %>%
        bb_donutchart() %>% 
        bb_legend(position = 'bottom') %>%
        bb_donut(title = input_title, width = 70) %>% 
        bb_colors_manual(
          setNames(c('#E58606','#5D69B1','#52BCA3','#99C945'), c(input_beneficiary_vector))
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
               hcaes(x = select_month, y = total_amount_of_cash_by_quarter)) %>% 
        hc_tooltip(pointFormat = "<b>{point.total_amount_of_cash_by_quarter:,.0f}</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
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
          cash_transfer_by_delivery_mechanism = (cash_transfer_by_delivery_mechanism/over_all_total_cash)*100
        ) %>%
        arrange(-cash_transfer_by_delivery_mechanism) %>% 
        hchart(type = "bar",
               hcaes(x = select_delivery_mechanism, y = cash_transfer_by_delivery_mechanism), 
               dataLabels = list(enabled = TRUE, format="{point.cash_transfer_by_delivery_mechanism:.1f}%" )) %>%  
        hc_tooltip(pointFormat = "<b>{point.cash_transfer_by_delivery_mechanism:.1f}%</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
        hc_xAxis( title = list(text = NULL)) %>% 
        hc_yAxis(title = list(text = ""), labels = FALSE ) 
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
               dataLabels = list(enabled = TRUE, format="{point.total_cash_by_parter:,.0f}" )) %>%
        hc_tooltip(pointFormat = "<b>{point.total_cash_by_parter:,.0f}</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
        hc_xAxis( title = list(text = NULL) ) %>% 
        hc_yAxis(title = list(text = ""), labels = FALSE ) 
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
          across(everything(), ~sum(.x, na.rm = TRUE))
        ) %>% 
        rename_with(~gsub("ps.", "", .x, fixed=TRUE))
      

      div_data <- paste('<div class=\"table\">', glue("<h5> PSN households{display_in_title}</h5>"))
      thead_data <- paste('<table class=\"table\"> ')
      
      data_child_at_risk <- paste('<tr><td>', '<img src="child_at_risk.png" height="32"></img>'  ,'</td> <td>', "Child at Risk"  ,'</td> <td>', df_data_indicators %>% select(child_at_risk) %>% pull()   ,'</td> </tr>')
      data_disability <- paste('<tr><td>', '<img src="disability.png" height="32"></img>'  ,'</td> <td>', "Disability"  ,'</td> <td>', df_data_indicators %>% select(disability) %>% pull()   ,'</td> </tr>')
      data_Older_person_at_risk <- paste('<tr><td>', '<img src="Older_person_at_risk.png" height="32"></img>'  ,'</td> <td>', "Older Person at Risk"  ,'</td> <td>', df_data_indicators %>% select(Older_person_at_risk) %>% pull()   ,'</td> </tr>')
      data_serious_medical_condition <- paste('<tr><td>', '<img src="serious_medical_condition.png" height="32"></img>'  ,'</td> <td>', "Serious Medical Condition"  ,'</td> <td>', df_data_indicators %>% select(serious_medical_condition) %>% pull()   ,'</td> </tr>')
      data_single_parent_or_caregiver <- paste('<tr><td>', '<img src="single_parent_or_caregiver.png" height="32"></img>'  ,'</td> <td>', "Single Parent or Caregiver"  ,'</td> <td>', df_data_indicators %>% select(single_parent_or_caregiver) %>% pull()   ,'</td> </tr>')
      data_specific_legal_and_physical_protection_needs <- paste('<tr><td>', '<img src="specific_legal_and_physical_protection_needs.png" height="32"></img>'  ,'</td> <td>', "Specific Legal and Physical Protection needs"  ,'</td> <td>', df_data_indicators %>% select(specific_legal_and_physical_protection_needs) %>% pull()   ,'</td> </tr>')
      data_unaccompanied_or_separated_child <- paste('<tr><td>', '<img src="unaccompanied_or_separated_child.png" height="32"></img>'  ,'</td> <td>', "Unaccompanied or Separated Child"  ,'</td> <td>', df_data_indicators %>% select(unaccompanied_or_separated_child) %>% pull()   ,'</td> </tr>')
      data_woman_at_risk <- paste('<tr><td>', '<img src="woman_at_risk.png" height="32"></img>'  ,'</td> <td>', "Woman at Risk"  ,'</td> <td>', df_data_indicators %>% select(woman_at_risk) %>% pull()   ,'</td> </tr>')
      
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
                         options = providerTileOptions(minZoom = 7, maxZoom = 8), 
                         group="Esri Gray Canvas") %>% 
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
    proxy = leafletProxy("map", data = input_data)  
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
# update year module ------------------------------------------------------
cbiUpdateYear <- function(id, input_year_choices, input_selected){
  moduleServer(id, function(input, output, session){
    updateSelectInput(session, "yearperiod", 
                      label = "Select Year", 
                      choices = c("All", input_year_choices),
                      selected = input_selected)
  })
}

