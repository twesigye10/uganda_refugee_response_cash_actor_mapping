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
# get employment data
seoEmploymentDataServer <- function(id, input_data){
  moduleServer(id, function(input, output, session){
    
    df_data_indicators <- input_data %>%
      summarise( 
        em.employ_0_14_m = sum(people_engaged_in_short_term_employment_opportunities_male_0_14, na.rm = T),
        em.employ_0_14_f = sum(people_engaged_in_short_term_employment_opportunities_female_0_14, na.rm = T),
        em.employ_15_17_m = sum(people_engaged_in_short_term_employment_opportunities_male_15_17, na.rm = T),
        em.employ_15_17_f = sum(people_engaged_in_short_term_employment_opportunities_female_15_17, na.rm = T),
        em.employ_18_35_m = sum(people_engaged_in_short_term_employment_opportunities_male_18_35, na.rm = T),
        em.employ_18_35_f = sum(people_engaged_in_short_term_employment_opportunities_female_18_35, na.rm = T),
        em.employ_36_59_m = sum(people_engaged_in_short_term_employment_opportunities_male_36_59, na.rm = T),
        em.employ_36_59_f = sum(people_engaged_in_short_term_employment_opportunities_female_36_59, na.rm = T),
        em.employ_60_plus_m = sum(people_engaged_in_short_term_employment_opportunities_male_60, na.rm = T),
        em.employ_60_plus_f = sum(people_engaged_in_short_term_employment_opportunities_female_60, na.rm = T)
      ) %>% 
      select(starts_with("em.")) %>% 
      rename_with(~gsub("em.", "", .x, fixed=TRUE))
    
    return(df_data_indicators)
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
               hcaes(x = select_quarter, y = total_amount_of_cash_by_quarter)) %>%  
        hc_tooltip(pointFormat = "<b>{point.total_amount_of_cash_by_quarter:,.0f}</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
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
          cash_transfer_by_delivery_mechanism = (cash_transfer_by_delivery_mechanism/over_all_total_cash)*100
        ) %>%
        arrange(-cash_transfer_by_delivery_mechanism) %>% 
        hchart(type = "bar",
               hcaes(x = delivery_mechanism, y = cash_transfer_by_delivery_mechanism),
               dataLabels = list(enabled = TRUE, format="{point.cash_transfer_by_delivery_mechanism:.1f}%" )) %>%
        hc_tooltip(pointFormat = "<b>{point.cash_transfer_by_delivery_mechanism:.1f}%</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
        hc_xAxis( title = list(text = NULL) ) %>% 
        hc_yAxis(title = list(text = ""), labels = FALSE ) 
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
               dataLabels = list(enabled = TRUE, format="{point.total_cash_by_parter:,.0f}" )) %>%
        hc_tooltip(pointFormat = "<b>{point.total_cash_by_parter:,.0f}</b>" ) %>%
        hc_title( text = input_title, margin = 5, align = "left" )%>% 
        hc_xAxis( title = list(text = NULL) ) %>% 
        hc_yAxis(title = list(text = ""), labels = FALSE ) 
    })
    
  })
}
# table employment module ------------------------------------------------------
seoTableForEmploy <- function(id, input_data ){
  moduleServer(id, function(input, output, session){
    output$seotable <-  renderUI({
      
      div_data <- paste('<div class=\"table\">', glue("<h5> Employment by Age{display_in_title}</h5>"))
      thead_data <- paste('<table class=\"table\"> ')
      
      data_employ_0_14 <- paste('<tr><td>',  '0 - 14' ,'</td> <td>', '<img src="employ_0_14_f.png" height="32"></img>', input_data %>% select(employ_0_14_f) %>% pull() ,'</td> <td>','<img src="employ_0_14_m.png" height="32"></img>', input_data %>% select(employ_0_14_m) %>% pull() ,'</td> </tr>')
      data_employ_15_17 <- paste('<tr><td>', '15 - 17'  ,'</td> <td>', '<img src="employ_15_17_f.png" height="32"></img>', input_data %>% select(employ_15_17_f) %>% pull() ,'</td> <td>','<img src="employ_15_17_m.png" height="32"></img>', input_data %>% select(employ_15_17_m) %>% pull() ,'</td> </tr>')
      data_employ_18_35 <- paste('<tr><td>', '18 - 35'  ,'</td> <td>', '<img src="employ_18_35_f.png" height="32"></img>', input_data %>% select(employ_18_35_f) %>% pull() ,'</td> <td>','<img src="employ_18_35_m.png" height="32"></img>', input_data %>% select(employ_18_35_m) %>% pull() ,'</td> </tr>')
      data_employ_36_59 <- paste('<tr><td>',  '36 - 59' ,'</td> <td>', '<img src="employ_36_59_f.png" height="32"></img>', input_data %>% select(employ_36_59_f) %>% pull() ,'</td> <td>','<img src="employ_36_59_m.png" height="32"></img>', input_data %>% select(employ_36_59_m) %>% pull() ,'</td> </tr>')
      data_employ_60_plus <- paste('<tr><td>', '60 +'  ,'</td> <td>', '<img src="employ_60_plus_f.png" height="32"></img>', input_data %>% select(employ_60_plus_f) %>% pull() ,'</td> <td>','<img src="employ_60_plus_m.png" height="32"></img>', input_data %>% select(employ_60_plus_m) %>% pull() ,'</td> </tr>')
      
      tfooter_data <- paste('</table> </div>')
      
      HTML(paste(div_data, thead_data, data_employ_0_14, data_employ_15_17, 
                 data_employ_18_35, data_employ_36_59,  data_employ_60_plus, 
                 tfooter_data))
      
    })
  })
}
# table Cash value of transfer per individual per day module ------------------------------------------------------
seoTableForCVPD <- function(id, input_data ){
  moduleServer(id, function(input, output, session){
    output$seocvpdtable <-  renderUI({
      
      df_data_cvpd <- input_data %>%
        summarise( 
          cash_value_transfer_m = median(cash_value_of_transfer_per_individual_per_day_male_ugx, na.rm = T),
          cash_value_transfer_f = median(cash_value_of_transfer_per_individual_per_day_female_ugx, na.rm = T),
        )
      
      div_data <- paste('<div class=\"table\">', glue("<h5> Median Cash transfer per individual per day{display_in_title}</h5>"))
      thead_data <- paste('<table class=\"table\"> ')
      
      data_cvpd <- paste('<tr><td>', '<img src="cash_value_transfer_f.png" height="32"></img>', "UGX ", 
                         scales::comma_format()(df_data_cvpd %>% select(cash_value_transfer_f) %>% pull()) ,
                         '</td> <td>','<img src="cash_value_transfer_m.png" height="32"></img>',  "UGX ",
                         scales::comma_format()(df_data_cvpd %>% select(cash_value_transfer_m) %>% pull()) ,
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
                         options = providerTileOptions(minZoom = 7, maxZoom = 8), 
                         group="Esri Gray Canvas") %>% 
        setView(lng = 32.2903, lat= 1.3733, zoom = 7.25) %>% 
        addMiniMap( width = 100, height = 100, position = "bottomleft", zoomAnimation = TRUE,  toggleDisplay = TRUE) %>% 
        addEasyButton(easyButton(
          icon="fa-globe", title="Home",
          onClick=JS("function(btn, seo_map){ seo_map.setView(new L.LatLng(1.3733,32.2903), 7.25) }")))
    })
  })
}
# seo dynamic map layer module ------------------------------------------------------
seoCreatingMap <- function(id, input_data){
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
    proxy = leafletProxy("seo_map", data = input_data) 
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
# seo dynamic map labels module ------------------------------------------------------
seoMapLabels <- function(id, input_data){
  moduleServer(id, function(input, output, session){
    # label districts in the map
    labels_district <- ~sprintf(
      "<strong>%s</strong>",
      ifelse(!is.na(cash_transfers_by_district),  stringr::str_to_title(ADM2_EN), "" ) 
    ) %>% 
      lapply(htmltools::HTML)
    # add labels on the map
    proxy = leafletProxy("seo_map", data=input_data ) 
    proxy %>%
      clearMarkers() %>%
      addLabelOnlyMarkers( label = labels_district, 
                           labelOptions = labelOptions(noHide = T, textOnly = TRUE)
      )
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

