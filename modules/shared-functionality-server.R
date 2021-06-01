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
                                          inp_field_district, input_district_click){
  moduleServer(id, function(input, output, session){
    input_df %>% 
      filter({{inp_field_year}} == input_selected_year, {{inp_field_district}} == input_district_click  )
  })
}

# filter cash data by district
dfShapeDefault <- function(id, input_shape_data, input_cash_data, input_field_district, input_field_analysis, input_field_join_district){
  moduleServer(id, function(input, output, session){
    # UI selectors to filter shape data
    df_by_district_cash_data <- input_cash_data %>%
      select({{input_field_district}}, {{input_field_analysis}}) %>%
      group_by({{input_field_district}}) %>%
      summarise(cash_transfers_by_district = sum({{input_field_analysis}}, na.rm = T)) %>%
      filter(cash_transfers_by_district > 0)
    
    df_shape_data <- input_shape_data%>%
      left_join(df_by_district_cash_data, by = c("ADM2_EN" = input_field_join_district))
    return(
      df_shape_data
    )
  })
}

# dynamic map layer module ------------------------------------------------------
dynamicMapLayer <- function(id, input_map, input_data){
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
    
    # construct the dynamic map
    proxy = leafletProxy(input_map, data = input_data)  
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
                na.label = "<span class = \"nowrap\">Not Assessed </span>"
      )
  })
}

# Refugee hosting districts layer -----------------------------------------
refugeeHostLayer <- function(id, input_map, input_data){
  moduleServer(id, function(input, output, session){
    # Create a continuous palette function
    pal <- colorFactor( c("#F3F3F3", "#b6b6b7"), levels = c("Other Host", "None Host")
    )
    # construct the dynamic map
    proxy = leafletProxy(input_map, data = input_data)  
    proxy %>% 
      # clearControls() %>% 
      addPolygons(
        color = "white",
        options = pathOptions(clickable = FALSE),
        fillColor = ~pal(col_legenend_factor),
        fillOpacity = 0.7,
        weight = 1,
        opacity = 0.7,
        layerId = ~ADM2_EN,
        dashArray = "3"
      ) %>% 
      addLegend(position ="bottomright",
                pal = pal,
                values = ~col_legenend_factor,
                title = "Other Districsts",
                opacity  = 1
      )
  })
}

# dynamic map labels module ------------------------------------------------------
dynamicMapLabels <- function(id, input_map, input_data){
  moduleServer(id, function(input, output, session){
    # label districts in the map
    labels_district <- ~sprintf(
      "<strong>%s</strong>",
      stringr::str_to_title(ADM2_EN)
    ) %>% 
      lapply(htmltools::HTML)
    # add labels on the map
    proxy = leafletProxy(input_map, data=input_data ) 
    proxy %>%
      clearMarkers() %>%
      addLabelOnlyMarkers( label = labels_district, 
                           labelOptions = labelOptions(noHide = T, textOnly = TRUE)
      )
  })
}