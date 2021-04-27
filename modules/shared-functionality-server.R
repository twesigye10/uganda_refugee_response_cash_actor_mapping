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
      left_join(df_by_district_cash_data, by = c("ADM2_EN"=input_field_join_district))
    return(
      df_shape_data
    )
  })
  
}