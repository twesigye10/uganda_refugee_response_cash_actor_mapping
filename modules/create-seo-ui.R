# UI module -------------------------------------------------------------

tabPageSEOUI <- function(
  id, label, inp_yearperiod, inp_yearperiod_choices,
  inp_quarterperiod, inp_mapreset, inp_selecteddistrict, outp_hhreceivingcash,
  outp_plotcashquarter, outp_map, outp_data, outp_cvpd, outp_plotdeliverymechanism, outp_plotcashpartner){
  ns <- NS(id)
  
  tabPanel( label,
            # Sidebar
            sidebarLayout(
              # side panel
              sidebarPanel(width = 4,
                           fluidRow(
                             column(width = 4,
                                    selectInput(ns(inp_yearperiod), 
                                                "Select Year", 
                                                choices = c("All", unique(as.character(inp_yearperiod_choices))),
                                                selected = "All"
                                    )
                             ),
                             column(width = 4,
                                    selectInput(ns(inp_quarterperiod), 
                                                "Select Quarter", 
                                                choices = c("All"),
                                                selected = "All"
                                    )
                             ),
                             column(width = 4,
                                    actionButton(ns(inp_mapreset), "Reset Map"),
                                    textOutput(ns(inp_selecteddistrict))
                             ),
                             
                           ),
                           billboarderOutput(ns(outp_hhreceivingcash) ),
                           highchartOutput(ns(outp_plotcashquarter))
              ),
              # end side panel
              
              # main panel
              mainPanel(width = 8,
                        
                        fluidRow(
                          column(width = 8,
                                 # map
                                 leafletOutput(ns(outp_map), height = "100%")
                          ),
                          column(width = 4,
                                 # table age groups
                                 uiOutput(ns(outp_data)),
                                 # table cash value per day
                                 uiOutput(ns(outp_cvpd))
                          )
                        ),
                        
                        fluidRow(
                          column(width = 6,
                                 # Select Delivery Mechanism
                                 highchartOutput(ns(outp_plotdeliverymechanism))
                          ),
                          column(width = 6,
                                highchartOutput(ns(outp_plotcashpartner))
                          )
                        )
              )
              # end main panel
            )
            # end sidebar layout
  )
  
}

