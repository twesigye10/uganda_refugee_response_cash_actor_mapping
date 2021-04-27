# UI module -------------------------------------------------------------

tabPageUI <- function(
  id, label, inp_yearperiod, inp_yearperiod_choices,
  inp_quarterperiod, inp_mapreset, inp_selecteddistrict, outp_hhreceivingcash,
  outp_plotcashquarter, outp_map, outp_plotdeliverymechanism, outp_plotcashpartner){
  ns <- NS(id)
  
  tabPanel( label,
            # Sidebar
            sidebarLayout(
              # side panel
              sidebarPanel(
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
              mainPanel(
                
                # map
                leafletOutput(ns(outp_map), height = "60%"),
                
                fluidRow(
                  column(width = 6,
                         # Select Delivery Mechanism
                         highchartOutput(ns(outp_plotdeliverymechanism) )
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

