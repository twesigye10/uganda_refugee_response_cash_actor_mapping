# Readme UI module -------------------------------------------------------------

readmeUI <- function(id){
  
              div(class="read-me-div",
                  h2("About"),
                  div(class="page-text",
                      p( span("This dashboard offers information on the who, what, when, and where (4Ws) 
                      of cash-based interventions in Uganda. It was developed by REACH Uganda, in collaboration with 
                      UNHCR and the Uganda Cash Working Group, with funding from USAID's Bureau for Humanitarian Assistance. 
                      The information presented here is based on partner activities as reported to the Activity Info platform 
                      from the first quarter in 2019 until present. Data and information displayed in the dashboard is updated 
                      on a quarterly basis following the Activity Info reporting cycle. Partners can update or submit new data 
                      about cash related activities they have implemented by following this "), strong( a("ActivityInfo.", style="color:#333", href ="https://www.activityinfo.org/login?page=%23database%2Fd0000010295%2Ff0000022161")), 
                      span("For inquiries on how to submit data to the platform please contact (UNHCR focal person)."))
                  ),
                  h2("How it works"),
                  
                  div(class="page-text",
                      p("The tabs at the top of the page allow you to view data on cash-based interventions within that sector. 
                      Every tab is accompanied by a short text explaining that sector. When you select a tab, the map and graphs change to 
                      show key indicators within that sector."),
                      p("The central map shows data by total transfer value in Ugandan Shillings (UGX)."),
                      p("By hovering over or clicking on a district, 
                      you are able to view sector data and indicators for that selected district."),
                      p("You can also filter and view data by year or quarter."),
                      p("After you have selected a particular district and you wish to return, 
                      click the National Overview button to reset the map and indicators. ")),
                  h2("Contact"),
                  div(class="page-text",
                      p( span("If you have comments or feedback on this dashboard, 
                      please share them with us at "), strong(a(href= "mailto:uganda@reach-initiative.org", "REACH", style="color:#333")) )
                  ),
                  div(class = "page-partners", img(height = 50, width = 40, src="LogoOPM.png"),
                               img(height = 40, width = 100, src="reach-logo-informing.png"),
                               img(height = 40, width = 100, src="UNHCR-visibility-horizontal-White-CMYK-v2015.png"),
                               img(height = 40, width = 100, src="USAID logo white.png"),
                               img(height = 40, width = 100, src="wfp-logo-standard-white-en_transparent.png"))
                  
              )

  
}

