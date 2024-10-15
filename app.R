#Code developed for shiny app supporting FAO report
#Author: Denisse Fierro Arcos

# Loading R libraries -----------------------------------------------------
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(bslib)
library(cmocean)
library(scales)
library(sf)
library(rnaturalearth)
library(ggiraph)

options(sass.cache = FALSE)

# Loading data ------------------------------------------------------------
base_folder <- "/rd/gem/private/users/camillan/FAO_Report/"

#Loading ensemble biomass change
maps_data <- list.files(base_folder, 
                        "ensemble_perc_bio_change_data_map_tiles.csv", 
                        recursive = T, full.names = T) |> 
  read_csv()

#Ensemble percentage change in biomass by countries
count_bio <- list.files(base_folder, "ensemble_perc_bio_change_country.csv",
                        recursive = T, full.names = T) |> 
  read_csv() |> 
  rename(name = figure_name)

#Ensemble percentage change in biomass by FAO regions
fao_bio <- list.files(base_folder, "ensemble_perc_bio_change_fao_region.csv",
                      recursive = T, full.names = T) |> 
  read_csv() |> 
  rename(name = NAME_EN)

#Ensemble percentage change in biomass by LME regions
lme_bio <- list.files(base_folder, "ensemble_perc_bio_change_lme.csv",
                      recursive = T, full.names = T) |> 
  read_csv() |> 
  rename(name = name_merge)

#Table of summary statistics
table_stats_admin <- list.files(base_folder, "table_stats_country_admin.csv",
                                recursive = T, full.names = T) |> 
  read_csv()

#List of countries
country_list <- maps_data |> 
  distinct(figure_name) |> 
  drop_na() |> 
  rename("name"= "figure_name") |> 
  arrange(name)

#List of LMEs
lme_list <- maps_data |>
  distinct(name_merge) |>
  drop_na() |>
  rename("name"= "name_merge") |> 
  arrange(name)

#List of FAO regions
fao_list <- maps_data |>
  distinct(NAME_EN) |> 
  drop_na() |> 
  rename("name"= "NAME_EN") |> 
  arrange(name)

#Map of the world
world <- ne_countries(returnclass = "sf", scale = "medium")
world_360 <- list.files(base_folder, "world_360deg.shp",
                        recursive = T, full.names = T) |> 
  read_sf()

#Biomass change - World map
levs <- c("Decrease >30%", "Increase <10%",  "Decrease 20 to 30%", 
          "Increase 10 to 20%", "Decrease 10 to 20%", "Increase 20 to 30%", 
          "Decrease <10%",  "Increase >30%", "No data")

#Ensure category column is a factor and ordered
table_stats_admin_shp <- list.files(base_folder, 
                                    "biomass_shapefile_projected.shp",
                                    recursive = T, full.names = T) |> 
  read_sf() |>
  mutate(category = factor(category, levels = levs, ordered = T))

# Define colors for each fill category for the global summary maps
fill_colors <- c(
  "Decrease >30%" = "#AC390E",
  "Decrease 20 to 30%" = "#C4603E",
  "Decrease 10 to 20%" = "darksalmon",
  "Decrease <10%" = "wheat",
  "Increase <10%" = "honeydew3",
  "Increase 10 to 20%" = "lightblue2",
  "Increase 20 to 30%" = "#4297D3",
  "Increase >30%" = "#1B194B",
  "No data" = "#f7f7f7"
)

# Supporting information --------------------------------------------------
#Create custom-made color palette
scale_fill_custom <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                              option = "D", values = NULL, space = "Lab", 
                              na.value = "white", guide = "colourbar", 
                              aesthetics = "fill") {
  continuous_scale(aesthetics, 
                   palette = gradient_n_pal(c(cmocean("matter", start = 0.1, 
                                                      end = 0.8, 
                                                      direction = -1)(123),
                                              cmocean("delta", start = 0.49, 
                                                      end = 0.5)(20),
                                              cmocean("deep", start = 0.1, 
                                                      end = 0.8)(123)), values,
                                            space), 
                   na.value = na.value, guide = guide, ...)
}

#Define base steps for maps
base_map <- list(geom_tile_interactive(aes(tooltip = tooltip, data_id = rowid)),
                 scale_fill_binned(limits = c(-50, 50), n.breaks = 8,
                                   type = scale_fill_custom, oob = oob_squish,
                                   name = "% change in fish biomass"),
                 coord_cartesian(),
                 #Adding world
                 geom_sf(inherit.aes = F, data = world, lwd = 0.25,
                         color = "black", show.legend = F),
                 theme_bw(),
                 guides(fill = guide_colorbar(title.position = "top", 
                                              title.hjust = 0.5, barwidth = 20, 
                                              barheight = 2, 
                                              ticks.linewidth = 1, 
                                              frame.linewidth = 0.5,
                                              ticks.colour = "#444444",
                                              frame.colour = "#444444",
                                              title.theme = element_text(
                                                face = "plain", size = 13),
                                              label.theme = element_text(
                                                size = 13))),
                 theme(axis.title = element_blank(), 
                       panel.border = element_rect(colour = NA),
                       plot.title = element_text(hjust = 0.5),
                       legend.position = "bottom", 
                       title = element_text(size = 13, face = "bold"),
                       axis.text = element_text(size = 11)))

#Function to improve map ratios for plotting
scaler <- function(x, type, ratio = F){
  if((x > 0 & type == "min") | (x < 0 & type == "min")){
    x <- ifelse(ratio == T, x-3, x-6)
  }else if((x < 0 & type == "max") | (x > 0 & type == "max")){
    x <- ifelse(ratio == T, x+2, x+5)
  }else if(x == 0 & type == "min"){
    x <- ifelse(ratio == T, x-1, x-2)
  }else{
    x <- ifelse(ratio == T, x+1, x+2)
  }
  return(x)
}

# Defining user interface -------------------------------------------------
ui <- navbarPage(title = div(img(src = "FishMIP_white_no-bg_logo.png",
                                 height = 75, width = 225,
                                 style = "display: block; margin-left: auto; 
                                 margin-right: 15px; margin-bottom: 5px"), 
                             "Interactive Tool"),
                 inverse = T,
                 theme = bs_theme(bootswatch = "lux", font_scale = 1.1),
                 fluid = T,
                 tabPanel(title = "Global Map by Country",
                          titlePanel("World maps of projected fish biomass 
                                     change"),
                          "Here we present the mean estimated changes in fish \
                          biomass across the entire FishMIP ensemble \
                          (including 10 ecosystem models) in relation to our \
                          reference period (mean between 2005-2014) within the\
                          boundaries of a country's exclusive economic zone\
                          (EEZ).",
                          br(),
                          "Choose the scenario and decade of your interest for\
                          the interactive map to appear on your screen. Allow\
                          up to a minute for the map to show on your screen.",
                          br(),
                          "More info about interactive plot.",
                          br(),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons(inputId = "world_scenario",
                                           label = "Choose emissions scenario",
                                           choiceNames =
                                             c("SSP1-2.6 (low emissions)",
                                               "SSP5-8.5 (high emissions)"),
                                           choiceValues = c("ssp126", "ssp585"),
                                           selected = NULL
                              ),
                              radioButtons(inputId = "world_decade",
                                           "Choose decade of projected change",
                                           choiceNames = 
                                             c("2041-2050 (medium term)",
                                               "2091-2100 (long term)"),
                                           choiceValues = c("2041-2050",
                                                            "2091-2100"),
                                           selected = NULL
                              ),
                              p("Click the 'Download' button below to get the \
                                data used to create this map."),
                              #Download button
                              downloadButton(outputId = "download_world",
                                             label = "Download"
                              )
                            ),
                            mainPanel(
                              fluidRow(
                                girafeOutput(outputId = "plot_world")
                              )
                            )
                          )),
                 tabPanel(title = "Map by Marine Region",
                          titlePanel("Maps of projected fish biomass change"),
                          br(),
                          "Here we present the mean estimated changes in fish \
                          biomass across the entire FishMIP ensemble \
                          (including 10 ecosystem models) in relation to our \
                          reference period (mean between 2005-2014).",
                          br(),
                          br(),
                          "To see changes in the area of your interest, click \
                          on the group you want to visualise and select the \
                          area of your choice from the drop down list. You can \
                          also choose the emissions scenario and decade of \
                          decade of your interest.",
                          br(),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("sectors_maps",
                                           "Choose group you would like to \
                                           visualise",
                                           choiceNames =
                                             c("Exclusive Economic Zones (EEZs)",
                                               "FAO Major Fishing Areas",
                                               "Large Marine Ecosystems (LMEs)"
                                             ),
                                           choiceValues = c("EEZ", "FAO", 
                                                            "LME"),
                                           selected = NULL
                              ),
                              selectInput(inputId = "region_maps",
                                          label = "Choose your area of interest",
                                          choices = NULL
                              ),
                              radioButtons(inputId = "region_scenario",
                                           label = "Choose emissions scenario",
                                           choiceNames = 
                                             c("SSP1-2.6 (low emissions)",
                                               "SSP5-8.5 (high emissions)"),
                                           choiceValues = c("ssp126", "ssp585"),
                                           selected = NULL
                              ),
                              radioButtons(inputId = "region_decade",
                                           "Choose decade of projected change",
                                           choiceNames = 
                                             c("2041-2050 (medium term)",
                                               "2091-2100 (long term)"),
                                           choiceValues = c("2041-2050",
                                                            "2091-2100"),
                                           selected = NULL
                              ),
                              p("Click the 'Download' button below to get the \
                                data used to create the map shown on the \
                                right."),
                              #Download button
                              downloadButton(outputId = "download_map",
                                             label = "Download"
                              )
                            ),
                            mainPanel(
                              fluidRow(girafeOutput(outputId = "plot_maps1"))
                            )
                          )
                 ),
                 tabPanel(title = "Compare Scenarios Through Time",
                          titlePanel("Time series of fish biomass change"),
                          br(),
                          "Select the area of your interest to see how fish \
                          biomass is estimated to change until 2100 under two \
                          emissions scenarios: SSP1-2.6 and SSP5-8.5.",
                          br(),
                          br(),
                          "The estimated change shown in the plot is the mean \
                          percentage change for FishMIP model ensemble in \
                          relation to the historical reference period (mean for \
                          the decade between 2005 and 2014). The shaded areas \
                          show the standard deviation across the 10 ecosystem \
                          models that form the FishMIP ensemble.",
                          br(),
                          br(),
                          "The horizontal grey dashed line shows no difference \
                          between a particular year and the reference decade \
                          (2005-2014). The vertical grey line shows the end of \
                          the historical period and the different emissions \
                          scenarios.",
                          br(),
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("sectors_ts",
                                           "Choose group you would like to \
                                           visualise",
                                           choiceNames =
                                             c("Exclusive Economic Zones (EEZs)",
                                               "FAO Major Fishing Areas",
                                               "Large Marine Ecosystems (LMEs)"
                                             ),
                                           choiceValues = c("EEZ", "FAO",
                                                            "LME"),
                                           selected = NULL
                              ),
                              selectInput(inputId = "region_ts",
                                          label = "Choose your area of interest",
                                          choices = NULL
                              ),
                              p("Click the 'Download' button below to get the \
                                data used to create this time series plot."),
                              #Download button
                              downloadButton(outputId = "download_ts",
                                             label = "Download")
                            ),
                            mainPanel(
                              girafeOutput(outputId = "plot_ts")
                            ))
                 ),
                 tabPanel(title = "About",
                          titlePanel("About this website"),
                          "This tool shows estimates of fish biomass change
                          under two different future climate scenarios: low
                          emissions (SSP1-2.6) and high emissions (SSP5-8.5). 
                          For each scenario, results shown are the mean 
                          percentage change across 10 ecosystem model 
                          simulations making up the FishMIP ensemble.",
                          br(),
                          br(),
                          "This tool was developed by FishMIP researchers 
                          Denisse Fierro Arcos, Gage Clawson, Camilla Novaglio 
                          & Julia Blanchard based at the Institute for Marine 
                          & Antarctic Studies (IMAS) at University of Tasmania.
                          This tool supports the 'Climate Change Risks to Marine
                          Ecosystems and Fisheries' report for the FAO
                          published in July 2024, which can be accessed ",
                          tags$a(href="https://fishmip.org/publications.html",
                                 "here."),
                          br(),
                          br(),
                          h3("Who is FishMIP?"),
                          "The Fisheries and Marine Ecosystem Model 
                          Intercomparison Project (FishMIP) is an network of 
                          more than 100 marine ecosystem modellers and 
                          researchers from around the world. Our goal is to 
                          bring together our collective understanding to help 
                          better project the long-term impacts of climate change
                          on fisheries and marine ecosystems, and to use our 
                          findings to help inform policy. You can find more 
                          information about FishMIP on our ", 
                          tags$a(href="https://fishmip.org/", "website."),
                          br(),
                          br(),
                          h3("How should I use this tool?"),
                          "This site has three tabs that allow you to visualise
                          and download our data at different levels of detail.",
                          br(),
                          "- The", strong("'Global Map by Country'"), "tab 
                          consists of country-level summaries of projected 
                          changes in fish biomass by the FishMIP ensemble.",
                          br(),
                          "- Dive deeper into our data by exploring projected 
                          fish biomass changes under the ", 
                          strong("'Map Changes by Marine Region'"), " tab. Here,
                          you can map changes for the Country and Territory 
                          Exclusive Economic Zone, FAO Major Fishing Area, or 
                          Large Marine Ecosystem of your choice.",
                          br(),
                          "- Compare two climate scenarios trajectories (low 
                          and high emissions) through time for the marine 
                          region of your choice in the ", 
                          strong("'Compare Scenarios Through Time'"), " tab.",
                          br(),
                          br(),
                          h3("How should I cite data from this site?"),
                          "You can download the data used to create the plots
                          shown in this interactive tool using the 'Download' 
                          button included under each tab. As a condition of 
                          this tool to access data, you must cite its use. 
                          Please use the following citations:",
                          br(),
                          "- Fierro-Arcos D., Novaglio, C., Clawson, S.G., &
                          Blanchard J.L. (2024). Shiny app to explore FishMIP 
                          climate change projections by country and marine 
                          regions.",
                          br(),
                          "- Novaglio, C., Fierro-Arcos D., Clawson, S.G., 
                          Blanchard J.L. & FishMIP (2024). Data and code used 
                          to produce maps and projections in FAO Technical 
                          Paper 707.",
                          br(),
                          br(),
                          "When using the data product in a publication, please 
                          include the following citation in addition to the 
                          data product citations provided above:",
                          br(),
                          "Blanchard, J.L., Novaglio, C., eds. (2024). Climate 
                          change risks to marine ecosystems and fisheries: 
                          Future projections from the Fisheries and Marine 
                          Ecosystems Model Intercomparison Project. FAO 
                          Fisheries and Aquaculture Technical Paper No. 707. 
                          Rome, FAO.",
                          br(),
                          br(),
                          h3("How can I contact you?"),
                          "If you have any ideas on how to improve this app or
                          if you found any issues, you can ",
                          tags$a(href = "https://github.com/Fish-MIP/FAO_report_shiny/issues",
                                 "create an issue"),
                          " in our GitHub repository.",
                          br(),
                          br(),
                          h3("Acknowledgments"),
                          "The development of this tool was funded by the 
                          Australian Government through the Australian Research 
                          Council (ARC) Future Fellowship Project FT210100798. 
                          We gratefully acknowledge contributions from 
                          coordinators and contributing modellers of the FishMIP
                          and ISIMIP communities. We would also like to 
                          acknowledge the use of computing facilities provided
                          by Digital Research Services, IT Services at the 
                          University of Tasmania.",
                          br(),
                          br(),
                          fluidRow(
                            column(4, img(src = "IMAS_logo.png", height = 150,
                                          width = 300, style = "display: block; 
                                          margin-left: auto;
                                          margin-right: auto")),
                            column(4, img(src = "FishMIP_logo.png", 
                                          height = 150, width = 450, 
                                          style = "display: block;
                                          margin-left: auto; 
                                          margin-right: auto")),
                            column(4, img(src = "UN_OceanDecadeLogo_cropped.png", 
                                          height = 150, width = 300, 
                                          style = "display: block; 
                                          margin-left: auto; margin-bottom: 5px;
                                          margin-right: auto"))),
                          br()
                 )
                 )


server <- function(input, output, session) {
  
  ########## Global overview tab ----
  output$download_world <- downloadHandler(
    filename = function(){
      "table_stats_country_admin.csv"
    },
    #Creating name of download file based on original file name
    content = function(file){
      write_csv(table_stats_admin, file)
    }
  )
  
  #Select correct data 
  world_map_data <- reactive({
    data <- table_stats_admin_shp |>
      filter(scenario == input$world_scenario, decade == input$world_decade)
  })
  
  #Plot data
  output$plot_world <- renderGirafe({
    p1 <- ggplot()+
      geom_sf_interactive(data = world_map_data(), 
                          aes(fill = category, tooltip = tooltip, 
                              data_id = iso_code), show.legend = TRUE)+
      scale_fill_manual(values = fill_colors, breaks = levs, labels = levs, 
                        drop = F)+
      theme_bw()+
      theme(panel.border = element_rect(colour = NA),
            plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",
            title = element_text(size = 10, face = "bold"),
            axis.title = element_blank(),
            legend.key.height = unit(1, "mm"),
            legend.key.width = unit(1, "mm"))+
      labs(fill = "% change in fish biomass")+
      guides(fill = guide_legend(title.position = "top", title.hjust = 0.5,
                                 title.vjust = 1, nrow = 2, label.vjust = 1,
                                 label.position = "bottom", 
                                 label.hjust = 0.5))
    
    return(girafe(code = print(p1)) |>
             girafe_options(opts_zoom(max = 5),
                            opts_toolbar(hidden = c("zoom_rect")),
                            opts_hover(css = "stroke: gray1;stroke-width: 1px"),
                            opts_tooltip(opacity = .8),
                            opts_selection(type = "none", only_shiny = T)))
  })
  
  ########## Maps tab ----
  region_list_maps <- reactive({
    if(input$sectors_maps == "FAO"){
      data <- fao_list
    }else if(input$sectors_maps == "EEZ"){
      data <- country_list
    }else if(input$sectors_maps == "LME"){
      data <- lme_list
    }
  })
  
  observeEvent(region_list_maps(), {
    choices <- region_list_maps()$name
    updateSelectInput(inputId = "region_maps",
                      choices = choices)})
  
  maps_df <- reactive({
    if(input$sectors_maps == "LME"){
      df <- maps_data |>
        filter(name_merge == input$region_maps) |> 
        select(!c(figure_name, NAME_EN)) |> 
        rename("region_name" = "name_merge")
    }else if(input$sectors_maps == "FAO"){
      df <- maps_data |>
        filter(NAME_EN == input$region_maps) |> 
        select(!c(figure_name, name_merge)) |> 
        rename("region_name" = "NAME_EN")
    }else if(input$sectors_maps == "EEZ"){
      df <- maps_data |> 
        filter(figure_name == input$region_maps)|> 
        select(c(!NAME_EN, name_merge)) |> 
        rename("region_name" = "figure_name")
    }
    
    df <- df |> 
        filter(scenario == input$region_scenario & 
                 decade == input$region_decade)
    
    #Adjusting map proportions
    validate(
      need(df$longitude != "", 
           # display custom message 
           "Please wait while we render the map for your chosen area.")
      )
    minx <- min(df$longitude)
    maxx <- max(df$longitude)
    miny <- min(df$latitude)
    maxy <- max(df$latitude)
    rangex <- abs(abs(maxx)-abs(minx))
    rangey <- abs(abs(maxy)-abs(miny))
    if(rangex == 0 & str_detect(input$region_maps, 
                                "Americas|Europe|Antarct|France", negate = T)){
      df <- df |>
        mutate(longitude = longitude%%360)
      minx <- min(df$longitude)
      maxx <- max(df$longitude)
      rangex <- abs(abs(maxx)-abs(minx))
      base_map[[4]] <- geom_sf(inherit.aes = F, data = world_360, lwd = 0.25,
                               color = "black", show.legend = F)
    }else{
      base_map[[4]] <- geom_sf(inherit.aes = F, data = world, lwd = 0.25,
                               color = "black", show.legend = F)
    }
    
    if(rangex >= 1.15*rangey){
      ylims <- c(scaler(miny, "min"),
                 scaler(maxy, "max"))
      xlims <- c(scaler(minx, "min", ratio = T),
                 scaler(maxx, "max", ratio = T))
    }else if(rangey >= 1.15*rangex){
      xlims <- c(scaler(minx, "min"),
                 scaler(maxx, "max"))
      ylims <- c(scaler(miny, "min", ratio = T),
                 scaler(maxy, "max", ratio = T))
    }else{
      xlims <- c(scaler(minx, "min"),
                 scaler(maxx, "max"))
      ylims <- c(scaler(miny, "min"),
                 scaler(maxy, "max"))
    }
    return(list(df = df, 
                xlims = xlims, 
                ylims = ylims,
                base_map = base_map))
  })
  
  output$plot_maps1 <- renderGirafe({
    p1 <- ggplot(maps_df()$df, aes(x = longitude, y = latitude,
                                   fill = mean_change))+
      maps_df()$base_map+
      lims(x = maps_df()$xlims, y = maps_df()$ylims)+
      theme(axis.text.x = element_text(angle = 45, vjust = 0.765,
                                       hjust = 0.65, size = 8.75),
            axis.text.y = element_text(size = 8.75))

    return(girafe(code = print(p1)) |>
             girafe_options(opts_zoom(max = 5),
                            opts_toolbar(hidden = c("zoom_rect")),
                            opts_selection(type = "none", only_shiny = T)))
  })
  
  down_name_map <- reactive({
    region_name <- input$region_maps |> 
      #changing to lower case
      str_to_lower() |> 
      #removing accents
      iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') |> 
      #Removing parentheses
      str_remove_all("\\(|\\)") |> 
      #Replaces spaces " " with dashes "-"
      str_replace_all(" ", "-") |> 
      #Remove apostrophes in names
      str_replace("'", "")
    region_name <- str_c("ensemble_perc_change_fish_bio_", 
                         input$region_scenario, "_", input$region_decade, "_",
                         region_name, ".csv")
    return(region_name)
  })
  
  output$download_map <- downloadHandler(
    filename = function(){
      down_name_map()
    },
    #Creating name of download file based on original file name
    content = function(file){
      df <- maps_df()$df |> 
        select(!tooltip)
      write_csv(df, file)
    }
  )
  
  ########## Time series tab ----
  region_list <- reactive({
    if(input$sectors_ts == "EEZ"){
      data <- country_list
      df <- count_bio
    }else if(input$sectors_ts == "FAO"){
      data <- fao_list
      df <- fao_bio
    }else if(input$sectors_ts == "LME"){
      data <- lme_list
      df <- lme_bio
    }
    return(list(df = df,
                df_list = data))
  })
  
  observeEvent(region_list(), {
    choices <- region_list()$df_list$name
    updateSelectInput(inputId = "region_ts",
                      choices = choices)})
  
  down_name <- reactive({
    region_name <- input$region_ts |> 
      #changing to lower case
      str_to_lower() |> 
      #removing accents
      iconv(from = 'UTF-8', to = 'ASCII//TRANSLIT') |> 
      #Removing parentheses
      str_remove_all("\\(|\\)|\\.") |> 
      #Replaces spaces " " with dashes "-"
      str_replace_all(" ", "-") |> 
      #Remove apostrophes in names
      str_replace("'|,|--", "")
    region_name <- str_c("mean_ensemble_perc_change_fish_bio_timeseries_", 
                         region_name, "_1950-2100.csv")
    return(region_name)
  })
  
  ts_df <- reactive({
    df <- region_list()$df |> 
      filter(name == input$region_ts)
    return(df)
  })
  
  output$plot_ts <- renderGirafe({
    p <- ggplot(data = ts_df(), aes(x = year, y = mean_change, 
                                    colour = scenario, group = scenario))+
      geom_point_interactive(aes(tooltip = tooltip, data_id = year), 
                             size = 0.1, hover_nearest = T)+
      geom_line(linewidth = 0.5)+
      #Adding no change line at 0 for reference
      geom_hline_interactive(aes(tooltip = paste0("No difference from ",
                                                  "reference period"), 
                                 data_id = "Nodiff"), yintercept = 0, 
                             color = "grey80", linewidth = 0.65,
                             linetype = 2)+
      #Adding line dividing historical period and future projections
      geom_vline_interactive(aes(tooltip = 
                                   paste0("End of historical period, ",
                                          "start of emissions scenarios"), 
                                 data_id = "hist_ssp"),
                             xintercept = 2015, color = "grey80", 
                             linewidth = 0.65)+
      #Adding SD as shading
      geom_ribbon(aes(ymin = mean_change-sd_change,
                      ymax = mean_change+sd_change, fill = scenario),
                  alpha = 0.3, color = NA)+
      #Manually setting colours to be used in plots
      scale_color_manual(values = c("historical" = "black",
                                    "ssp126" = "#33bbee",
                                    "ssp585" = "#ee3377"),
                         name = "Scenarios",
                         labels = c("Historical", "SSP1-2.6", "SSP5-8.5"))+
      scale_fill_manual(values = c("historical" = "black",
                                   "ssp126" = "#33bbee",
                                   "ssp585" = "#ee3377"),
                        name = "Scenarios",
                        labels = c("Historical", "SSP1-2.6", "SSP5-8.5"))+
      guides(color = guide_legend(nrow = 1, title.position = "left"))+
      theme_classic()+
      scale_x_continuous(breaks = seq(1950, 2100, 10))+
      labs(y = "Change in exploitable fish biomass (%)")+
      theme(legend.position = "top", legend.justification = "center",
            legend.text = element_text(size = 10.5),
            legend.title = element_text(size = 10.5),
            panel.grid.minor.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 9.75, hjust = 0.2),
            axis.text.x = element_text(angle = 45, vjust = 0.765,
                                       hjust = 0.65, size = 10),
            axis.text.y = element_text(size = 10))
    
    return(girafe(ggobj = p, height_svg = 3) |> 
             girafe_options(opts_selection(type = "none", only_shiny = T)))
  })
  
  output$download_ts <- downloadHandler(
    filename = function(){
      down_name()
    },
    #Creating name of download file based on original file name
    content = function(file){
      df <- ts_df() |> 
        select(!tooltip)
      write_csv(df, file)
    }
  )
}

shinyApp(ui = ui, server = server)