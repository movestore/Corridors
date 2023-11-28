library("shiny")
library("shinydashboard")
library("fresh")
library("move")
# library("ggplot2")
library("shinyBS") ## to display message when hovering over input element in UI
library("lubridate")
# library("geosphere")
# library("dismo")
# library("rgeos")
library("stringr")
library("shinyWidgets")
# library("ggsn")
library("shinycssloaders")
# library("maptools")
# library("circular")
# 
# 
library("leaflet")
# library(viridis)
# library(plyr)
# library(dplyr)

## TODO
# - make un-/selection of corridors possible, or at least by number of segments
# - make it possible to save the selected corridors within the object
# - make it posible to see all track on one map

## https://www.youtube.com/watch?v=gGEY82qA3BI  ##  modules
## module shinydashboard site:stackoverflow.com


shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    dashboardPage(
      dashboardHeader(title = "Corridors per individual",titleWidth=300),
      dashboardSidebar(uiOutput(ns("SidebarUI")),
                       tags$style( ## to make a vertical scroll bar on the sidebar so all tabs can be accessed while seeing the main panel
                         "#sidebarItemExpanded {
                      overflow: auto;
                      height: calc(100vh - 50px) !important;
                     }")),
      dashboardBody(uiOutput(ns("TabUI")),
                    use_theme(create_theme(
                      adminlte_color(
                        light_blue = "#46505a"
                      ),
                      adminlte_sidebar(
                        width = "300px",
                        dark_bg = "#343e48",
                        dark_hover_bg = "#2879ca",
                        dark_color = "#e1eaf2"
                      ),
                      adminlte_global(
                        content_bg = "#dfe7ef"
                      )))
      )
    )
  )
}

shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  current <- reactiveVal(data)
  
  namesCorresp <- data.frame(nameInd=namesIndiv(data) , tabIndv=str_replace_all(namesIndiv(data), "[^[:alnum:]]", ""))
  ntabs <- length(namesIndiv(data))
  tabnames <- str_replace_all(namesIndiv(data), "[^[:alnum:]]", "") #it does not allow punctuation or spaces
  speedPropnames <- paste0(tabnames, '_speedProp') 
  circPropnames <- paste0(tabnames, '_circProp') 
  timeThinnames <- paste0(tabnames, '_timeThin')
  # clustDistnames <- paste0(tabnames, '_clustDist')
  updateButton <- paste0(tabnames,'_updateButton')
  plotnames <- paste0("plot_",tabnames) 
  
  
  output$SidebarUI <- renderUI({
    Menus <- vector("list", ntabs)
    for(i in 1:ntabs){
      Menus[[i]] <-   menuItem(tabnames[i], icon=icon("paw"), tabName = tabnames[i], selected = i==1) }
    do.call(function(...) sidebarMenu(id = ns('sidebarMenuUI'),...), Menus)
  })
  
  output$TabUI <- renderUI({
    Tabs <- vector("list", ntabs)
    for(i in 1:ntabs){
      Tabs[[i]] <- tabItem(tabName = tabnames[i],
                           fluidRow( ## make the same as in "population corridors"
                             column(3,sliderInput(inputId=ns(speedPropnames[i]),label="Speed", min=0,max=1, value=0.75, step=0.01),
                                    bsTooltip(id=ns(speedPropnames[i]), title="Proportion of speeds which are high enough to be a valid corridor point (default: speeds that are greater than 75 % of all speeds)", placement = "bottom", trigger = "hover", options = list(container = "body"))),
                             column(3,sliderInput(inputId=ns(circPropnames[i]),label="Parallelism", min=0,max=1, value=0.25, step=0.01),
                                    bsTooltip(id=ns(circPropnames[i]), title="Proportion of the circular variances that is low enough to be a valid corridor point. Low values indicate that the segments are (near) parallel (default: variances that are lower than 25 % of all variances)", placement = "bottom", trigger = "hover", options = list(container = "body"))
                             ), ## maybe change wording to make it simpler: the lower the value, the more parallel are the segments
                             column(2, numericInput(ns(timeThinnames[i]),"Thin track to X mins",min=0,max=60, value=0, step=1),
                                    bsTooltip(id=ns(timeThinnames[i]), title="This is specially recommended for high resolution tracks to ease finding regions with parallel segments. Default (=0) no thinning", placement = "bottom", trigger = "hover", options = list(container = "body"))
                             ),
                             # column(3, numericInput(ns(clustDistnames[i]),"Radius of corridor cluster circles (mts)",value=300),
                             #        bsTooltip(id=ns(clustDistnames[i]), title="All identified corridor segments that fall within a circle will be grouped as a corridor cluster", placement = "bottom", trigger = "hover", options = list(container = "body"))
                             #        ),
                             column(1,offset=3,actionBttn(ns(updateButton[i]), label="Update!", style="fill", color="success",icon=icon("redo"),size="md"))
                           ),
                           withSpinner(leafletOutput(ns(plotnames[i]), height = "85vh"), type=5, size=1.5,color= "#28b78d") ## color the same as update button
      )
    }
    do.call(tabItems, Tabs)
  })
  
  ## get input values for each tab
  RVtab <- reactiveValues()
  
  observe({
    RVtab$indv <- namesCorresp$nameInd[namesCorresp$tabIndv==input$sidebarMenuUI]
  })
  ## get input values for each tab after hitting update button
  RVupdate <- reactiveValues()
  observeEvent({input[[paste0(input$sidebarMenuUI, '_updateButton')]]},{
    RVupdate$thintime <- input[[paste0(input$sidebarMenuUI, '_timeThin')]]
    RVupdate$speedProp <- input[[paste0(input$sidebarMenuUI, '_speedProp')]]
    RVupdate$circProp <- input[[paste0(input$sidebarMenuUI, '_circProp')]]
    # RVupdate$clustDist <- input[[paste0(input$sidebarMenuUI, '_clustDist')]]
  })
  
  for(i in 1:ntabs){
    output[[plotnames[i]]] <- renderLeaflet({
      dataSubInd <-  data[[RVtab$indv]]
      if(!input[[paste0(input$sidebarMenuUI, '_updateButton')]]){ ## default plot
        
        dataSubIndTime <- dataSubInd ## no thining by time
        
        ## calculating corridors and extracting some data for plots
        corridorCalc <- corridor(x=dataSubIndTime, speedProp=.75, circProp=.25, plot=FALSE)#, minNBsegments = 4)
        corridorCalc$LocID <- 1:n.locs(corridorCalc)
        indDF <- data.frame(long=coordinates(corridorCalc)[,1],lat=coordinates(corridorCalc)[,2],burstId=c(as.character(burstId(corridorCalc)),NA),LocID=corridorCalc$LocID)
        corrDFr <- which(indDF$burstId%in%c("corridor"))
        
        if(length(corrDFr)<=1){  ## if levels do not contain "corridor" OR if there is only 1 corridor point identified it cannot be clustered and gives an error
          ## to create the text box no corridors found
          tag.map.title <- tags$style(HTML(".leaflet-control.map-title {
          transform: translate(-50%,20%);
          position: fixed !important;
          left: 50%;
          text-align: center;
          padding-left: 10px; 
          padding-right: 10px; 
          background: rgba(255,255,255,0.75);
          font-weight: bold;
          font-size: 28px;
          color: black;
          }"))
          title <- tags$div(
            tag.map.title, HTML("No corridors found")
          )  
          
          map1 <- leaflet(corridorCalc) %>% addTiles()%>%
            addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
            addProviderTiles("Esri.WorldImagery", group = "Aerial") %>% 
            addPolylines(lng = coordinates(corridorCalc)[,1],lat = coordinates(corridorCalc)[,2],weight=2, opacity=0.7, layerId=namesIndiv(corridorCalc),group =namesIndiv(corridorCalc))
          map1 %>%
            addControl(title, position = "topleft", className="map-title") %>% # to add the ext box of corridors not found
            addScaleBar(position="bottomright",
                        options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
            addLayersControl(
              baseGroups = c("TopoMap","Aerial"),
              # overlayGroups = c("indiv","corr"),
              options = layersControlOptions(collapsed = FALSE))
          
        } else { ## there are corridors found
          
          ### FIND A WAY TO SELECT/UNSELECT CORRIDORS
          # corridor segment midpoints
          # midCrPts <- corridorCalc@data[burstId(corridorCalc)=="corridor",c("segMid_x","segMid_y","LocID")]
          # coordinates(midCrPts) <- ~segMid_x+segMid_y
          # projection(midCrPts) <- projection(dataSubIndTime)
          # ## distance matrix
          # dist <- distm(midCrPts, fun=distGeo)
          # ## clustering
          # hc <- hclust(as.dist(dist), method="single") # complete single
          # # define clusters based on a tree "height" cutoff distance (distance between corridor clusters) and add them to the SpDataFrame
          # midCrPts$clusterID <- cutree(hc, h=300) # h=RVupdate$clustDist
          # # add nb of corridor segments per cluster to SpDataFrame
          # tbCount <- data.frame(table(midCrPts$clusterID))
          # midCrPts$nbCorrInClust <- NA
          # for(j in tbCount$Var1){
          #   midCrPts$nbCorrInClust[midCrPts$clusterID==j] <- tbCount$Freq[tbCount$Var1==j]
          # }
          # #HERE##### make here clusters selecable, unselectable, remove those with lesss than X points, etc
          # midCrPts <- midCrPts[midCrPts$nbCorrInClust>1,]
          # 
          # # get the centroid coords for each cluster
          # centClust <- matrix(ncol=2, nrow=max(midCrPts$clusterID))
          # centClust <- data.frame(x=NA,y=NA,clusterID=unique(midCrPts$clusterID))
          # for(i in unique(midCrPts$clusterID)){
          #   centClust[centClust$clusterID==i,c("x","y")] <- gCentroid(subset(midCrPts, clusterID == i))@coords
          # }
          # 
          # indDF <- merge(indDF, midCrPts@data, by="LocID", all.x=T)
          # corrDFr <- which(indDF$burstId%in%c("corridor"))
          
          map1 <- leaflet(corridorCalc) %>% addTiles()%>%
            addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
            addProviderTiles("Esri.WorldImagery", group = "Aerial") %>% 
            addPolylines(lng = coordinates(corridorCalc)[,1],lat = coordinates(corridorCalc)[,2],weight=2, opacity=0.7, layerId=namesIndiv(corridorCalc),group ="track")
          for(n in corrDFr){
            map1 <- map1 %>%
              addPolylines(data=indDF[n:(n+1),],lng=~long,lat=~lat, color="red",weight = 6,opacity = 0.7, group="potential corridors")
          }
          map1 %>% 
            # addCircles(data=centClust, lng=~x, lat=~y, group=~clusterID, radius=300, color="red") %>% #radius=RVupdate$clustDist
            addScaleBar(position="bottomright",
                        options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
            addLayersControl(
              baseGroups = c("TopoMap","Aerial"),
              # overlayGroups = centClust$clusterID,
              overlayGroups = c("track","potential corridors"),
              options = layersControlOptions(collapsed = FALSE)) 
        }
      } else { ## updated plot
        if(RVupdate$thintime==0){dataSubIndTime <- dataSubInd ## no thining by time
        } else {
          dataSubIndTime <- dataSubInd[!duplicated(round_date(timestamps(dataSubInd), paste0(RVupdate$thintime," mins"))),]
        }
        ## calculating corridors and extracting some data for plots
        corridorCalc <- corridor(x=dataSubIndTime, speedProp=RVupdate$speedProp, circProp=RVupdate$circProp, plot=FALSE)#, minNBsegments = 2)
        corridorCalc$LocID <- 1:n.locs(corridorCalc)
        indDF <- data.frame(long=coordinates(corridorCalc)[,1],lat=coordinates(corridorCalc)[,2],burstId=c(as.character(burstId(corridorCalc)),NA),LocID=corridorCalc$LocID)
        corrDFr <- which(indDF$burstId%in%c("corridor"))
        if(length(corrDFr)<=1){  ## if levels do not contain "corridor" OR if there is only 1 corridor point identified it cannot be clustered and gives an error
          ## to create the text box no corridors found
          tag.map.title <- tags$style(HTML(".leaflet-control.map-title {
          transform: translate(-50%,20%);
          position: fixed !important;
          left: 50%;
          text-align: center;
          padding-left: 10px; 
          padding-right: 10px; 
          background: rgba(255,255,255,0.75);
          font-weight: bold;
          font-size: 28px;
          color: black;
          }"))
          title <- tags$div(
            tag.map.title, HTML("No corridors found")
          )  
          
          map1 <- leaflet(corridorCalc) %>% addTiles()%>%
            addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
            addProviderTiles("Esri.WorldImagery", group = "Aerial") %>% 
            addPolylines(lng = coordinates(corridorCalc)[,1],lat = coordinates(corridorCalc)[,2],weight=2, opacity=0.7, layerId=namesIndiv(corridorCalc),group =namesIndiv(corridorCalc))
          map1 %>%
            addControl(title, position = "topleft", className="map-title") %>% # to add the ext box of corridors not found
            addScaleBar(position="bottomright",
                        options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
            addLayersControl(
              baseGroups = c("TopoMap","Aerial"),
              # overlayGroups = c("indiv","corr"),
              options = layersControlOptions(collapsed = FALSE))
          
        } else { # corridors found
          ## include here as above how to select/unselec corridors when found out how to do that
          map1 <- leaflet(corridorCalc) %>% addTiles()%>%
            addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
            addProviderTiles("Esri.WorldImagery", group = "Aerial") %>% 
            addPolylines(lng = coordinates(corridorCalc)[,1],lat = coordinates(corridorCalc)[,2],weight=2, opacity=0.7, layerId=namesIndiv(corridorCalc),group ="track")
          for(n in corrDFr){
            map1 <- map1 %>%
              addPolylines(data=indDF[n:(n+1),],lng=~long,lat=~lat, color="red",weight = 6,opacity = 0.7, group="potential corridors")
          }
          map1 %>% 
            # addCircles(data=centClust, lng=~x, lat=~y, group=~clusterID, radius=medSegleng*2, color="red") %>%
            addScaleBar(position="bottomright",
                        options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
            addLayersControl(
              baseGroups = c("TopoMap","Aerial"),
              # overlayGroups = centClust$clusterID,
              overlayGroups = c("track","potential corridors"),
              options = layersControlOptions(collapsed = FALSE)) 
        }
      }
      
    })
  }
  return(reactive({ current() }))
}

