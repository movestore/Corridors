library("shiny")
library("shinydashboard")
library("fresh")
library("move2")
library("sf")
# library("dplyr")
library("move")
# library("ggplot2")
library("shinyBS") ## to display message when hovering over input element in UI
library("lubridate")
# library("geosphere")
# library("dismo")
library("rgeos")
library("stringr")
library("shinyWidgets")
# library("ggsn")
library("shinycssloaders")
library("maptools")
library("circular")
# 
# 
library("leaflet")
# library(viridis)
# library(plyr)
library("dplyr")

## TODO
# think about what to to woth the all indivi corr plot...

## ToDO
# - when app to change track id exists, add it to readme
# - adjust height of panel
# - add message to please make github issue for useful options
# - make un-/selection of corridors possible, or at least by number of segments
# - make it possible to save the selected corridors within the object

## https://www.youtube.com/watch?v=gGEY82qA3BI  ##  modules
## module shinydashboard site:stackoverflow.com


shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    dashboardPage(
      dashboardHeader(title = "Corridors per track",titleWidth=300),
      dashboardSidebar(uiOutput(ns("SidebarUI")),
                       tags$style( ## to make a vertical scroll bar on the sidebar so all tabs can be accessed while seeing the main panel
                         "#sidebarItemExpanded {
                      overflow: auto;
                      height: calc(100vh - 50px) !important;
                     }")),
      dashboardBody(uiOutput(ns("TabUI")),
                    # tags$head(tags$style("#TabUI{height:65vh !important;}")),
                    use_theme(
                      create_theme(
                        adminlte_color(light_blue = "#46505a"),
                        adminlte_sidebar(width = "300px",dark_bg = "#343e48",dark_hover_bg = "#2879ca",dark_color = "#e1eaf2"),
                        adminlte_global(content_bg = "#dfe7ef")
                      )
                    )
      )
    )
  )
}

shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  
  #####################
  ### transform data ##
  #####################
  data  <- mutate(data, LocID = 1:nrow(data))
  data  <- mutate(data, corridorBehavior = NA)
  # data  <- mutate(data, corridorBehaviorClusterID = NA)
  data_out <- reactiveVal(data)
  dataMv2 <- data
  if(st_is_longlat(dataMv2)){dataMv2 <- dataMv2}else{dataMv2 <- st_transform(dataMv2,crs="EPSG:4326")}  ## to not change projection of original data
  dataMv <- moveStack(to_move(dataMv2))
  
  indDF_rv <- reactiveVal()
  #################
  ### tab naming ##
  #################
  namesCorresp <- data.frame(nameInd=namesIndiv(dataMv) , tabIndv=str_replace_all(namesIndiv(dataMv), "[^[:alnum:]]", ""))
  ntabs <- length(namesIndiv(dataMv))
  tabnames <- str_replace_all(namesIndiv(dataMv), "[^[:alnum:]]", "") #it does not allow punctuation or spaces
  speedPropnames <- paste0(tabnames, '_speedProp') 
  circPropnames <- paste0(tabnames, '_circProp') 
  timeThinnames <- paste0(tabnames, '_timeThin')
  clustDistnames <- paste0(tabnames, '_clustDist')
  clustNbnames <- paste0(tabnames, '_clustNb')
  allorsinglenames <- paste0(tabnames, '_allorsingle')
  updateButton <- paste0(tabnames,'_updateButton')
  plotnames <- paste0("plot_",tabnames) 
  
  ###################
  ### tab creation ##
  ###################
  output$SidebarUI <- renderUI({
    Menus <- vector("list", ntabs)
    for(i in 1:ntabs){
      Menus[[i]] <-   menuItem(tabnames[i], icon=icon("paw"), tabName = tabnames[i], selected = i==1) }
    do.call(function(...) sidebarMenu(id = ns('sidebarMenuUI'),...), Menus)
  })
  
  ##############################
  ### interactive UI creation ##
  ##############################
  output$TabUI <- renderUI({
    Tabs <- vector("list", ntabs)
    for(i in 1:ntabs){
      Tabs[[i]] <- tabItem(tabName = tabnames[i],
                           fluidRow( ## make the same as in "population corridors"
                             column(3,sliderInput(inputId=ns(speedPropnames[i]),label="Speed", min=0,max=1, value=0.75, step=0.01),
                                    bsTooltip(id=ns(speedPropnames[i]), title="Proportion of speeds which are high enough to be a valid corridor point (default: speeds that are greater than 75 % of all speeds)", placement = "bottom", trigger = "hover", options = list(container = "body"))),
                             column(3,sliderInput(inputId=ns(circPropnames[i]),label="Parallelism", min=0,max=1, value=0.25, step=0.01),
                                    bsTooltip(id=ns(circPropnames[i]), title="Proportion of the circular variances that is low enough to be a valid corridor point. Low values indicate that the segments are (near) parallel (default: variances that are lower than 25 % of all variances)", placement = "bottom", trigger = "hover", options = list(container = "body"))), ## maybe change wording to make it simpler: the lower the value, the more parallel are the segments
                             column(1, numericInput(ns(timeThinnames[i]),"Thin track to X mins", value=0, step=1),
                                    bsTooltip(id=ns(timeThinnames[i]), title="This is specially recommended for high resolution tracks to ease finding regions with parallel segments. Default (=0) no thinning", placement = "bottom", trigger = "hover", options = list(container = "body"))),
                             column(1, numericInput(ns(clustDistnames[i]),"Cluster width (m)",value=500), #diameter of the cluster, i.e. width of the corridor
                                    bsTooltip(id=ns(clustDistnames[i]), title="All identified corridor segments that fall within a circle of diameter X will be grouped as a corridor cluster. It can be seen as the width of the corridor", placement = "bottom", trigger = "hover", options = list(container = "body"))),
                             column(1, numericInput(ns(clustNbnames[i]),"Segment number",value=3), #number of segments that corridor should have within a cluster
                                    bsTooltip(id=ns(clustNbnames[i]), title="Minimum number of segments that will define a cluster. Clusters with fewer segments will be excluded", placement = "bottom", trigger = "hover", options = list(container = "body"))),
                             column(1,offset=1,actionBttn(ns(updateButton[i]), label="Update!", style="fill", color="success",icon=icon("redo"),size="md"))#
                           ),
                           withSpinner(leafletOutput(ns(plotnames[i]), height = "85vh"), type=5, size=1.5,color= "#28b78d") ## color the same as update button
      )
    }
    do.call(tabItems, Tabs)
  })
  
  ##############################
  ### get input values for UI ##
  ##############################
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
    RVupdate$clustDist <- input[[paste0(input$sidebarMenuUI, '_clustDist')]]
    RVupdate$clustNb <- input[[paste0(input$sidebarMenuUI, '_clustNb')]]
    RVupdate$allorsingle <- input[[paste0(input$sidebarMenuUI, '_allorsingle')]]
  })
  ##############################
  ### corridor calculation ##
  ##############################
  for(i in 1:ntabs){
      
      ######################
      ### per individual ##
      ######################
      output[[plotnames[i]]] <- renderLeaflet({
        #########################
        ##### plot functions ####
        #########################
        noCorrPlot_ind <- function(inputCorr){
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
          
          map1 <- leaflet(inputCorr) %>% addTiles()%>%
            addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
            addProviderTiles("Esri.WorldImagery", group = "Aerial") %>% 
            addPolylines(lng = coordinates(inputCorr)[,1],lat = coordinates(inputCorr)[,2],weight=2, opacity=0.5, color="black", layerId=namesIndiv(inputCorr),group =namesIndiv(inputCorr))
          map1 %>%
            addControl(title, position = "topleft", className="map-title") %>% # to add the ext box of corridors not found
            addScaleBar(position="bottomright",
                        options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
            addLayersControl(
              baseGroups = c("TopoMap","Aerial"),
              # overlayGroups = c("indiv","corr"),
              options = layersControlOptions(collapsed = FALSE))
        }
        
        midPts_ind <- function(inputCorr, d, ns){### calculating clusters to define which segments belong "to the same corridor"
          ## corridor segment midpoints
          midCrPts <- inputCorr@data[burstId(inputCorr)=="corridor",c("segMid_x","segMid_y","LocID")]
          coordinates(midCrPts) <- ~segMid_x+segMid_y
          projection(midCrPts) <- projection(dataMv)
          ## distance matrix
          dist <- distm(midCrPts, fun=distGeo)
          ## clustering
          hc <- hclust(as.dist(dist), method="single") # complete single
          # define clusters based on a tree "height" cutoff distance (distance between corridor clusters) and add them to the SpDataFrame
          midCrPts$clusterID <- cutree(hc, h=d/2) # d=RVupdate$clustDist
          # add nb of corridor segments per cluster to SpDataFrame
          tbCount <- data.frame(table(midCrPts$clusterID))
          midCrPts$nbCorrInClust <- NA
          for(j in tbCount$Var1){
            midCrPts$nbCorrInClust[midCrPts$clusterID==j] <- tbCount$Freq[tbCount$Var1==j]
          }
          ## remove those with lesss than X points
          midCrPts <- midCrPts[midCrPts$nbCorrInClust>ns,] #ns=RVupdate$clustNb
        }
        
        corrPlot_ind <- function(inputCorr,indDF,midCrPts){
          # get the centroid coords for each cluster
          centClust <- data.frame(x=NA,y=NA,clusterID=unique(midCrPts$clusterID))
          for(i in unique(midCrPts$clusterID)){
            centClust[centClust$clusterID==i,c("x","y")] <- gCentroid(subset(midCrPts, clusterID == i))@coords
          }
          
          indDF <- merge(indDF, midCrPts@data, by="LocID", all.x=T)
          ## only selected corridors acording to clustNb and clustDist are assigned with corridor
          indDF$burstId <- "no.corridor"
          indDF$burstId[indDF$LocID%in%midCrPts@data$LocID] <- "corridor"
          indDF_rv(indDF)
          # corrDFr <- which(indDF$burstId%in%c("corridor")) ## to plot all corridors at once
          
          ## color palette so clusters can be identified
          selClusterIDs <- unique(indDF$clusterID)[complete.cases(unique(indDF$clusterID))]
          pal <- colorFactor(
            palette = "magma",
            domain = selClusterIDs
          )
          
          map1 <- leaflet(inputCorr) %>% addTiles()%>%
            addProviderTiles("Esri.WorldTopoMap", group = "TopoMap") %>%
            addProviderTiles("Esri.WorldImagery", group = "Aerial") %>% 
            addPolylines(lng = coordinates(inputCorr)[,1],lat = coordinates(inputCorr)[,2],weight=2, opacity=0.5, color="black", layerId=namesIndiv(inputCorr),group ="track")
          # for(n in corrDFr){ ## to plot all corridors at once
          #   map1 <- map1 %>%
          #   addPolylines(data=indDF[n:(n+1),],lng=~long,lat=~lat, color=~pal(indDF$clusterID[n]),weight = 6,opacity = 0.8, group="potential corridors")
          # }
          for(s in selClusterIDs){
            corrLocs <- which(indDF$clusterID%in%s)
            for(n in corrLocs){
              map1 <- map1 %>%
                addPolylines(data=indDF[n:(n+1),],lng=~long,lat=~lat, color=~pal(indDF$clusterID[n]),weight = 6,opacity = 0.8, group=paste0("potential corridors cl:",s))
            }
          }
          map1 %>% 
            # addCircles(data=centClust, lng=~x, lat=~y, group=~clusterID, radius=RVupdate$clustDist/2, color="gold") %>% #radius=RVupdate$clustDist
            addScaleBar(position="bottomright",
                        options=scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
            addLayersControl(
              baseGroups = c("TopoMap","Aerial"),
              overlayGroups = c("track",paste0("potential corridors cl:",selClusterIDs)),#,centClust$clusterID),
              options = layersControlOptions(collapsed = TRUE)) 
        }
        #########################
        
        ##################################
        ### default plot per individual ##
        ##################################
        dataSubInd <-  dataMv[[RVtab$indv]]
        if(!input[[paste0(input$sidebarMenuUI, '_updateButton')]]){ ## default plot
          
          dataSubIndTime <- dataSubInd ## no thinning by time
          
          ## calculating corridors and extracting some data for plots
          corridorCalc <- corridor(x=dataSubIndTime, speedProp=.75, circProp=.25, plot=FALSE)
          indDF <- data.frame(long=coordinates(corridorCalc)[,1],lat=coordinates(corridorCalc)[,2],burstId=c(as.character(burstId(corridorCalc)),NA),LocID=corridorCalc$LocID)
          corrDFr <- which(indDF$burstId%in%c("corridor"))
          indDF_rv(indDF)
          ######################################################
          ### default plot per individual -- with no corridors ##
          ######################################################
          if(length(corrDFr)<=1){  ## if levels do not contain "corridor" OR if there is only 1 corridor point identified it cannot be clustered and gives an error
            noCorrPlot_ind(corridorCalc)  
          } else { ## there are corridors found
            ######################################################
            ### default plot per individual -- with corridors ##
            ######################################################
            mP <- midPts_ind(corridorCalc,d=500,ns=3)
            if(length(unique(mP$clusterID))>=1){
              corrPlot_ind(corridorCalc,indDF,mP)
            }else{
              noCorrPlot_ind(corridorCalc)
            }
          }
        } else { ## updated plot
          ##################################
          ### updated plot per individual ##
          ##################################
          if(RVupdate$thintime==0){dataSubIndTime <- dataSubInd ## no thining by time
          } else {
            dataSubIndTime <- dataSubInd[!duplicated(round_date(timestamps(dataSubInd), paste0(RVupdate$thintime," aminutes"))),]
          }
          ## calculating corridors and extracting some data for plots
          corridorCalc <- corridor(x=dataSubIndTime, speedProp=RVupdate$speedProp, circProp=RVupdate$circProp, plot=FALSE)
          indDF <- data.frame(long=coordinates(corridorCalc)[,1],lat=coordinates(corridorCalc)[,2],burstId=c(as.character(burstId(corridorCalc)),NA),LocID=corridorCalc$LocID)
          corrDFr <- which(indDF$burstId%in%c("corridor"))
          indDF_rv(indDF)
          ######################################################
          ### updated plot per individual -- with NO corridors ##
          ######################################################
          if(length(corrDFr)<=1){  ## if levels do not contain "corridor" OR if there is only 1 corridor point identified it cannot be clustered and gives an error
            noCorrPlot_ind(corridorCalc)  
            
          } else { # corridors found
            ######################################################
            ### updated plot per individual -- with corridors ##
            ######################################################
            ## include here as above how to select/unselec corridors when found out how to do that
            mP <- midPts_ind(corridorCalc,d=RVupdate$clustDist,ns=RVupdate$clustNb)
            if(length(unique(mP$clusterID))>=1){
              corrPlot_ind(corridorCalc,indDF,mP)
            }else{
              noCorrPlot_ind(corridorCalc)
            }
          }
        }
      })
    
    observe({ 
      selected_groups <- req(input[[paste0(plotnames[i],"_groups")]])
      selected_groups_sub <- selected_groups[grepl("potential corridors cl:", selected_groups)] 
      selClusts <- as.numeric(gsub("potential corridors cl:", "", selected_groups_sub, perl=TRUE))
      selClusts <- selClusts[complete.cases(selClusts)]
      indDFmod <- indDF_rv()
      selLocIDs <- indDFmod$LocID[indDFmod$clusterID%in%selClusts]
      data$corridorBehavior[data$LocID%in%selLocIDs] <- "corridor"
      data_out(data)
    })
  }
  return(data_out)
}

