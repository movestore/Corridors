library("shiny")
library("shinydashboard")
library("dashboardthemes")
library("move")
library("ggplot2")
library("shinyBS") ## to display message when hovering over input element in UI
library("lubridate")
library("geosphere")
library("dismo")
library("rgeos")
library("stringr")


## input data 
data(fishers)
dataInp <- fishers

# dataInp <- readRDS("/home/anne/MoveAppsGit/allesMoegliche/data/carn.rds")
####


ui <- dashboardPage(
  dashboardHeader(title = "Corridors per individual",titleWidth=300),
  dashboardSidebar(uiOutput("Sidebar"),
                   tags$style( ## make a vertical scroll bar on the sidebar so all tabs can be accessed while seeing the main panel
                     "#sidebarItemExpanded {
                      overflow: auto;
                      height: calc(100vh - 50px) !important;
                     }")),
  dashboardBody(uiOutput("TabUI"),
                shinyDashboardThemes( #https://github.com/nik01010/dashboardthemes
                  theme = "grey_dark"
                ))
  # ,skin = 'green'
)


server <- function(input, output) {
  
  namesCorresp <- data.frame(nameInd=namesIndiv(dataInp) , tabIndv=str_replace_all(namesIndiv(dataInp), "[^[:alnum:]]", ""))
  ntabs <- length(namesIndiv(dataInp))
  tabnames <- str_replace_all(namesIndiv(dataInp), "[^[:alnum:]]", "") #it does not allow punctuation or spaces
  speedPropnames <- paste0(tabnames, '_speedProp') 
  circPropnames <- paste0(tabnames, '_circProp') 
  timeThinnames <- paste0(tabnames, '_timeThin')
  clustDistnames <- paste0(tabnames, '_clustDist')
  plotnames <- paste0("plot_",tabnames) 
  
  
  output$Sidebar <- renderUI({
    Menus <- vector("list", ntabs)
    for(i in 1:ntabs){
      Menus[[i]] <-   menuItem(tabnames[i], icon=icon("paw"), tabName = tabnames[i], selected = i==1) }
    do.call(function(...) sidebarMenu(id = 'sidebarMenu',...), Menus)
  })
  
  output$TabUI <- renderUI({
    Tabs <- vector("list", ntabs)
    for(i in 1:ntabs){
      Tabs[[i]] <- tabItem(tabName = tabnames[i],
                           fluidRow(
                             column(3,sliderInput(inputId=speedPropnames[i],label="Speed", min=0,max=1, value=0.75, step=0.01),
                                    bsTooltip(id=speedPropnames[i], title="Proportion of speeds which are high enough to be a valid corridor point (default: speeds that are greater than 75 % of all speeds)", placement = "bottom", trigger = "hover", options = list(container = "body"))),
                             column(3,sliderInput(inputId=circPropnames[i],label="Parallelism", min=0,max=1, value=0.25, step=0.01),
                                    bsTooltip(id=circPropnames[i], title="Proportion of the circular variances that is low enough to be a valid corridor point. Low values indicate that the segments are (near) parallel (default: variances that are lower than 25 % of all variances)", placement = "bottom", trigger = "hover", options = list(container = "body"))), ## maybe change wording to make it simpler: the lower the value, the more parallel are the segments
                             column(3, numericInput(timeThinnames[i],"Thin track to X mins",min=0,max=60, value=0, step=1),
                                    bsTooltip(id=timeThinnames[i], title="This is specially recomended for high resolution tracks to ease finding regions wtih paralell segments. Default (=0) no thining", placement = "bottom", trigger = "hover", options = list(container = "body"))),
                             column(3, numericInput(clustDistnames[i],"Distance between corridor clusters (mts)",value=300),
                                    bsTooltip(id=clustDistnames[i], title="The radius of the cicles displayed on the map correspond to this value. All identified 'corridor segments' that fall within each cicle will be identified as a corridor", placement = "bottom", trigger = "hover", options = list(container = "body")))
                           ),
                           plotOutput(plotnames[i],dblclick = paste0(plotnames[i],"_dblclick"), brush = brushOpts(id = paste0(plotnames[i],"_brush"),resetOnNew = TRUE), height = "80vh")
      )
    }
    do.call(tabItems, Tabs)
  })
  
  ## zoom into each plot with double click, back to full with double click                        
  ranges <- reactiveValues(x_range = NULL, y_range = NULL) ## for zoom of plot
  observeEvent(input[[paste0("plot_",input$sidebarMenu,"_dblclick")]], {
    brush <- input[[paste0("plot_",input$sidebarMenu, "_brush")]]
    if (!is.null(brush)) {
      ranges$x_range <- c(brush$xmin, brush$xmax)
      ranges$y_range <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x_range <- NULL
      ranges$y_range <- NULL
    }
  })
  
  ## get input values for each tab
  RV <- reactiveValues()
  observe({
    RV$indv <- namesCorresp$nameInd[namesCorresp$tabIndv==input$sidebarMenu]
    RV$thintime <- input[[paste0(input$sidebarMenu, '_timeThin')]]
    RV$speedProp <- input[[paste0(input$sidebarMenu, '_speedProp')]]
    RV$circProp <- input[[paste0(input$sidebarMenu, '_circProp')]]
    RV$clustDist <- input[[paste0(input$sidebarMenu, '_clustDist')]]
  })
  
  
  for(i in 1:ntabs){
    output[[plotnames[i]]] <- renderPlot({
      dataSubInd <-  dataInp[[RV$indv]]
      
      if(RV$thintime==0){dataSubIndTime <- dataSubInd ## no thining by time
      } else {
        dataSubIndTime <- dataSubInd[!duplicated(round_date(timestamps(dataSubInd), paste0(RV$thintime," mins"))),]
      }
      ## extracting some data for plots
      indDF <- data.frame(long=coordinates(dataSubIndTime)[,1],lat=coordinates(dataSubIndTime)[,2])
      indivName <- namesIndiv(dataSubIndTime)
      
      corridorCalc <- corridor(x=dataSubIndTime, speedProp=RV$speedProp, circProp=RV$circProp, plot=FALSE)
      
      if(!any(burstId(corridorCalc)=="corridor") || length(burstId(corridorCalc)[burstId(corridorCalc)=="corridor"])==1){  ## if levels do not contain "corridor" OR if there is only 1 corridor point identified it cannot be clustered and gives an error
        
        cntr_x <- (max(indDF$long)+min(indDF$long))/2
        cntr_y <- (max(indDF$lat)+min(indDF$lat))/2
        
        ggplot()+geom_path(data=indDF,aes(long,lat))+
          labs(title=indivName, x ="Longitude", y = "Latitude")+ 
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))+
          geom_label(aes(x=cntr_x,y=cntr_y), label="No corridors found", color="red", size=8)+
          coord_fixed(xlim = ranges$x_range, ylim = ranges$y_range, expand = T)
      } else {
        # ## to give each end pt of corridor segment also corridor id
        # crptsPosition <- which(burstId(corridorCalc)=="corridor")
        # dataSubIndTime$corridor <- "no.corridor" 
        # dataSubIndTime$corridor[crptsPosition] <- "corridor"
        # dataSubIndTime$corridor[crptsPosition+1] <- "corridor"
        
        ## corridor segment midpoints
        midCrPts <- corridorCalc@data[burstId(corridorCalc)=="corridor",c("segMid_x","segMid_y")] 
        coordinates(midCrPts) <- ~segMid_x+segMid_y
        projection(midCrPts) <- projection(dataSubIndTime)
        ## distance matrix
        dist <- distm(midCrPts, fun=distGeo)
        ## clustering
        hc <- hclust(as.dist(dist), method="single") # complete single
        # define clusters based on a tree "height" cutoff distance (distance between corridor clusters) and add them to the SpDataFrame
        midCrPts$clust <- cutree(hc, h=RV$clustDist)
        # add nb of corridor segments per cluster to SpDataFrame
        tbCount <- data.frame(table(midCrPts$clust))
        for(j in tbCount$Var1){
          midCrPts$nbCorrInClust[midCrPts$clust==j] <- tbCount$Freq[tbCount$Var1==j]
        }
        ##### make here clusters selecable, unselectable, remove those with lesss than X points, etc
        # get the centroid coords for each cluster
        centClust <- matrix(ncol=2, nrow=max(midCrPts$clust))
        centClust <- data.frame(x=NA,y=NA,clusterID=unique(midCrPts$clust))
        for(i in unique(midCrPts$clust)){
          centClust[centClust$clusterID==i,c("x","y")] <- gCentroid(subset(midCrPts, clust == i))@coords
        }
        # compute circles around the cluster centroids with "clustDist" radius
        clusterCircles <- circles(centClust[,c("x","y")], d=RV$clustDist, lonlat=T,dissolve=F)
        
       
        cifort <- fortify(clusterCircles@polygons, region="ID")
        
        ggplot()+geom_path(data=indDF,aes(long,lat))+
          geom_point(data=as.data.frame(midCrPts), aes(segMid_x, segMid_y,color=as.factor(clust)))+
          geom_polygon(data=cifort, aes(long, lat, group=id,colour=id), fill=NA)+
          labs(title=indivName, x ="Longitude", y = "Latitude", color="corridor cluster")+ 
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1))+
          coord_fixed(xlim = ranges$x_range, ylim = ranges$y_range, expand = T)
      }
      
    })
  }
  
}

shinyApp(ui, server)



