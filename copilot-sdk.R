library(jsonlite)
source("logger.R")
source("ShinyModule.R")

inputFileName = "threeindv.rds" #"leroyMS.rds" "carn.rds
outputFileName = "output.rds"

if(file.exists("configuration.json")) {
  args <- read_json("configuration.json")
} else {
  args <- list()
}

#################################################################
########################### Arguments ###########################
# The data parameter will be added automatically if input data is available
# The name of the field in the vector must be exactly the same as in the shiny module signature
# Example:
# shinyModule <- function(input, output, session, username, password)
# The parameter must look like:
#    args[["username"]] = "any-username"
#    args[["password"]] = "any-password"

# Add your arguments of your r function here
#args[["year"]] = 2014

#################################################################
#################################################################

storeConfiguration <- function(configuration) {
  write_json(configuration, "configuration.json", auto_unbox = TRUE)
  logger.info("Stored configuration of shinyModule to 'configuration.json'")
}

ui <- fluidPage(
  tags$head(singleton(tags$script(src = 'ws-keep-alive-fix.js'))),
  tags$link(rel = "stylesheet", type = "text/css", href = "ws-keep-alive-fix.css"),
  do.call(shinyModuleUserInterface, c("shinyModule", "shinyModule", args)),
  dataTableOutput("table"), #Is neccessary for storing result

  if (exists("shinyModuleConfiguration")) {
    actionButton("storeConfiguration", "Store current configuration")
  },

  # ws-heartbeat fix
  # kudos: https://github.com/rstudio/shiny/issues/2110#issuecomment-419971302
  textOutput("ws_heartbeat")
)

readInput <- function(sourceFile) {
  input <- NULL
  if(!is.null(sourceFile) && sourceFile != "") {
    logger.debug("Loading file from %s", sourceFile)
    input <- tryCatch({
        # 1: try to read input as move RDS file
        readRDS(file = sourceFile)
      },
      error = function(readRdsError) {
        tryCatch({
          # 2 (fallback): try to read input as move CSV file
          move(sourceFile, removeDuplicatedTimestamps=TRUE)
        },
        error = function(readCsvError) {
          # collect errors for report and throw custom error
          stop(paste(sourceFile, " -> readRDS(sourceFile): ", readRdsError, "move(sourceFile): ", readCsvError, sep = ""))
        })
      })
  } else {
    logger.debug("Skip loading: no source File")
  }

  input
}

server <- function(input, output, session) {
  inputData <- readInput(inputFileName)
  
  shinyModuleArgs <- c(shinyModule, "shinyModule", args)
  if (!is.null(inputData)) {
    shinyModuleArgs[["data"]] <- inputData
  }
  
  result <- tryCatch({
        do.call(callModule, shinyModuleArgs)
    },
    error = function(e) {
      logger.error(paste("ERROR:", e))
      stop(e) # re-throw the exception
    }
  )
  
  observeEvent(input$storeConfiguration, {
    logger.info("Start reading configuration from shinyModule")
    storeConfiguration(shinyModuleConfiguration("shinyModule", input))
  })

  output$table <- renderDataTable({
    if (!is.null(outputFileName) &&
      outputFileName != "" &&
      !is.null(result())) {
      logger.info(paste("Storing file to '", outputFileName, "'", sep = ""))
      saveRDS(result(), file = outputFileName)
    } else {
      logger.warn("Skip store result: no output File or result is missing.")
    }
  })

  # ws-heartbeat fix
  # kudos: https://github.com/rstudio/shiny/issues/2110#issuecomment-419971302
  output$ws_heartbeat <- renderText({
    req(input$heartbeat)
    input$heartbeat
  })
}

shinyApp(ui, server)