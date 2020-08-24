if(!require(pacman)){install.packages(pacman)}
pacman::p_load(rpredictit, DBI, RSQLite, png, dplyr, curl, rstudioapi, lubridate,magrittr)

# This is an attempt to get the current price of every open contract every 8 seconds...
# If we do this that means that a machine at a unique IP Address should be only running this script.
# https://www.predictit.org/api/marketdata/all/
# if we do ever decide to do auto trading know that this exists:
# https://github.com/kcinnick/pyredictit

# Some Global Variables...
delaySecondsBetweenCalls <- 60
configUseInMemoryDatabase <- FALSE
querySourceVal <- 2 # MarketDataAll / Open

# if configUseInMemoryDatabase is TRUE, then the sqlite database will be created in memory only
# [TODO] Not all calls are yet built to handle TRUE
# [TODO] set to TRUE, then we should save it's values to disk occasionally...

# Function scriptFileName() attempts to find the current scripts path to use relative path
# Checks to see if script is source'd via:
# command line, R console, RStudio,RStudio Console,RStudio Run Selection
# returns '' if we are unable to find
# http://stackoverflow.com/a/35842176/2292993
# build and view db with https://sqlitebrowser.org/
scriptFileName <- function() {
  tryCatch({
    cmdArgs <- commandArgs(trailingOnly = FALSE)
    matchFound = "--file=" %>% grep(cmdArgs)
    if (matchFound %>% length() > 0) {
      "--file=" %>% sub("", cmdArgs[matchFound]) %>% normalizePath() %% return()
    } else if ("fileName" %in% (sys.frames()[[1]] %>% ls())){
      sys.frames()[[1]]$fileName %>% normalizePath() %>% return()
    } else if (sys.frames()[[1]]$ofile %>% is.null()){
      if (pacman::p_exists(rstudioapi)){
        pth = rstudioapi::getActiveDocumentContext()$path
        if (pth == '') {
          rstudioapi::getSourceEditorContext()$path %>%
            normalizePath() %>% return()
        } else {pth %>% normalizePath() %>% return()}
      } else {
        message("WARNING:Unable to find script path automatically select this script.")
        file.choose() %>% return()
      }
    } else {
      sys.frames()[[1]]$ofile %>% normalizePath() %>% return()
    }
  }, error = function(e) {
    message("WARNING:Unable to find script path automatically select this script.")
    file.choose() %>% return()
  })
}
if (stringr::str_length(scriptFileName()) >0 ) {
  projectDirectory <- dirname(scriptFileName())
} else {
  if (Sys.info()$sysName == "Windows") {
    projectDirectory <- file.path("p:","data","r","predictit_dataset","rpredictit_dataset",fsep="\\")
  } else {
    projectDirectory <- file.path("/mnt","p","data","r","predictit_dataset","rpredictit_dataset")
  }
  message("Warning: Unable to find relative path of script, project path set to:",projectDirectory)
}
# A usefull sqlite tutorial:
# https://www.sqlitetutorial.net

# to use the seakintruth fork
# pacman::p_load(devtoools)
# devtools::install_github('seakintruth/rpredictit')

# the api documentation requests calls be a minute apart, but from testing the
# slow down error message occus after the 10th call in a minute as of 02/11/2019

# ------------------------------------------------------------------------------

delayDurringSiteMaintanence <- function(check.url){
  # handle a site maintenance message:
  http.response <- httr::GET(check.url)
  trading.suspended <- http.response$headers$`pi-tradingsuspendedmessage`
  repeat{
    if (!is.null(trading.suspended) ){
      message("API Warning: Trying again in a minute. ",trading.suspended)
      # we just made a call so wait our delay period
      queryTimeBegin <- Sys.time()
      http.response <- httr::GET(check.url)
      # handle a site maintenance message:
      trading.suspended <- http.response$headers$`pi-tradingsuspendedmessage`
      query.time.end <- Sys.time()
      sleepLessProcessTime(query.time.end, queryTimeBegin)
    } else {
      break
    }
  }
}

siteReturnedNothing<- function(http.response){
  is.null(
    jsonlite::fromJSON(
      rawToChar(
        http.response$content
      )
    )
  ) | (rawToChar(
    http.response$content
  ) == "[]")
}

sleepLessProcessTime <-function(time.begin = NULL, time.end = NULL){
  if (is.null(time.begin)|is.null(time.end)){
    Sys.sleep(delaySecondsBetweenCalls)
  } else {
    if(abs(as.double(difftime(time.begin, time.end, tz, units ="secs")))<delaySecondsBetweenCalls){
      Sys.sleep(delaySecondsBetweenCalls-abs(as.double(difftime(time.begin, time.end, tz, units ="secs"))))
    }
  }
}
.dbSendQueryFetch<-function(sql.statement,db){
  sql.result <- RSQLite::dbSendQuery(
    conn=db,
    sql.statement
  )
  fetch.data <-RSQLite::dbFetch(sql.result)
  dbClearResult(sql.result)
  return(fetch.data)
}

.dbSendQueryClear<-function(sql.statement,db){
  results <- RSQLite::dbSendQuery(
    conn=db,
    sql.statement
  )
  RSQLite::dbClearResult(results)
  return(NULL)
}

openDbConn <- function(databaseFileName,fUseInMemoryDatabase){
  if(fUseInMemoryDatabase == TRUE){
    # Create an in memory database, copied from file if it exists
    db <- DBI::dbConnect(
      RSQLite::SQLite(),
      ":memory:"
    )
    if(file.exists(databaseFileName)){
      dbFile <- DBI::dbConnect(
        RSQLite::SQLite(),
        dbname=databaseFileName
      )
      RSQLite::sqliteCopyDatabase(
        dbFile,
        db
      )
      DBI::dbDisconnect(dbFile)
    }
  } else{
    db <- DBI::dbConnect(
      RSQLite::SQLite(),
      dbname=databaseFileName
    )
  }
  return(db)
}

#function with pipes
executeSqlFromFile <- function(sqlFile,db){
  tryExecuteSQlFromFile <- try(
    queryReturn <-  sqlFile %>%
      readLines(warn=FALSE) %>%
      paste(collapse=" ") %>%
      strsplit(";") %>%
      unlist() %>%
      lapply(FUN=.dbSendQueryClear,db),
    silent=TRUE)
  return(queryReturn)
}

# Same function without pipes
exmpleFunctionWithouthPipes_executeSqlFromFile <- function(sqlFile,db){
  createSql <-
    unlist(
      strsplit(
        paste(
          readLines(
            sqlFile,
            warn=FALSE
          ),
          collapse=""
        ),
        ";"
      )
    )
  try(
    queryReturn <- lapply(
      createSql,
      FUN=.dbSendQueryClear,
      db
    ),
    silent=TRUE
  )
}

buildDbTablesIfNeeded <- function(databaseFileName,configUseInMemoryDatabase){
  db <- openDbConn(databaseFileName, configUseInMemoryDatabase)
  # Create database and all planned tables if no tables exist,
  # setup foregin keys with: https://www.techonthenet.com/sqlite/foreign_keys/foreign_keys.php
  # on refactor we could create tables one at a time if that particular table is missing by checking:
  # .dbSendQueryFetch("SELECT DISTINCT tbl_name FROM sqlite_master WHERE tbl_name =='expectedTableName'")
  # Create any missin the database tables
  # SQL documentation: https://www.sqlite.org/lang.html
  # These sql queries are built to only execute if a table or index is missing

  # Create Tables
  # [TODO] marketObservation should include these:
  # FOREIGN KEY (contractId) REFERENCES contract (contractId)
  # FOREIGN KEY (marketId) REFERENCES market (marketId)

  attemptCreate <- file.path(projectDirectory,"sql","01aCreateDbTables.sql") %>%
    executeSqlFromFile(db)

  # Create indexes
  #[TODO] add CREATE UNIQUE INDEX IF NOT EXISTS `contract_id_index` ON `marketObservation` (`contractId`	ASC);
  attemptIndex <- file.path(projectDirectory,"sql","01bCreateDbIndexes.sql") %>%
    executeSqlFromFile(db)

  # Create indexes
  #Insert lookup_querySource rows we expect an error here if the querySourceId allready exists, so we ignore it
  querySource <- .dbSendQueryFetch(
    "SELECT querySourceid
        FROM querySource
        ",
    db
  )
  if (!length(querySource[,1]) == 11){
    errFillLookupQuerySource <- try(
      executeSqlFromFile(
        file.path(projectDirectory,"sql","01cInsert-querySource-DefaultValues.sql"),
        db
      )
    )
  }
  #Insert lookup_querySource rows we expect an error here if the querySourceId allready exists, so we ignore it
  statusSource <- .dbSendQueryFetch(
    "SELECT statusId
        FROM status
        ",
    db
  )
  if (!length(querySource[,1]) == 2){
    errFillLookupQuerySource <- try(
      executeSqlFromFile(
        file.path(projectDirectory,"sql","01dInsert-marketStatus-DefaultValues.sql"),
        db
      )
    )
  }
  #errFillLookupQuerySource
  # Clean up open database connections
  DBI::dbDisconnect(db)
}

getOpenMarkets <- function(databaseFileName,configUseInMemoryDatabase){
  db <- openDbConn(databaseFileName, configUseInMemoryDatabase)
  all.market.data.open <- rpredictit::all_markets()
  fSaveAsNull <- FALSE
  # check for a site down error
  if(is.na(all.market.data.open)){
    delayDurringSiteMaintanence("https://www.predictit.org/api/marketdata/all/")
  } else {
    # querySourceVal
    marketObservation <- all.market.data.open

    marketObservation[["querySourceId"]] <- rep(
      querySourceVal,
      length(unlist(marketObservation[1]))
    )
    marketObservation <- marketObservation %>%
      mutate(dateEnd=lubridate::ymd_hms(dateEnd)) %>%
      mutate(timeStamp=lubridate::ymd_hms(timeStamp)) %>%
      mutate(marketStatusId=dplyr::case_when(
        marketObservation$status == "Open" ~ 1,
        marketObservation$status == "Closed" ~ 0)
      ) %>%
      mutate(contractStatusId=dplyr::case_when(
        marketObservation$contract_status == "Open" ~ 1,
        marketObservation$contract_status == "Closed" ~ 0)
      ) %>%
      dplyr::rename(
        dateTimeStamp=timeStamp,
        dateEnd=dateEnd,
        contractId=contract_id,
        contractStatusId=contractStatusId,
        marketId=id,
        marketStatusId=marketStatusId,
        lastTradePrice=lastTradePrice,
        bestBuyYesCost=bestBuyYesCost,
        bestBuyNoCost=bestBuyNoCost,
        bestSellYesCost=bestSellYesCost,
        bestSellNoCost=bestSellNoCost,
        lastClosePrice=lastClosePrice,
        displayOrder=displayOrder,
        querySourceId=querySourceId
      ) %>%
      dplyr::select(
        dateTimeStamp,
        dateEnd,
        contractId,
        contractStatusId,
        marketId,
        marketStatusId,
        lastTradePrice,
        bestBuyYesCost,
        bestBuyNoCost,
        bestSellYesCost,
        bestSellNoCost,
        lastClosePrice,
        displayOrder,
        querySourceId
      )

    error.checking <- try(
      results <- DBI::dbAppendTable(conn=db,
        "marketObservation",marketObservation
      )
    )
  }
}

# Kicks off getting all open markets daily
buildDbTablesIfNeeded(file.path(projectDirectory,"data","openMarkets.sqlite"), configUseInMemoryDatabase)
repeat{
  queryStartTime <- Sys.time()
  # Repeat forever, getting the new content
  getOpenMarkets(file.path(projectDirectory,"data","openMarkets.sqlite"), configUseInMemoryDatabase)
  executionTime <- as.double(difftime(Sys.time(),queryStartTime, tz,units ="secs"))
  ## Delay one hour between each calls start time.
  message("search complete, waiting until next allowed call. Will resume at:" , lubridate::now()+executionTime)

  if(executionTime < delaySecondsBetweenCalls){
    Sys.sleep(delaySecondsBetweenCalls-executionTime)
  }
}
