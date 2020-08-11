pacman::p_load(rpredictit, DBI, RSQLite, png, dplyr, curl, rstudioapi)
configUseInMemoryDatabase <- FALSE
# if configUseInMemoryDatabase is TRUE, then the sqlite database will be
# read into memory, and all manipulations will happen in RAM, uppon completion
# we write the database from memory back to file.
# see: https://www.sqlite.org/inmemorydb.html
# building out a normalized schema, with: https://dbdiagram.io/d
# under seakintruth's github account
# then using https://www.rebasedata.com/convert-mysql-to-sqlite-online
scriptFileName <- function() {
    # https://stackoverflow.com/a/32016824/2292993
    cmdArgs <- commandArgs(trailingOnly = FALSE)
    needle <- "--file="
    match <- grep(needle, cmdArgs)
    tryAlternate <- FALSE
    if (length(match) > 0) {
        # Rscript via command line
        return(normalizePath(sub(needle, "", cmdArgs[match])))
    } else {
        ls_vars <- ls(sys.frames()[[1]])
        if (exists(ls_vars)) {
            if ("fileName" %in% ls_vars) {
                # Source'd via RStudio
                return(normalizePath(sys.frames()[[1]]$fileName))
            } else {
                tryAlternate <- TRUE
            }
        } else {
            tryAlternate <- TRUE
        }
    }
    if (tryAlternate){
        if (!is.null(sys.frames()[[1]]$ofile)) {
            # Source'd via R console
            return(normalizePath(sys.frames()[[1]]$ofile))
        } else {
            # RStudio Run Selection
            # http://stackoverflow.com/a/35842176/2292993
            pth = rstudioapi::getActiveDocumentContext()$path
            if (pth!='') {
                return(normalizePath(pth))
            } else {
                # RStudio Console
                tryCatch(
                    {
                        pth <- rstudioapi::getSourceEditorContext()$path
                        pth <- normalizePath(pth)
                    }, error <- function(e) {
                        # normalizePath('') issues warning/error
                        pth <- ''
                    }
                )
                return(pth)
            }
        }
    }
}
tmpFileName <- try(scriptFileName())
if (class(tmpFileName)=="try-error"){
    message("WARNING:	Unable to find script path automatically")
    #projectDirectory <- file.path("/media","jeremy","250GbUsb","data","r","predictit")
    projectDirectory <- file.path("D:","data","r","predictit")
} else {
    projectDirectory <- dirname(tmpFileName)
}
# A usefull sqlite tutorial:
# https://www.sqlitetutorial.net

# to use the seakintruth fork
# pacman::p_load(devtoools)
# devtools::install_github('seakintruth/rpredictit')

delaySeconds <- 8
# the api documentation requests calls be a minute apart, but from testing the
# slow down error message occus after the 10th call in a minute as of 02/11/2019

# Some Global Variables...
# ------------------------------------------------------------------------------
# [TODO] columns$.... represent the columns needed for a normalized relational db
# this is not yet implemented...
# columns <- list(
#     market="
#     id INTEGER,
#     name TEXT,
#     shortName TEXT,
#     image TEXT,
#     image_rowId INTEGER
#     ",
#     contract="
#     contract_id INTEGER,
#     image_rowId INTEGER,
#     contract_name TEXT,
#     contract_shortName TEXT
#     ",
#     marketObservation="
#     timeStamp DATETIME,
#     id INTEGER,
#     status TEXT,
#     contract_id INTEGER,
#     contract_status TEXT,
#     lastTradePrice DOUBLE,
#     bestBuyYesCost DOUBLE,
#     bestBuyNoCost DOUBLE,
#     bestSellYesCost DOUBLE,
#     bestSellNoCost DOUBLE,
#     lastClosePrice DOUBLE,
#     displayOrder INTEGER,
#     querySource INTEGER
#     ",
#     chartData="
#     marketId INTEGER,
#     contractId INTEGER,
#     date DATETIME,
#     openSharePrice DOUBLE,
#     highSharePrice DOUBLE,
#     lowSharePrice DOUBLE,
#     closeSharePrice DOUBLE,
#     tradeVolume DOUBLE,
#     querySource INTEGER
#     "
# )
#
# marketColumns <- (
#     "
#     id INTEGER,
#     name TEXT,
#     shortName TEXT,
#     image TEXT,
#     url TEXT
#     "
# )
#
# contractColumns <- (
#     "
#     contract_id INTEGER,
#     contract_image TEXT,
#     contract_name TEXT,
#     contract_shortName TEXT
#     "
# )
#
# marketObservationColumns <- (
#     "
#     timeStamp DATETIME,
#     id INTEGER,
#     name TEXT,
#     shortName TEXT,
#     image TEXT,
#     url TEXT,
#     status TEXT,
#     contract_id INTEGER,
#     dateEnd DATETIME,
#     contract_image TEXT,
#     contract_name TEXT,
#     contract_shortName TEXT,
#     contract_status TEXT,
#     lastTradePrice DOUBLE,
#     bestBuyYesCost DOUBLE,
#     bestBuyNoCost DOUBLE,
#     bestSellYesCost DOUBLE,
#     bestSellNoCost DOUBLE,
#     lastClosePrice DOUBLE,
#     displayOrder INTEGER,
#     querySource INTEGER
#     "
# )
#
# chartDataColumns <- (
#     "
#     marketId INTEGER,
#     contractId INTEGER,
#     contractName TEXT,
#     date DATETIME,
#     openSharePrice DOUBLE,
#     highSharePrice DOUBLE,
#     lowSharePrice DOUBLE,
#     closeSharePrice DOUBLE,
#     tradeVolume DOUBLE,
#     querySource INTEGER
#     "
# )

delayDurringSiteMaintanence <- function(http.response, check.url){
    # handle a site maintenance message:
    trading.suspended <- http.response$headers$`pi-tradingsuspendedmessage`
    repeat{
        if (!is.null(trading.suspended) ){
            message("API Warning: Trying again in a minute. ",trading.suspended)
            # we just made a call so wait a minute
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
        Sys.sleep(delaySeconds)
    } else {
        if(abs(as.double(difftime(time.begin, time.end, tz, units ="secs")))<delaySeconds){
            Sys.sleep(delaySeconds-abs(as.double(difftime(time.begin, time.end, tz, units ="secs"))))
        }
    }
}

requestMarketopenValue <- function(openMarkets,db){
    # rather than using a lapply here a for loop is used in this process as we NEVER plan on parallizing
    for (market.id in seq_along(openMarkets)){
        queryTimeBegin <- Sys.time()
        message("getting market: ",openMarkets[market.id])
        get.url <- paste0(
            "https://www.predictit.org/api/marketdata/markets/",
            openMarkets[market.id]
        )
        http.response <- httr::GET(get.url)
        delayDurringSiteMaintanence(http.response, get.url)
        if(siteReturnedNothing(http.response)){
            message("Warning: market ID ",openMarkets[market.id]," returned no content")
            error.checking <- try(
                results <- RSQLite::dbSendQuery(
                    conn=db,
                    paste0(
                        "INSERT INTO marketNotExist (marketId,querySourceId) ",
                        "VALUES (", openMarkets[market.id], ",",1,");"
                    )
                )
            )
            dbClearResult(results)
        } else{
            this.market <- rpredictit::single_market(openMarkets[market.id])
            this.market[["querySource"]] <- rep(1,length(unlist(this.market[1])))
            message("attempting to query id:",openMarkets[market.id])
            # [TODO] Query here to make new marketId in table market, if row doesn't exist yet
            market.observations <- this.market[
                c(
                    "timeStamp",
                    "contract_id",
                    "contract_status",
                    "id",
                    "status",
                    "lastTradePrice",
                    "bestBuyYesCost",
                    "bestBuyNoCost",
                    "bestSellYesCost",
                    "bestSellNoCost",
                    "lastClosePrice",
                    "displayOrder",
                    "querySource"
                )
            ]
             names(market.observations) <- c(
                "dateTimeStamp",
                "contractId",
                "contractStatus",
                "marketId",
                "marketStatus",
                "lastTradePrice",
                "bestBuyYesCost",
                "bestBuyNoCost",
                "bestSellYesCost",
                "bestSellNoCost",
                "lastClosePrice",
                "displayOrder",
                "querySourceId"
            )
            error.checking <- try(
                result <- DBI::dbAppendTable(conn=db,"marketObservation",this.market)
            )
        }
        query.time.end <- Sys.time()
        # we just made a call so wait a minute
        sleepLessProcessTime(query.time.end, queryTimeBegin)
    }
}

appendChartData <- function(db,id,timeFrame){
    if (timeFrame %in% c("24h","7d","30d","90d")) {
        fSaveAsNull <- FALSE
        querySourceVal <- switch(
            timeFrame,
            "24h" = 4,
            "7d"=6,
            "30d" = 8,
            "90d" = 10
        )
        # check for a site down error
        get.url <- paste0(
            "https://www.predictit.org/api/Public/GetMarketChartData/",
            id,
            "?timespan=",
            timeFrame,
            "&showHidden=true"
        )
        http.response <- httr::GET(
            get.url
        )
        delayDurringSiteMaintanence(http.response, get.url)
        # Get the data again and append it to the table
        if(nchar(rawToChar(http.response$content))==0){
            fSaveAsNull <- TRUE
        } else {
            if(siteReturnedNothing(http.response)){
                fSaveAsNull <- TRUE
            } else {
                r <- jsonlite::fromJSON(rawToChar(http.response$content))
                this.market <- NULL
                error.checking <- NULL
                if(length(r)>0){
                    error.checking <- try(
                        this.market  <- as.data.frame(r) %>%
                            select(-.data$dateString) %>%
                            select(-.data$lineColor)
                    )
                }
                if (!is.null(this.market)) {
                    this.market[["querySource"]] <- rep(
                        querySourceVal,
                        length(unlist(this.market[1]))
                    )
                    message("Saving market id : ",id," @ ",timeFrame)
                    error.checking <- try(
                        results <- DBI::dbAppendTable(conn=db,"marketopenChartData",this.market)
                    )
                } else {
                    fSaveAsNull <- TRUE
                }
            }
        }
        if(fSaveAsNull){
            #Store these marketId values in a nul"l table so we can exlude them from future attempts
            message("Warning: market ID ",id,"@",timeFrame," returned no content")
            error.checking <- try(
                results <- RSQLite::dbSendQuery(
                    conn=db,
                    paste0(
                        "INSERT INTO marketNotExist (marketId, querySourceId) ",
                        "VALUES (", id, ",",querySourceVal,");"
                    )
                )
            )
            error.checking <-try(
                dbClearResult(results)
            )
        }
    } else {
        message("ERROR : appendChartData requires a valid timeFrame 24h/7d/30d/90d")
    }
}

getChartDataValue <- function(openMarkets,db){
    # rather than using a lapply here a for loop is used in this process as we NEVER plan on parallelizing
    for (market.id in seq_along(openMarkets)){
        queryTimeBegin <- Sys.time()
        message("getting market chart data: ",openMarkets[market.id])
        appendChartData(db,openMarkets[market.id],"24h")
        appendChartData(db,openMarkets[market.id],"7d")
        appendChartData(db,openMarkets[market.id],"30d")
        appendChartData(db,openMarkets[market.id],"90d")
        query.time.end <- Sys.time()
        # we just made a call so wait a minute
        sleepLessProcessTime(query.time.end, queryTimeBegin)
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

executeSqlFromFile <- function(sqlFile,db){
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

getOpenMarkets <- function(databaseFileName,configUseInMemoryDatabase){
    # all markets are updated every delaySeconds seconds...
    # so for a history we would need to capture a new timestamp's worth of data every minute...
    # If I was to do this then I need to normalize the data into a table RSQLlight?
    # may need for streaming annalysis?
    # pacman::p_load(stream)

    db <- openDbConn(databaseFileName, configUseInMemoryDatabase)
    # Don't forget to clean up open database connections with:
    # DBI::dbDisconnect(db)
    existing.tables <- DBI::dbListTables(db)
    all.market.data.now <- rpredictit::all_markets()
    #get all open markets: (starting at 1231)
    openMarkets <- setdiff(seq(1231,max(all.market.data.now$id)),all.market.data.now$id)

    # Create database and all planned tables if no tables exist,
    # setup foregin keys with: https://www.techonthenet.com/sqlite/foreign_keys/foreign_keys.php
    # on refactor we could create tables one at a time if that particular table is missing by checking:
    # .dbSendQueryFetch("SELECT DISTINCT tbl_name FROM sqlite_master WHERE tbl_name =='expectedTableName'")
    # Create any missin the database tables
    # SQL documentation: https://www.sqlite.org/lang.html
    # These sql queries are built to only execute if a table or index is missing
    # Create Tables
    executeSqlFromFile(
        file.path(projectDirectory,"sql","01aCreateDbTables.sql"),
        db
    )
    # Create indexes
    executeSqlFromFile(
        file.path(projectDirectory,"sql","01bCreateDbIndexes.sql"),
        db
    )

    #Insert lookup_querySource rows we expect an error here if the querySourceId allready exists, so we ignore it
    try(
        .dbSendQueryClear(
            "
            INSERT INTO querySource (querySourceId,name,shortName,status) VALUES
            (1,'MarketData by market ID','Market','Closed'),
            (2,'MarketDataAll','All Data','Open'),
            (3,'MarketTweetData','Tweets','Open'),
            (4,'GetMarketChartData;24 Hours prior to Close','24h Closed','Closed'),
            (5,'GetMarketChartData;24 Hour on an active market','24h Active'),
            (6,'GetMarketChartData;7 Days prior to Close','7d Closed','Closed'),
            (7,'GetMarketChartData;7 Days on an active market','7d Active','Active'),
            (8,'GetMarketChartData;30 Days prior to Close','30d Closed','Closed'),
            (9,'GetMarketChartData;30 Days on an active market','30d Active','Active'),
            (10,'GetMarketChartData;90 Days prior to Close','90d Closed','Closed'),
            (11,'GetMarketChartData;90 Days on an active market','90d Active','Active');
            ",
            db
        ),
        silent = TRUE
    )
    existingopenData <- .dbSendQueryFetch(
        "
        SELECT DISTINCT marketId
        FROM marketObservation
        WHERE marketStatus LIKE 'open'
        ",
        db
    )
    existingChartData <- .dbSendQueryFetch(
        "
        SELECT DISTINCT marketId
        FROM marketChartData
        WHERE querySourceId IN (4,6,8,10)
        ",
        db
    )
    nullMarketId <- .dbSendQueryFetch(
        "
        SELECT DISTINCT marketId
        FROM marketNotExist
        WHERE querySourceId == 1
        ",
        db
    )
    # get all open markets that we don't yet have and that we tried, but returned NULL contents
    openMarketsMissingData <- base::setdiff(openMarkets,union(unlist(existingopenData),unlist(nullMarketId)))
    openMarketsMissingChartData <- base::setdiff(openMarkets,union(unlist(existingChartData),unlist(nullMarketId)))

    # [TODO] fix requestMarketopenValue AND getChartDataValue
    if(length(openMarketsMissingData)==0){
        message("No newly open markets exist")
    } else {
        requestMarketopenValue(openMarketsMissingData, db)
    }
    if(length(openMarketsMissingChartData)==0){
        message("No newly open markets exist")
    } else {
        getChartDataValue(openMarketsMissingChartData,db)
    }

    if(configUseInMemoryDatabase == TRUE){
        # save our in memory database to file
        if(file.exists(databaseFileName)){
            file.remove(databaseFileName)
        }
        dbFile <- DBI::dbConnect(
            RSQLite::SQLite(),
            dbname=databaseFileName
        )
        RSQLite::sqliteCopyDatabase(
            db,
            dbFile
        )
        DBI::dbDisconnect(dbFile)
    }
    # Clean up open database connections
    DBI::dbDisconnect(db)
}

# Kicks off getting all open markets daily
repeat{
    queryTimeBeginDaily <- Sys.time()
    # Repeat forever, getting the new content
    getOpenMarkets(file.path(projectDirectory,"data","data.sqlite"), configUseInMemoryDatabase)
    query.time.end.daily <- Sys.time()

    ## Delay one day between each call
    ## Note: Should probably schedule this script to run daily with the user's OS scheduling solution,
    ## but currently the first run could take roughtly 4 days to finish
    message("search complete, waiting a day.")
    days.worth.of.seconds <- 86400
    if(as.double(difftime(query.time.end.daily,queryTimeBeginDaily, tz,units ="secs"))<days.worth.of.seconds){
        Sys.sleep(days.worth.of.seconds-as.double(difftime( query.time.end.daily,queryTimeBeginDaily, tz,units ="secs")))
    }
}
