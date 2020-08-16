# Ensure we can use package management
if(!require(pacman)){install.packages(pacman)}
pacman::p_load(rpredictit, DBI, RSQLite, png, dplyr, curl, rstudioapi)
configUseInMemoryDatabase <- FALSE
# if configUseInMemoryDatabase is TRUE, then the sqlite database will be
# read into memory, and all manipulations will happen in RAM, uppon completion
# we write the database from memory back to file.
# see: https://www.sqlite.org/inmemorydb.html
# building out a normalized schema, with: https://dbdiagram.io/d
# then using https://www.rebasedata.com/convert-mysql-to-sqlite-online

pacman::p_load(magrittr)
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
projectDirectory <- dirname(scriptFileName())

# pacman::p_load(uuid)

createMarketSql <-
"
    CREATE TABLE `market` (
        `marketId`	INTEGER,
        `name`	TEXT,
        `sortName`	TEXT,
        `url`	TEXT,
        `dateEnd`	DATETIME,
        `image`	TEXT,
        PRIMARY KEY(`marketId`)
    );
"

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
columns <- list(
    market="
        id INTEGER,
        name TEXT,
        shortName TEXT,
        image TEXT,
        image_rowId INTEGER
    ",
    contract="
        contract_id INTEGER,
        image_rowId INTEGER,
        contract_name TEXT,
        contract_shortName TEXT
    ",
    marketObservation="
        timeStamp DATETIME,
        id INTEGER,
        status TEXT,
        contract_id INTEGER,
        contract_status TEXT,
        lastTradePrice DOUBLE,
        bestBuyYesCost DOUBLE,
        bestBuyNoCost DOUBLE,
        bestSellYesCost DOUBLE,
        bestSellNoCost DOUBLE,
        lastClosePrice DOUBLE,
        displayOrder INTEGER,
        querySource INTEGER
    ",
    chartData="
        marketId INTEGER,
        contractId INTEGER,
        date DATETIME,
        openSharePrice DOUBLE,
        highSharePrice DOUBLE,
        lowSharePrice DOUBLE,
        closeSharePrice DOUBLE,
        tradeVolume DOUBLE,
        querySource INTEGER
    "
)

marketColumns <- (
    "
    id INTEGER,
    name TEXT,
    shortName TEXT,
    image TEXT,
    url TEXT
    "
)

contractColumns <- (
    "
    contract_id INTEGER,
    contract_image TEXT,
    contract_name TEXT,
    contract_shortName TEXT
    "
)

marketObservationColumns <- (
    "
    timeStamp DATETIME,
    id INTEGER,
    name TEXT,
    shortName TEXT,
    image TEXT,
    url TEXT,
    status TEXT,
    contract_id INTEGER,
    dateEnd DATETIME,
    contract_image TEXT,
    contract_name TEXT,
    contract_shortName TEXT,
    contract_status TEXT,
    lastTradePrice DOUBLE,
    bestBuyYesCost DOUBLE,
    bestBuyNoCost DOUBLE,
    bestSellYesCost DOUBLE,
    bestSellNoCost DOUBLE,
    lastClosePrice DOUBLE,
    displayOrder INTEGER,
    querySource INTEGER
    "
)

chartDataColumns <- (
    "
    marketId INTEGER,
    contractId INTEGER,
    contractName TEXT,
    date DATETIME,
    openSharePrice DOUBLE,
    highSharePrice DOUBLE,
    lowSharePrice DOUBLE,
    closeSharePrice DOUBLE,
    tradeVolume DOUBLE,
    querySource INTEGER
    "
)

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

getMarketClosedValue <- function(closedMarkets,db){
    # rather than using a lapply here a for loop is used in this process as we NEVER plan on parallizing
    for (market.id in seq_along(closedMarkets)){
        queryTimeBegin <- Sys.time()
        message("getting market: ",closedMarkets[market.id])
        get.url <- paste0(
            "https://www.predictit.org/api/marketdata/markets/",
            closedMarkets[market.id]
        )
        http.response <- httr::GET(get.url)
        delayDurringSiteMaintanence(http.response, get.url)
        if(siteReturnedNothing(http.response)){
            message("Warning: market ID ",closedMarkets[market.id]," returned no content")
            error.checking <- try(
                results <- RSQLite::dbSendQuery(
                    conn=db,
                    paste0(
                        "INSERT INTO market_id_null (id,querySource) ",
                        "VALUES (", closedMarkets[market.id], ",",1,");"
                    )
                )
            )
            dbClearResult(results)
        } else{
            this.market <- rpredictit::single_market(closedMarkets[market.id])
            this.market[["querySource"]] <- rep(1,length(unlist(this.market[1])))
            message("attempting to query id:",closedMarkets[market.id])
            error.checking <- try(
                result <- DBI::dbAppendTable(conn=db,"market_observations",this.market)
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
                        results <- DBI::dbAppendTable(conn=db,"marketClosedChartData",this.market)
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
                        "INSERT INTO market_id_null (id, querySource) ",
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

getChartDataValue <- function(closedMarkets,db){
    # rather than using a lapply here a for loop is used in this process as we NEVER plan on parallelizing
    for (market.id in seq_along(closedMarkets)){
        queryTimeBegin <- Sys.time()
        message("getting market chart data: ",closedMarkets[market.id])
        appendChartData(db,closedMarkets[market.id],"24h")
        appendChartData(db,closedMarkets[market.id],"7d")
        appendChartData(db,closedMarkets[market.id],"30d")
        appendChartData(db,closedMarkets[market.id],"90d")
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

getClosedMarkets <- function(){
    # all markets are updated every delaySeconds seconds...
    # so for a history we would need to capture a new timestamp's worth of data every minute...
    # If I was to do this then I need to normalize the data into a table RSQLlight?
    # may need for streaming annalysis?
    # pacman::p_load(stream)
    databaseFileName <- file.path(projectDirectory,"data","closed.sqlite")
    if(configUseInMemoryDatabase == TRUE){
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
    existing.tables <- DBI::dbListTables(db)
    all.market.data.now <- rpredictit::all_markets()
    #get all closed markets: (starting at 1231)
    closedMarkets <- setdiff(seq(1231,max(all.market.data.now$id)),all.market.data.now$id)

    # Create database and all planned tables if no tables exist,
    # setup foregin keys with: https://www.techonthenet.com/sqlite/foreign_keys/foreign_keys.php
    # on refactor we could create tables one at a time if that particular table is missing by checking:
    # .dbSendQueryFetch("SELECT DISTINCT tbl_name FROM sqlite_master WHERE tbl_name =='expectedTableName'")
    if(length(existing.tables)==0 ){
        # Create the base tables
        # SQL documentation: https://www.sqlite.org/lang.html
        queryStatements <- c(
            paste0(
                "CREATE TABLE market (",
                marketColumns,",
                PRIMARY KEY (id)
                )"
            ),
            paste0(
                "CREATE TABLE contract (",
                contractColumns,",
                PRIMARY KEY (contract_id)
                )"
            ),
            paste0(
                "CREATE TABLE market_observations (",
                marketObservationColumns,",
                PRIMARY KEY (timeStamp, id, contract_id, querySource)
                )"
            ),
            paste0(
                "CREATE TABLE marketClosedChartData (",
                chartDataColumns,",
                PRIMARY KEY (date, marketId, contractId, querySource)
                )"
            ),
                "CREATE TABLE image (
                	imageUrl	TEXT,
                	image	BLOB,
                	type	TEXT,
                	rowId	INTEGER PRIMARY KEY AUTOINCREMENT,
                	length	INTEGER
                )"
            ,
            "
                CREATE TABLE `market_id_null` (
                	`id`	INTEGER,
                	`querySource`	INTEGER,
                	PRIMARY KEY(`id`,`querySource`)
                )
            "
            ,
            "
                CREATE TABLE lookup_querySource (
                    id INTEGER,
                    Name TEXT,
                    shortName TEXT,
                    PRIMARY KEY (id)
                )
            "
        )

        base::lapply(queryStatements,FUN=.dbSendQueryClear,db)
        # Create indexes, and insert lookup_querySource rows
        queryStatements <- c(
                "CREATE UNIQUE INDEX `contract_id_index` ON `market_observations` (
	                `contract_id`	ASC
                );"
            ,
            "CREATE UNIQUE INDEX `image_id_index` ON `image` (
                `imageUrl`	ASC
            );",
            "CREATE UNIQUE INDEX `null_id_index` ON `market_id_null` (
            	`id`	ASC,
            	`querySource`	ASC
            );",
            "CREATE UNIQUE INDEX `id_index` ON `lookup_querySource` (
	            `id` ASC
            );",
            "
            INSERT INTO lookup_querySource (id,Name,shortName) VALUES
            	(1,'MarketData by market ID','Market'),
            	(2,'MarketDataAll','All Data'),
            	(3,'MarketTweetData','Tweets'),
            	(4,'GetMarketChartData;24 Hours prior to Close','24h Closed'),
            	(5,'GetMarketChartData;24 Hour on an active market','24h Active'),
            	(6,'GetMarketChartData;7 Days prior to Close','7d Closed'),
            	(7,'GetMarketChartData;7 Days on an active market','7d Active'),
            	(8,'GetMarketChartData;30 Days prior to Close','30d Closed'),
            	(9,'GetMarketChartData;30 Days on an active market','30d Active'),
            	(10,'GetMarketChartData;90 Days prior to Close','90d Closed'),
            	(11,'GetMarketChartData;90 Days on an active market','90d Active');
            "
        )
        base::lapply(queryStatements,FUN=.dbSendQueryClear,db)
        # [TODO] add any views or triggers that may be needed:
        # https://www.sqlite.org/lang_createtrigger.html
        # https://www.sqlite.org/lang_createview.html

        closedMarketsMissingData <- closedMarkets
        closedMarketsMissingChartData <- closedMarkets
    } else {
        existingClosedData <- .dbSendQueryFetch(
            "
                SELECT DISTINCT id
                FROM market_observations
                WHERE Status LIKE 'Closed'
            ",
            db
        )
        existingChartData <- .dbSendQueryFetch(
            "
                SELECT DISTINCT marketId as id
                FROM marketClosedChartData
                WHERE querySource == 4
            ",
            db
        )
        nullMarketId <- .dbSendQueryFetch(
            "
                SELECT DISTINCT id
                FROM market_id_null
                WHERE querySource == 1
            ",
            db
        )
        # get all closed markets that we don't yet have and that we tried, but returned NULL contents
        closedMarketsMissingData <- base::setdiff(closedMarkets,union(unlist(existingClosedData),unlist(nullMarketId)))
        nullMarketId <- .dbSendQueryFetch(
            "
                SELECT DISTINCT id
                FROM market_id_null
                WHERE querySource IN (4,6,8,10)
            ",
            db
        )
        closedMarketsMissingChartData <- base::setdiff(closedMarkets,union(unlist(existingChartData),unlist(nullMarketId)))
    }
    if(length(closedMarketsMissingData)==0){
        message("No newly closed markets exist")
    } else {
        getMarketClosedValue(closedMarketsMissingData, db)
    }
    if(length(closedMarketsMissingChartData)==0){
        message("No newly closed markets exist")
    } else {
        getChartDataValue(closedMarketsMissingChartData,db)
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

# Kicks off getting all closed markets daily
repeat{
    queryTimeBeginDaily <- Sys.time()
    # Repeat forever, getting the new content
    getClosedMarkets()
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
