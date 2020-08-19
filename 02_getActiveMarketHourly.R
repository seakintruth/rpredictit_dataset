# Ensure we can use package management
if(!require(pacman)){install.packages(pacman)}
pacman::p_load(rpredictit, DBI, RSQLite, png, dplyr, curl, rstudioapi, lubridate)


configUseInMemoryDatabase <- FALSE
# if configUseInMemoryDatabase is TRUE, then the sqlite database will be

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

delayDurringSiteMaintanence <- function(http.response, check.url){
    # handle a site maintenance message:
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
            market.observations <- this.market %>%
                dplyr::rename(
                    dateTimeStamp = timeStamp,
                    contractId = contract_id,
                    contractStatus = contract_status,
                    marketId = id,
                    marketStatus = status,
                    querySourceId = querySource
                ) %>%
                dplyr::select(
                    dateTimeStamp,
                    dateEnd,
                    contractId,
                    contractStatus,
                    marketId,
                    marketStatus,
                    lastTradePrice,
                    bestBuyYesCost,
                    bestBuyNoCost,
                    bestSellYesCost,
                    bestSellNoCost,
                    lastClosePrice,
                    displayOrder,
                    querySourceId
                ) %>%
                dplyr::mutate(dateEnd = lubridate::ymd_hms(dateEnd))
        }
        error.appendMarketObservation <- try(
            result <- DBI::dbAppendTable(conn=db,"marketObservation",market.observations)
        )
        contracts <- this.market %>%
            dplyr::rename(
                contractId = contract_id,
                imageUrl = contract_image,
                marketId = id,
                contractName = contract_name,
                contractShortName = contract_shortName
            ) %>%
            dplyr::select(
                contractId ,
                imageUrl ,
                marketId ,
                contractName,
                contractShortName,
                dateEnd
            ) %>%
            dplyr::mutate(dateEnd = lubridate::ymd_hms(dateEnd))

        error.appendContract <- try(
            result <- DBI::dbAppendTable(conn=db,"contract",contracts)
        )
        markets <- this.market %>%
            dplyr::rename(
                marketId = id,
                imageUrl = image
            ) %>%
            dplyr::select(
                marketId ,
                name ,
                shortName ,
                url,
                imageUrl ,
                dateEnd
            ) %>%
            dplyr::mutate(dateEnd = lubridate::ymd_hms(dateEnd))
        error.appendMarket <- try(
            result <- DBI::dbAppendTable(conn=db,"market",markets)
        )
        query.time.end <- Sys.time()
        # we just made a call so wait our delay period
        sleepLessProcessTime(query.time.end, queryTimeBegin)
    }
}

appendChartData <- function(db,id,timeFrame){
    if (timeFrame %in% c("24h","7d","30d","90d")) {
        fSaveAsNull <- FALSE
        querySourceVal <- switch(
            timeFrame,
            "24h" = 4,
            "7d" = 6,
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
                    # Normalize the data a place into the database

                    marketChartData <- this.market %>%
                        mutate(date=lubridate::ymd_hms(date)) %>%
                        dplyr::rename(dateTimeStamp = date,
                                      querySourceId = querySource) %>%
                        dplyr::select(marketId,
                                      contractId,
                                      dateTimeStamp,
                                      querySourceId,
                                      openSharePrice,
                                      highSharePrice,
                                      lowSharePrice,
                                      closeSharePrice,
                                      tradeVolume
                      )
                    error.checking <- try(
                        results <- DBI::dbAppendTable(conn=db,
                                      "marketChartData",marketChartData)
                    )
                    # we aren't saving contract info from chart data,
                    # we save it from the API call...
                    if (FALSE) {
                        error.checking.add.contract <- try(
                            results <- DBI::dbAppendTable(
                                conn=db,
                                "contract",
                                this.market %>%
                                    dplyr::select(contractId,marketId,contractName) %>%
                                    unique()
                            )
                        )
                        if (class(error.checking.add.contract) == "try-error") {
                            #[todo] contractId allready exists, maybe update contract Name?
                        }
                    }
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
    # rather than using a lapply here a for loop is used in this process as we
    # NEVER plan on parallelizing
    for (market.id in seq_along(openMarkets)){
        queryTimeBegin <- Sys.time()
        message("getting market chart data: ",openMarkets[market.id])
        appendChartData(db,openMarkets[market.id],"7d")
        appendChartData(db,openMarkets[market.id],"24h")
        appendChartData(db,openMarkets[market.id],"30d")
        appendChartData(db,openMarkets[market.id],"90d")
        query.time.end <- Sys.time()
        # we just made a call so wait our delay period
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

#function with pipes
executeSqlFromFile <- function(sqlFile,db){
    queryReturn <-  sqlFile %>%
        readLines(warn=FALSE) %>%
        paste(collapse=" ") %>%
        strsplit(";") %>%
        unlist() %>%
        lapply(FUN=.dbSendQueryClear,db) %>%
        try(silent=TRUE)
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


getOpenMarkets <- function(databaseFileName,configUseInMemoryDatabase){
    # usage:
    # getOpenMarkets(file.path(projectDirectory,"data","openMarkets.sqlite"), configUseInMemoryDatabase)
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
    #get all markets: (starting at 1100)
    openMarkets <- setdiff(seq(1100,max(all.market.data.now$id)),all.market.data.now$id)
    # openMarkets <- all.market.data.now
    # Create database and all planned tables if no tables exist,
    # setup foregin keys with: https://www.techonthenet.com/sqlite/foreign_keys/foreign_keys.php
    # on refactor we could create tables one at a time if that particular table is missing by checking:
    # .dbSendQueryFetch("SELECT DISTINCT tbl_name FROM sqlite_master WHERE tbl_name =='expectedTableName'")
    # Create any missin the database tables
    # SQL documentation: https://www.sqlite.org/lang.html
    # These sql queries are built to only execute if a table or index is missing
    # Create Tables

    attemptCreate <- file.path(projectDirectory,"sql","01aCreateDbTables.sql") %>%
        executeSqlFromFile(db)

    # Create indexes
    attemptIndex <- file.path(projectDirectory,"sql","01bCreateDbIndexes.sql") %>%
        executeSqlFromFile(db)

        # Create indexes
    #Insert lookup_querySource rows we expect an error here if the querySourceId allready exists, so we ignore it
    errFillLookupQuerySource <- try(
        executeSqlFromFile(
            file.path(projectDirectory,"sql","01cInsert-querySource-DefaultValues.sql"),
            db
        )
    )
    #errFillLookupQuerySource

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


hoursWorthOfSeconds <- 60^2
daysWorthOfSeconds <- hoursWorthOfSeconds * 24

# Kicks off getting all open markets daily
repeat{
    queryTimeBeginHourly <- Sys.time()
    # Repeat forever, getting the new content
    #getOpenMarkets(file.path(projectDirectory,"data","data.sqlite"), configUseInMemoryDatabase)
    getOpenMarkets(file.path(projectDirectory,"data","openMarkets.sqlite"), configUseInMemoryDatabase)
    queryTimeEndHourly <- Sys.time()

    ## Delay one day between each call
    ## Note: Should probably schedule this script to run daily with the user's OS scheduling solution,
    ## but currently the first run could take roughtly 4 days to finish
    message("search complete, waiting a hour. Will resume at:")

    if(as.double(difftime(queryTimeEndHourly,queryTimeBeginHourly, tz,units ="secs"))<hoursWorthOfSeconds){
        Sys.sleep(hoursWorthOfSeconds-as.double(difftime( queryTimeEndHourly,queryTimeBeginHourly, tz,units ="secs")))
    }
}
