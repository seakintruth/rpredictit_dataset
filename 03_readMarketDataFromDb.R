# Ensure we can use package management
if(!require(pacman)){install.packages(pacman)}
pacman::p_load(RSQLite, DBI,magrittr)

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

.open.predictit.db <- function(){
    db <- DBI::dbConnect(
        RSQLite::SQLite(),
        dbname=file.path(projectDirectory,"data","closed.sqlite")
    )
    return(db)
}

readClosedMarketDataFromDb <- function(){
    db <- .open.predictit.db()
    test.existing.closed.result <- RSQLite::dbSendQuery(
        conn=db,
        "SELECT * FROM market_observations WHERE Status LIKE 'Closed'"
    )
    test.existing.closed.data <-RSQLite::dbFetch(test.existing.closed.result)
    dbClearResult(test.existing.closed.result)
    RSQLite::dbDisconnect(db)
    return(test.existing.closed.data)
}

readOpenMarketDataFromDb <- function(){
    db <- .open.predictit.db()
    test.existing.open.result <- RSQLite::dbSendQuery(
        conn=db,
        "SELECT * FROM market_observations WHERE Status LIKE 'Open'"
    )
    test.existing.open.data <-RSQLite::dbFetch(test.existing.open.result)
    dbClearResult(test.existing.open.result)
    RSQLite::dbDisconnect(db)
    return(test.existing.open.data)
}

readNullIdMarketDataFromDb <- function(){
    db <- .open.predictit.db()
    null.id.result <- RSQLite::dbSendQuery(
        conn=db,
        "SELECT DISTINCT id FROM market_id_null"
    )
    null.id.data <-RSQLite::dbFetch(null.id.result)
    dbClearResult(null.id.result)
    RSQLite::dbDisconnect(db)
    return(null.id.data)
}

readClosedChartDataFromDb <- function(){
    db <- .open.predictit.db()
    result <- RSQLite::dbSendQuery(
        conn=db,
        "
        SELECT
        	marketClosedChartData._rowid_ as chartDataRowId,
        	marketClosedChartData.*,
        	market_observations._rowid_ as observationRowId,
        	market_observations.*,
        	lookup_querySource.shortName as querySourceName
        FROM
        	marketClosedChartData
        	INNER JOIN market_observations on marketClosedChartData.contractId = market_observations.contract_id
        	INNER JOIN lookup_querySource on marketClosedChartData.querySource = lookup_querySource.id
        WHERE
        	marketClosedChartData.querySource in (4,6,8,10)
        	AND market_observations.querySource = 1
        "
    )
    data <-RSQLite::dbFetch(result)
    dbClearResult(result)
    RSQLite::dbDisconnect(db)
    return(data)
}

closed.results <- readClosedMarketDataFromDb()
open.results <- readOpenMarketDataFromDb()
null.results <- readNullIdMarketDataFromDb()
closed.chart.data <- readClosedChartDataFromDb()

saveRDS(closed.results,file.path(projectDirectory,"data","closedResults.RDS"))
saveRDS(open.results,file.path(projectDirectory,"data","openResults.RDS"))
saveRDS(null.results,file.path(projectDirectory,"data","nullResults.RDS"))
saveRDS(closed.chart.data,file.path(projectDirectory,"data","closedChartData.RDS"))

