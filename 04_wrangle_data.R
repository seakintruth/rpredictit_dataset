# Ensure we can use package management
if(!require(pacman)){install.packages(pacman)}
# Load all of this script's packages
pacman::p_load(tidyverse)


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

#load our data set if not present
if (!exists("closed.results")) {
    projectDirectory %>%
        file.path("03_readMarketDataFromDb.R") %>%
        source(echo=TRUE)
    # timestamp is the date/time the data was pulled...
    closed.results.original <- closed.results
    closed.chart.data.original <- closed.chart.data
}

# Start wrangling!
# convert dateEnd column to dateTime
# update our timestamp and date column to be a date/time value
# most common format is:   ymd_hms() , some are not,  attempting to sue guess_formats
closed.results <- closed.results %>%
    mutate(dateEnd = closed.results$dateEnd %>% lubridate::ymd_hms()) %>%
    mutate(timeStamp = closed.results$timeStamp %>% lubridate::as_datetime()) %>%
    mutate(dateEndCharacterLength = closed.results.original$dateEnd %>% stringr::str_length())

closed.results %>% dplyr::select(dateEnd,timeStamp) %>% summary()
# nearly a quarter of all data has no end date, so we can't use this value as is

# we should be able to get our last date from the max date of closed.chart.data
# for each contract
# convert date column to date object
closedChartDate <- closed.chart.data %>%
    dplyr::select(contractId,date) %>%
    dplyr::mutate(date = lubridate::as_date(lubridate::ymd_hms(date)))

glimpse(closedChartDate)

# this takes a minute, but it worked!
closedChartDate_max <- closedChartDate %>%
    group_by(contractId) %>%
    summarise(lastDate = max(date))


# finding character values that are shorter than expected...
dateEndShort <- closed.results %>%
    dplyr::select(dateEndCharacterLength,dateEnd) %>%
    dplyr::filter(dateEndCharacterLength < 19) %>%
    dplyr::select(dateEnd)



# set our keys to the same name so we can join on that.
closedChartDate_max <- closedChartDate_max %>%
    dplyr::rename(contract_id = contractId)
closed.results.endDate <-  closed.results %>%
    dplyr::full_join(closedChartDate_max,by="contract_id")

#tibble::glimpse(closed.results.endDate)
colSums(is.na(closed.results.endDate))
# we still have 5k and 6k missing last date so lets dplyr::coalesce()
# to see if we gained anything from the chart data

# closed.results.endDate$lastDate
closed.results.endDate$lastEndDate <-  dplyr::coalesce(
    closed.results.endDate$lastDate,
    closed.results.endDate$dateEnd %>% lubridate::as_date()
)

colSums(is.na(closed.results.endDate))
# Yup, that works, we went from 25% down to 7.7% missing end dates!

summary(closed.results.endDate$lastDate)
#closed.results %>% tibble::glimpse()
# Markets with a single contract are yes/no questions
# (Yes = .99 and no= 0.01 price) let's create a cloumn identifying those
#summary(closed.results)
#summary(closed.results.endDate)
# let's export our cleaned up data
closed.results.clean <- closed.results.endDate %>%
    rename(
        lastTradeDate = lastDate,
        lastClosedDate = lastEndDate,
        expectedDateEnd = dateEnd,
        marketName = name,
        marketShortName = shortName,
        market_id = id
    ) %>%
    dplyr::mutate(expectedDateEnd =  lubridate::as_date(expectedDateEnd))  %>%
    select(
        -contract_image,-contract_status,-image,
        -status,-url,-querySource,-bestBuyYesCost,-bestBuyNoCost,
        -bestSellNoCost,-bestSellYesCost
    )

rm("closed.results","closed.chart.data","closed.results.endDate","closedChartDate")

# saveRDS(closed.results.clean,file.path(projectDirectory,"data", "closedResultsClean.RDS"))
# ------------------------------------------------------
# a little exploration
# ------------------------------------------------------
# glimpse( closed.results.clean)
market.obs <- closed.results.clean %>%
    dplyr::group_by(market_id,marketName,marketShortName,timeStamp) %>%
    count()

contract.obs <- closed.results.clean %>%
    dplyr::select(
        lastClosedDate,
        market_id,
        contract_id,
        contract_name,
        contract_shortName,
        lastTradePrice,
        lastClosePrice
    )

midRangeFilter <- .15

contractMidFilter <- contract.obs %>%
    dplyr::filter(lastTradePrice < (1-midRangeFilter)) %>%
    dplyr::filter(lastTradePrice > midRangeFilter)

contractClosedYes <- contract.obs %>%
    dplyr::filter(lastTradePrice >= (1-midRangeFilter))
contractClosedNo <- contract.obs %>%
    dplyr::filter(lastTradePrice <= (midRangeFilter))

contractClosedYes_Summary <- contractClosedYes %>%
    select(lastTradePrice,lastClosedDate)
contractClosedNo_Summary <- contractClosedNo %>%
    select(lastTradePrice,lastClosedDate)
summary(contractClosedYes_Summary)
summary(contractClosedNo_Summary)
# --------------------------------------------------------------------------
# Daily Grouping Frequency
contractClosedYesDaily <- contractClosedYes %>%
    select(lastClosedDate,lastTradePrice)

contractClosedNoDaily <- contractClosedNo %>%
    select(lastClosedDate,lastTradePrice)

contractClosedYesDailyCount <- contractClosedYesDaily %>%
    group_by(lastClosedDate) %>%
    tally() %>%
    dplyr::filter(!is.na(lastClosedDate)) %>%
    rename('Contract Count' = n,'Planned Close Date' = lastClosedDate)

contractClosedNoDailyCount <- contractClosedNoDaily %>%
    group_by(lastClosedDate) %>%
    tally() %>%
    dplyr::filter(!is.na(lastClosedDate)) %>%
    rename('Contract Count' = n,'Planned Close Date' = lastClosedDate)

# Plot Points
#plot(contractClosedYesDailyCount,type="p",col="red")
#lines(contractClosedNoDailyCount,type="p",col="blue")

# Plot hist and lines
#plot(contractClosedYesDailyCount,type="h", col="red",lwd=2)
#lines(stats::lowess(contractClosedYesDailyCount),col="red",lwd = 5)
#plot(contractClosedNoDailyCount,type="h", col="light blue",lwd = 5)
#lines(stats::lowess(contractClosedNoDailyCount),col="blue",lwd = 5)
# --------------------------------------------------------------------------
# Weekly Grouping Frequency
contractClosedYesWeekly <- contractClosedYes %>%
    select(lastClosedDate,lastTradePrice) %>%
    mutate(lastClosedDate=lubridate::isoweek(lastClosedDate)) %>%
    select(lastClosedDate,lastTradePrice)
#summary(contractClosedYesWeekly)
contractClosedNoWeekly <- contractClosedNo %>%
    select(lastClosedDate,lastTradePrice) %>%
    mutate(contractClosedYesWeekly=lubridate::as_datetime(contractClosedNo$lastClosedDate)) %>%
    mutate(lastClosedDate=lubridate::isoweek(lastClosedDate)) %>%
    select(lastClosedDate,lastTradePrice)
#summary(contractClosedNoWeekly)
contractClosedYesWeeklyCount <- contractClosedYesWeekly %>%
    group_by(lastClosedDate) %>%
    tally() %>%
    dplyr::filter(!is.na(lastClosedDate)) %>%
    rename('Contract Count' = n,'Planned Close Week' = lastClosedDate)
contractClosedNoWeeklyCount <- contractClosedNoWeekly %>%
    group_by(lastClosedDate) %>%
    tally() %>%
    dplyr::filter(!is.na(lastClosedDate)) %>%
    rename('Contract Count' = n,'Planned Close Week' = lastClosedDate)

# Plot Points
# plot(contractClosedNoWeeklyCount,type="p",col="blue")
# lines(contractClosedYesWeeklyCount,type="p",col="red")

# Plot hist and lines

# plot(contractClosedNoWeeklyCount,type="h", col="light blue",lwd=5)
# lines(stats::lowess(contractClosedNoWeeklyCount),type="l",col="blue",lwd = 5)

# plot(contractClosedYesWeeklyCount,type="h", col="pink",lwd=5)
# lines(stats::lowess(contractClosedYesWeeklyCount),type="l",col="red",lwd = 5)
# --------------------------------------------------------------------------
# Monthly Grouping Frequency
contractClosedYesMonthly <- contractClosedYes %>%
    select(lastClosedDate,lastTradePrice) %>%
    mutate(lastClosedDate=lubridate::month(lastClosedDate)) %>%
    select(lastClosedDate,lastTradePrice)
contractClosedNoMonthly <- contractClosedNo %>%
    select(lastClosedDate,lastTradePrice) %>%
    mutate(lastClosedDate=lubridate::month(lastClosedDate)) %>%
    select(lastClosedDate,lastTradePrice)

contractClosedYesMonthlyCount <- contractClosedYesMonthly %>%
    group_by(lastClosedDate) %>%
    tally() %>%
    dplyr::filter(!is.na(lastClosedDate)) %>%
    rename('Contract Count' = n,'Planned Close Month' = lastClosedDate)
contractClosedNoMonthlyCount <- contractClosedNoMonthly %>%
    group_by(lastClosedDate) %>%
    tally() %>%
    dplyr::filter(!is.na(lastClosedDate)) %>%
    rename('Contract Count' = n,'Planned Close Month' = lastClosedDate)

# Plot Points
#plot(contractClosedNoMonthlyCount,type="p",col="blue")
#lines(contractClosedYesMonthlyCount,type="p",col="red")


# Plot hist and lines
#plotYlimRange <-c(
#    contractClosedYesMonthlyCount$`Contract Count` #,
##    contractClosedNoMonthlyCount$`Contract Count`
#)
# plotYlimRange <-c(
#    min(plotYlimRange,na.rm=TRUE),
#    max(plotYlimRange,na.rm=TRUE)
# )
# plot(contractClosedNoMonthlyCount,type="h", col="light blue",lwd=20)
# lines(stats::lowess(contractClosedNoMonthlyCount),type="l",col="blue",lwd = 5)

#plot(contractClosedYesMonthlyCount,type="h", col="pink",lwd=20)
#lines(stats::lowess(contractClosedYesMonthlyCount),type="l",col="red",lwd = 5)

# --------------------------------------------------------------------------
# [TODO] Export all plots , This doesn't work as expected skipping for now
if (FALSE) {
    plots.png.paths <- list.files(
        list.files(
            tempdir(),
            pattern="rs-graphics",
            full.names = TRUE
        ),
        full.names = TRUE
    )
    setdiff(plots.png.paths,plots.png.prior.to.paths)
    dir.create(file.path(projectDirectory,"plots"),recursive=TRUE,showWarnings = FALSE)
    file.copy(
        setdiff(
            plots.png.paths,
            plots.png.prior.to.paths
        ),
        file.path(projectDirectory,"plots")
    )
}

undecidedContracts <- market.obs %>%
    dplyr::filter(market_id %in% contractMidFilter$market_id)
# skimr::skim(undecidedContracts)

write.csv(
    undecidedContracts,
    file.path(projectDirectory,"undecided_contracts.csv"))

# str(closed.results.clean)
# glimpse(closed.results.clean)
# summary(closed.results.clean)
# skimr::skim(closed.results.clean)
