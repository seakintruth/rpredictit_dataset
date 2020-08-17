# Ensure we can use package management
if(!require(pacman)){install.packages(pacman)}
# Load all of this script's packages
pacman::p_load(tidyverse,ggplot2,corrplot,olsrr) #,
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

if (!exists("closed.results.clean")) {
  projectDirectory %>%
    file.path("04_wrangle_data.R") %>%
    source(echo=TRUE)
}

scatter.smooth(
  x=closed.results.clean$lastClosedDate,
  y=closed.results.clean$Count_of_Contracts.x,
  main = paste0("Count of Contracts ~ Last Trade Price")
)

str(closed.results.clean$lastClosedDate)


hist(closed.results.clean$Count_of_Contracts.x,)
str(closed.results.clean)
glimpse(closed.results.clean)
summary(closed.results.clean)
skimr::skim(closed.results.clean)

#: where is the data with visdat for our missing dates
closed.results.clean %>%
  dplyr::select(
    expectedDateEnd, lastTradeDate,lastClosedDate
  ) %>%
  rename(lastDateCoalesed=lastClosedDate)  %>%
  visdat::vis_dat( )

closed.results.clean %>%
  dplyr::select(
    expectedDateEnd, lastTradeDate,lastClosedDate
  ) %>%
  rename(lastDateCoalesed=lastClosedDate)  %>%
visdat::vis_miss()

closed.market.count.contracts <- closed.results.clean %>%
  dplyr::select(market_id) %>%
  dplyr::group_by(market_id) %>%
  dplyr::count() %>%
  dplyr::rename(Count_of_Contracts=n
)

boxplot(closed.market.count.contracts$Count_of_Contracts)
plot(closed.market.count.contracts$Count_of_Contracts)

closed.market.many.contracts <- closed.market.count.contracts %>%
  dplyr::filter(Count_of_Contracts>10 & Count_of_Contracts<50 )

boxplot(closed.market.many.contracts$Count_of_Contracts)
summary(closed.market.many.contracts)
plot(closed.market.many.contracts$Count_of_Contracts)

# add our count of contracts to the cleaned dataset
closed.results.clean <- closed.results.clean %>%
  dplyr::full_join(closed.market.count.contracts, by="market_id")

#  glimpse(closed.results.clean)
glimpse(closed.results.clean)

# filter for the trump tweet contracts
trump.tweet.markets <- closed.results.clean %>%
  mutate(marketName = stringr::str_to_lower(marketName)) %>%
  filter(stringr::str_detect(marketName,pattern = "trump")) %>%
  filter(stringr::str_detect(marketName,pattern = "tweet")) %>%
  filter(stringr::str_detect(marketName,pattern = "noon")) %>%
  filter(stringr::str_detect(marketName,pattern = "mention",negate=TRUE)) %>%
  filter(stringr::str_detect(marketName,pattern = "trumpjr",negate=TRUE)) %>%
  mutate(contractNameLength= stringr::str_length(contract_shortName)) %>%
  filter(contractNameLength < 15) %>%
  mutate(contract_shortName = stringr::str_replace(contract_shortName,"\\ or\\ more", "+")) %>%
  mutate(contract_shortName = stringr::str_replace(contract_shortName,"\\ or\\ fewer", "-")) %>%
  mutate(contract_shortName = stringr::str_replace(contract_shortName,"\\ or\\ less", "-")) %>%
  mutate(marketShortName = stringr::str_to_lower(marketShortName)) %>%
  mutate(marketShortName = stringr::str_replace(marketShortName,"\\?", "")) %>%
  mutate(marketShortName = stringr::str_replace(marketShortName,"noon\\ ", "")) %>%
  mutate(marketShortName = stringr::str_replace(marketShortName,"noon", "")) %>%
  mutate(marketShortName = stringr::str_replace(marketShortName,"\\@realdonaldtrump\\ tweets\\ ", "")) %>%
  mutate(marketShortName = stringr::str_replace(marketShortName,"trump\\ tweets\\ ", "")) %>%
  mutate(marketShortName = stringr::str_replace_all(marketShortName,"\\ ", "")) %>%
  mutate(marketShortName = stringr::str_replace_all(marketShortName,"to", "\\-")) %>%
  mutate(contract_shortName = stringr::str_replace_all(contract_shortName,"\\ ", "")) %>%
  mutate(contract_shortName = stringr::str_replace(contract_shortName,"\\-(.*)", "")) %>%
  mutate(contract_shortName = stringr::str_replace(contract_shortName,"\\+", ".1")) %>%
  mutate(contract_lowerBinValue = as.numeric(contract_shortName))

view(trump.tweet.markets)

trump.tweet.markets.tmp <- trump.tweet.markets %>%
  filter(!is.na(lastClosedDate)) %>%
  group_by(market_id) %>%
  summarise(lastMarketClosedDate = max(lastClosedDate))

trump.tweet.markets <- full_join(trump.tweet.markets, trump.tweet.markets.tmp,by="market_id")

filterRange <- .15
trump.tweet.markets.yes <- trump.tweet.markets %>%
  filter(lastTradePrice >= (1-filterRange))
trump.tweet.markets.maybe <- trump.tweet.markets %>%
  filter(lastTradePrice < (1-filterRange) & lastTradePrice >=(0+filterRange))
trump.tweet.markets.no <- trump.tweet.markets %>%
  filter(lastTradePrice < (0+filterRange))

unique(trump.tweet.markets.yes$contract_lowerBinValue)
view(trump.tweet.markets.yes)
plot(trump.tweet.markets.yes$lastMarketClosedDate,trump.tweet.markets.yes$contract_lowerBinValue)
summary(trump.tweet.markets.yes$contract_lowerBinValue)
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max.
# 19.00   50.00   70.00   96.82  130.00  484.00

# number of years of data based on number of weeks of yes contracts
datasetDaysByWeek <- length(trump.tweet.markets.yes$contract_lowerBinValue)*7
datasetDaysByWeek/365
# 3.547 years


#number of years of data based on last date - first date
datasetDaysByDate <- (
  trump.tweet.markets.yes$lastMarketClosedDate %>%
    max()
  - trump.tweet.markets.yes$lastMarketClosedDate %>%
    min()
) %>%
as.numeric()

datasetDaysByDate/365
# 3.389 years

NumberOfExtraWeeks <- (datasetDaysByWeek - datasetDaysByDate)/7
# 8.286 weeks
# [todo] need to find out why this descrepancy exists, it's not obvious at first glance

# There appears to be a major divergance in values 1/3rd of the way through 2019
# So let's treat these as two different processes
dtNewProcess <- lubridate::mdy("3/15/2019")
trump.tweet.markets.yes.section1 <- trump.tweet.markets.yes %>%
  filter(lastMarketClosedDate < dtNewProcess)
trump.tweet.markets.yes.section2 <- trump.tweet.markets.yes %>%
  filter(lastMarketClosedDate >= dtNewProcess)
hist(trump.tweet.markets.yes$contract_lowerBinValue)
hist(trump.tweet.markets.yes.section1$contract_lowerBinValue)
hist(trump.tweet.markets.yes.section2$contract_lowerBinValue)
#plot all points
#plot(trump.tweet.markets$lastMarketClosedDate,trump.tweet.markets$contract_lowerBinValue)
#plot(trump.tweet.markets.no$lastMarketClosedDate,trump.tweet.markets.no$contract_lowerBinValue)
plot(trump.tweet.markets.yes$lastMarketClosedDate,trump.tweet.markets.yes$contract_lowerBinValue)
plot(trump.tweet.markets.yes.section1$lastMarketClosedDate,trump.tweet.markets.yes.section1$contract_lowerBinValue)
plot(trump.tweet.markets.yes.section2$lastMarketClosedDate,trump.tweet.markets.yes.section2$contract_lowerBinValue)

trump.tweet.markets.yes %>%
  select(lastMarketClosedDate,contractNameLength,contract_lowerBinValue,) %>%
  summary()
skimr::skim(trump.tweet.markets.yes)

#look as some linear regression models
plot(lm(contract_lowerBinValue ~ lastMarketClosedDate , data = trump.tweet.markets.yes))
plot(lm(contract_lowerBinValue ~ lastMarketClosedDate , data = trump.tweet.markets.yes.section1))
plot(lm(contract_lowerBinValue ~ lastMarketClosedDate , data = trump.tweet.markets.yes.section2))

# Model Selection:
# All Tweets
displaySimpleModelStats <- function(tweetDataToDescribe,strTitle) {
  par(mfrow=c(1, 1))  # divides graph area in 1 columns
  scatter.smooth(
    x=tweetDataToDescribe$lastMarketClosedDate,
    y=tweetDataToDescribe$contract_lowerBinValue,
    main = paste0(strTitle,"Closed Date ~ Tweets")
  )
  par(mfrow=c(1, 2))  # divides graph area in 2 columns
  boxplot(
    tweetDataToDescribe$contract_lowerBinValue,
    main=paste0(strTitle,"Tweets"),
    sub=paste("Number of outlier rows: ",
        length(boxplot.stats(trump.tweet.markets.yes$contract_lowerBinValue)$out)
      )
    )  # box plot for speed
  boxplot(tweetDataToDescribe$lastMarketClosedDate, main=paste0(strTitle,"Date"), sub=paste("Number of outlier rows: ",length(boxplot.stats(trump.tweet.markets.yes$contract_lowerBinValue)$out)))  # box plot for speed
  cor(as.numeric(tweetDataToDescribe$lastMarketClosedDate), tweetDataToDescribe$contract_lowerBinValue)  #correlation between variables
  #simple linear regression model with one independent variable
  simpleModel <- lm(contract_lowerBinValue ~ lastMarketClosedDate , data = tweetDataToDescribe)
  print(simpleModel)
  summary(simpleModel)
  par(mfrow=c(1, 1))  # divides graph area back to one
  plot(simpleModel$effects)
  plot(simpleModel$residuals)
}

displaySimpleModelStats(trump.tweet.markets.yes, "All: ")
displaySimpleModelStats(trump.tweet.markets.yes.section1,paste0("<",dtNewProcess, ": "))
displaySimpleModelStats(trump.tweet.markets.yes.section2,paste0(">",dtNewProcess, ": "))

par(mfrow=c(1, 1))  # divides graph area back to one
