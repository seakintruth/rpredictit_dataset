pacman::p_load(rpredictit, DBI, RSQLite, stream)
# If we do decied to normalize the tables in the RSQLite database we will need:
# should build out a normalized schema, could use: https://dbdiagram.io/d
# pacman::p_load(uuid)

# to use the seakintruth fork
# pacman::p_load(devtoools)
# devtools::install_github('seakintruth/rpredictit')

# all markets are updated every 60 seconds...
# so for a history we would need to capture a new timestamp's worth of data every minute...
# If I was to do this then I need to normalize the data into a table RSQLlight?
project.dir <- file.path("/media","jeremy","250GbUsb","data","r","predictit")


# may need for streaming annalysis?
# pacman::p_load(stream)


db = DBI::dbConnect(
  RSQLite::SQLite(), 
  dbname=file.path(project.dir,"price_data.sqlite")
)

marketObservationColumns <- ("
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
")
existing.tables <- DBI::dbListTables(db)
if(length(existing.tables)==0 ){
  # Create the base tables
  # SQL documentation: https://www.sqlite.org/lang.html
  dbSendQuery(conn=db,
    paste0(
      "CREATE TABLE market_observations
      (", 
        marketObservationColumns,
        "PRIMARY KEY (timeStamp, id, contract_id)
      )"
    )
  )
  all.market.data.now <- rpredictit::all_markets()
  #get all closed markets:
  closed.markets <- setdiff(seq(1220,max(all.market.data.now$id)),all.market.data.now$id)
  for (market.id in seq_along(closed.markets)){
    Sys.sleep(60)
    message("gettng market: ",closed.markets[market.id])
    check.url <- httr::GET(
      paste0("https://www.predictit.org/api/marketdata/markets/",
        closed.markets[market.id]
      )
    )
    # handle a site maintenance message:
    trading.suspended <- check.url$headers$`pi-tradingsuspendedmessage`
    repeat{
      if (!is.null(trading.suspended) ){
        message("API Warning:",trading.suspended)
        Sys.Sleep(60)
        check.url <- httr::GET(
          paste0("https://www.predictit.org/api/marketdata/markets/",
            closed.markets[market.id]
          )
        )
        # handle a site maintenance message:
        trading.suspended <- check.url$headers$`pi-tradingsuspendedmessage`
      } else {
        break    
      }  
    }
    if(
      !is.null(
        jsonlite::fromJSON(
          rawToChar(
            check.url$content
          )
        )
      )
    ){
      this.market <- rpredictit::single_market(closed.markets[market.id])
      insert.fields <- colnames(this.market)
      message("attempting to query id:",closed.markets[market.id])
      error.checking <- try(
        DBI::dbAppendTable(conn=db,"market_observations",this.market)
      )
    } else {
      message("Warning: market ID ",closed.markets[market.id]," returned 'null'")
    }
  }
} else {
  
  
}

repeat{
  # Repeat forever, getting the new content as long as timestamp is updateded!
  # Delay 60 seconds between each call
  Sys.sleep(60)
  # Repeat forever, getting the new content as long as timestamp is updateded!
  all.market.data.now <- rpredictit::all_markets()
  unique(all.market.data.now$timeStamp)
  uuid::UUIDgenerate(TRUE)
  #export one table per file, or to a RSQLite database ?
  # see: https://www.sqlite.org/whentouse.html
  # application file format
  rpredictit::single_market(4100)
  
  
  # Write the datastream to file, then use another script to read the csv file with
  # stream, see: https://cran.r-project.org/web/packages/stream/vignettes/stream.pdf
  length(unique(all.market.data.now$id))

}
head(all.market.data.now)
str(all.market.data.now)
unique(all.market.data.now$id)
unique(all.market.data.now$name)
unique(all.market.data.now$shortName)
unique(all.market.data.now$image)

unique(all.market.data.now$status)
unique(all.market.data.now$contract_name)
unique(all.market.data.now$lastTradePrice)
unique(all.market.data.now$bestBuyNoCost)
unique(all.market.data.now$bestBuyYesCost)
unique(all.market.data.now$bestSellNoCost)
unique(all.market.data.now$bestSellYesCost)


#blob:https://www.predictit.org/e0f4cc7d-faa8-424f-9f75-06b5aa336430

#all.tweet.market.data <- rpredictit::tweet_markets()
#tr(all.tweet.market.data)
rpredictit::runExample("demo")

?rpredictit::runExample()
?rpredictit::markets_table()
?rpredictit::historical_plot()
DBI::dbAppendTable()