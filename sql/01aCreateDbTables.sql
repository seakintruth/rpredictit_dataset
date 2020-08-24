CREATE TABLE IF NOT EXISTS market (
  marketId INTEGER PRIMARY KEY,
  name TEXT,
  shortName TEXT,
  url TEXT,
  imageUrl TEXT,
  dateEnd DATETIME,
  FOREIGN KEY (imageUrl) REFERENCES image (imageUrl)
);
CREATE TABLE IF NOT EXISTS contract (
  imageUrl TEXT,
  contractId INTEGER PRIMARY KEY,
  marketId INTEGER,
  contractName TEXT,
  contractShortName TEXT,
  dateEnd DATETIME,
  FOREIGN KEY (imageUrl) REFERENCES image (imageUrl),
  FOREIGN KEY (marketId) REFERENCES market (marketId)
);
CREATE TABLE IF NOT EXISTS querySource (
  querySourceId INTEGER PRIMARY KEY,
  name TEXT,
  shortName TEXT,
  status TEXT
);
CREATE TABLE IF NOT EXISTS status (
  statusId INTEGER PRIMARY KEY,
  name TEXT
);
CREATE TABLE IF NOT EXISTS marketNotExist (
  marketId INTEGER,
  querySourceId INTEGER,
  PRIMARY KEY (marketId, querySourceId),
  FOREIGN KEY (querySourceId) REFERENCES querySource (querySourceId)
);
CREATE TABLE IF NOT EXISTS image (
  imageUrl TEXT PRIMARY KEY,
  imageType TEXT
  fileName TEXT,
  directory TEXT,
  fileExtension TEXT,
  md5checksum TEXT,
  imageAttribute
);
CREATE TABLE IF NOT EXISTS marketObservation (
  dateTimeStamp DATETIME,
  dateEnd DATETIME,
  contractId INTEGER,
  contractStatusId TEXT,
  marketId INTEGER,
  marketStatusId INTEGER,
  lastTradePrice DOUBLE,
  bestBuyYesCost DOUBLE,
  bestBuyNoCost DOUBLE,
  bestSellYesCost DOUBLE,
  bestSellNoCost DOUBLE,
  lastClosePrice DOUBLE,
  displayOrder INTEGER,
  querySourceId INTEGER,
  PRIMARY KEY (dateTimeStamp, contractId, querySourceId, marketStatusId,contractStatusId),
FOREIGN KEY (contractId) REFERENCES contract (contractId)
FOREIGN KEY (marketStatusId) REFERENCES status (statusId)
FOREIGN KEY (contractStatusId) REFERENCES status (statusId)
FOREIGN KEY (marketId) REFERENCES market (marketId)
FOREIGN KEY (querySourceId) REFERENCES querySource (querySourceId)
);
CREATE TABLE IF NOT EXISTS marketChartData (
  marketId INTEGER,
  contractId INTEGER,
  dateTimeStamp DATETIME,
  querySourceId INTEGER,
  openSharePrice DOUBLE,
  highSharePrice DOUBLE,
  lowSharePrice DOUBLE,
  closeSharePrice DOUBLE,
  tradeVolume DOUBLE,
  PRIMARY KEY (contractId, dateTimeStamp, querySourceId),
  FOREIGN KEY (marketId) REFERENCES market (marketId),
  FOREIGN KEY (contractId) REFERENCES contract (contractId),
FOREIGN KEY (querySourceId) REFERENCES querySource (querySourceId)
);
