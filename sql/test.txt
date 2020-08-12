CREATE TABLE market (
  marketId INTEGER PRIMARY KEY,
  name TEXT,
  sortName TEXT,
  url TEXT,
  imageUrl TEXT,
  dateEnd DATETIME,
  FOREIGN KEY (imageUrl) REFERENCES image (imageUrl)
);

CREATE TABLE contract (
  imageUrl TEXT,
  contractId INTEGER PRIMARY KEY,
  marketId INTEGER,
  contractName TEXT,
  contractShortName TEXT,
  dateEnd DATETIME,
  FOREIGN KEY (imageUrl) REFERENCES image (imageUrl),
  FOREIGN KEY (marketId) REFERENCES market (marketId)
);

CREATE TABLE querySource (
  querySourceId INTEGER PRIMARY KEY,
  name TEXT,
  shortName TEXT
);

CREATE TABLE marketNotExist (
  marketId INTEGER,
  querySourceId INTEGER,
  PRIMARY KEY (marketId, querySourceId),
  FOREIGN KEY (querySourceId) REFERENCES querySource (querySourceId)
);

CREATE TABLE image (
  imageUrl TEXT PRIMARY KEY,
  image BLOB,
  type TEXT
);

CREATE TABLE marketObservation (
  dateTimeStamp DATETIME,
  contractId INTEGER,
  contractStatus TEXT,
  marketId INTEGER,
  marketStatus TEXT,
  lastTradePrice DOUBLE,
  bestBuyYesCost DOUBLE,
  bestBuyNoCost DOUBLE,
  bestSellYesCost DOUBLE,
  bestSellNoCost DOUBLE,
  lastClosePrice DOUBLE,
  displayOrder INTEGER,
  querySourceId INTEGER,
  PRIMARY KEY (dateTimeStamp, contractId, querySourceId),
FOREIGN KEY (contractId) REFERENCES contract (contractId)
FOREIGN KEY (marketId) REFERENCES market (marketId)
FOREIGN KEY (querySourceId) REFERENCES querySource (querySourceId)
);

CREATE TABLE marketChartData (
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
