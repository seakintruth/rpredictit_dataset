CREATE UNIQUE INDEX IF NOT EXISTS `contract_id_index` ON `marketObservation` (
	`contractId`	ASC
); 
CREATE UNIQUE INDEX IF NOT EXISTS `image_id_index` ON `image` (
	`imageUrl`	ASC
);
 CREATE UNIQUE INDEX IF NOT EXISTS `null_id_index` ON `marketNotExist` (
	`marketId`	ASC,
	`querySourceId`	ASC
); 
CREATE UNIQUE INDEX IF NOT EXISTS `id_index` ON `querySource` (
	`querySourceId` ASC
);
INSERT INTO querySource (querySourceId,name,shortName) VALUES 
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
