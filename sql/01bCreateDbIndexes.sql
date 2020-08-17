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