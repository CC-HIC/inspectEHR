CREATE TABLE `provenance` (
  `filename` TEXT,
  `file_id` INTEGER PRIMARY KEY NOT NULL,
  `date_created` REAL,
  `version` TEXT,
  `site` TEXT,
  `theme` TEXT,
  `notes` TEXT
);
CREATE TABLE `variables` (
  `code_name` TEXT PRIMARY KEY NOT NULL,
  `long_name` TEXT,
  `primary_column` TEXT,
  `string` TEXT,
  `string2` TEXT,
  `string3` TEXT,
  `datetime` TEXT,
  `date` TEXT,
  `time` TEXT,
  `real` TEXT,
  `integer` TEXT,
  `integer2` TEXT
);
CREATE TABLE `events` (
  `code_name` TEXT,
  `string` TEXT,
  `string2` TEXT,
  `string3` TEXT,
  `datetime` REAL,
  `date` REAL,
  `time` REAL,
  `real` REAL,
  `integer` INTEGER,
  `integer2` INTEGER,
  `episode_id` TEXT,
  `event_id` INTEGER PRIMARY KEY NOT NULL
);
CREATE TABLE `episodes` (
  `episode_id` TEXT PRIMARY KEY NOT NULL,
  `nhs_number` TEXT,
  `start_date` REAL,
  `provenance` INTEGER
);
CREATE TABLE `event_validation` (
  `event_id` INTEGER,
  `rule_id` INTEGER
);
CREATE TABLE `episode_validation` (
  `episode_id` TEXT,
  `rule_id` INTEGER
);
