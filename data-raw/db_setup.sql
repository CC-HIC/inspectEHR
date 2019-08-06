CREATE TABLE `provenance` (
  `filename` TEXT,
  `file_id` INTEGER PRIMARY KEY NOT NULL,
  `date_created` TEXT,
  `version` TEXT,
  `date_parsed` TEXT,
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
  `datetime` TEXT,
  `date` TEXT,
  `time` TEXT,
  `real` REAL,
  `integer` INTEGER,
  `integer2` INTEGER,
  `episode_id` INTEGER,
  `event_id` INTEGER PRIMARY KEY NOT NULL
);
CREATE TABLE `episodes` (
  `episode_id` INTEGER PRIMARY KEY NOT NULL,
  `nhs_number` TEXT,
  `start_date` TEXT,
  `provenance` INTEGER
);
