BEGIN TRANSACTION;
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
  `primary_column` TEXT NOT NULL,
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
CREATE INDEX variables_index ON variables(code_name);
CREATE TABLE `episodes` (
  `episode_id` INTEGER PRIMARY KEY NOT NULL,
  `nhs_number` TEXT NOT NULL,
  `start_date` TEXT NOT NULL,
  `provenance` INTEGER NOT NULL,
  FOREIGN KEY (provenance) REFERENCES provenance (file_id)
);
CREATE INDEX episodes_index ON episodes(episode_id, nhs_number, start_date);
CREATE TABLE `events` (
  `code_name` TEXT NOT NULL,
  `string` TEXT,
  `string2` TEXT,
  `string3` TEXT,
  `datetime` TEXT,
  `date` TEXT,
  `time` TEXT,
  `real` REAL,
  `integer` INTEGER,
  `integer2` INTEGER,
  `episode_id` INTEGER NOT NULL,
  `event_id` INTEGER PRIMARY KEY NOT NULL,
  FOREIGN KEY (episode_id) REFERENCES episodes (episode_id),
  FOREIGN KEY (code_name) REFERENCES variables (code_name)
);
CREATE INDEX events_index ON events(code_name, episode_id, datetime);
COMMIT;
VACUUM;
