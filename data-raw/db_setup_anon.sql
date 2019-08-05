BEGIN TRANSACTION;

ALTER TABLE variables RENAME TO _variables_old;

CREATE TABLE `variables` (
  `code_name` TEXT PRIMARY KEY NOT NULL,
  `long_name` TEXT NOT NULL,
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

INSERT INTO variables
  (code_name, long_name, primary_column, string, string2, string3,
  datetime, date, time, real, integer, integer2)
  SELECT code_name, long_name, primary_column, string, string2, string3,
  datetime, date, time, real, integer, integer2
  FROM _variables_old;

CREATE INDEX variables_index ON variables(code_name, primary_column);
DROP TABLE _variables_old;
COMMIT;

BEGIN TRANSACTION;

ALTER TABLE events RENAME TO _events_old;

CREATE TABLE `events` (
  `event_id` INTEGER PRIMARY KEY,
  `episode_id` TEXT NOT NULL,
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

  FOREIGN KEY (code_name)
  REFERENCES variables (code_name)
);

INSERT INTO events (episode_id, event_id, code_name, string, string2, string3,
  datetime, date, time, real, integer, integer2)
  SELECT episode_id, event_id, code_name, string, string2, string3,
  datetime, date, time, real, integer, integer2
  FROM _events_old;

CREATE INDEX events_index ON events(code_name, episode_id, datetime);
DROP TABLE _events_old;

COMMIT;

BEGIN TRANSACTION;

ALTER TABLE spells RENAME TO _spells_old;

CREATE TABLE `spells` (
  `spell_id` INTEGER,
  `spell_position` INTEGER NOT NULL,
  `episode_id` TEXT NOT NULL,

  PRIMARY KEY (spell_id, spell_position),
  FOREIGN KEY (episode_id)
  REFERENCES events (episode_id)

);

INSERT INTO spells
  (spell_id, spell_position, episode_id)
  SELECT spell_id, spell_position, episode_id
  FROM _spells_old;

CREATE INDEX spells_index ON spells(episode_id, spell_id);

DROP TABLE _spells_old;

COMMIT;
VACUUM;
