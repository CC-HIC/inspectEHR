BEGIN TRANSACTION;
CREATE INDEX  variables_index  ON variables USING hash
(
    code_name
);
CREATE INDEX  episode_id_index  ON episodes
(
    episode_id
);
CREATE INDEX  episode_start_index  ON episodes
(
    episode_id,
    start_date
);
CREATE INDEX  events_index  ON events
(
    code_name,
    episode_id
);
CREATE INDEX  events_time_index  ON events
(
    datetime
);
CREATE INDEX  events_code_index  ON events
(
    code_name
);
CREATE INDEX  events_episodeid_index  ON events
(
    episode_id
);
CREATE INDEX  events_string_index  ON events
(
    string
);
COMMIT;
VACUUM;
