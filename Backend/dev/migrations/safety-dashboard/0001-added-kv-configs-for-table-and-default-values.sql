CREATE TABLE atlas_safety_dashboard.system_configs (
    id VARCHAR(255) PRIMARY KEY,
    config_value TEXT
);


INSERT INTO atlas_safety_dashboard.system_configs (id, config_value)
VALUES ('kv_configs', '{"enableKVForWriteAlso":[{"nameOfTable":"Table1","percentEnable":100}],"enableKVForRead":["Table2"]}');


INSERT INTO atlas_safety_dashboard.portal_configs
  (config_name, created_at, id, updated_at, value)
VALUES
  ('BULK_UPLOAD_COUNT', now(), 'a9c80141-6ee8-48a6-a589-e595e5b8a54f',now() , '100');


INSERT INTO atlas_safety_dashboard.portal_configs
  (config_name, created_at, id, updated_at, value)
VALUES
  ('BULK_SEARCH_COUNT', now(), '120f6a56-b88c-4065-8402-d8d5d58ffa07',now() , '100');