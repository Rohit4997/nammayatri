ALTER TABLE atlas_app.merchant_service_usage_config
ADD COLUMN calculate_multi_zonal character varying(100) NOT NULL DEFAULT 'OSRM_SHARDED';