ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config
ADD COLUMN calculate_multi_zonal character varying(100) NOT NULL DEFAULT 'OSRM_SHARDED';


INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, service_name, config_json)
SELECT m.id, 'Maps_OSRM_SHARDED',
    'cityToRegionUrlMap',
        json_build_object(
            'bangalore', 'localhost:5001',
            'delhi', 'localhost:5001',
            'mysore', 'localhost:5001',
            'hyderabad', 'localhost:5001',
            'tumkuru', 'localhost:5001',
            'kochi', 'localhost:5001',
            'chennai', 'localhost:5001',
            'kolkata', 'localhost:5001',
            'karnataka', 'localhost:5001'
            'default', 'localhost:5000'
        ),
    'radiusDeviation', 20
FROM atlas_driver_offer_bpp.merchant m;