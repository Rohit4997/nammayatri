INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json)
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
        ),
    'radiusDeviation', 20
FROM atlas_app.merchant m;
