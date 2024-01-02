CREATE TABLE atlas_safety_dashboard.role (
id character(36) NOT NULL,
name character varying(255) NOT NULL,
dashboard_access_type character varying(255) NOT NULL,
description character varying(1024) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16419_primary PRIMARY KEY (id)
,CONSTRAINT  unique_name UNIQUE (name)
);

ALTER TABLE atlas_safety_dashboard.role OWNER TO atlas_safety_dashboard_user;

CREATE TABLE atlas_safety_dashboard.access_matrix (
id character(36) NOT NULL,
role_id character(36) REFERENCES atlas_safety_dashboard.role (id) NOT NULL,
api_entity character varying(255) NOT NULL,
user_access_type character varying(255) NOT NULL,
user_action_type character varying(255) NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16402_primary PRIMARY KEY (id)
,CONSTRAINT  unique_role_id_api_entity_user_action_type UNIQUE (role_id, api_entity, user_action_type)
);

ALTER TABLE atlas_safety_dashboard.access_matrix OWNER TO atlas_safety_dashboard_user;

INSERT INTO atlas_safety_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at) VALUES
    ('37947162-3b5d-4ed6-bcac-08841be1534d', 'POLICE ADMIN', 'DASHBOARD_ADMIN', 'Police admin can create and assign other roles', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('e5a69a26-d165-455a-a711-33a41e0d47c6', 'MERCHANT ADMIN', 'MERCHANT_ADMIN', 'Merchant admin can create and assign roles at partner level', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('d06b7cd9-408d-4e2a-b0b2-2b871a5138dd', 'MERCHANT CHECKER', 'MERCHANT_CHECKER', 'Merchant admin can create and assign roles at partner level', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('84a47cba-3e8b-4205-aba8-379eaaa28c78', 'MERCHANT MAKER', 'MERCHANT_MAKER', 'Merchant admin can create and assign roles at partner level', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00'),
    ('bedb63cd-3661-4d17-861c-518f00dc32e4', 'DASHBOARD USER', 'DASHBOARD', 'Merchant admin can create and assign roles at partner level', '2022-09-12 15:15:42.104639+00', '2022-09-12 15:15:42.104639+00');

ALTER TABLE atlas_safety_dashboard.person
    ADD COLUMN role_id character(36) DEFAULT 'e5a69a26-d165-455a-a711-33a41e0d47c6' NOT NULL;

ALTER TABLE
   atlas_safety_dashboard.person
ADD
   CONSTRAINT person_role_id_fkey FOREIGN KEY (role_id) REFERENCES atlas_safety_dashboard.role(id);

UPDATE atlas_safety_dashboard.person
    SET role_id = '37947162-3b5d-4ed6-bcac-08841be1534d' where first_name = 'juspay_admin';


CREATE TABLE atlas_safety_dashboard.merchant (
id character(36) NOT NULL,
short_id character varying(255) NOT NULL,
default_operating_city Text  NOT NULL,
is2fa_mandatory Boolean NOT NULL DEFAULT false,
supported_operating_cities Text[] NOT NULL,
server_name character varying(255),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
CONSTRAINT  merchant_pkey PRIMARY KEY (id),
CONSTRAINT  unique_short_id UNIQUE (short_id)
);

ALTER TABLE atlas_safety_dashboard.merchant OWNER TO atlas_safety_dashboard_user;

INSERT INTO atlas_safety_dashboard.merchant (id, short_id, server_name,default_operating_city,supported_operating_cities, created_at) VALUES
    ('d92db186-39d3-48a4-ad1f-78a0c3f840fd', 'YATRI_PARTNER', 'DRIVER_OFFER_BPP','Bangalore',ARRAY['Bangalore'], now ()),
    ('94bbea0d-3c52-479b-81f5-eca4969ae797', 'NAMMA_YATRI_PARTNER', 'DRIVER_OFFER_BPP','Kochi',ARRAY['Kochi'], now ());



CREATE TABLE atlas_safety_dashboard.merchant_access (
id character(36) NOT NULL,
person_id character(36) REFERENCES atlas_safety_dashboard.person (id) NOT NULL,
secret_key character varying(255) ,
is2fa_enabled Boolean NOT NULL DEFAULT false,
is2fa_mandatory Boolean NOT NULL DEFAULT false,
merchant_short_id Text NOT NULL,
merchant_id character(36) NOT NULL REFERENCES atlas_safety_dashboard.merchant (id) NOT NULL,
operating_city Text,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16475_primary PRIMARY KEY (id)
,CONSTRAINT  unique_person_id_merchant_id UNIQUE (person_id, merchant_id)
);
ALTER TABLE atlas_safety_dashboard.merchant_access OWNER TO atlas_safety_dashboard_user;

ALTER TABLE atlas_safety_dashboard.registration_token
    ADD COLUMN server_name character varying(255) DEFAULT 'DRIVER_OFFER_BPP' NOT NULL;

INSERT INTO atlas_safety_dashboard.merchant_access (id, person_id, created_at, merchant_id, is2fa_enabled, merchant_short_id, operating_city) VALUES
 ('1c5f95c3-69e5-b252-5f1b-a71f08cfa763', 'cd69ae25-1641-4e6d-b9f4-5bae63e2a537', '2022-09-12 15:15:42.104639+00', '94bbea0d-3c52-479b-81f5-eca4969ae797', false, 'NAMMA_YATRI_PARTNER', 'Bangalore'),
 ('2c795a5b-49f0-98d6-d9f8-b06566a2c0a0', '59f5bd8c-8268-4e8e-bcab-c21da7e496d4', '2022-09-12 15:15:42.104639+00', '94bbea0d-3c52-479b-81f5-eca4969ae797', false, 'NAMMA_YATRI_PARTNER', 'Bangalore'),
 ('483a7d06-29e0-5a1c-4b35-49794ee5b8da', '25e77f37-75e5-4665-8ed0-4be2af35940a', '2022-09-12 15:15:42.104639+00', '94bbea0d-3c52-479b-81f5-eca4969ae797', false, 'NAMMA_YATRI_PARTNER', 'Bangalore'),
 ('a1418ee5-acc1-f83f-0a63-1890349494b0', '3680f4b5-dce4-4d03-aa8c-5405690e87bd', '2022-09-12 15:15:42.104639+00', '94bbea0d-3c52-479b-81f5-eca4969ae797', false, 'NAMMA_YATRI_PARTNER', 'Bangalore'),
 ('a1418ee5-acc1-f83f-0a63-1890349494b3', '3680f4b5-dce4-4d03-aa8c-5405690e87bd', '2022-09-12 15:15:42.104639+00', 'd92db186-39d3-48a4-ad1f-78a0c3f840fd', false, 'YATRI_PARTNER', 'Kochi'),
 ('c3552fdb-74d5-b2b1-b760-1101cbdd4b5b', 'a77b0507-ae01-42dd-a075-264f59d89049', '2022-09-12 15:15:42.104639+00', '94bbea0d-3c52-479b-81f5-eca4969ae797', false, 'NAMMA_YATRI_PARTNER', 'Bangalore');



ALTER TABLE atlas_safety_dashboard.registration_token
    ADD COLUMN merchant_id character (36) NOT NULL REFERENCES atlas_safety_dashboard.merchant (id) DEFAULT 'd92db186-39d3-48a4-ad1f-78a0c3f840fd';

UPDATE atlas_safety_dashboard.registration_token
    SET merchant_id = '94bbea0d-3c52-479b-81f5-eca4969ae797' WHERE server_name = 'DRIVER_OFFER_BPP';


CREATE TABLE atlas_safety_dashboard.transaction (
id character(36) NOT NULL,
requestor_id character(36) REFERENCES atlas_safety_dashboard.person (id),
merchant_id character(36) REFERENCES atlas_safety_dashboard.merchant (id),
common_driver_id character(36),
common_ride_id character(36),
endpoint character varying(255) NOT NULL,
server_name character varying(255) NOT NULL,
request text,
response text,
response_error text,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
CONSTRAINT  idx_transaction_primary PRIMARY KEY (id)
);

ALTER TABLE atlas_safety_dashboard.transaction OWNER TO atlas_safety_dashboard_user;


--------------------------------------- REGISTRATION TOKEN TABLE MIGRATIONS -----------------------------------------
-- Add new column 'operating_city' in 'registration_token' table

ALTER TABLE atlas_safety_dashboard.registration_token
ADD COLUMN operating_city Text DEFAULT 'Bangalore' NOT NULL;

--------------------------------------------------------- END ---------------------------------------------------------


ALTER TABLE atlas_safety_dashboard.merchant ADD COLUMN server_names Text[];

UPDATE atlas_safety_dashboard.merchant as T1
SET server_names = ARRAY[server_name];

ALTER TABLE atlas_safety_dashboard.merchant ALTER COLUMN server_names SET NOT NULL;

UPDATE atlas_safety_dashboard.merchant set server_names = '{DRIVER_OFFER_BPP, DRIVER_OFFER_BPP_MANAGEMENT}' where server_name = 'DRIVER_OFFER_BPP';


ALTER TABLE atlas_safety_dashboard.merchant ADD COLUMN company_name Text;
ALTER TABLE atlas_safety_dashboard.merchant ADD COLUMN domain Text;
ALTER TABLE atlas_safety_dashboard.merchant ADD COLUMN website Text;
ALTER TABLE atlas_safety_dashboard.merchant ALTER COLUMN server_name DROP NOT NULL;
ALTER TABLE atlas_safety_dashboard.merchant ADD COLUMN auth_token Text;
ALTER TABLE atlas_safety_dashboard.person ADD COLUMN dashboard_access_type Text;
ALTER TABLE atlas_safety_dashboard.registration_token ADD COLUMN enabled BOOLEAN DEFAULT TRUE NOT NULL;