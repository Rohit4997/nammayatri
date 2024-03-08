CREATE TABLE atlas_safety_dashboard.suspect_flag_request ();

ALTER TABLE atlas_safety_dashboard.suspect_flag_request ADD COLUMN admin_approval text NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect_flag_request ADD COLUMN approved_by text ;
ALTER TABLE atlas_safety_dashboard.suspect_flag_request ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.suspect_flag_request ADD COLUMN dl text ;
ALTER TABLE atlas_safety_dashboard.suspect_flag_request ADD COLUMN first_name text NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect_flag_request ADD COLUMN flagged_by text NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect_flag_request ADD COLUMN flagged_category text NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect_flag_request ADD COLUMN flagged_reason text NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect_flag_request ADD COLUMN flagged_status text NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect_flag_request ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect_flag_request ADD COLUMN last_name text NOT NULL;
ALTER TABLE atlas_safety_dashboard.suspect_flag_request ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_safety_dashboard.suspect_flag_request ADD COLUMN voter_id text ;
ALTER TABLE atlas_safety_dashboard.suspect_flag_request ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_safety_dashboard.suspect_flag_request ADD PRIMARY KEY ( id);