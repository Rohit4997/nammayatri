CREATE TABLE atlas_driver_offer_bpp.lms_video_translation ();

ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD COLUMN completed_threshold_in_percentage integer ;
ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD COLUMN completed_watch_count integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD COLUMN duration integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD COLUMN language text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD COLUMN start_threshold_in_percentage integer ;
ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD COLUMN threshold_enabled boolean NOT NULL default False;
ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD COLUMN thumbnail_image text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD COLUMN title text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD COLUMN url text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD COLUMN video_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD COLUMN view_count integer NOT NULL default 0;
ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD COLUMN yt_video_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.lms_video_translation ADD PRIMARY KEY ( language, video_id);