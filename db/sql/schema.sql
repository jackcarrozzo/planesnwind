drop table if exists flight_tracks;
drop table if exists wind_set_data;
drop table if exists wind_sets;
drop table if exists flights;
drop table if exists airports;



create table airports(
       id serial primary key,
       ident varchar(8) not null,
       name text default '',
       state varchar(3) default '',
       posn point not null,
       elev_ft integer default -1,
       contig_us integer default 0,
       meta hstore default ''
);
create unique index airports_ident_ind on airports(ident);
create index airports_contig_ind on airports(contig_us);
create index airports_ident_contig_ind on airports(ident,contig_us);

create table flights(
       id serial primary key,
       ident varchar(10) not null,
       origin varchar(4) not null,
       dest varchar(4) not null,
       dept_ts timestamp not null,
       arrive_ts timestamp not null,
       acft_type varchar(8) default '',
       runstate integer default 0,
       meta hstore default ''
);
create index flights_deptarrts_ind on flights(dept_ts,arrive_ts);
create index flights_runstate_ind on flights(runstate);

create table flight_tracks(
       id serial primary key,
       flight_id integer references flights(id) on delete cascade,
       tracktype integer default 0,
       ts timestamp not null,
       posn point not null,
       alt integer default -1,
       gspd integer default -1,
       meta hstore default ''
);
create index flight_tracks_flightid_ind on flight_tracks(flight_id);
create index flight_tracks_flightidtrtype_ind on flight_tracks(flight_id,tracktype);
create index flight_tracks_trtype_ind on flight_tracks(tracktype);

create table wind_sets(
       id serial primary key,
       ts_fetched timestamp not null,
       valid_day integer not null,
       valid_time varchar(4) not null,
       valid_tz varchar(2) not null,
       foruse_start varchar(4) not null,
       foruse_end varchar(4) not null,
       foruse_tz varchar(2) not null,
       start_ts timestamp not null,,
       end_ts timestamp not null,
       err integer default 0,
       meta hstore default ''
);
create index wind_sets_startendts_ind on wind_sets(start_ts,end_ts);
create index wind_sets_startts_ind on wind_sets(start_ts);
create index wind_sets_endts_ind on wind_sets(end_ts);


create table wind_set_data(
       id serial primary key,
       wind_set_id integer references wind_sets(id) on delete cascade,
       station_ident varchar(4) not null,
       alt integer not null,
       wind_spd integer not null,
       wind_dir integer not null,
       temp_c integer not null
);
create index wind_set_data_setid_ind on wind_set_data(wind_set_id);
create index wind_set_data_setidalt_ind on wind_set_data(wind_set_id,alt);
create index wind_set_data_alt_ind on wind_set_data(alt);

create table wind_model_data(
       id serial primary key,
       wind_set_id integer references wind_sets(id) on delete cascade,
       mkey varchar(32) not null,
       mval real not null
);
create index wind_model_data_windsetid_ind on wind_model_data(wind_set_id);

create table worker_conf(
       id serial primary key,
       confkey varchar(32) not null,
       confval text default '',
       meta hstore default ''
);

create table worker_state(
       id serial primary key,
       wname varchar(64) not null,
       wstate varchar(64) not null,
       last_update_ts timestamp not null,
       last_touch_ts timestamp not null,
       info text default '',
       meta hstore default ''
);

create table worker_logs(
       id serial primary key,
       wname varchar(64) not null,
       ltag varchar(64) not null,
       ts timestamp not null,
       val real default -1.0,
       txt text default ''
);

create table known_flights(
       id serial primary key,
       ident varchar(10) not null,
       origin varchar(4) not null,
       dest varchar(4) not null,
       avg_hr real not null,
       last_fetched_ts timestamp not null,
       fetchstate integer default 0,
       meta hstore default ''
);
alter table known_flights add try_again_after_ts timestamp default null;
create index known_flights_avghrlastfetchtsfstate_ind on known_flights(avg_hr,last_fetched_ts,fetchstate);
create unique index known_flights_identorigdest_ind on known_flights(ident,origin,dest);

create table wx_sites(
       id serial primary key,
       ident varchar(4) not null,
       name text default '',
       posn point not null,
       alt_ft integer not null
);
