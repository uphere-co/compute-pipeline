create table article_tag (
  id serial PRIMARY KEY,
  sha256 bytea NOT NULL,
  tag text NOT NULL
);

create table article_author (
  id serial PRIMARY KEY,
  sha256 bytea NOT NULL,
  author text NOT NULL
);


create table article (
  id serial PRIMARY KEY,
  sha256 bytea NOT NULL,
  url text NOT NULL,
  modified timestamp with time zone, 
  published timestamp with time zone,
  toplevel_section text,
  section_title text,
  section_url text,
  section_taxonomy_id text,
  collection text,
  
  constraint unique_sha256 UNIQUE (sha256)
);




