-- 1
CREATE DATABASE AnimeCatalog
    ON PRIMARY (
        NAME = AnimeCatalog_dat,
        FILENAME = '/var/opt/mssql/data/animecatalog.mdf',
        SIZE = 10MB,
        MAXSIZE = UNLIMITED,
        FILEGROWTH = 10MB
    )
    LOG ON (
        NAME = AnimeCatalog_log,
        FILENAME = '/var/opt/mssql/data/animecatalog.ldf',
        SIZE = 10MB,
        MAXSIZE = UNLIMITED,
        FILEGROWTH = 10MB
    );
GO
USE AnimeCatalog;
GO

-- 2
CREATE TABLE anime (
    anime_id int PRIMARY KEY,
    name text NOT NULL,
    release_date date NOT NULL
);
GO

-- 3
ALTER DATABASE AnimeCatalog
    ADD FILEGROUP AnimeCatalogGroup;
GO
ALTER DATABASE AnimeCatalog
    ADD FILE (
        NAME = AnimeCatalog_file,
        FILENAME = '/var/opt/mssql/data/animecatalog.ndf',
        SIZE = 10MB,
        MAXSIZE = UNLIMITED,
        FILEGROWTH = 10MB
    ) TO FILEGROUP AnimeCatalogGroup;
GO

-- 4
ALTER DATABASE AnimeCatalog
    MODIFY FILEGROUP AnimeCatalogGroup DEFAULT;
GO

-- 5
CREATE TABLE author (
    id int PRIMARY KEY,
    name text NOT NULL
);
GO

INSERT INTO author (id, name) values (1, 'ehdwjkhdfkrwf');
GO

ALTER DATABASE AnimeCatalog
    MODIFY FILEGROUP [primary] DEFAULT;
GO

CREATE TABLE author_tmp (
    id int PRIMARY KEY,
    name text NOT NULL
);
GO

INSERT INTO author_tmp (id, name)
SELECT id, name from author;

SELECT * FROM author_tmp;

-- 6
DROP TABLE author;
GO
ALTER DATABASE AnimeCatalog
    REMOVE FILE AnimeCatalog_file;
GO
ALTER DATABASE AnimeCatalog
    REMOVE FILEGROUP AnimeCatalogGroup;
GO

-- 7
CREATE SCHEMA AnimeCatalogSchema;
GO
ALTER SCHEMA AnimeCatalogSchema
    TRANSFER anime;
GO
DROP TABLE AnimeCatalogSchema.anime;
GO
DROP SCHEMA AnimeCatalogSchema;
GO

USE master;
GO
DROP DATABASE AnimeCatalog;
GO

