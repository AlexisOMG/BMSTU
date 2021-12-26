USE master;
GO
CREATE DATABASE EdTech
    ON PRIMARY (
        NAME = EdTech_dat,
        FILENAME = '/var/opt/mssql/data/EdTech.mdf',
        SIZE = 1MB,
        MAXSIZE = UNLIMITED,
        FILEGROWTH = 1MB
    )
    LOG ON (
        NAME = EdTech_log,
        FILENAME = '/var/opt/mssql/data/EdTech.ldf',
        SIZE = 1MB,
        MAXSIZE = UNLIMITED,
        FILEGROWTH = 1MB
    );
GO
USE EdTech;
GO

CREATE TABLE Teacher (
    TeacherID int PRIMARY KEY,
    Name varchar(MAX) NOT NULL,
    Phone varchar(11),
    DateOfBirth date NOT NULL DEFAULT GETDATE(),
    Email VARCHAR(256) NOT NULL UNIQUE

);
GO

INSERT INTO Teacher (TeacherID, Name, Phone, DateOfBirth, Email)
    VALUES (1, 'Alexis', '84958888888', CONVERT(date, '01/28/2001'), 'test1@gmail.com'),
           (2, 'Lina', '84958888881', CONVERT(date, '01/28/1999'), 'test2@gmail.com'),
           (3, 'WhoIsIt', '84958888882', GETDATE(), 'test3@gmail.com');
GO

select * from sys.dm_tran_locks;

-- USE master
-- GO
-- DROP DATABASE EdTech
-- GO
