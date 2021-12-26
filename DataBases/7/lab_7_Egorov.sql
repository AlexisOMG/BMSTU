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

-- 1 --
CREATE TABLE Teacher (
    TeacherID int IDENTITY(1, 1) PRIMARY KEY,
    Name varchar(MAX) NOT NULL,
    Phone varchar(11) NOT NULL
);
GO

ALTER TABLE  Teacher
    ADD About text NOT NULL
    DEFAULT 'The coolest teacher';
GO

ALTER TABLE  Teacher
    ADD DateOfBirth date NOT NULL
    DEFAULT GETDATE();
GO

INSERT INTO Teacher (Name, Phone)
    VALUES ('Alexis', '88888888888'),
           ('Lina', '88888888881'),
           ('WhoIsIt', '88888888882');
GO

CREATE VIEW TeacherView (Name, About, Phone, DateOfBirth) AS
    SELECT t.Name, t.About, t.Phone, t.DateOfBirth
    FROM Teacher AS t
    WHERE DateOfBirth >= CONVERT(date, '01/28/2001')
    WITH CHECK OPTION;
GO

INSERT INTO TeacherView (Name, About, Phone, DateOfBirth)
    VALUES ('NoName', 'hahaha', '88888888883', CONVERT(DATE, '01/28/1999'));
GO

SELECT * FROM TeacherView;
GO

-- 2 --

CREATE TABLE Course (
    CourseID uniqueidentifier PRIMARY KEY DEFAULT NEWID(),
    Description varchar(MAX) NOT NULL,
    TeacherID int DEFAULT 1,
    CONSTRAINT Teacher_FK FOREIGN KEY (TeacherID)
        REFERENCES Teacher(TeacherID)
        ON DELETE CASCADE
);
GO

INSERT INTO Course (Description, TeacherID)
    VALUES ('The coolest course', 1),
           ('Main Course', 2),
           ('Useless course', 3);
GO

CREATE VIEW CourseView (CourseID, Description, TeacherName) WITH SCHEMABINDING AS
    SELECT c.CourseID, c.Description, t.Name TeacherName
    FROM dbo.Course c
    INNER JOIN dbo.Teacher t on t.TeacherID = c.TeacherID
    WITH CHECK OPTION;
GO

SELECT * FROM CourseView;
GO

-- 3 --

CREATE INDEX TeacherIndex on Teacher (DateOfBirth, Phone asc) INCLUDE (Name);
GO

SELECT * FROM Teacher WHERE Phone = '88888888888' and DateOfBirth >= CONVERT(date, '01/28/2001');
GO

-- 4 --
CREATE UNIQUE CLUSTERED INDEX CourseViewIndex on CourseView (CourseID);
GO

SELECT * FROM CourseView;
GO


USE master;
GO
DROP DATABASE EdTech;
GO

