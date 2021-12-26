USE master
GO

CREATE DATABASE firstDB
GO
USE firstDB
GO

CREATE TABLE Teacher (
    TeacherID int PRIMARY KEY CHECK (TeacherID BETWEEN 1 AND 99),
    Name varchar(MAX) NOT NULL,
    Phone varchar(11),
    DateOfBirth date NOT NULL
)
GO


CREATE DATABASE secondDB;
GO
USE secondDB
GO

CREATE TABLE Teacher (
    TeacherID int PRIMARY KEY CHECK (TeacherID >= 100),
    Name varchar(MAX) NOT NULL,
    Phone varchar(11),
    DateOfBirth date NOT NULL
)
GO

CREATE VIEW TeacherView AS
    SELECT * FROM firstDB.dbo.Teacher
    UNION ALL
    SELECT * FROM secondDB.dbo.Teacher
GO

USE firstDB
GO

CREATE VIEW TeacherView AS
    SELECT * FROM firstDB.dbo.Teacher
    UNION ALL
    SELECT * FROM secondDB.dbo.Teacher
GO

INSERT INTO TeacherView (TeacherID, Name, Phone, DateOfBirth)
VALUES (1, 'Alexey', '89778888888', CONVERT(date, '01/28/2001'))
GO
INSERT INTO TeacherView (TeacherID, Name, Phone, DateOfBirth)
VALUES (100, 'Artem', '89779999999', CONVERT(date, '06/21/2001'))
GO

SELECT * FROM firstDB.dbo.Teacher
GO
SELECT * FROM secondDB.dbo.Teacher
GO

UPDATE secondDB.dbo.Teacher SET TeacherID = 50 WHERE TeacherID = 100
GO
UPDATE firstDB.dbo.Teacher SET TeacherID = 150 WHERE TeacherID = 1
GO

SELECT * FROM firstDB.dbo.Teacher
GO
SELECT * FROM secondDB.dbo.Teacher
GO

DELETE FROM secondDB.dbo.Teacher
GO
SELECT * FROM secondDB.dbo.Teacher
GO


USE master
GO
DROP DATABASE firstDB
GO
DROP DATABASE secondDB
GO
