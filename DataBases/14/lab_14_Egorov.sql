USE master
GO

CREATE DATABASE firstDB
GO
USE firstDB
GO

CREATE TABLE Teacher (
    TeacherID int PRIMARY KEY,
    Name varchar(MAX) NOT NULL
)
GO


CREATE DATABASE secondDB;
GO
USE secondDB
GO

CREATE TABLE Teacher (
    TeacherID int PRIMARY KEY,
    Phone varchar(11),
    DateOfBirth date NOT NULL
)
GO

USE firstDB
GO

CREATE VIEW TeacherView AS
    SELECT t1.TeacherID, t1.Name, t2.Phone, t2.DateOfBirth FROM firstDB.dbo.Teacher t1, secondDB.dbo.Teacher t2
    WHERE t1.TeacherID = t2.TeacherID
GO

CREATE TRIGGER TeacherViewInsert ON TeacherView
INSTEAD OF INSERT AS
    INSERT INTO firstDB.dbo.Teacher (TeacherID, Name) SELECT ins.TeacherID, ins.Name FROM inserted ins
    INSERT INTO secondDB.dbo.Teacher (TeacherID, Phone, DateOfBirth) SELECT ins.TeacherID, ins.Phone, ins.DateOfBirth FROM inserted ins
GO

CREATE TRIGGER TeacherViewUpdate ON TeacherView
INSTEAD OF UPDATE AS
    IF UPDATE(TeacherID)
        BEGIN
            RAISERROR('Unable to update TeacherID', 10, 1)
        END
    ELSE
        BEGIN
        UPDATE firstDB.dbo.Teacher SET Name = ins.Name
            FROM inserted ins WHERE ins.TeacherID = firstDB.dbo.Teacher.TeacherID
        UPDATE secondDB.dbo.Teacher SET Phone = ins.Phone, DateOfBirth = ins.DateOfBirth
            FROM inserted ins WHERE ins.TeacherID = secondDB.dbo.Teacher.TeacherID
        END
GO

CREATE TRIGGER TeacherViewDelete ON TeacherView
INSTEAD OF DELETE AS
    DELETE t1 FROM firstDB.dbo.Teacher t1 INNER JOIN deleted del ON del.TeacherID = t1.TeacherID
    DELETE t2 FROM secondDB.dbo.Teacher t2 INNER JOIN deleted del ON del.TeacherID = t2.TeacherID
GO

INSERT INTO TeacherView (TeacherID, Name, Phone, DateOfBirth) VALUES
    (1, 'Alex', '88888888888', CONVERT(date, '06/21/2001'))
GO
SELECT * FROM firstDB.dbo.Teacher
GO
SELECT * FROM secondDB.dbo.Teacher
GO

UPDATE TeacherView SET Name = 'Artem', DateOfBirth = CONVERT(date, '01/28/2001') WHERE TeacherID = 1
GO
SELECT * FROM firstDB.dbo.Teacher
GO
SELECT * FROM secondDB.dbo.Teacher
GO

DELETE FROM TeacherView
GO
SELECT * FROM firstDB.dbo.Teacher
GO
SELECT * FROM secondDB.dbo.Teacher
GO

USE master
GO
DROP DATABASE firstDB
GO
DROP DATABASE secondDB
GO


