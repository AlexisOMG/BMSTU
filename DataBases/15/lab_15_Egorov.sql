USE master
GO

CREATE DATABASE firstDB
GO
USE firstDB
GO

CREATE TABLE Teacher (
    TeacherID int PRIMARY KEY,
    Name varchar(MAX) NOT NULL,
    Phone varchar(11),
    DateOfBirth date NOT NULL
)
GO

CREATE DATABASE secondDB;
GO
USE secondDB
GO

CREATE TABLE Course (
    CourseID int PRIMARY KEY,
    CourseName varchar(MAX) NOT NULL,
    Description varchar(MAX),
    TeacherID int NOT NULL
);
GO

CREATE TRIGGER CourseInsert ON Course FOR INSERT
AS
    IF EXISTS(SELECT 1 FROM inserted ins LEFT JOIN firstDB.dbo.Teacher T ON ins.TeacherID = T.TeacherID WHERE T.TeacherID IS NULL)
    BEGIN
        RAISERROR('Invalid TeacherID', 10, 1)
        ROLLBACK
    END
GO

CREATE TRIGGER CourseUpdate ON Course FOR UPDATE
AS
    IF UPDATE(CourseID) OR UPDATE(TeacherID) AND EXISTS(SELECT 1 FROM inserted ins LEFT JOIN firstDB.dbo.Teacher T ON ins.TeacherID = T.TeacherID WHERE T.TeacherID IS NULL)
    BEGIN
        RAISERROR('Invalid TeacherID OR updating CourseID', 10, 2)
        ROLLBACK
    END
GO

USE firstDB
GO

CREATE TRIGGER TeacherDelete ON Teacher FOR DELETE
AS
    DELETE C FROM secondDB.dbo.Course C INNER JOIN deleted del ON C.TeacherID = del.TeacherID
GO

CREATE TRIGGER TeacherUpdate ON Teacher FOR UPDATE
AS
    IF UPDATE(TeacherID)
    BEGIN
        RAISERROR('Unable to update TeacherID', 10, 4)
        ROLLBACK
    END
GO

INSERT INTO secondDB.dbo.Course (CourseID, CourseName, Description, TeacherID)
VALUES (1, 'Math', 'Cool MATH', 1)
GO

INSERT INTO firstDB.dbo.Teacher (TeacherID, Name, Phone, DateOfBirth)
VALUES (1, 'Alexey', '88888888888', CONVERT(date, '01/28/2001')),
       (2, 'Dima', '99999999999', CONVERT(date, '06/21/2001'))
GO
INSERT INTO secondDB.dbo.Course (CourseID, CourseName, Description, TeacherID)
VALUES (1, 'Math', 'Cool MATH', 1),
       (2, 'CS', 'Cool CS', 2)
GO

SELECT * FROM firstDB.dbo.Teacher
GO
SELECT * FROM secondDB.dbo.Course
GO

UPDATE secondDB.dbo.Course SET TeacherID = -1 WHERE CourseID = 1
GO

UPDATE secondDB.dbo.Course SET TeacherID = 2, CourseName = 'MATH+' WHERE CourseID = 1
GO

SELECT * FROM firstDB.dbo.Teacher
GO
SELECT * FROM secondDB.dbo.Course
GO

DELETE FROM firstDB.dbo.Teacher WHERE TeacherID = 2
GO

SELECT * FROM firstDB.dbo.Teacher
GO
SELECT * FROM secondDB.dbo.Course
GO




USE master
GO
DROP DATABASE firstDB
GO
DROP DATABASE secondDB
GO
