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
    TeacherID uniqueidentifier PRIMARY KEY DEFAULT NEWID(),
    Name varchar(MAX) NOT NULL,
    Phone varchar(11),
    DateOfBirth date NOT NULL DEFAULT GETDATE(),
    Email VARCHAR(256) NOT NULL UNIQUE

);
GO

-- INSERT INTO Teacher (Name, Phone, Email)
-- VALUES ('Alex', '89999999999', 'test1@maul.ru'),
--        ('Tema', '89999999933', 'test2@maul.ru')
-- GO


CREATE TABLE Course (
    CourseID uniqueidentifier PRIMARY KEY DEFAULT NEWID(),
    Description varchar(MAX) NOT NULL,
    TeacherID uniqueidentifier,
    CONSTRAINT Teacher_FK FOREIGN KEY (TeacherID)
        REFERENCES Teacher(TeacherID)
        ON DELETE CASCADE
);
GO

-- 1 --
CREATE TRIGGER TeacherInsertTrigger ON Teacher
    FOR INSERT
    AS
    INSERT INTO Course (Description, TeacherID) SELECT 'Math', ins.TeacherID FROM inserted as ins;
GO

CREATE TRIGGER TeacherUpdateTrigger ON Teacher
    FOR UPDATE
    AS
    IF EXISTS(SELECT 1 FROM inserted WHERE inserted.Name LIKE '%NIGA%')
        BEGIN
            RAISERROR('BLM', 13, 0)
            ROLLBACK
        end

GO

CREATE TRIGGER TeacherDeleteTrigger ON Teacher
    FOR DELETE
    AS
    DECLARE @cnt INT
    SELECT @cnt = COUNT(*) FROM deleted
    PRINT FORMATMESSAGE('Deleted %d rows', @cnt)
GO

INSERT INTO Teacher (Name, Phone, DateOfBirth, Email)
    VALUES ('Alexis', '84958888888', CONVERT(date, '01/28/2001'), 'test1@gmail.com'),
           ('Lina', '84958888881', CONVERT(date, '01/28/1999'), 'test2@gmail.com'),
           ('WhoIsIt', '84958888882', GETDATE(), 'test3@gmail.com');
GO

UPDATE Teacher SET Name = 'NIGA' WHERE Name = 'Alexis';
GO

DELETE FROM Teacher WHERE Name = 'Alexis';
GO

-- 2 --
CREATE VIEW CourseView (CourseDescription, TeacherName, TeacherEmail) WITH SCHEMABINDING AS
    SELECT  c.Description CourseDescription, t.Name TeacherName, t.Email TeacherEmail
    FROM dbo.Course c
    INNER JOIN dbo.Teacher t on t.TeacherID = c.TeacherID
GO

CREATE TRIGGER CourseViewInsertTrigger ON CourseView
    INSTEAD OF INSERT
    AS
    INSERT INTO Teacher (Email, Name)
    SELECT DISTINCT (ins.TeacherEmail), ins.TeacherName FROM inserted AS ins
    WHERE NOT EXISTS(SELECT t.Email FROM Teacher t WHERE t.Email = ins.TeacherEmail)

    INSERT INTO Course (Description, TeacherID)
        SELECT ins.CourseDescription, t.TeacherID
        FROM Teacher as t
        INNER JOIN inserted as ins on t.Email = ins.TeacherEmail
GO

CREATE TRIGGER CourseViewDeleteTrigger ON CourseView
    INSTEAD OF DELETE
    AS
    DELETE C FROM Course C
    INNER JOIN deleted del on del.CourseDescription = C.Description
    INNER JOIN Teacher T on T.Email = del.TeacherEmail
    WHERE C.TeacherID = T.TeacherID

GO

CREATE TRIGGER CourseViewUpdateTrigger ON CourseView
    INSTEAD OF UPDATE
    AS
    IF UPDATE(TeacherEmail)
        BEGIN
            RAISERROR('Unable to change email', 13, 2)
            ROLLBACK
        END
    ELSE
        BEGIN
            IF UPDATE(TeacherName)
            BEGIN
                UPDATE Teacher SET Name = ins.TeacherName FROM inserted as ins where Teacher.Email = ins.TeacherEmail
            END
            IF UPDATE(CourseDescription)
            BEGIN
                UPDATE Course SET Description = ins.CourseDescription
                FROM Teacher t
                INNER JOIN inserted ins ON ins.TeacherEmail = t.Email
                INNER JOIN deleted del ON del.TeacherEmail = t.Email
                INNER JOIN Course c on t.TeacherID = c.TeacherID
                where CourseID = c.CourseID and Description = del.CourseDescription
            END
        END
GO


INSERT INTO CourseView (CourseDescription, TeacherName, TeacherEmail)
    VALUES ('cooldescr', 'Vadim', 'test6@gmail.com'), ('another', 'Vadim', 'test6@gmail.com');
SELECT * FROM Teacher
SELECT * FROM Course
GO

UPDATE CourseView SET CourseView.TeacherName = 'Dima' WHERE CourseView.TeacherEmail = 'test6@gmail.com';
SELECT * FROM Teacher
GO

DELETE FROM CourseView WHERE CourseView.TeacherEmail = 'test6@gmail.com'
SELECT * FROM Teacher
SELECT * FROM Course
GO




USE master
GO
DROP DATABASE EdTech
GO
