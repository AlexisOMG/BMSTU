USE master;
GO

-- 1 --
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
    TeacherID int IDENTITY(1, 1) PRIMARY KEY,
    Name text NOT NULL
);
GO

SELECT IDENT_CURRENT('Teacher');
GO
SELECT SCOPE_IDENTITY();
GO
SELECT @@IDENTITY;
GO


-- 2 --
ALTER TABLE  Teacher
    ADD Phone varchar(max)
    CONSTRAINT Len_Phone_Constraint
    CHECK (LEN(Phone) = 11);
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
    VALUES ('DEFAULT', '88888888888');
GO

-- 3 --
CREATE TABLE Course (
    CourseID uniqueidentifier PRIMARY KEY DEFAULT NEWID(),
    Description text NOT NULL,
    TeacherID int DEFAULT 1,
    CONSTRAINT Teacher_FK FOREIGN KEY (TeacherID)
        REFERENCES Teacher(TeacherID)
        ON DELETE CASCADE
);
GO

-- 4 --
CREATE SEQUENCE ClassSequence
    AS int
    START WITH 1
    INCREMENT BY 1;
GO

CREATE TABLE Class (
    ClassID int PRIMARY KEY DEFAULT NEXT VALUE FOR ClassSequence,
    Duration float NOT NULL,
    Date date NOT NULL,
    StartTime time NOT NULL
);
GO

-- 5 CASCADE --
INSERT INTO Teacher (Name, Phone)
    VALUES ('Alexey', '89999999999');
GO

INSERT INTO Course (Description, TeacherID)
    VALUES ('The coolest course', SCOPE_IDENTITY());
GO

SELECT * FROM Teacher;
GO
SELECT * FROM Course;
GO

DELETE Teacher WHERE TeacherID = IDENT_CURRENT('Teacher');
GO

SELECT * FROM Teacher;
GO
SELECT * FROM Course;
GO
DELETE  FROM Course;
GO

-- 5 SET NULL --
ALTER TABLE Course
    DROP CONSTRAINT Teacher_FK;
GO

ALTER TABLE Course
    ADD CONSTRAINT Teacher_FK FOREIGN KEY (TeacherID)
        REFERENCES Teacher(TeacherID)
        ON DELETE SET NULL;
GO

INSERT INTO Teacher (Name, Phone)
    VALUES ('Alexey', '89999999999');
GO

INSERT INTO Course (Description, TeacherID)
    VALUES ('The coolest course', SCOPE_IDENTITY());
GO

SELECT * FROM Teacher;
GO
SELECT * FROM Course;
GO

DELETE Teacher WHERE TeacherID = IDENT_CURRENT('Teacher');
GO
SELECT * FROM Teacher;
GO
SELECT * FROM Course;
GO
DELETE  FROM Course;
GO

-- 5 SET DEFAULT --
ALTER TABLE Course
    DROP CONSTRAINT Teacher_FK;
GO

ALTER TABLE Course
    ADD CONSTRAINT Teacher_FK FOREIGN KEY (TeacherID)
        REFERENCES Teacher(TeacherID)
        ON DELETE SET DEFAULT;
GO

INSERT INTO Teacher (Name, Phone)
    VALUES ('Alexey', '89999999999');
GO

INSERT INTO Course (Description, TeacherID)
    VALUES ('The coolest course', SCOPE_IDENTITY());
GO

SELECT * FROM Teacher;
GO
SELECT * FROM Course;
GO

DELETE Teacher WHERE TeacherID = IDENT_CURRENT('Teacher');
GO
SELECT * FROM Teacher;
GO
SELECT * FROM Course;
GO
DELETE  FROM Course;
GO

-- 5 NO ACTION --
ALTER TABLE Course
    DROP CONSTRAINT Teacher_FK;
GO

ALTER TABLE Course
    ADD CONSTRAINT Teacher_FK FOREIGN KEY (TeacherID)
        REFERENCES Teacher(TeacherID)
        ON DELETE NO ACTION;
GO

INSERT INTO Teacher (Name, Phone)
    VALUES ('Alexey', '89999999999');
GO

INSERT INTO Course (Description, TeacherID)
    VALUES ('The coolest course', SCOPE_IDENTITY());
GO

SELECT * FROM Teacher;
GO
SELECT * FROM Course;
GO

DELETE Teacher WHERE TeacherID = IDENT_CURRENT('Teacher');
GO

SELECT * FROM Teacher;
GO
SELECT * FROM Course;
GO
DELETE  FROM Course;
GO

-- after script --
USE master;
GO
DROP DATABASE EdTech;
GO
