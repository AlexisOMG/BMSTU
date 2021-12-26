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
    TeacherID int IDENTITY(1, 1) PRIMARY KEY,
    Name varchar(MAX) NOT NULL,
    Phone varchar(11) NOT NULL,
    DateOfBirth date NOT NULL DEFAULT GETDATE()
);
GO

INSERT INTO Teacher (Name, Phone, DateOfBirth)
    VALUES ('Alexis', '84958888888', CONVERT(date, '01/28/2001')),
           ('Lina', '84958888881', CONVERT(date, '01/28/1999')),
           ('WhoIsIt', '84958888882', GETDATE());
GO

-- 1 --

CREATE PROC dbo.GetTeachers
    @teachersCursor CURSOR VARYING OUTPUT
AS
    SET @teachersCursor = CURSOR FORWARD_ONLY STATIC FOR SELECT * FROM Teacher
    OPEN @teachersCursor;
GO

DECLARE @curs CURSOR;
EXEC dbo.GetTeachers @teachersCursor = @curs OUTPUT;
FETCH NEXT FROM @curs;
WHILE @@FETCH_STATUS = 0
    BEGIN
        FETCH NEXT FROM @curs;
    END;
CLOSE @curs;
DEALLOCATE @curs;
GO

-- 2 --
CREATE FUNCTION dbo.TransformPhones(@Phone varchar(11))
RETURNS varchar(max)
AS
    BEGIN
        DECLARE @res varchar(12)
        SELECT @res = CONCAT('+7', SUBSTRING(@Phone, 2, 10));
        RETURN @res;
    END;
GO

CREATE PROC dbo.GetTeachersPhones
    @teacherPhones CURSOR VARYING OUTPUT
AS
    SET @teacherPhones = CURSOR FORWARD_ONLY STATIC FOR SELECT Name, dbo.TransformPhones(Phone) AS Phone FROM Teacher
    OPEN @teacherPhones;
GO

DECLARE @curs CURSOR;
EXEC dbo.GetTeachersPhones @teacherPhones = @curs OUTPUT;
FETCH NEXT FROM @curs;
WHILE @@FETCH_STATUS = 0
    BEGIN
        FETCH NEXT FROM @curs;
    END;
CLOSE @curs;
DEALLOCATE @curs;
GO

-- 3 --

CREATE FUNCTION dbo.IsAdult(@DateOfBirth date)
RETURNS BIT
AS
    BEGIN
        IF @DateOfBirth <= CONVERT(date, '01/01/2003')
            RETURN 1
        RETURN 0
    END;
GO

CREATE PROC dbo.GetAdultTeachers
AS
    DECLARE @teachers_cursor CURSOR
    EXEC dbo.GetTeachers @teachersCursor = @teachers_cursor OUTPUT

    DECLARE @TeacherID INT
    DECLARE @Name VARCHAR(MAX)
    DECLARE @Phone VARCHAR(11)
    DECLARE @DateOfBirth DATE

    FETCH NEXT FROM @teachers_cursor INTO @TeacherID, @Name, @Phone, @DateOfBirth;
    WHILE @@FETCH_STATUS = 0
    BEGIN
        IF dbo.IsAdult(@DateOfBirth) = 'true'
            PRINT FORMATMESSAGE('ADULT TEACHER: TeacherID = %d, TeacherName = %s', @TeacherID, @Name)
        FETCH NEXT FROM @teachers_cursor INTO @TeacherID, @Name, @Phone, @DateOfBirth
    END
    CLOSE @teachers_cursor
    DEALLOCATE @teachers_cursor
GO

EXEC dbo.GetAdultTeachers
GO

-- 4 --

CREATE FUNCTION dbo.GetAdults_inline()
RETURNS TABLE
AS RETURN (
    SELECT Name, dbo.TransformPhones(Phone) AS Phone FROM Teacher WHERE DateOfBirth <= CONVERT(date, '01/01/2003')
)
GO

CREATE FUNCTION dbo.GetAdults_non_inline()
RETURNS @res TABLE (
    Name VARCHAR(MAX),
    Phone VARCHAR(MAX)
)
AS
    BEGIN
        INSERT @res
            SELECT Name, dbo.TransformPhones(Phone) AS Phone FROM Teacher
            WHERE DateOfBirth <= CONVERT(date, '01/01/2003')
        RETURN
    END
GO

CREATE PROC dbo.GetTeachersWithTableFunc
    @teacherCursor CURSOR VARYING OUTPUT
AS
    SET @teacherCursor = CURSOR FORWARD_ONLY STATIC FOR SELECT * FROM dbo.GetAdults_inline()
    OPEN @teacherCursor
GO

DECLARE @teachers_cursor CURSOR
EXEC dbo.GetTeachersWithTableFunc @teacherCursor = @teachers_cursor OUTPUT

DECLARE @Name VARCHAR(MAX)
DECLARE @Phone VARCHAR(MAX)
FETCH NEXT FROM @teachers_cursor INTO @Name, @Phone
WHILE @@FETCH_STATUS = 0
    BEGIN
        PRINT @Name + ' ' + @Phone
        FETCH NEXT FROM @teachers_cursor INTO @Name, @Phone
    END
CLOSE @teachers_cursor
DEALLOCATE @teachers_cursor
GO


USE master;
GO
DROP DATABASE EdTech;
GO


