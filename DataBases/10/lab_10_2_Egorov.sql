USE EdTech;
GO

BEGIN TRANSACTION
    UPDATE Teacher SET Name = 'Artem' WHERE TeacherID = 3;

    WAITFOR DELAY '00:00:05';

    SELECT * FROM Teacher;
COMMIT TRANSACTION;
GO

BEGIN TRANSACTION
    UPDATE Teacher SET Name = 'Dima' WHERE TeacherID = 3;

    WAITFOR DELAY '00:00:05';

    SELECT * FROM Teacher;
COMMIT TRANSACTION;
GO

BEGIN TRANSACTION
    INSERT INTO Teacher (TeacherID, Name, Phone, DateOfBirth, Email)
    VALUES (4, 'Lina4', '84958888441', CONVERT(date, '01/28/1994'), 'test4@gmail.com');
COMMIT TRANSACTION;
GO

BEGIN TRANSACTION
    INSERT INTO Teacher (TeacherID, Name, Phone, DateOfBirth, Email)
    VALUES (5, 'Lina5', '84958888551', CONVERT(date, '01/28/1995'), 'test5@gmail.com');
COMMIT TRANSACTION;
GO


