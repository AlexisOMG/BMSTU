USE master
GO
CREATE DATABASE EdTech
GO
USE EdTech
GO

CREATE TABLE Teacher (
    TeacherID int PRIMARY KEY,
    Firstname varchar(MAX) NOT NULL,
    Surname varchar(MAX) NOT NULL,
    Middlename varchar(MAX) NOT NULL,
    About varchar(MAX),
    Phone varchar(11) NOT NULL UNIQUE,
    Email VARCHAR(256) NOT NULL UNIQUE
);
GO

CREATE TABLE Student (
    StudentID int PRIMARY KEY,
    Firstname varchar(MAX) NOT NULL,
    Surname varchar(MAX) NOT NULL,
    Middlename varchar(MAX) NOT NULL,
    DateOfBirth date NOT NULL DEFAULT GETDATE(),
    Phone varchar(11) NOT NULL UNIQUE,
    Email VARCHAR(256) NOT NULL UNIQUE
);
GO

CREATE TABLE Course (
    CourseID int PRIMARY KEY,
    CourseName varchar(MAX) NOT NULL,
    Description varchar(MAX),
    StartDate date NOT NULL,
    EndDate date NOT NULL,
    TeacherID int NOT NULL ,
    CONSTRAINT Course_Teacher_FK FOREIGN KEY (TeacherID)
        REFERENCES Teacher(TeacherID)
        ON DELETE CASCADE
);
GO

CREATE TABLE Class (
    ClassID int PRIMARY KEY,
    Duration float NOT NULL,
    Theme varchar(MAX) NOT NULL,
    StartDate datetime NOT NULL,
    CourseID int NOT NULL,
    CONSTRAINT Class_Course_FK FOREIGN KEY (CourseID)
        REFERENCES Course(CourseID)
        ON DELETE CASCADE
);
GO

CREATE TABLE Progress (
    Score int NOT NULL,
    CourseID int NOT NULL,
    StudentID int NOT NULL,

    CONSTRAINT Progress_Course_FK FOREIGN KEY (CourseID)
        REFERENCES Course(CourseID)
        ON DELETE CASCADE,
    CONSTRAINT Progress_Student_FK FOREIGN KEY (StudentID)
        REFERENCES Student(StudentID)
        ON DELETE CASCADE,
    PRIMARY KEY (CourseID, StudentID)
);
GO

ALTER TABLE Progress
    ADD Comment varchar(MAX);
GO

CREATE TABLE Visits (
    StudentID int NOT NULL,
    ClassID int NOT NULL,

    CONSTRAINT Visits_Class_FK FOREIGN KEY (ClassID)
        REFERENCES Class(ClassID)
        ON DELETE CASCADE,
    CONSTRAINT Visits_Student_FK FOREIGN KEY (StudentID)
        REFERENCES Student(StudentID)
        ON DELETE CASCADE,
    PRIMARY KEY (ClassID, StudentID)
);
GO

INSERT INTO Teacher (TeacherID, Firstname, Surname, Middlename, About, Phone, Email)
VALUES (1, 'Aleksandr', 'Aleksandrov', 'Aleksandrovich', 'Nothing', '84951111111', 'aleksandr@test.ru'),
       (2, 'Ivan', 'Ivanov', 'Ivanovich', 'Nothing', '84951111112', 'ivanov@test.ru'),
       (3, 'Aleksey', 'Alekseev', 'Alekseevich', 'Nothing', '84951111113', 'aleksey@test.ru'),
       (4, 'Maksim', 'Maksimov', 'Maksimovich', 'Nothing', '84951111114', 'maksim@test.ru'),
       (5, 'Artem', 'Belousov', 'Artemovich', 'Nothing', '84951111115', 'belousov@student.ru')
GO

INSERT INTO Student (StudentID, Firstname, Surname, Middlename, DateOfBirth, Phone, Email)
VALUES (1, 'Aleksey', 'Egorov', 'Nikolaevich', CONVERT(date, '01/28/2001'), '84951111114', 'egorov@student.ru'),
       (2, 'Artemiy', 'Belousov', 'Artemovich', CONVERT(date, '06/21/2001'), '84951111115', 'belousov@student.ru'),
       (3, 'Dmitry', 'Zhuk', 'Olegovich', CONVERT(date, '06/08/2001'), '84951111116', 'zhuk@student.ru');
GO

UPDATE Student SET Firstname = 'Artem' WHERE StudentID = 2;
GO

INSERT INTO Course (CourseID, CourseName, Description, StartDate, EndDate, TeacherID)
VALUES (1, 'Math', 'Hard Math', CONVERT(date, '09/01/2021'), CONVERT(date, '05/31/2022'), 1),
       (2, 'Galois', 'Hard Course', CONVERT(date, '09/01/2021'), CONVERT(date, '05/31/2022'), 2),
       (3, 'CS', 'Pretty Course', CONVERT(date, '09/01/2021'), CONVERT(date, '05/31/2022'), 3),
       (4, 'Easy Math', 'Easy Math', CONVERT(date, '09/01/2021'), CONVERT(date, '05/31/2022'), 1),
       (5, 'ICPC', 'Win ICPC', CONVERT(date, '09/01/2021'), CONVERT(date, '05/31/2022'), 5)
GO

INSERT INTO Class (ClassID, Duration, Theme, StartDate, CourseID)
VALUES (1, 3, 'First lesson', '2021-01-01 08:30:00', 1),
       (2, 3, 'Second lesson', '2021-01-02 08:30:00', 1),
       (3, 3, 'Third lesson', '2021-01-03 08:30:00', 1);
GO

INSERT INTO Progress (Score, Comment, CourseID, StudentID)
SELECT 50,
       'Good student',
       1,
       s.StudentID
FROM Student s;
GO

UPDATE Progress SET Score = 100 WHERE StudentID = 3;
GO
UPDATE Progress SET Score = 85 WHERE StudentID = 2;
GO

INSERT INTO Visits (StudentID, ClassID)
SELECT s.StudentID,
       1
FROM Student s;
GO

CREATE FUNCTION GetNotVisitedCourses()
RETURNS TABLE
AS
    RETURN (
        SELECT DISTINCT C2.CourseName NotVisitedCourse, C2.CourseID FROM Visits V
            INNER JOIN Class C on V.ClassID = C.ClassID
            FULL JOIN Course C2 on C.CourseID = C2.CourseID
        WHERE V.ClassID IS NULL
    )
GO
SELECT * FROM GetNotVisitedCourses()
GO

CREATE FUNCTION GetVisitedCourses()
RETURNS TABLE
AS
    RETURN (
        SELECT DISTINCT C.CourseName VisitedCourse, C.CourseID FROM Course C
        WHERE EXISTS(
            SELECT 1 FROM Visits V
                INNER JOIN Class C2 on C2.ClassID = V.ClassID
                INNER JOIN Course CC ON CC.CourseID = C2.CourseID
            WHERE CC.CourseID = C.CourseID
            )
    )
GO
SELECT * FROM GetVisitedCourses()
GO

CREATE FUNCTION GetCourseExcellentStudentIDs(@CourseID INT)
RETURNS TABLE
AS
    RETURN (
        SELECT P.StudentID FROM Progress P
        WHERE P.CourseID = @CourseID AND P.Score BETWEEN 85 AND 100
        ORDER BY P.Score DESC OFFSET 0 ROWS
    )
GO

CREATE FUNCTION GetExcellentStudents(@CourseID INT)
RETURNS TABLE
AS
    RETURN (
        SELECT S.StudentID, S.Firstname, S.Surname, S.Middlename FROM Student S
        WHERE S.StudentID IN (SELECT E.StudentID FROM GetCourseExcellentStudentIDs(@CourseID) E)
    )
GO
SELECT * FROM GetExcellentStudents(1)
GO

CREATE FUNCTION GetMathematicalCourses()
RETURNS TABLE
AS
    RETURN (
        SELECT C.CourseID, C.CourseName FROM Course C
        WHERE C.CourseName LIKE '%Math%'
    )
GO
SELECT * FROM GetMathematicalCourses()
GO

CREATE FUNCTION GetTeacherAmountOfCourses()
RETURNS TABLE
AS
    RETURN (
        SELECT T.TeacherID, COUNT(C.CourseID) AmountOfCourses FROM Course C
        RIGHT JOIN Teacher T on T.TeacherID = C.TeacherID
        GROUP BY T.TeacherID
    )
GO
SELECT * FROM GetTeacherAmountOfCourses()
GO

CREATE FUNCTION GetTeachersWithCourses()
RETURNS TABLE
AS
    RETURN (
        SELECT T.TeacherID, T.Firstname FROM Course C
        RIGHT JOIN Teacher T on T.TeacherID = C.TeacherID
        GROUP BY T.TeacherID, T.Firstname HAVING COUNT(C.CourseID) > 0
    )
GO
SELECT * FROM GetTeachersWithCourses()
GO

CREATE FUNCTION GetTeachersWithoutCourses()
RETURNS TABLE
AS
    RETURN (
        SELECT T.TeacherID, T.Firstname FROM Teacher T
        EXCEPT SELECT TeacherID, Firstname FROM GetTeachersWithCourses()
    )
GO
SELECT * FROM GetTeachersWithoutCourses()
GO

CREATE FUNCTION GetAmountOfCoursesPerTeacher()
RETURNS INT
AS
    BEGIN
        DECLARE @res INT
        SELECT @res = AVG(AmountOfCourses) FROM GetTeacherAmountOfCourses()
        RETURN @res
    END
GO
SELECT dbo.GetAmountOfCoursesPerTeacher()
GO

CREATE FUNCTION GetMAXCoursesPerTeacher()
RETURNS INT
AS
    BEGIN
        DECLARE @res INT
        SELECT @res = MAX(AmountOfCourses) FROM GetTeacherAmountOfCourses()
        RETURN @res
    END
GO
CREATE FUNCTION GetMINCoursesPerTeacher()
RETURNS INT
AS
    BEGIN
        DECLARE @res INT
        SELECT @res = MIN(AmountOfCourses) FROM GetTeacherAmountOfCourses()
        RETURN @res
    END
GO
CREATE FUNCTION GetTeachersWithMAXCourses()
RETURNS TABLE
AS
    RETURN (
        SELECT T.TeacherID, T.Firstname, T.Surname, T.Middlename FROM Teacher T
        INNER JOIN GetTeacherAmountOfCourses() CNT
           ON T.TeacherID = CNT.TeacherID
        WHERE CNT.AmountOfCourses = dbo.GetMAXCoursesPerTeacher()
    )
GO
SELECT * FROM GetTeachersWithMAXCourses()
GO

CREATE FUNCTION GetTotalHoursAmountForCourse(@CourseID INT)
RETURNS FLOAT
AS
    BEGIN
        DECLARE @res FLOAT
        SELECT @res = SUM(C.Duration) FROM Class C WHERE C.CourseID = @CourseID
        RETURN @res
    END
GO
SELECT dbo.GetTotalHoursAmountForCourse(1)
GO

CREATE FUNCTION GetStudentTeacher()
RETURNS TABLE
AS
    RETURN (
        SELECT S.Firstname, S.Surname, S.Middlename, S.Email FROM Student S
        INTERSECT
        SELECT T.Firstname, T.Surname, T.Middlename, T.Email FROM Teacher T
    )
GO
SELECT * FROM GetStudentTeacher()
GO

CREATE FUNCTION GetAllUsersWithRoles()
RETURNS TABLE
AS
    RETURN (
        SELECT S.Firstname, S.Surname, S.Middlename, S.Email, 'Student' AS Role FROM Student S
        UNION ALL
        SELECT T.Firstname, T.Surname, T.Middlename, T.Email, 'Teacher' AS Role FROM Teacher T
    )
GO
SELECT * FROM GetAllUsersWithRoles()
GO

CREATE FUNCTION GetAllUsers()
RETURNS TABLE
AS
    RETURN (
        SELECT S.Firstname, S.Surname, S.Middlename, S.Email FROM Student S
        UNION
        SELECT T.Firstname, T.Surname, T.Middlename, T.Email FROM Teacher T
        ORDER BY Firstname, Email OFFSET 0 ROWS
    )
GO
SELECT * FROM GetAllUsers()
GO

USE master
GO
DROP DATABASE EdTech
GO
c1 c2 c3 c4
1  a  a  2
2  a  a  2
3  b  b  7
3  b  b  5
4  c  c  8
5  d nul nul
nul nul g 9
