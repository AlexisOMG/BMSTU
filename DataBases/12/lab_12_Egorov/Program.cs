using System;
using System.Collections.Generic;
using System.Data;
using System.Data.SqlClient;

namespace lab_12_Egorov
{
    class Teacher
    {
        public int? ID { get; }
        public String Name;
        public String Phone;
        public String Email;

        public Teacher(String Name, String Phone, String Email, int? ID = null)
        {
            this.Name = Name;
            this.Phone = Phone;
            this.Email = Email;
            this.ID = ID;
        }

        public override string ToString()
        {
            return $"Name: {this.Name}, Phone: {this.Phone}, Email: {this.Email}";
        }
    }

    interface ITeacherTable
    {
        Teacher GetTeacher(int ID);
        Teacher[] ListTeachers();
        int CreateTeacher(Teacher teacher);
        void UpdateTeacher(int ID, Teacher teacher);
        void DeleteTeacher(int ID);
    }

    class TeacherTableConn : ITeacherTable
    {
        const string nameParam = "@name";
        const string phoneParam = "@phone";
        const string emailParam = "@email";
        private readonly string connString;

        public TeacherTableConn(string connString)
        {
            this.connString = connString;
        }

        public Teacher GetTeacher(int ID)
        {
            using (SqlConnection conn = new SqlConnection(this.connString))
            {
                conn.Open();
                SqlCommand sqlCommand = conn.CreateCommand();
                sqlCommand.CommandText = "SELECT TeacherID, Name, Phone, Email FROM Teacher WHERE TeacherID = @ID";

                SqlParameter IDParam = new SqlParameter
                {
                    Value = ID,
                    ParameterName = "@ID",
                };

                sqlCommand.Parameters.Add(IDParam);

                try
                {
                    SqlDataReader dataReader = sqlCommand.ExecuteReader();
                    dataReader.Read();

                    Teacher teacher = new Teacher(
                            dataReader["Name"].ToString(),
                            dataReader["Phone"].ToString(),
                            dataReader["Email"].ToString(),
                            Convert.ToInt32(dataReader["TeacherID"]));

                    dataReader.Close();
                    return teacher;
                } catch (Exception e)
                {
                    Console.WriteLine(e.Message);
                    return null;
                }
            }
        }

        public Teacher[] ListTeachers()
        {
            using (SqlConnection conn = new SqlConnection(this.connString))
            {
                conn.Open();
                SqlCommand sqlCommand = conn.CreateCommand();
                sqlCommand.CommandText = "SELECT TeacherID, Name, Phone, Email FROM Teacher";

                try
                {
                    SqlDataReader dataReader = sqlCommand.ExecuteReader();
                    List<Teacher> res = new List<Teacher>();
                    while (dataReader.Read())
                    {
                        Teacher teacher = new Teacher(
                            dataReader["Name"].ToString(),
                            dataReader["Phone"].ToString(),
                            dataReader["Email"].ToString(),
                            Convert.ToInt32(dataReader["TeacherID"]));
                        res.Add(teacher);
                    }
                    dataReader.Close();
                    return res.ToArray();
                } catch (Exception e)
                {
                    Console.WriteLine(e.Message);
                    return null;
                }
            }
        }

        public int CreateTeacher(Teacher teacher)
        {
            using (SqlConnection conn = new SqlConnection(this.connString))
            {
                conn.Open();

                SqlCommand insertCommand = conn.CreateCommand();
                insertCommand.CommandText = $"INSERT INTO Teacher (Name, Phone, Email) VALUES ({nameParam}, {phoneParam}, {emailParam}); SELECT SCOPE_IDENTITY()";

                SqlParameter name = new SqlParameter
                {
                    Value = teacher.Name,
                    ParameterName = nameParam,
                };
                insertCommand.Parameters.Add(name);

                SqlParameter phone = new SqlParameter
                {
                    Value = teacher.Phone,
                    ParameterName = phoneParam,
                };
                insertCommand.Parameters.Add(phone);

                SqlParameter email = new SqlParameter
                {
                    Value = teacher.Email,
                    ParameterName = emailParam,
                };
                insertCommand.Parameters.Add(email);

                try
                {
                    object ID = insertCommand.ExecuteScalar();
                    return Convert.ToInt32(ID);
                } catch (Exception e)
                {
                    Console.WriteLine(e.Message);
                    return -1;
                }
            }
        }

        public void UpdateTeacher(int ID, Teacher teacher)
        {
            using (SqlConnection conn = new SqlConnection(this.connString))
            {
                conn.Open();

                SqlCommand updateCommand = conn.CreateCommand();
                updateCommand.CommandText = $"UPDATE Teacher SET Name = {nameParam}, Phone = {phoneParam}, Email = {emailParam} WHERE TeacherID = @ID";

                SqlParameter name = new SqlParameter
                {
                    Value = teacher.Name,
                    ParameterName = nameParam,
                };
                updateCommand.Parameters.Add(name);

                SqlParameter phone = new SqlParameter
                {
                    Value = teacher.Phone,
                    ParameterName = phoneParam,
                };
                updateCommand.Parameters.Add(phone);

                SqlParameter email = new SqlParameter
                {
                    Value = teacher.Email,
                    ParameterName = emailParam,
                };
                updateCommand.Parameters.Add(email);

                SqlParameter IDParam = new SqlParameter
                {
                    Value = ID,
                    ParameterName = "@ID",
                };
                updateCommand.Parameters.Add(IDParam);

                try
                {
                    updateCommand.ExecuteNonQuery();
                } catch (Exception e)
                {
                    Console.WriteLine(e.Message);
                }
            }
        }

        public void DeleteTeacher(int ID)
        {
            using (SqlConnection conn = new SqlConnection(this.connString))
            {
                conn.Open();

                SqlCommand deleteCommand = conn.CreateCommand();
                deleteCommand.CommandText = "DELETE FROM Teacher WHERE TeacherID = @ID";

                SqlParameter IDParam = new SqlParameter
                {
                    Value = ID,
                    ParameterName = "@ID",
                };
                deleteCommand.Parameters.Add(IDParam);

                try
                {
                    deleteCommand.ExecuteNonQuery();
                } catch (Exception e)
                {
                    Console.WriteLine(e.Message);
                }
            }
        }
    }

    class TeacherTableDisconn : ITeacherTable
    {
        private readonly string connString;
        private readonly DataSet TeachersDataSet;
        private readonly SqlDataAdapter TeacherAdapter;

        public TeacherTableDisconn(string connString)
        {
            this.connString = connString;

            DataSet dataset = new DataSet("EdTech");

            DataColumn ID = new DataColumn("TeacherID", typeof(int));
            ID.ReadOnly = true;
            ID.Unique = true;
            ID.AllowDBNull = false;
            ID.AutoIncrement = true;

            DataColumn name = new DataColumn("Name", typeof(string));
            name.AllowDBNull = false;

            DataColumn phone = new DataColumn("Phone", typeof(string));

            DataColumn email = new DataColumn("Email", typeof(string));
            email.Unique = true;
            email.AllowDBNull = false;

            DataTable table = new DataTable("Teacher");
            table.Columns.AddRange(new DataColumn[] { ID, name, phone, email });
            table.PrimaryKey = new DataColumn[] { ID };

            dataset.Tables.Add(table);

            this.TeachersDataSet = dataset;

            using (SqlConnection conn = new SqlConnection(this.connString))
            {
                conn.Open();
                SqlDataAdapter adapter = new SqlDataAdapter("SELECT * FROM Teacher", conn);

                adapter.InsertCommand = conn.CreateCommand();
                adapter.InsertCommand.CommandText = "INSERT INTO Teacher (Name, Phone, Email) VALUES (@name, @phone, @email)";
                SqlParameter insertNameParam = new SqlParameter
                {
                    ParameterName = "@name",
                    SqlDbType = SqlDbType.VarChar,
                    SourceColumn = "Name",
                };
                adapter.InsertCommand.Parameters.Add(insertNameParam);
                adapter.InsertCommand.Parameters.Add("@phone", SqlDbType.VarChar, 11, "Phone");
                adapter.InsertCommand.Parameters.Add("@email", SqlDbType.VarChar, 256, "Email");

                adapter.UpdateCommand = conn.CreateCommand();
                adapter.UpdateCommand.CommandText = "UPDATE Teacher SET Name = @name, Phone = @phone, Email = @email WHERE TeacherID = @teacherID";
                SqlParameter updateNameParam = new SqlParameter
                {
                    ParameterName = "@name",
                    SqlDbType = SqlDbType.VarChar,
                    SourceColumn = "Name",
                };
                adapter.UpdateCommand.Parameters.Add(updateNameParam);
                adapter.UpdateCommand.Parameters.Add("@phone", SqlDbType.VarChar, 11, "Phone");
                adapter.UpdateCommand.Parameters.Add("@email", SqlDbType.VarChar, 256, "Email");
                adapter.UpdateCommand.Parameters.Add("@teacherID", SqlDbType.Int, 10, "TeacherID");

                adapter.DeleteCommand = conn.CreateCommand();
                adapter.DeleteCommand.CommandText = "DELETE FROM Teacher WHERE TeacherID = @teacherID";
                adapter.DeleteCommand.Parameters.Add("@teacherID", SqlDbType.Int, 10, "TeacherID");

                this.TeacherAdapter = adapter;

                try
                {
                    this.TeacherAdapter.Fill(this.TeachersDataSet.Tables["Teacher"]);
                } catch (Exception e)
                {
                    Console.WriteLine(e.Message);
                }
            }
        }

        public void Sync()
        {
            using (SqlConnection conn = new SqlConnection(this.connString))
            {
                conn.Open();
                try
                {
                    this.TeacherAdapter.Update(this.TeachersDataSet.Tables["Teacher"]);
                } catch (Exception e)
                {
                    Console.WriteLine(e.Message);
                }
            }
        }

        public int CreateTeacher(Teacher teacher)
        {
            DataRow row = this.TeachersDataSet.Tables["Teacher"].NewRow();
            row["Name"] = teacher.Name;
            row["Phone"] = teacher.Phone;
            row["Email"] = teacher.Email;
            this.TeachersDataSet.Tables["Teacher"].Rows.Add(row);
            return Convert.ToInt32(row["TeacherID"]);
        }

        public void DeleteTeacher(int ID)
        {
            DataRow row = this.TeachersDataSet.Tables["Teacher"].Rows.Find(ID);
            this.TeachersDataSet.Tables["Teacher"].Rows.Remove(row);
        }

        public Teacher GetTeacher(int ID)
        {
            DataRow dataReader = this.TeachersDataSet.Tables["Teacher"].Rows.Find(ID);

            Teacher teacher = new Teacher(
                    dataReader["Name"].ToString(),
                    dataReader["Phone"].ToString(),
                    dataReader["Email"].ToString(),
                    Convert.ToInt32(dataReader["TeacherID"]));

            return teacher;
        }

        public Teacher[] ListTeachers()
        {
            DataTableReader dataReader = this.TeachersDataSet.Tables["Teacher"].CreateDataReader();
            List<Teacher> res = new List<Teacher>();
            while (dataReader.Read())
            {
                Teacher teacher = new Teacher(
                    dataReader["Name"].ToString(),
                    dataReader["Phone"].ToString(),
                    dataReader["Email"].ToString(),
                    Convert.ToInt32(dataReader["TeacherID"]));
                res.Add(teacher);
            }
            dataReader.Close();
            return res.ToArray();
        }

        public void UpdateTeacher(int ID, Teacher teacher)
        {
            DataRow row = this.TeachersDataSet.Tables["Teacher"].Rows.Find(ID);
            row["Name"] = teacher.Name;
            row["Phone"] = teacher.Phone;
            row["Email"] = teacher.Email;
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            string connectionString = System.Configuration.ConfigurationManager.ConnectionStrings["default"].ConnectionString;

            Console.WriteLine("CONNECTED");
            TeacherTableConn teacherConnected = new TeacherTableConn(connectionString);
            Console.WriteLine("Teachers in table:");
            foreach(var teacher in teacherConnected.ListTeachers())
            {
                Console.Write("\t"+teacher.ID.ToString() + " ");
                Console.WriteLine(teacher.ToString());
            }
            int createdTeacherID = teacherConnected.CreateTeacher(new Teacher("Artem", "88888888888", "test3@mail.com"));
            Teacher createdTeacher = teacherConnected.GetTeacher(createdTeacherID);
            Console.WriteLine("Created Teacher: " + createdTeacher.ToString());
            Console.WriteLine($"Created Teacher ID: {createdTeacherID}");
            Teacher updatedTeacher = new Teacher("Vadim", createdTeacher.Phone, createdTeacher.Email);
            teacherConnected.UpdateTeacher(createdTeacherID, updatedTeacher);
            updatedTeacher = teacherConnected.GetTeacher(createdTeacherID);
            Console.WriteLine("Updated Teacher: " + updatedTeacher.ToString());
            teacherConnected.DeleteTeacher(createdTeacherID);
            Console.WriteLine($"Delete Teacher with ID = {createdTeacher}, Amount of teachers in tables: {teacherConnected.ListTeachers().Length}");

            Console.WriteLine("\nNOT CONNECTED");
            TeacherTableDisconn teacherTableDisconn = new TeacherTableDisconn(connectionString);
            Console.WriteLine("Teachers in table:");
            foreach (var teacher in teacherTableDisconn.ListTeachers())
            {
                Console.Write("\t" + teacher.ID.ToString() + " ");
                Console.WriteLine(teacher.ToString());
            }
            createdTeacherID = teacherTableDisconn.CreateTeacher(new Teacher("Artem", "88888888888", "test3@mail.com"));
            createdTeacher = teacherTableDisconn.GetTeacher(createdTeacherID);
            Console.WriteLine("Created Teacher: " + createdTeacher.ToString());
            Console.WriteLine($"Created Teacher ID: {createdTeacherID}");
            updatedTeacher = new Teacher("Vadim", createdTeacher.Phone, createdTeacher.Email);
            teacherTableDisconn.UpdateTeacher(createdTeacherID, updatedTeacher);
            updatedTeacher = teacherTableDisconn.GetTeacher(createdTeacherID);
            Console.WriteLine("Updated Teacher: " + updatedTeacher.ToString());
            teacherTableDisconn.DeleteTeacher(createdTeacherID);
            teacherTableDisconn.Sync();
            Console.WriteLine($"Delete Teacher with ID = {createdTeacher}, Amount of teachers in tables: {teacherConnected.ListTeachers().Length}");
        }
    }
}
