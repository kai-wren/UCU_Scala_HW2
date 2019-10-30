# UCU_Scala_HW2

//run mysql container:
docker run -p 3316:3306 --name some-mysql -e MYSQL_ROOT_PASSWORD=my-secret-pw -d mysql:tag

//to connect via console:
docker exec -it mysql1 mysql -uroot -p

//DB:
CREATE DATABASE week2;
USE week2;
CREATE TABLE salesOrder (docID INT, custID VARCHAR(20), amount FLOAT, currency VARCHAR(20));


//command to run:
scala -classpath .\target\scala-2.13\hw2.jar Exercise1