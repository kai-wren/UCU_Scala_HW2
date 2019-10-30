import java.sql.{Connection, DriverManager}

object Exercise7 extends App {

  object MysqlConnection {
    def getConnection(): Connection = {
      val driver = "com.mysql.cj.jdbc.Driver"
      val url = "jdbc:mysql://localhost:3316/week2"
      val username = "root"
      val password = "root"
      Class.forName(driver);
      DriverManager.getConnection(url, username, password)
    }
  }

  case class salesOrder(docID: Int, custID: String, amount: Double, currency: String)

  class salesOrderDAO {
    def create(docID: Int, custID: String, amount: Double, currency: String) = {
      val statement = MysqlConnection.getConnection().createStatement()
      statement.execute("INSERT INTO salesOrder (docID,custID,amount,currency) VALUES(" + docID + ",\"" + custID + "\",\"" + amount + "\",\"" + currency + "\");")
    }

    def parseRS(rS: java.sql.ResultSet, list: List[salesOrder]=Nil): List[salesOrder] = {
      if (rS.next()) {
        val value = salesOrder(rS.getInt("docID"), rS.getString("custID"), rS.getDouble("amount"), rS.getString("currency"))
        parseRS(rS, value :: list)
      } else {
        list
      }
    }

    def findById(docID: Int): Option[salesOrder] = {
      val statement = MysqlConnection.getConnection().createStatement()
      val resultSet = statement.executeQuery("SELECT * FROM salesOrder where docID=\"" + docID + "\";")

      val sO: Option[salesOrder] = if (resultSet.first()) Some(salesOrder(resultSet.getInt("docID"), resultSet.getString("custID"), resultSet.getDouble("amount"), resultSet.getString("currency"))) else None
      sO
    }

    def findByCustID(custID: String): Option[List[salesOrder]] = {
      val statement = MysqlConnection.getConnection().createStatement()
      val resultSet = statement.executeQuery("SELECT * FROM salesOrder where custID=\"" + custID + "\";")
      val sOList = Some(parseRS(resultSet))
        if (sOList.get.isEmpty) None else sOList
    }

    def update(docID: Int, custID: String = "", amount: Double = 0, currency: String = "") = {
      val statement = MysqlConnection.getConnection().createStatement()
      val dao = new salesOrderDAO
      val someSO: Option[salesOrder] = dao.findById(docID)
      if (someSO != None) {
        val oldSO: salesOrder = someSO.get
        if (!custID.isEmpty && custID != oldSO.custID) statement.execute("UPDATE salesOrder set custID=\"" + custID + "\" where docID=" + docID + ";")
        if (amount != 0 && amount != oldSO.amount) statement.execute("UPDATE salesOrder set amount=" + amount + " where docID=" + docID + ";")
        if (!currency.isEmpty && currency != oldSO.currency) statement.execute("UPDATE salesOrder set currency=\"" + currency + "\" where docID=" + docID + ";")
      } else throw new NoSuchElementException("No such record")
      }

    def remove(docID: Int)={
      val statement = MysqlConnection.getConnection().createStatement()
      statement.execute("DELETE FROM salesOrder WHERE docID=" + docID + ";")
    }


  }

//  val dao = new salesOrderDAO
////    dao.create(6, "del", 500, "USD")
//  println(dao.findById(3).getOrElse("No such record found"))
//  println(dao.findByCustID("abc").getOrElse("No such records found"))
//
////  dao.update(1, "abc", 100, "USD")
//  dao.update(1, custID = "abc", amount = 100)
//  println(dao.findById(1).getOrElse("No such record found"))
//
//  dao.remove(6)
//  println(dao.findById(6).getOrElse("No such record found"))

  //Business logic
  def createOrUpdate(docID: Int, custID: String, amount: Double, currency: String): salesOrder = {
    val dao = new salesOrderDAO
    val oldSO: Option[salesOrder] = dao.findById(6)
    if (oldSO != None) {
      dao.update(docID, custID, amount, currency)
    }
    else {
      dao.create(docID, custID, amount, currency)
    }
    dao.findById(docID).get
  }

  //checking result
  val newSO = createOrUpdate(6, "qwerty", 700, "UAH")
  println(newSO)
}
