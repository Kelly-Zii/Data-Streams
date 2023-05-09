import com.typesafe.config.{Config, ConfigFactory}

import java.io.InputStreamReader
import java.sql.{Connection, DriverManager}

/** Extract database details from the configuration file and create a database connection */
object DbDetails:
  Class.forName("org.postgresql.Driver")

  private val cfg: Config =
    ConfigFactory.parseReader(new InputStreamReader(getClass.getClassLoader.getResourceAsStream("secret.conf")))
  private val dbpwd = cfg.getString("db.passwd")

  private val dbuser = cfg.getString("db.user")
  private val dbUrl = cfg.getString("db.name")

  var conn: Connection = null

  try {
    conn = DriverManager.getConnection(dbUrl, dbuser, dbpwd)
    println("Database connection established successfully.")
  } catch {
    case e: Exception =>
      println(s"Error initializing database connection: $e")
      throw e
  }








