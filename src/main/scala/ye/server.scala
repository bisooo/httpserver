package main.scala.ye

import java.io.{BufferedInputStream, BufferedReader, BufferedWriter, File, FileInputStream, FileNotFoundException, IOException, InputStreamReader, OutputStreamWriter, PrintWriter}
import java.net.{ServerSocket, Socket, SocketException}
import java.nio.file.{Files, Paths}

import scala.concurrent.TimeoutException
import scala.io.Source


class http ( socket : ServerSocket )
{

  // HTTP server replies
  val http200 = "HTTP/1.1 200 OK\r\n"
  val http403 = "HTTP/1.1 403 Forbidden\r\n"
  val http404 = "HTTP/1.1 404 Not Found\r\n"
  val http405 = "HTTP/1.1 405 Method Not Allowed\r\n"
  val http500 = "HTTP/1.1 500 Internal Server Error\r\n"

  val htmltext = "Content-Type: text/html\r\n"
  val csstext = "Content-Type: text/css\r\n"
  val gifimg = "Content-Type: image/gif\r\n"
  val jpegimg = "Content-Type: image/jpeg\r\n"
  val pngimg = "Content-Type: image/png\r\n"
  val svgimg = "Content-Type: image/svg+xml\r\n"

  val docsize = "Content-Length: "


  def start(): Unit =
  {
    println("ACTIVE @ localhost:" + socket.getLocalPort + "\n")
    // " GAME LOOP "
    while ( true )
    {
      // creates a new socket for a newly incoming connection
      val client = socket.accept()
      try {
        // "multi-threading" with call to function (request) to handle each client
        val thread = new Thread(new Runnable() {
          def run() = request(client)
        })
        thread.start()
      } catch {
        //case e : SocketException => println("SHUTTING DOWN SERVER...")
        case e : TimeoutException => sendhtml("/error/500.html",client,500) // error 500
      }

    }
  }

  def request( client : Socket ): Unit =
  {
    // buffer reader for the input stream - to receive HTTP REQUESTS from the client
    val get = new BufferedReader( new InputStreamReader( client.getInputStream ) )
    val httprequest = get.readLine()
    println("=== " + httprequest + " ===\n")
    //  splitting the request into method("GET") / directory("/") / version of HTTP("HTTP/1.1")
    val check = httprequest.split(" ")
    // variable to store the requested directory / absolute path can be used to check if file exists
    var path = ""
    if ( check(0) == "GET" && check(2) == "HTTP/1.1" )
    {
      check(1) match
      {
        // loads index.html if no directory or file is specified
        case "/" => path = "/index.html"
        case _ => path = check(1)
      }
      // seperates main directory from file ( "dir" / "file" )
      val dir = path.split("/")
      val directory = dir(1)
      // checks if a "restricted" directory is accessed - error 403
      if ( directory == "ignore" || directory == "error" )
      { println("! ERROR 403 !\n") ; sendhtml("/error/403.html",client,403) } // error 403
      else
      {
        if ( path.contains(".") )
        {
          // seperates filename from its extension ( "filename" . "extension" )
          val ext = path.split("\\.")
          val filetype = ext(1)
          filetype match
          {
            case "html" => sendhtml(path,client,200)
            case "css" => sendhtml(path,client,200)

            case "png" => sendimg(path,client,"png")
            case "jpeg" => sendimg(path,client,"jpg")
            case "jpg" => sendimg(path,client,"jpg")
            case "gif" => sendimg(path,client,"gif")
            case "svg" => sendimg(path,client,"svg")
            case "ico" =>
            case _ =>
          }
        }
        else
        {
          // if a directory is requested then the (index.html) of that directory is loaded
          val home = System.getProperty("user.home")
          val filename = s"${home}/Desktop/UNI/Sem5/OOP/project/src/main/root" + path
          if ( Files.exists(Paths.get(filename)) )
          {
            path += "/index.html"
            sendhtml(path,client,200)
          }
          else
          { sendhtml("/error/404.html",client,404)}
        }
      }
    }
    else
    { println("! ERROR 405 !\n") ; sendhtml("/error/405.html",client,405) } // error 405

  }

  def sendhtml( path : String , client : Socket , res : Int ) : Unit =
  {
    // buffer writer for the output stream - to send HTTP RESPONSES to the client
    val send = new PrintWriter( new BufferedWriter( new OutputStreamWriter( client.getOutputStream ) ) )

    val home = System.getProperty("user.home")
    val filename = s"${home}/Desktop/UNI/Sem5/OOP/project/src/main/root" + path
    // variable used to store file content
    var content = ""
    try {
      // reads data from the specified file and stores it into a string then closes the stream
      val input = Source.fromFile(filename)
      content = input.getLines().mkString
      val close = try input.mkString finally input.close()
      // HTTP RESPONSE "200 OK" - accepted request
      // Content type & length - html
      // empty line then the content of the html file
      var reply = ""
      res match
      {
        case 200 => reply = http200
        case 403 => reply = http403
        case 404 => reply = http404
        case 500 => reply = http500
      }
      reply +=  htmltext + docsize + content.size + "\r\n\r\n" + content
      println(reply + "\n")
      send.println(reply)
      send.flush()
    } catch {
      case e: FileNotFoundException => println("! ERROR 404 !\n") ; sendhtml("/error/404.html",client,404) // error 404
      case e: IOException => println("! ERROR 500 !\n") ; sendhtml("/error/505.html",client,500) //error 500
    }


  }

  def sendimg( path : String , client : Socket , ext: String ) : Unit =
  {
    // buffer writer for the output stream - to send HTTP RESPONSES to the client - TEXT
    val send = new PrintWriter( new BufferedWriter( new OutputStreamWriter( client.getOutputStream ) ) )
    // buffer writer for the output stream - to send byte array ( image )
    val os = client.getOutputStream

    val home = System.getProperty("user.home")
    val filename = s"${home}/Desktop/UNI/Sem5/OOP/project/src/main/root" + path
    try
    {
      // reads image data from the specified file through a buffer stream into a byte array
      val myFile = new File(filename)
      val bytes = new Array[Byte](myFile.length.asInstanceOf[Int])
      val filestream = new FileInputStream(myFile)
      val bufferstream = new BufferedInputStream(filestream)
      bufferstream.read(bytes, 0, bytes.length)
      filestream.close()
      bufferstream.close()

      var reply = http200
      ext match
      {
        case "png" => reply += pngimg
        case "jpeg" => reply += jpegimg
        case "jpg" => reply += jpegimg
        case "gif" => reply += gifimg
        case "svg" => reply += svgimg
      }
      reply += docsize + bytes.length + "\r\n"
      print(reply + bytes + "\n\n")
      send.println(reply)
      send.flush()
      os.write(bytes, 0, bytes.length)
      os.flush()
    } catch {
      case e: FileNotFoundException => println("! ERROR 404 !\n") ; sendhtml("/error/404.html",client,404) // error 404
      case e: IOException => println("! ERROR 500 !\n") ; sendhtml("/error/505.html",client,500) //error 500
    }

  }

  def stop(): Unit =
  {
    println("YA DUN KNOW")
    socket.close()
  }
}

object server extends App
{
  // creates a socket for TCP/IP on port 8080
  val socket = new ServerSocket(8080 )
  val server = new http( socket )
  // CATCHES SIGNAL CTRL-C and stops server from running
  Runtime.getRuntime.addShutdownHook( new Thread( () => server.stop() ))
  server.start()
}

