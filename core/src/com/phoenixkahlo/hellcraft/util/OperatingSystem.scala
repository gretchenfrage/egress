package com.phoenixkahlo.hellcraft.util

import java.io.{BufferedReader, InputStreamReader}

sealed trait OperatingSystem
case object Windows extends OperatingSystem
case object Mac extends OperatingSystem
case object Unix extends OperatingSystem
case object Solaris extends OperatingSystem

object Env {
  lazy val os: OperatingSystem = {
    val os = System.getProperty("os.name").toLowerCase
    if (os.indexOf("win") >= 0) Windows
    else if (os.indexOf("mac") >= 0) Mac
    else if (os.indexOf("nix") >= 0 || os.indexOf("nux") >= 0 || os.indexOf("aix") > 0) Unix
    else if (os.indexOf("sunos") >= 0) Solaris
    else ???
  }

  lazy val physicalProcessors: Int = {
    val command: String = os match {
      case Mac => "sysctl -n machdep.cpu.core_count"
      case Unix => "lscpu"
      case Windows => "cmd /C WMIC CPU Get /Format:List"
      case _ => ???
    }
    val process: Process =
      if (os == Mac) {
        val cmd = Array[String]("/bin/sh", "-c", command)
        Runtime.getRuntime.exec(cmd)
      } else Runtime.getRuntime.exec(command)
    val reader = new BufferedReader(new InputStreamReader(process.getInputStream))
    var line: String = null
    var cores: Int = -1
    var sockets: Int = -1
    while ({line = reader.readLine(); line != null}) os match {
      case Mac =>
        cores = Integer.parseInt(line)
      case Unix =>
        if (line.contains("Core(s) per socket:"))
          cores = Integer.parseInt(line.split("\\s+")(line.split("\\s+").length - 1))
        if (line.contains("Socket(s):"))
          sockets = Integer.parseInt(line.split("\\s+")(line.split("\\s+").length - 1))
      case Windows =>
        if (line.contains("NumberOfCores"))
          cores = Integer.parseInt(line.split("=")(1))
      case _ => ???
    }
    if (os == Unix) cores * sockets
    else cores
  }
}

object PhysProc extends App {
  println(Env.physicalProcessors)
}