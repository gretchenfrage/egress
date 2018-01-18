package com.phoenixkahlo.hellcraft.singleplayer

import com.phoenixkahlo.hellcraft.service.procedures.PhysicsServiceProcedure
import com.phoenixkahlo.hellcraft.service.{ServiceProcedure, ServiceTagTable}
import com.phoenixkahlo.hellcraft.util.threading.{Promise, UniExecutor}

class Services {
  val table = new ServiceTagTable[ServiceProcedure]

  def start(): Unit = {
    table += new PhysicsServiceProcedure

    for (procedure <- table.toSeq)
      procedure.begin()
  }

  def close(): Promise = Promise({
    for (service <- table.toSeq)
      service.close()
  }, UniExecutor.execc)
}
