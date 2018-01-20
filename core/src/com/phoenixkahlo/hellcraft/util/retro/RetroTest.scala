package com.phoenixkahlo.hellcraft.util.retro

import java.util.concurrent.Executors

import scala.collection.SortedMap

object RetroTest extends App {
  val exec: Runnable => Unit = Executors.newFixedThreadPool(4).execute

  //Retro("hello world", exec).map(_.toUpperCase, exec).observe((str, v) => println(str), v => ())


  val input = new Array[SetRetro[Int]](100)
  for (i <- input.indices)
    input(i) = new SetRetro[Int]

  val sum = input.fold(Retro(0, _.run()))((r1, r2) => r1.flatMap(i1 => r2.map(i2 => i1 + i2, exec)))

  //sum.observe((n, v) => println("observing: " + n), v => ())

  val waiter = new Thread(() => {
    println(sum.hardAwait)
  })
  waiter.start()

  for (set <- input)
    set.set(1)

  waiter.join()

  for (set <- input)
    set.set(2)

  println(sum.hardAwait)

}
