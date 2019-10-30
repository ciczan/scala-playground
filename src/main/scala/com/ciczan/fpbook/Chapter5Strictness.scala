package com.ciczan.fpbook

object Chapter5Strictness {

  def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
    if(cond) onTrue()
    else onFalse()

  def thisIsLazy[A](thunk: () => A, eval: Boolean, orElse: A): A = if (eval) thunk() else orElse

  def thisIsLazier[A](thunk: => A, eval: Boolean, orElse: A): A = if (eval) thunk else orElse

  def main(args: Array[String]) = {

    val lazy1 = thisIsLazy(() => {
      println("Eval"); "Foi"
    }, true, "Não foi")

    val lazy2 = thisIsLazier({println("Eval2"); "Foi2"} , true, "Não foi2")


    println(s"Lazies: $lazy1 and $lazy2")
  }

}
