/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package l2


object Main {
  def doFile(filename:String, verbose:Int=1, sigma:L2Interpreter.Memory)= {
    var sigma_ret:L2Interpreter.Memory = sigma
    try {
      sigma_ret = L2Interpreter.doExprs(L2Parser.applyFile(filename), verbose, sigma)
    }catch {
      case ParserError(s) => println(s)
    }
    println()
    sigma_ret
  }

  def doExecution(code:String, verbose:Int=1, sigma:L2Interpreter.Memory):L2Interpreter.Memory = {
    var sigma_ret:L2Interpreter.Memory = sigma
    try {
      sigma_ret = L2Interpreter.doExprs(L2Parser.applyM(code), verbose, sigma)
    }catch {
      case ParserError(s) => println(s)
    }
    println()
    sigma_ret
  }

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    var verbose = 0;
    var interactive = false;
    var file = "";
    for (a <- args) a match {
      case "-h" | "--help"    =>
        println("Usage: scala Main [--help|-h][--verbose|-v][--interactive|-i][filename]")
      case "-v" | "--verbose" =>
        verbose = 1
      case "-i" | "--interactive" =>
        interactive = true
      case x =>
        file = x
        interactive = false
    }
    if (file.length>0)
      doFile(file, verbose, Map[Address, scala.Int]())
    else{
      interactive = true
      println("L2++ Interpreter")
      println("\tType .exit to finish")
    }
    if (interactive){
      var sigma = Map[Address, scala.Int]()
      while (true) {
        var str = readLine(">>")
        if (str==".exit")
          return
        while (!str.contains(";;"))
            str+=readLine("  ")
        sigma = doExecution(str, verbose, sigma)
      }
    }
  }



}


