package com.github.luzhuomi.obsidian

import scala.collection.Map._


/**
  * an AST Path is a sequence of integers that represent the hierachical position of a statement
  * in a AST.
  * 
  * Example
  * int get (int x) {
  *   int i = lpos;  // 1
  *   int r = -1;    // 2
  *   try            // 3
  *   {              // 3,1  * a block stmt
  *     if (x < i)   // 3,1,1
  *     {            // 3,1,1,1  * a block stmt
  *        throw  new Exception(); // 3,1,1,1,1
  *     } 
  *     else 
  *     {            // 3,1,1,2   * a block stmt
  *        while (i < x)   // 3,1,1,2,1 
  *        {               // 3,1,2,2,1,1
  *           int t = f1 + f2;   // 3,1,2,2,1,1,1
  *           f1 = f2;           // 3,1,2,2,1,1,2
  *           f2 = t;            // 3,1,2,2,1,2,3
  *           i = i + 1;         // 3,1,2,2,1,2,4 
  *        }
  *        lpos = i;       // 3,1,1,2,2
  *        r = f2;         // 3,1,1,2,3
  *     }
  *   } 
  *   catch (Exception e)
  *   {              // 3,2  * a block stmt
  *      println("..."); // 3,2,1
  *   }
  *   return r;      // 4 
  * }
  */

object ASTPath {
    type ASTPath = List[Int]
}
