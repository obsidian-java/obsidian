package com.github.luzhuomi.obsidian

/**
  * We need a labelling step before the desugaring step (or even before the flattening step?) to loop / switch with continue and break statements
  * 
  * 1. all loop and switch statements must be explicitly label.
  * 2. all break statments must have a target label
  * 3. all continue statement must have a target label
  * 
  * consider the following 
  * 
  * switch (e) {
  *    case x:
  *      stmt1;
  *      break;
  *    ...
  * }
  * becomes
  * 
  * L1: switch(e) {
  *    case x:
  *     stmt1;
  *     break L1;
  * }
  * 
  * 
  * while (e) {
  *    stmt1;
  *    continue;
  * }
  * 
  * becomes
  * 
  * L2: while (e) {
  *    stmt1;
  *    continue L2;
  * }
  * 
  * 
  * A simple state monad should serve the purpose
  * 
  * We use the judgetment l |- stmt => stmt, 
  * _ denotes a none label
  * 
  * 
  * -----------------------(BreakNone)
  * l |- break => break l
  * 
  *
  * -------------------------(BreakSome)
  * l |- break l' => break l' 
  * 
  * -----------------------(ContinueNone)
  * l |- continue => continue l
  * 
  * 
  * -----------------------(ContinueSome)
  * l |- continue l' => continue l'
  * 
  * 
  * l' is fresh
  * l' |- stmt1 => stmt1' ... l' |- stmtn => stmtn' 
  * ----------------------------------------------------------------------------------------------------------------------(Switch)
  * l |- switch e ( case c1: stmt1; ; case c2: stmt2; ... )  => l' : switch e ( case c1: stmt1'; ; case c2: stmt2'; ... ) 
  * 
  * 
  * 
  * l' is fresh
  * l' |- stmt => stmt'
  * ------------------------------------------------(While)
  * l |- while (e) { stmt } => while (e) { stmt' }
  * 
  * 
  * 
  * --------------------(Assignment)
  * l |- x = e => x = e
  * 
  * 
  * l |- stmt1 => stmt1'  
  * l |- stmt2 => stmt2'
  * ------------------------------------------(Seq)
  * l |- stmt1; stmt2 => stmt1'; stmt2';
  * 
  *  l' |- stmt => stmt' 
  * ----------------------------------------(Label)
  * l |- l' : stmt => l' : stmt'
  * 
  * l |- stmt1 => stmt1'
  * l |- stmt2 => stmt2'
  * l |- stmt3 => stmt3'
  * --------------------------------------------------------------------------------------------------------------------(Try)
  * l |- try { stmt1 } catch (T e) { stmt2 } finally {stmt3} => try { stmt1' } catch (T e) { stmt2' } finally {stmt3'}
  * 
  * 
  * hm... we need to keep track of two labels? because we might have a case
  * 
  * while (e) {
  *   switch (e2) {
  *       case c1:
  *        continue
  *   }
  * }
  * 
  * where we expect
  * 
  * 
  * l1: while  (e) {
  *    l2: switch (e2) {
  *         case c1:
  *           continue l1
  * }
  * 
  * */

object Label {

}