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
  * 
  *  Wait! we need to keep track of two labels? because we might have a case
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
  * 
  * We use the judgetment l_c, l_b |- stmt => stmt, 
  * l_c is a continuable label, l_b is a breakable label,   when we don't care a label in the context, we use _
  * 
  * 
  * -----------------------(BreakNone)
  * l_c, l_b |- break => break l_b
  * 
  *
  * -------------------------(BreakLabel)
  * l_c, l_b |- break l' => break l' 
  * 
  * -----------------------(ContinueNone)
  * l_c, l_b |- continue => continue l_c
  * 
  * 
  * -----------------------(ContinueLabel)
  * l_c, l_b |- continue l' => continue l'
  * 
  * 
  * l_b' is fresh
  * l_c, l_b' |- stmt1 => stmt1' ... l' |- stmtn => stmtn' 
  * ----------------------------------------------------------------------------------------------------------------------(SwitchNone)
  * l_c, _ |- switch e ( case c1: stmt1; ; case c2: stmt2; ... )  => l_b' : switch e ( case c1: stmt1'; ; case c2: stmt2'; ... ) 
  * 
  * l_c, l_b' |- stmt1 => stmt1' ... l' |- stmtn => stmtn' 
  * ----------------------------------------------------------------------------------------------------------------------(SwitchLabel)
  * l_c, _ |- l_b' : switch e ( case c1: stmt1; ; case c2: stmt2; ... )  => l_b' : switch e ( case c1: stmt1'; ; case c2: stmt2'; ... ) 
  * 
  * 
  * l' is fresh
  * l', l' |- stmt => stmt'
  * ----------------------------------------------------------------------------------(WhileNone)
  * _, _ |- while (e) { stmt } => while (e) { stmt' } => l' : while (e) { stmt' } 
  * 
  * 
  * l', l' |- stmt => stmt'
  * ----------------------------------------------------------------------------------(WhileLabel)
  * _, _ |- l' : while (e) { stmt } => while (e) { stmt' } => l' : while (e) { stmt' } 
  * 
  * --------------------(Assignment)
  * l |- x = e => x = e
  * 
  * 
  * l_c, l_b |- stmt1 => stmt1'  
  * l_c, l_b |- stmt2 => stmt2'
  * ------------------------------------------(Seq)
  * l_c, l_b |- stmt1; stmt2 => stmt1'; stmt2';
  * 
  * l_c, l_b |- stmt => stmt' 
  * ----------------------------------------(Label)  do we need this case?
  * l_c, l_b |- l' : stmt => l' : stmt'
  * 
  * l_c, l_b |- stmt1 => stmt1'
  * l_c, l_b |- stmt2 => stmt2'
  * l_c, l_b |- stmt3 => stmt3'
  * --------------------------------------------------------------------------------------------------------------------(Try)
  * l_c, l_b |- try { stmt1 } catch (T e) { stmt2 } finally {stmt3} => try { stmt1' } catch (T e) { stmt2' } finally {stmt3'}
  * 
  * 
  * 
  * */

object Label {

}