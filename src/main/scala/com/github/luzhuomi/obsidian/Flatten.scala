package com.github.luzhuomi.obsidian

import com.github.luzhuomi.scalangj.Syntax._
import com.github.luzhuomi.scalangj.Syntax


/**
  * We need a flattening step before the desugaring step to handle nested post-/pre-increment and assignment
  * consider the following 

public class Test {
    public static void main(String [] args) {
	int x = 0;
	// System.out.println(x++ + x++);
	// is flattened to
	/*
	int x1, x2;
	x1 = x++;
	x2 = x++;
	System.out.println(x1 + x2);
	*/
	// is desugared to
	/*
	int x1, x2;
	{
	    x1 = x;
	    x = x + 1;
	}
	{ 
	    x2 = x;
	    x = x + 1;
	}
	System.out.println(x1 + x2);
	*/


	// System.out.println(++x + ++x);
	// is flattened to
	
	// int x1, x2;
	// x1 = ++x;
	// x2 = ++x;
	// System.out.println(x1 + x2);
	
	
	// is desugared to
	
	int x1, x2;
	{
	    x = x + 1;
	    x1 = x;
	}
	{ 
	    x = x + 1;
	    x2 = x;
	}
	System.out.println(x1 + x2);
	

    }


}


	Another example, what about 

	System.out.println(x = (y = (z++) + 1))

	identify the top level expression to be flattened, which is x = (y = (z++) + 1), because it is not an statement itself.

	we go left-most inner most (evaluate strategy)

	x = (y = (z++) +1 )  ==>_(z++)

	z1 = z++;
	x = (y = z1+1) 

	we are done with the flattening

	
	z1 = z++;
	x = (y = z1+1) 

	is desugared to 

	{
		z1 = z;
		z = z + 1;
	}
	{
		y = z1 + 1;
		x = y;
	}

	we are done with the deguaring


    After flattening, all post-increment should appearing in the top level of the RHS of 
    some assignment statement only

  */

  object Flatten {
	  trait NestedAssignment[A] {
		  /**
			* nestedAssign: return the left most inner most nested assignment if it exists
			*
			* @param a
			* @return an optional pair consists of the nested assignment and the context 
			*   a context is A with an expression place hodler
			*/
		  def nestedAssignment(a:A):Option[(Exp, Exp => A)]
	  }

	  object naOps {
		  def nestedAssignment[A](a:A)(implicit d:NestedAssignment[A]):Option[(Exp, Exp => A)] = {
			  d.nestedAssignment(a)
		  }
	  }

	  implicit def ExpNestedAssignmentInstance:NestedAssignment[Exp] = new NestedAssignment[Exp] {
			override def nestedAssignment(e:Exp):Option[(Exp, Exp => Exp)] = e match {
				case Lit(_) => None
				case ClassLit(_) => None 
				case This => None
				case ThisClass(_) => None
				case InstanceCreation(type_args, type_decl, args, body) => None
				case QualInstanceCreation(exp, type_args, id, args, body) => None
				case ArrayCreate(ty, exps, num_dims) => None
				case ArrayCreateInit(ty, size, init) => naOps.nestedAssignment(init) match {
					case None => None
					case Some((exp, initCtxt)) => Some((exp, e1 => ArrayCreateInit(ty,size, initCtxt(e1))))
				}
				case FieldAccess_(access) => None // the exp in field access should not contain assignment
				case MethodInv(methdInv) => naOps.nestedAssignment(methdInv) match {
					case None => None 
					case Some((exp, methdInvCtxt)) => Some((exp, e1=> MethodInv(methdInvCtxt(e1))))
				}
				case ArrayAccess(idx) => naOps.nestedAssignment(idx) match {
					case None => None
					case Some((exp, idxCtxt)) => Some((exp, e1=>ArrayAccess(idxCtxt(e1))))
				}
				case ExpName(name) => None
				case PostDecrement(exp) => naOps.nestedAssignment(exp) match {
					case None => None
					case Some((exp, expCtxt)) => Some((exp, e1=>PostDecrement(e1)))
				}

			}

	  }

	  implicit def ArrayInitNestedAssignmentInstance:NestedAssignment[ArrayInit] = new NestedAssignment[ArrayInit] {
		  override def nestedAssignment(a: Syntax.ArrayInit): Option[(Syntax.Exp, Syntax.Exp => Syntax.ArrayInit)] = None //TODO: FixMe
	  }

	  implicit def MethodInvocationNestedAssignmentInstance:NestedAssignment[MethodInvocation] = new NestedAssignment[MethodInvocation] {
		  override def nestedAssignment(a: Syntax.MethodInvocation): Option[(Syntax.Exp, Syntax.Exp => Syntax.MethodInvocation)] = None // TODO: FixMe
	  }
	  
	  implicit def ArrayIndexNestedAssignmentInstance:NestedAssignment[ArrayIndex] = new NestedAssignment[ArrayIndex] {
		  override def nestedAssignment(a: Syntax.ArrayIndex): Option[(Syntax.Exp, Syntax.Exp => Syntax.ArrayIndex)] = None // TODO: FixMe
	  }
  }