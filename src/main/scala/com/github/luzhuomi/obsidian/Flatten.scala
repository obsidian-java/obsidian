package com.github.luzhuomi.obsidian

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
  }