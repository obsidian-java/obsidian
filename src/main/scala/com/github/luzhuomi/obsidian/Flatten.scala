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
    After flattening, all post-increment should appearing in the top level of the RHS of 
    some assignment statement only

  */

  object Flatten {
	  
  }