public class LabeledBreak {
    public static void main(String [] argv) {
	f();
    }
    public static void f() {
	compare: 
	for (int rowNum = 0; rowNum < 3; rowNum++) {
	    for (int colNum = 0; colNum < 4; colNum++) {
		System.out.println("rowNum:" + rowNum + " colNum" + colNum);
		if (rowNum == 1 && colNum == 3) {
		    break compare;
		}
	    }
	}
    }
}
