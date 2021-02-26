public class Bubble {
    public static void main(String [] argv) {
	int [] nums = {13, 3, 4, 14, 67, 45, 34};
	bubble(nums);
	for (int i = 0; i < nums.length; i ++) {
	    System.out.println(nums[i]);
	}
    }

    public static void bubble(int [] nums) {
	int t;
	boolean changed = false;
	for (int i = nums.length - 1; i > 0 ; i--) {
	    changed = false;
	    for (int j = 0; j < i; j++) {
		if (nums[j] > nums[j+1]) {
		    t = nums[j+1];
		    nums[j+1] = nums[j];
		    nums[j] = t;
		    changed = true;
		}
	    }
	    // System.out.println(i);
	    // optimization
	    if (!changed) { break; }
	}
    }
}
