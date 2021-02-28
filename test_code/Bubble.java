public class Bubble {
    public static void main(String [] argv) {
		int [] nums = {13, 3, 4, 14, 67, 45, 34};
		bubble(nums);
		for (int i = 0; i < nums.length; i ++) {
			System.out.println(nums[i]);
		}
    }


    public static void bubble(int [] nums) {
		int t; // 1
		boolean changed = false;
		int i = nums.length - 1;
		int j;
		while(i > 0) { // 2 
			changed = false; // 3
			j = 0;
			while(j < i) { // 4
				if (nums[j] > nums[j+1]) { // 5
					t = nums[j+1];         // 6
					nums[j+1] = nums[j];
					nums[j] = t;
					changed = true;
				} else {
					// 7
				}
				j = j + 1; // 8
			}
			// System.out.println(i);
			// optimization
			if (!changed) { // 9
				break; // 10
			} else {
			// 11
			}
			i = i - 1; // 12
		}
    }

}
