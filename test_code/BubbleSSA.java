
public class BubbleSSA {
    public static void main(String [] argv) {
		int [] nums = {13, 3, 4, 14, 67, 45, 34};
		bubble(nums);
		for (int i = 0; i < nums.length; i ++) {
			System.out.println(nums[i]);
		}
    }

	public static void bubble(int [] nums) {

		int t_0; // 0
		boolean changed_0 = false;
		int i_0 = nums.length - 1;
		int j_0;
		int i_1, i_1p, i_2 ,i_3, i_3p, i_4, i_8, i_11;
		int j_1, j_1p, j_2, j_3, j_3p, j_4, j_7, j_8;
		int t_1, t_1p, t_3, t_3p, t_5, t_4, t_8;
		boolean changed_1, changed_1p, changed_3, changed_3p, changed_5, changed_4, changed_8;
		// need to initialized, otherwise java compiler complains that these variables 
		// might not be initialized
		j_0 = 0;
		t_0 = 0;
		changed_3p = false;
		i_3p = 0;
		j_3p = 0;
		t_3p = 0;

		/* join { 
			i_1 = 0:i_0, 11: i_11
			j_1 = 0:j_0, 11: j_8
			t_1 = 0:t_0; 11: t_8
			changed_1 = 0:changed_0, 11: changed_8
		   }
		*/
		// phi resolution
		i_1 = i_0;
		j_1 = j_0;
		t_1 = t_0;
		changed_1 = changed_0;
		while(i_1 > 0) { // 1 
			changed_1 = false; // 2
			j_2 = 0; 
			/*
			join {
				i_3 = 2: i_1, 7: i_4
				j_3 = 2: j_2, 7: j_7
				t_3 = 2: t_1, 7: t_4
				changed_3 = 2: changed_1, 7: changed_4 
			}
			*/
			// phi resolution
			i_3 = i_1;
			j_3 = j_2;
			t_3 = t_1;
			changed_3 = changed_1;
			while(j_3 < i_3) { // 3
				if (nums[j_3] > nums[j_3+1]) { // 4
					t_5 = nums[j_3+1];         // 5
					nums[j_3+1] = nums[j_3];
					nums[j_3] = t_5;
					changed_5 = true;
					// phi resolution
					j_4 = j_3;
					changed_4 = changed_5;
					i_4 = i_3;
					t_4 = t_5;
				} else {
					// 6
					// phi resolution
					j_4 = j_3;
					changed_4 = changed_3;
					i_4 = i_3;
					t_4 = t_3;
				}
				/*
				join {
					j_4 = 5:j_3, 6:j_3
					changed_4 = 5:changed_5, 6:changed_3
					i_4 = 5:i_3, 6:i_3
					t_4 = 5:t_5, 6:t_3
				}
				*/
				j_7 = j_4 + 1; // 7 // stopped here
				// phi resolution for start of while
				i_3 = i_4;
				j_3 = j_7;
				t_3 = t_4;
				changed_3 = changed_4;				
				// phi resolution for end of while 
				// only applicable when the loop condition is not satisfied
				if (!(j_3 < i_3)) {
					i_3p = i_4;
					j_3p = j_7;
					t_3p = t_4;
					changed_3p = changed_4;
				}
			}
			/*
			join {
				i_3p = 3:i_4,
				j_3p = 3:j_7
				t_3p = 3:t_4
				changed_3p = 7:changed_4;
			}
			*/
			// System.out.println(i_3p);
			// optimization
			if (!changed_3p) { // 8
				i_1p = i_3p;
				j_1p = j_3p;
				changed_1p = changed_3p;
				t_1p = t_3p;
				break; // 9
			} else {
				// 10
				// phi resolution
				changed_8 = changed_3p;
				i_8 = i_3p;
				j_8 = j_3p;
				t_8 = t_3p;
			} 
			/*
			join {
				changed_8 = 10: changed_3p
				i_8 = 10: i_3p
				j_8 = 10: j_3p
				t_8 = 10: t_3p
			} 
			*/
			i_11 = i_8 - 1; // 11


			// phi resolution for start of while join
			i_1 = i_11;
			j_1 = j_8;
			changed_1 = changed_8;
			t_1 = t_8;
			// phi resolution for end of while join
			// this should only be fired when the loop condition is false
			if (!(i_1 > 0)) {
				i_1p = i_11;
				j_1p = j_8;
				changed_1p = changed_8;
				t_1p = t_8;
			}

		} // end of while
		/*
		join {
			changed_1p = 9: changed_3p, 11: changed_8,
			i_1p = 9: i_3p, 1: i_11,
			j_1p = 9: j_3p, 1: j_8,
			t_1p = 9: t_3p, 1: t_8
		}
		*/
		// phi resolution

		// 12
		return;
    }

}
