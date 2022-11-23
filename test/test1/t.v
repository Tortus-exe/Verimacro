mod t (i l clk, i l [15] w_data, i l [2] w_addr, i l w_en, i l [2] r_addr, o l [15] r_data) {
	l [15] m[0:7];
	r_data <-> m[r_addr];
	ff (posedge clk) {
		if (w_en) { 
			m[w_addr] <= w_data;
		}
	}
}
