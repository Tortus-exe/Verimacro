module t(input logic clk, input logic [15:0] w_data, input logic [2:0] w_addr, input logic w_en, input logic [2:0] r_addr, output logic [15:0] r_data);
	logic [15:0] m[0:7];
	assign r_data = m[r_addr];
	always_ff @(posedge clk) begin
		if(w_en) begin
			m[w_addr] <= w_data;
		end
	end
	always_comb begin
		case(a)
			2'b11: a=0;
			2'b10: a=2;
			2'b00: a=1;
			2'b01: a=3;
			default: a=2;
		endcase
 end
endmodule
