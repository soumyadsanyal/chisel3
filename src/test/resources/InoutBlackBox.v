
module InoutReaderBlackBox(
  inout [31:0] bus,
  output [31:0] out
);
  assign bus = 32'dz;
  assign out = bus;
endmodule

module InoutWriterBlackBox(
  inout [31:0] bus,
  input [31:0] in
);
  assign bus = in;
endmodule

