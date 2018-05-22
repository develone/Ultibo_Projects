////////////////////////////////////////////////////////////////////////////////
//
// Filename: 	speechfifo.v
//
// Project:	wbuart32, a full featured UART with simulator
//
// Purpose:	To test/demonstrate/prove the wishbone access to the FIFO'd
//		UART via sending more information than the FIFO can hold,
//	and then verifying that this was the value received.
//
//	To do this, we "borrow" a copy of Abraham Lincolns Gettysburg address,
//	make that the FIFO isn't large enough to hold it, and then try
//	to send this address every couple of minutes.
//
//	With some minor modifications (discussed below), this RTL should be
//	able to be run as a top-level testing file, requiring only that the
//	clock and the transmit UART pins be working.
//
// Creator:	Dan Gisselquist, Ph.D.
//		Gisselquist Technology, LLC
//
////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2015-2017, Gisselquist Technology, LLC
//
// This program is free software (firmware): you can redistribute it and/or
// modify it under the terms of  the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License, or (at
// your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTIBILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program.  (It's in the $(ROOT)/doc directory, run make with no
// target there if the PDF file isn't present.)  If not, see
// <http://www.gnu.org/licenses/> for a copy.
//
// License:	GPL, v3, as defined and found on www.gnu.org,
//		http://www.gnu.org/licenses/gpl.html
//
//
////////////////////////////////////////////////////////////////////////////////
//
//
// One issue with the design is how to set the values of the setup register.
// (*This is a comment, not a verilator attribute ... )  Verilator needs to
// know/set those values in order to work.  However, this design can also be
// used as a stand-alone top level configuration file.  In this latter case,
// the setup register needs to be set internal to the file.  Here, we use
// OPT_STANDALONE to distinguish between the two.  If set, the file runs under
// (* Another comment still ...) Verilator and we need to get i_setup from the
// external environment.  If not, it must be set internally.
//
`ifndef	VERILATOR
`define OPT_STANDALONE
`endif
//
module	speechfifo(i_clk,
`ifndef	OPT_STANDALONE
			i_setup,
`endif
			o_uart_tx);
	input		i_clk;
	output	wire	o_uart_tx;

	// Here we set i_setup to something appropriate to create a 115200 Baud
	// UART system from a 100MHz clock.  This also sets us to an 8-bit data
	// word, 1-stop bit, and no parity.  This will be overwritten by
	// i_setup, but at least it gives us something to start with/from.
	parameter	INITIAL_UART_SETUP = 31'd868;

	// Let's set our message length, in case we ever wish to change it in
	// the future
	localparam	MSGLEN=2203;

	// The i_setup wires are input when run under Verilator, but need to
	// be set internally if this is going to run as a standalone top level
	// test configuration.
`ifdef	OPT_STANDALONE
	wire	[30:0]	i_setup;
	assign	i_setup = INITIAL_UART_SETUP;
`else
	input	[30:0]	i_setup;
`endif

	reg		restart;
	reg		wb_stb;
	reg	[1:0]	wb_addr;
	reg	[31:0]	wb_data;

	wire		uart_stall;

	// We aren't using the receive interrupts, or the received data, or the
	// ready to send line, so we'll just mark them all here as ignored.

	/* verilator lint_off UNUSED */
	wire		uart_ack, tx_int;
	wire	[31:0]	uart_data;
	wire		ignored_rx_int, ignored_rxfifo_int;
	wire		rts_n_ignored;
	/* verilator lint_on UNUSED */

	/* verilator lint_on UNUSED */

	wire		txfifo_int;

	// The next four lines create a strobe signal that is true on the first
	// clock, but never after.  This makes for a decent power-on reset
	// signal.
	reg	pwr_reset;
	initial	pwr_reset = 1'b1;
	always @(posedge i_clk)
		pwr_reset <= 1'b0;

	// The message we wish to transmit is kept in "message".  It needs to be
	// set initially.  Do so here.
	//
	// Since the message has fewer than 2048 elements in it, we preset every
	// element to a space so that if (for some reason) we broadcast past the
	// end of our message, we'll at least be sending something useful.
	integer	i;
	reg	[7:0]	message [0:4095];
	initial begin
		// xx Verilator needs this file to be in the directory the file
		// is run from.  For that reason, the project builds, makes,
		// and keeps speech.hex in bench/cpp.  
		//
		// Vivado, however, wants speech.hex to be in a project file
		// directory, such as bench/verilog.  For that reason, the
		// build function in bench/cpp also copies speech.hex to the
		// bench/verilog directory.  You may need to make certain the
		// file is both built, and copied into a directory where your
		// synthesis tool can find it.
		//
		$readmemh("speech.hex", message);
		for(i=MSGLEN; i<4095; i=i+1)
			message[i] = 8'h20;

		//
		// The problem with the above approach is Xilinx's ISE program.
		// It's broken.  It can't handle HEX files well (at all?) and
		// has more problems with HEX's defining ROM's.  For that
		// reason, the mkspeech program can be tuned to create an
		// include file, speech.inc.  We include that program here.
		// It is rather ugly, though, and not a very elegant solution,
		// since it walks through every value in our speech, byte by
		// byte, with an initial line for each byte declaring what it
		// is to be.
		//
		// If you (need to) use this route, comment out both the 
		// readmemh, the for loop, and the message[i] = 8'h20 lines
		// above and uncomment the include line below.
		//
		// `include "speech.inc"
	end

	// Let's keep track of time, and send our message over and over again.
	// To do this, we'll keep track of a restart counter.  When this counter
	// rolls over, we restart our message.
	reg	[30:0]	restart_counter;
	// Since we want to start our message just a couple clocks after power
	// up, we'll set the reset counter just a couple clocks shy of a roll
	// over.
	initial	restart_counter = -31'd16;
	always @(posedge i_clk)
		restart_counter <= restart_counter+1'b1;

	// Ok, now that we have a counter that tells us when to start over,
	// let's build a set of signals that we can use to get things started
	// again.  This will be the restart signal.  On this signal, we just
	// restart everything.
	initial	restart = 0;
	always @(posedge i_clk)
		restart <= (restart_counter == 0);

	// Our message index.  This is the address of the character we wish to
	// transmit next.  Note, there's a clock delay between setting this 
	// index and when the wb_data is valid.  Hence, we set the index on
	// restart[0] to zero.
	reg	[11:0]	msg_index;
	initial	msg_index = 12'h000 - 12'h8;
	always @(posedge i_clk)
	begin
		if (restart)
			msg_index <= 0;
		else if ((wb_stb)&&(!uart_stall))
			// We only advance the index if a port operation on the
			// wbuart has taken place.  That's what the
			// (wb_stb)&&(!uart_stall) is about.  (wb_stb) is the
			// request for a transaction on the bus, uart_stall
			// tells us to wait 'cause the peripheral isn't ready. 
			// In our case, it's always ready, uart_stall == 0, but
			// we keep/maintain this logic for good form.
			//
			// Note also, we only advance when restart[0] is zero.
			// This keeps us from advancing prior to the setup
			// word.
			msg_index <= msg_index + 1'b1;
	end

	// What data will we be sending to the port?
	always @(posedge i_clk)
		if (restart)
			// The first thing we do is set the baud rate, and
			// serial port configuration parameters.  Ideally,
			// we'd only set this once.  But rather than complicate
			// the logic, we set it everytime we start over.
			wb_data <= { 1'b0, i_setup };
		else if ((wb_stb)&&(!uart_stall))
			// Then, if the last thing was received over the bus,
			// we move to the next data item.
			wb_data <= { 24'h00, message[msg_index] };

	// We send our first value to the SETUP address (all zeros), all other
	// values we send to the transmitters address.  We should really be
	// double checking that stall remains low, but its not required here.
	always @(posedge i_clk)
		if (restart)
			wb_addr <= 2'b00;
		else // if (!uart_stall)??
			wb_addr <= 2'b11;

	// Knowing when to stop sending the speech is important, but depends
	// upon an 11 bit comparison.  Since FPGA logic is best measured by the
	// number of inputs to an always block, we pull those 11-bits out of
	// the always block for wb_stb, and place them here on the clock prior.
	// If end_of_message is true, then we need to stop transmitting, and
	// wait for the next (restart) to get us started again.  We set that
	// flag hee.
	reg	end_of_message;
	initial	end_of_message = 1'b1;
	always @(posedge i_clk)
		if (restart)
			end_of_message <= 1'b0;
		else
			end_of_message <= (msg_index >= MSGLEN);

	// The wb_stb signal indicates that we wish to write, using the wishbone
	// to our peripheral.  We have two separate types of writes.  First,
	// we wish to write our setup.  Then we want to drop STB and write
	// our data.  Once we've filled half of the FIFO, we wait for the FIFO
	// to empty before issuing a STB again and then fill up half the FIFO
	// again.
	initial	wb_stb = 1'b0;
	always @(posedge i_clk)
		if (restart)
			// Start sending to the UART on a reset.  The first
			// thing we'll send will be the configuration, but
			// that's done elsewhere.  This just starts up the
			// writes to the peripheral wbuart.
			wb_stb <= 1'b1;
		else if (end_of_message)
			// Stop transmitting when we get to the end of our
			// message.
			wb_stb <= 1'b0;
		else if (txfifo_int)
			// If the FIFO is less than half full, then write to
			// it.
			wb_stb <= 1'b1;
		else
			// But once the FIFO gets to half full, stop.
			wb_stb <= 1'b0;

	// The WBUART can handle hardware flow control signals.  This test,
	// however, cannot.  The reason?  Simply just to keep things simple.
	// If you want to add hardware flow control to your design, simply
	// make rts an input to this module.
	//
	// Since this is an output only module demonstrator, what would be the
	// cts output is unused.
	wire	cts_n;
	assign	cts_n = 1'b0;

	// Finally--the unit under test--now that we've set up all the wires
	// to run/test it.
	wbuart	#(INITIAL_UART_SETUP)
		wbuarti(i_clk, pwr_reset,
			wb_stb, wb_stb, 1'b1, wb_addr, wb_data,
			uart_ack, uart_stall, uart_data,
			1'b1, o_uart_tx, cts_n, rts_n_ignored,
			ignored_rx_int, tx_int,
			ignored_rxfifo_int, txfifo_int);

endmodule
////////////////////////////////////////////////////////////////////////////////
//
// Filename: 	wbuart.v
//
// Project:	wbuart32, a full featured UART with simulator
//
// Purpose:	Unlilke wbuart-insert.v, this is a full blown wishbone core
//		with integrated FIFO support to support the UART transmitter
//	and receiver found within here.  As a result, it's usage may be
//	heavier on the bus than the insert, but it may also be more useful.
//
// Creator:	Dan Gisselquist, Ph.D.
//		Gisselquist Technology, LLC
//
////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2015-2016, Gisselquist Technology, LLC
//
// This program is free software (firmware): you can redistribute it and/or
// modify it under the terms of  the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License, or (at
// your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTIBILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program.  (It's in the $(ROOT)/doc directory.  Run make with no
// target there if the PDF file isn't present.)  If not, see
// <http://www.gnu.org/licenses/> for a copy.
//
// License:	GPL, v3, as defined and found on www.gnu.org,
//		http://www.gnu.org/licenses/gpl.html
//
//
////////////////////////////////////////////////////////////////////////////////
//
//
`default_nettype	none
//
`define	UART_SETUP	2'b00
`define	UART_FIFO	2'b01
`define	UART_RXREG	2'b10
`define	UART_TXREG	2'b11
module	wbuart(i_clk, i_rst,
		//
		i_wb_cyc, i_wb_stb, i_wb_we, i_wb_addr, i_wb_data,
			o_wb_ack, o_wb_stall, o_wb_data,
		//
		i_uart_rx, o_uart_tx, i_cts_n, o_rts_n,
		//
		o_uart_rx_int, o_uart_tx_int,
		o_uart_rxfifo_int, o_uart_txfifo_int);
	parameter [30:0] INITIAL_SETUP = 31'd25; // 4MB 8N1, when using 100MHz clock
	parameter [3:0]	LGFLEN = 4;
	parameter [0:0]	HARDWARE_FLOW_CONTROL_PRESENT = 1'b1;
	// Perform a simple/quick bounds check on the log FIFO length, to make
	// sure its within the bounds we can support with our current
	// interface.
	localparam [3:0]	LCLLGFLEN = (LGFLEN > 4'ha)? 4'ha
					: ((LGFLEN < 4'h2) ? 4'h2 : LGFLEN);
	//
	input	wire		i_clk, i_rst;
	// Wishbone inputs
	input	wire		i_wb_cyc;	// We ignore CYC for efficiency
	input	wire		i_wb_stb, i_wb_we;
	input	wire	[1:0]	i_wb_addr;
	input	wire	[31:0]	i_wb_data;	// and only use 30 lines here
	output	reg		o_wb_ack;
	output	wire		o_wb_stall;
	output	reg	[31:0]	o_wb_data;
	//
	input	wire		i_uart_rx;
	output	wire		o_uart_tx;
	// RTS is used for hardware flow control.  According to Wikipedia, it
	// should probably be renamed RTR for "ready to receive".  It tell us
	// whether or not the receiving hardware is ready to accept another
	// byte.  If low, the transmitter will pause.
	//
	// If you don't wish to use hardware flow control, just set i_cts_n to
	// 1'b0 and let the optimizer simply remove this logic.
	input	wire		i_cts_n;
	// CTS is the "Clear-to-send" signal.  We set it anytime our FIFO
	// isn't full.  Feel free to ignore this output if you do not wish to
	// use flow control.
	output	reg		o_rts_n;
	output	wire		o_uart_rx_int, o_uart_tx_int,
				o_uart_rxfifo_int, o_uart_txfifo_int;

	wire	tx_busy;

	//
	// The UART setup parameters: bits per byte, stop bits, parity, and
	// baud rate are all captured within this uart_setup register.
	//
	reg	[30:0]	uart_setup;
	initial	uart_setup = INITIAL_SETUP
		| ((HARDWARE_FLOW_CONTROL_PRESENT==1'b0)? 31'h40000000 : 0);
	always @(posedge i_clk)
		// Under wishbone rules, a write takes place any time i_wb_stb
		// is high.  If that's the case, and if the write was to the
		// setup address, then set us up for the new parameters.
		if ((i_wb_stb)&&(i_wb_addr == `UART_SETUP)&&(i_wb_we))
			uart_setup <= {
				(i_wb_data[30])
					||(!HARDWARE_FLOW_CONTROL_PRESENT),
				i_wb_data[29:0] };

	/////////////////////////////////////////
	//
	//
	// First, the UART receiver
	//
	//
	/////////////////////////////////////////

	// First the wires/registers this receiver depends upon
	wire		rx_stb, rx_break, rx_perr, rx_ferr, ck_uart;
	wire	[7:0]	rx_uart_data;
	reg		rx_uart_reset;

	// Here's our UART receiver.  Basically, it accepts our setup wires, 
	// the UART input, a clock, and a reset line, and produces outputs:
	// a stb (true when new data is ready), and an 8-bit data out value
	// valid when stb is high.
`ifdef	USE_LITE_UART
	rxuartlite	#(INITIAL_SETUP[23:0])
		rx(i_clk, (i_rst), i_uart_rx, rx_stb, rx_uart_data);
	assign	rx_break = 1'b0;
	assign	rx_perr  = 1'b0;
	assign	rx_ferr  = 1'b0;
	assign	ck_uart  = 1'b0;
`else
	// The full receiver also produces a break value (true during a break
	// cond.), and parity/framing error flags--also valid when stb is true.
	rxuart	#(INITIAL_SETUP) rx(i_clk, (i_rst)||(rx_uart_reset),
			uart_setup, i_uart_rx,
			rx_stb, rx_uart_data, rx_break,
			rx_perr, rx_ferr, ck_uart);
	// The real trick is ... now that we have this extra data, what do we do
	// with it?
`endif


	// We place it into a receiver FIFO.
	//
	// Here's the declarations for the wires it needs.
	wire		rx_empty_n, rx_fifo_err;
	wire	[7:0]	rxf_wb_data;
	wire	[15:0]	rxf_status;
	reg		rxf_wb_read;
	//
	// And here's the FIFO proper.
	//
	// Note that the FIFO will be cleared upon any reset: either if there's
	// a UART break condition on the line, the receiver is in reset, or an
	// external reset is issued.
	//
	// The FIFO accepts strobe and data from the receiver.
	// We issue another wire to it (rxf_wb_read), true when we wish to read
	// from the FIFO, and we get our data in rxf_wb_data.  The FIFO outputs
	// four status-type values: 1) is it non-empty, 2) is the FIFO over half
	// full, 3) a 16-bit status register, containing info regarding how full
	// the FIFO truly is, and 4) an error indicator.
	ufifo	#(.LGFLEN(LCLLGFLEN), .RXFIFO(1))
		rxfifo(i_clk, (i_rst)||(rx_break)||(rx_uart_reset),
			rx_stb, rx_uart_data,
			rx_empty_n,
			rxf_wb_read, rxf_wb_data,
			rxf_status, rx_fifo_err);
	assign	o_uart_rxfifo_int = rxf_status[1];

	// We produce four interrupts.  One of the receive interrupts indicates
	// whether or not the receive FIFO is non-empty.  This should wake up
	// the CPU.
	assign	o_uart_rx_int = rxf_status[0];

	// The clear to send line, which may be ignored, but which we set here
	// to be true any time the FIFO has fewer than N-2 items in it.
	// Why N-1?  Because at N-1 we are totally full, but already so full
	// that if the transmit end starts sending we won't have a location to
	// receive it.  (Transmit might've started on the next character by the
	// time we set this--thus we need to set it to one, one character before
	// necessary).
	wire	[(LCLLGFLEN-1):0]	check_cutoff;
	assign	check_cutoff = -3;
	always @(posedge i_clk)
		o_rts_n <= ((HARDWARE_FLOW_CONTROL_PRESENT)
			&&(!uart_setup[30])
			&&(rxf_status[(LCLLGFLEN+1):2] > check_cutoff));

	// If the bus requests that we read from the receive FIFO, we need to
	// tell this to the receive FIFO.  Note that because we are using a 
	// clock here, the output from the receive FIFO will necessarily be
	// delayed by an extra clock.
	initial	rxf_wb_read = 1'b0;
	always @(posedge i_clk)
		rxf_wb_read <= (i_wb_stb)&&(i_wb_addr[1:0]==`UART_RXREG)
				&&(!i_wb_we);

	// Now, let's deal with those RX UART errors: both the parity and frame
	// errors.  As you may recall, these are valid only when rx_stb is
	// valid, so we need to hold on to them until the user reads them via
	// a UART read request..
	reg	r_rx_perr, r_rx_ferr;
	initial	r_rx_perr = 1'b0;
	initial	r_rx_ferr = 1'b0;
	always @(posedge i_clk)
		if ((rx_uart_reset)||(rx_break))
		begin
			// Clear the error
			r_rx_perr <= 1'b0;
			r_rx_ferr <= 1'b0;
		end else if ((i_wb_stb)
				&&(i_wb_addr[1:0]==`UART_RXREG)&&(i_wb_we))
		begin
			// Reset the error lines if a '1' is ever written to
			// them, otherwise leave them alone.
			//
			r_rx_perr <= (r_rx_perr)&&(~i_wb_data[9]);
			r_rx_ferr <= (r_rx_ferr)&&(~i_wb_data[10]);
		end else if (rx_stb)
		begin
			// On an rx_stb, capture any parity or framing error
			// indications.  These aren't kept with the data rcvd,
			// but rather kept external to the FIFO.  As a result,
			// if you get a parity or framing error, you will never
			// know which data byte it was associated with.
			// For now ... that'll work.
			r_rx_perr <= (r_rx_perr)||(rx_perr);
			r_rx_ferr <= (r_rx_ferr)||(rx_ferr);
		end

	initial	rx_uart_reset = 1'b1;
	always @(posedge i_clk)
		if ((i_rst)||((i_wb_stb)&&(i_wb_addr[1:0]==`UART_SETUP)&&(i_wb_we)))
			// The receiver reset, always set on a master reset
			// request.
			rx_uart_reset <= 1'b1;
		else if ((i_wb_stb)&&(i_wb_addr[1:0]==`UART_RXREG)&&(i_wb_we))
			// Writes to the receive register will command a receive
			// reset anytime bit[12] is set.
			rx_uart_reset <= i_wb_data[12];
		else
			rx_uart_reset <= 1'b0;

	// Finally, we'll construct a 32-bit value from these various wires,
	// to be returned over the bus on any read.  These include the data
	// that would be read from the FIFO, an error indicator set upon
	// reading from an empty FIFO, a break indicator, and the frame and
	// parity error signals.
	wire	[31:0]	wb_rx_data;
	assign	wb_rx_data = { 16'h00,
				3'h0, rx_fifo_err,
				rx_break, rx_ferr, r_rx_perr, !rx_empty_n,
				rxf_wb_data};

	/////////////////////////////////////////
	//
	//
	// Then the UART transmitter
	//
	//
	/////////////////////////////////////////
	wire		tx_empty_n, txf_err, tx_break;
	wire	[7:0]	tx_data;
	wire	[15:0]	txf_status;
	reg		txf_wb_write, tx_uart_reset;
	reg	[7:0]	txf_wb_data;

	// Unlike the receiver which goes from RXUART -> UFIFO -> WB, the
	// transmitter basically goes WB -> UFIFO -> TXUART.  Hence, to build
	// support for the transmitter, we start with the command to write data
	// into the FIFO.  In this case, we use the act of writing to the 
	// UART_TXREG address as our indication that we wish to write to the 
	// FIFO.  Here, we create a write command line, and latch the data for
	// the extra clock that it'll take so that the command and data can be
	// both true on the same clock.
	initial	txf_wb_write = 1'b0;
	always @(posedge i_clk)
	begin
		txf_wb_write <= (i_wb_stb)&&(i_wb_addr == `UART_TXREG)
					&&(i_wb_we);
		txf_wb_data  <= i_wb_data[7:0];
	end

	// Transmit FIFO
	//
	// Most of this is just wire management.  The TX FIFO is identical in
	// implementation to the RX FIFO (theyre both UFIFOs), but the TX
	// FIFO is fed from the WB and read by the transmitter.  Some key
	// differences to note: we reset the transmitter on any request for a
	// break.  We read from the FIFO any time the UART transmitter is idle.
	// and ... we just set the values (above) for controlling writing into
	// this.
	ufifo	#(.LGFLEN(LGFLEN), .RXFIFO(0))
		txfifo(i_clk, (tx_break)||(tx_uart_reset),
			txf_wb_write, txf_wb_data,
			tx_empty_n,
			(!tx_busy)&&(tx_empty_n), tx_data,
			txf_status, txf_err);
	// Let's create two transmit based interrupts from the FIFO for the CPU.
	//	The first will be true any time the FIFO has at least one open
	//	position within it.
	assign	o_uart_tx_int = txf_status[0];
	//	The second will be true any time the FIFO is less than half
	//	full, allowing us a change to always keep it (near) fully 
	//	charged.
	assign	o_uart_txfifo_int = txf_status[1];

`ifndef	USE_LITE_UART
	// Break logic
	//
	// A break in a UART controller is any time the UART holds the line
	// low for an extended period of time.  Here, we capture the wb_data[9]
	// wire, on writes, as an indication we wish to break.  As long as you
	// write unsigned characters to the interface, this will never be true
	// unless you wish it to be true.  Be aware, though, writing a valid
	// value to the interface will bring it out of the break condition.
	reg	r_tx_break;
	initial	r_tx_break = 1'b0;
	always @(posedge i_clk)
		if (i_rst)
			r_tx_break <= 1'b0;
		else if ((i_wb_stb)&&(i_wb_addr[1:0]==`UART_TXREG)&&(i_wb_we))
			r_tx_break <= i_wb_data[9];
	assign	tx_break = r_tx_break;
`else
	assign	tx_break = 1'b0;
`endif

	// TX-Reset logic
	//
	// This is nearly identical to the RX reset logic above.  Basically,
	// any time someone writes to bit [12] the transmitter will go through
	// a reset cycle.  Keep bit [12] low, and everything will proceed as
	// normal.
	initial	tx_uart_reset = 1'b1;
	always @(posedge i_clk)
		if((i_rst)||((i_wb_stb)&&(i_wb_addr == `UART_SETUP)&&(i_wb_we)))
			tx_uart_reset <= 1'b1;
		else if ((i_wb_stb)&&(i_wb_addr[1:0]==`UART_TXREG)&&(i_wb_we))
			tx_uart_reset <= i_wb_data[12];
		else
			tx_uart_reset <= 1'b0;

`ifdef	USE_LITE_UART
	txuartlite #(INITIAL_SETUP[23:0]) tx(i_clk, (tx_empty_n), tx_data,
			o_uart_tx, tx_busy);
`else
	wire	cts_n;
	assign	cts_n = (HARDWARE_FLOW_CONTROL_PRESENT)&&(i_cts_n);
	// Finally, the UART transmitter module itself.  Note that we haven't
	// connected the reset wire.  Transmitting is as simple as setting
	// the stb value (here set to tx_empty_n) and the data.  When these
	// are both set on the same clock that tx_busy is low, the transmitter
	// will move on to the next data byte.  Really, the only thing magical
	// here is that tx_empty_n wire--thus, if there's anything in the FIFO,
	// we read it here.  (You might notice above, we register a read any
	// time (tx_empty_n) and (!tx_busy) are both true---the condition for
	// starting to transmit a new byte.)
	txuart	#(INITIAL_SETUP) tx(i_clk, 1'b0, uart_setup,
			r_tx_break, (tx_empty_n), tx_data,
			cts_n, o_uart_tx, tx_busy);
`endif

	// Now that we are done with the chain, pick some wires for the user
	// to read on any read of the transmit port.
	//
	// This port is different from reading from the receive port, since
	// there are no side effects.  (Reading from the receive port advances
	// the receive FIFO, here only writing to the transmit port advances the
	// transmit FIFO--hence the read values are free for ... whatever.)  
	// We choose here to provide information about the transmit FIFO
	// (txf_err, txf_half_full, txf_full_n), information about the current
	// voltage on the line (o_uart_tx)--and even the voltage on the receive
	// line (ck_uart), as well as our current setting of the break and
	// whether or not we are actively transmitting.
	wire	[31:0]	wb_tx_data;
	assign	wb_tx_data = { 16'h00, 
				i_cts_n, txf_status[1:0], txf_err,
				ck_uart, o_uart_tx, tx_break, (tx_busy|txf_status[0]),
				(tx_busy|txf_status[0])?txf_wb_data:8'b00};

	// Each of the FIFO's returns a 16 bit status value.  This value tells
	// us both how big the FIFO is, as well as how much of the FIFO is in 
	// use.  Let's merge those two status words together into a word we
	// can use when reading about the FIFO.
	wire	[31:0]	wb_fifo_data;
	assign	wb_fifo_data = { txf_status, rxf_status };

	// You may recall from above that reads take two clocks.  Hence, we
	// need to delay the address decoding for a clock until the data is 
	// ready.  We do that here.
	reg	[1:0]	r_wb_addr;
	always @(posedge i_clk)
		r_wb_addr <= i_wb_addr;

	// Likewise, the acknowledgement is delayed by one clock.
	reg	r_wb_ack;
	always @(posedge i_clk) // We'll ACK in two clocks
		r_wb_ack <= i_wb_stb;
	always @(posedge i_clk) // Okay, time to set the ACK
		o_wb_ack <= r_wb_ack;

	// Finally, set the return data.  This data must be valid on the same
	// clock o_wb_ack is high.  On all other clocks, it is irrelelant--since
	// no one cares, no one is reading it, it gets lost in the mux in the
	// interconnect, etc.  For this reason, we can just simplify our logic.
	always @(posedge i_clk)
		casez(r_wb_addr)
		`UART_SETUP: o_wb_data <= { 1'b0, uart_setup };
		`UART_FIFO:  o_wb_data <= wb_fifo_data;
		`UART_RXREG: o_wb_data <= wb_rx_data;
		`UART_TXREG: o_wb_data <= wb_tx_data;
		endcase

	// This device never stalls.  Sure, it takes two clocks, but they are
	// pipelined, and nothing stalls that pipeline.  (Creates FIFO errors,
	// perhaps, but doesn't stall the pipeline.)  Hence, we can just
	// set this value to zero.
	assign	o_wb_stall = 1'b0;

	// Make verilator happy
	// verilator lint_off UNUSED
	wire	[33:0] unused;
	assign	unused = { i_rst, i_wb_cyc, i_wb_data };
	// verilator lint_on UNUSED

endmodule
////////////////////////////////////////////////////////////////////////////////
//
// Filename: 	rxuart.v
//
// Project:	wbuart32, a full featured UART with simulator
//
// Purpose:	Receive and decode inputs from a single UART line.
//
//
//	To interface with this module, connect it to your system clock,
//	pass it the 32 bit setup register (defined below) and the UART
//	input.  When data becomes available, the o_wr line will be asserted
//	for one clock cycle.  On parity or frame errors, the o_parity_err
//	or o_frame_err lines will be asserted.  Likewise, on a break 
//	condition, o_break will be asserted.  These lines are self clearing.
//
//	There is a synchronous reset line, logic high.
//
//	Now for the setup register.  The register is 32 bits, so that this
//	UART may be set up over a 32-bit bus.
//
//	i_setup[30]	True if we are not using hardware flow control.  This bit
//		is ignored within this module, as any receive hardware flow
//		control will need to be implemented elsewhere.
//
//	i_setup[29:28]	Indicates the number of data bits per word.  This will
//		either be 2'b00 for an 8-bit word, 2'b01 for a 7-bit word, 2'b10
//		for a six bit word, or 2'b11 for a five bit word.
//
//	i_setup[27]	Indicates whether or not to use one or two stop bits.
//		Set this to one to expect two stop bits, zero for one.
//
//	i_setup[26]	Indicates whether or not a parity bit exists.  Set this
//		to 1'b1 to include parity.
//
//	i_setup[25]	Indicates whether or not the parity bit is fixed.  Set
//		to 1'b1 to include a fixed bit of parity, 1'b0 to allow the
//		parity to be set based upon data.  (Both assume the parity
//		enable value is set.)
//
//	i_setup[24]	This bit is ignored if parity is not used.  Otherwise,
//		in the case of a fixed parity bit, this bit indicates whether
//		mark (1'b1) or space (1'b0) parity is used.  Likewise if the
//		parity is not fixed, a 1'b1 selects even parity, and 1'b0
//		selects odd.
//
//	i_setup[23:0]	Indicates the speed of the UART in terms of clocks.
//		So, for example, if you have a 200 MHz clock and wish to
//		run your UART at 9600 baud, you would take 200 MHz and divide
//		by 9600 to set this value to 24'd20834.  Likewise if you wished
//		to run this serial port at 115200 baud from a 200 MHz clock,
//		you would set the value to 24'd1736
//
//	Thus, to set the UART for the common setting of an 8-bit word, 
//	one stop bit, no parity, and 115200 baud over a 200 MHz clock, you
//	would want to set the setup value to:
//
//	32'h0006c8		// For 115,200 baud, 8 bit, no parity
//	32'h005161		// For 9600 baud, 8 bit, no parity
//	
//
//
// Creator:	Dan Gisselquist, Ph.D.
//		Gisselquist Technology, LLC
//
////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2015-2016, Gisselquist Technology, LLC
//
// This program is free software (firmware): you can redistribute it and/or
// modify it under the terms of  the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License, or (at
// your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTIBILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program.  (It's in the $(ROOT)/doc directory.  Run make with no
// target there if the PDF file isn't present.)  If not, see
// <http://www.gnu.org/licenses/> for a copy.
//
// License:	GPL, v3, as defined and found on www.gnu.org,
//		http://www.gnu.org/licenses/gpl.html
//
//
////////////////////////////////////////////////////////////////////////////////
//
//
`default_nettype	none
//
// States: (@ baud counter == 0)
//	0	First bit arrives
//	..7	Bits arrive
//	8	Stop bit (x1)
//	9	Stop bit (x2)
//	c	break condition
//	d	Waiting for the channel to go high
//	e	Waiting for the reset to complete
//	f	Idle state
`define	RXU_BIT_ZERO		4'h0
`define	RXU_BIT_ONE		4'h1
`define	RXU_BIT_TWO		4'h2
`define	RXU_BIT_THREE		4'h3
`define	RXU_BIT_FOUR		4'h4
`define	RXU_BIT_FIVE		4'h5
`define	RXU_BIT_SIX		4'h6
`define	RXU_BIT_SEVEN		4'h7
`define	RXU_PARITY		4'h8
`define	RXU_STOP		4'h9
`define	RXU_SECOND_STOP		4'ha
// Unused 4'hb
// Unused 4'hc
`define	RXU_BREAK		4'hd
`define	RXU_RESET_IDLE		4'he
`define	RXU_IDLE		4'hf

module rxuart(i_clk, i_reset, i_setup, i_uart_rx, o_wr, o_data, o_break,
			o_parity_err, o_frame_err, o_ck_uart);
	parameter [30:0] INITIAL_SETUP = 31'd868;
	// 8 data bits, no parity, (at least 1) stop bit
	input	wire		i_clk, i_reset;
	input	wire	[30:0]	i_setup;
	input	wire		i_uart_rx;
	output	reg		o_wr;
	output	reg	[7:0]	o_data;
	output	reg		o_break;
	output	reg		o_parity_err, o_frame_err;
	output	wire		o_ck_uart;


	wire	[27:0]	clocks_per_baud, break_condition, half_baud;
	wire	[1:0]	data_bits;
	wire		use_parity, parity_even, dblstop, fixd_parity;
	reg	[29:0]	r_setup;
	reg	[3:0]	state;

	assign	clocks_per_baud = { 4'h0, r_setup[23:0] };
	// assign hw_flow_control = !r_setup[30];
	assign	data_bits   = r_setup[29:28];
	assign	dblstop     = r_setup[27];
	assign	use_parity  = r_setup[26];
	assign	fixd_parity = r_setup[25];
	assign	parity_even = r_setup[24];
	assign	break_condition = { r_setup[23:0], 4'h0 };
	assign	half_baud = { 5'h00, r_setup[23:1] }-28'h1;
	reg	[27:0]	baud_counter;
	reg		zero_baud_counter;


	// Since this is an asynchronous receiver, we need to register our
	// input a couple of clocks over to avoid any problems with 
	// metastability.  We do that here, and then ignore all but the
	// ck_uart wire.
	reg	q_uart, qq_uart, ck_uart;
	initial	q_uart  = 1'b0;
	initial	qq_uart = 1'b0;
	initial	ck_uart = 1'b0;
	always @(posedge i_clk)
	begin
		q_uart <= i_uart_rx;
		qq_uart <= q_uart;
		ck_uart <= qq_uart;
	end

	// In case anyone else wants this clocked, stabilized value, we
	// offer it on our output.
	assign	o_ck_uart = ck_uart;

	// Keep track of the number of clocks since the last change.
	//
	// This is used to determine if we are in either a break or an idle
	// condition, as discussed further below.
	reg	[27:0]	chg_counter;
	initial	chg_counter = 28'h00;
	always @(posedge i_clk)
		if (i_reset)
			chg_counter <= 28'h00;
		else if (qq_uart != ck_uart)
			chg_counter <= 28'h00;
		else if (chg_counter < break_condition)
			chg_counter <= chg_counter + 1;

	// Are we in a break condition?
	//
	// A break condition exists if the line is held low for longer than
	// a data word.  Hence, we keep track of when the last change occurred.
	// If it was more than break_condition clocks ago, and the current input
	// value is a 0, then we're in a break--and nothing can be read until
	// the line idles again.
	initial	o_break    = 1'b0;
	always @(posedge i_clk)
		o_break <= ((chg_counter >= break_condition)&&(~ck_uart))? 1'b1:1'b0;

	// Are we between characters?
	//
	// The opposite of a break condition is where the line is held high
	// for more clocks than would be in a character.  When this happens,
	// we know we have synchronization--otherwise, we might be sampling
	// from within a data word.
	//
	// This logic is used later to hold the RXUART in a reset condition
	// until we know we are between data words.  At that point, we should
	// be able to hold on to our synchronization.
	reg	line_synch;
	initial	line_synch = 1'b0;
	always @(posedge i_clk)
		line_synch <= ((chg_counter >= break_condition)&&(ck_uart));

	// Are we in the middle of a baud iterval?  Specifically, are we
	// in the middle of a start bit?  Set this to high if so.  We'll use
	// this within our state machine to transition out of the IDLE
	// state.
	reg	half_baud_time;
	initial	half_baud_time = 0;
	always @(posedge i_clk)
		half_baud_time <= (~ck_uart)&&(chg_counter >= half_baud);


	// Allow our controlling processor to change our setup at any time
	// outside of receiving/processing a character.
	initial	r_setup     = INITIAL_SETUP[29:0];
	always @(posedge i_clk)
		if (state >= `RXU_RESET_IDLE)
			r_setup <= i_setup[29:0];


	// Our monster state machine.  YIKES!
	//
	// Yeah, this may be more complicated than it needs to be.  The basic
	// progression is:
	//	RESET -> RESET_IDLE -> (when line is idle) -> IDLE
	//	IDLE -> bit 0 -> bit 1 -> bit_{ndatabits} -> 
	//		(optional) PARITY -> STOP -> (optional) SECOND_STOP
	//		-> IDLE
	//	ANY -> (on break) BREAK -> IDLE
	//
	// There are 16 states, although all are not used.  These are listed
	// at the top of this file.
	//
	//	Logic inputs (12):	(I've tried to minimize this number)
	//		state	(4)
	//		i_reset
	//		line_synch
	//		o_break
	//		ckuart
	//		half_baud_time
	//		zero_baud_counter
	//		use_parity
	//		dblstop
	//	Logic outputs (4):
	//		state
	//
	initial	state = `RXU_RESET_IDLE;
	always @(posedge i_clk)
	begin
		if (i_reset)
			state <= `RXU_RESET_IDLE;
		else if (state == `RXU_RESET_IDLE)
		begin
			if (line_synch)
				// Goto idle state from a reset
				state <= `RXU_IDLE;
			else // Otherwise, stay in this condition 'til reset
				state <= `RXU_RESET_IDLE;
		end else if (o_break)
		begin // We are in a break condition
			state <= `RXU_BREAK;
		end else if (state == `RXU_BREAK)
		begin // Goto idle state following return ck_uart going high
			if (ck_uart)
				state <= `RXU_IDLE;
			else
				state <= `RXU_BREAK;
		end else if (state == `RXU_IDLE)
		begin // Idle state, independent of baud counter
			if ((~ck_uart)&&(half_baud_time))
			begin
				// We are in the center of a valid start bit
				case (data_bits)
				2'b00: state <= `RXU_BIT_ZERO;
				2'b01: state <= `RXU_BIT_ONE;
				2'b10: state <= `RXU_BIT_TWO;
				2'b11: state <= `RXU_BIT_THREE;
				endcase
			end else // Otherwise, just stay here in idle
				state <= `RXU_IDLE;
		end else if (zero_baud_counter)
		begin
			if (state < `RXU_BIT_SEVEN)
				// Data arrives least significant bit first.
				// By the time this is clocked in, it's what
				// you'll have.
				state <= state + 1;
			else if (state == `RXU_BIT_SEVEN)
				state <= (use_parity) ? `RXU_PARITY:`RXU_STOP;
			else if (state == `RXU_PARITY)
				state <= `RXU_STOP;
			else if (state == `RXU_STOP)
			begin // Stop (or parity) bit(s)
				if (~ck_uart) // On frame error, wait 4 ch idle
					state <= `RXU_RESET_IDLE;
				else if (dblstop)
					state <= `RXU_SECOND_STOP;
				else
					state <= `RXU_IDLE;
			end else // state must equal RX_SECOND_STOP
			begin
				if (~ck_uart) // On frame error, wait 4 ch idle
					state <= `RXU_RESET_IDLE;
				else
					state <= `RXU_IDLE;
			end
		end
	end

	// Data bit capture logic.
	//
	// This is drastically simplified from the state machine above, based
	// upon: 1) it doesn't matter what it is until the end of a captured
	// byte, and 2) the data register will flush itself of any invalid
	// data in all other cases.  Hence, let's keep it real simple.
	// The only trick, though, is that if we have parity, then the data
	// register needs to be held through that state without getting
	// updated.
	reg	[7:0]	data_reg;
	always @(posedge i_clk)
		if ((zero_baud_counter)&&(state != `RXU_PARITY))
			data_reg <= { ck_uart, data_reg[7:1] };

	// Parity calculation logic
	//
	// As with the data capture logic, all that must be known about this
	// bit is that it is the exclusive-OR of all bits prior.  The first
	// of those will follow idle, so we set ourselves to zero on idle.
	// Then, as we walk through the states of a bit, all will adjust this
	// value up until the parity bit, where the value will be read.  Setting
	// it then or after will be irrelevant, so ... this should be good
	// and simplified.  Note--we don't need to adjust this on reset either,
	// since the reset state will lead to the idle state where we'll be
	// reset before any transmission takes place.
	reg		calc_parity;
	always @(posedge i_clk)
		if (state == `RXU_IDLE)
			calc_parity <= 0;
		else if (zero_baud_counter)
			calc_parity <= calc_parity ^ ck_uart;

	// Parity error logic
	//
	// Set during the parity bit interval, read during the last stop bit
	// interval, cleared on BREAK, RESET_IDLE, or IDLE states.
	initial	o_parity_err = 1'b0;
	always @(posedge i_clk)
		if ((zero_baud_counter)&&(state == `RXU_PARITY))
		begin
			if (fixd_parity)
				// Fixed parity bit--independent of any dat
				// value.
				o_parity_err <= (ck_uart ^ parity_even);
			else if (parity_even)
				// Parity even: The XOR of all bits including
				// the parity bit must be zero.
				o_parity_err <= (calc_parity != ck_uart);
			else
				// Parity odd: the parity bit must equal the
				// XOR of all the data bits.
				o_parity_err <= (calc_parity == ck_uart);
		end else if (state >= `RXU_BREAK)
			o_parity_err <= 1'b0;

	// Frame error determination
	//
	// For the purpose of this controller, a frame error is defined as a
	// stop bit (or second stop bit, if so enabled) not being high midway
	// through the stop baud interval.   The frame error value is
	// immediately read, so we can clear it under all other circumstances.
	// Specifically, we want it clear in RXU_BREAK, RXU_RESET_IDLE, and
	// most importantly in RXU_IDLE.
	initial	o_frame_err  = 1'b0;
	always @(posedge i_clk)
		if ((zero_baud_counter)&&((state == `RXU_STOP)
						||(state == `RXU_SECOND_STOP)))
			o_frame_err <= (o_frame_err)||(~ck_uart);
		else if ((zero_baud_counter)||(state >= `RXU_BREAK))
			o_frame_err <= 1'b0;

	// Our data bit logic doesn't need nearly the complexity of all that
	// work above.  Indeed, we only need to know if we are at the end of
	// a stop bit, in which case we copy the data_reg into our output
	// data register, o_data.
	//
	// We would also set o_wr to be true when this is the case, but ... we
	// won't know if there is a frame error on the second stop bit for 
	// another baud interval yet.  So, instead, we set up the logic so that
	// we know on the next zero baud counter that we can write out.  That's
	// the purpose of pre_wr.
	initial	o_data = 8'h00;
	reg	pre_wr;
	initial	pre_wr = 1'b0;
	always @(posedge i_clk)
		if (i_reset)
		begin
			pre_wr <= 1'b0;
			o_data <= 8'h00;
		end else if ((zero_baud_counter)&&(state == `RXU_STOP))
		begin
			pre_wr <= 1'b1;
			case (data_bits)
			2'b00: o_data <= data_reg;
			2'b01: o_data <= { 1'b0, data_reg[7:1] };
			2'b10: o_data <= { 2'b0, data_reg[7:2] };
			2'b11: o_data <= { 3'b0, data_reg[7:3] };
			endcase
		end else if ((zero_baud_counter)||(state == `RXU_IDLE))
			pre_wr <= 1'b0;

	// Create an output strobe, true for one clock only, once we know
	// all we need to know.  o_data will be set on the last baud interval,
	// o_parity_err on the last parity baud interval (if it existed,
	// cleared otherwise, so ... we should be good to go here.)
	initial	o_wr   = 1'b0;
	always @(posedge i_clk)
		if ((zero_baud_counter)||(state == `RXU_IDLE))
			o_wr <= (pre_wr)&&(!i_reset);
		else
			o_wr <= 1'b0;

	// The baud counter
	//
	// This is used as a "clock divider" if you will, but the clock needs
	// to be reset before any byte can be decoded.  In all other respects,
	// we set ourselves up for clocks_per_baud counts between baud
	// intervals.
	always @(posedge i_clk)
		if (i_reset)
			baud_counter <= clocks_per_baud-28'h01;
		else if (zero_baud_counter)
			baud_counter <= clocks_per_baud-28'h01;
		else case(state)
			`RXU_RESET_IDLE:baud_counter <= clocks_per_baud-28'h01;
			`RXU_BREAK:	baud_counter <= clocks_per_baud-28'h01;
			`RXU_IDLE:	baud_counter <= clocks_per_baud-28'h01;
			default:	baud_counter <= baud_counter-28'h01;
		endcase

	// zero_baud_counter
	//
	// Rather than testing whether or not (baud_counter == 0) within our
	// (already too complicated) state transition tables, we use
	// zero_baud_counter to pre-charge that test on the clock
	// before--cleaning up some otherwise difficult timing dependencies.
	initial	zero_baud_counter = 1'b0;
	always @(posedge i_clk)
		if (state == `RXU_IDLE)
			zero_baud_counter <= 1'b0;
		else
		zero_baud_counter <= (baud_counter == 28'h01);


endmodule


////////////////////////////////////////////////////////////////////////////////
//
// Filename: 	txuart.v
//
// Project:	wbuart32, a full featured UART with simulator
//
// Purpose:	Transmit outputs over a single UART line.
//
//	To interface with this module, connect it to your system clock,
//	pass it the 32 bit setup register (defined below) and the byte
//	of data you wish to transmit.  Strobe the i_wr line high for one
//	clock cycle, and your data will be off.  Wait until the 'o_busy'
//	line is low before strobing the i_wr line again--this implementation
//	has NO BUFFER, so strobing i_wr while the core is busy will just
//	cause your data to be lost.  The output will be placed on the o_txuart
//	output line.  If you wish to set/send a break condition, assert the
//	i_break line otherwise leave it low.
//
//	There is a synchronous reset line, logic high.
//
//	Now for the setup register.  The register is 32 bits, so that this
//	UART may be set up over a 32-bit bus.
//
//	i_setup[30]	Set this to zero to use hardware flow control, and to
//		one to ignore hardware flow control.  Only works if the hardware
//		flow control has been properly wired.
//
//		If you don't want hardware flow control, fix the i_rts bit to
//		1'b1, and let the synthesys tools optimize out the logic.
//
//	i_setup[29:28]	Indicates the number of data bits per word.  This will
//		either be 2'b00 for an 8-bit word, 2'b01 for a 7-bit word, 2'b10
//		for a six bit word, or 2'b11 for a five bit word.
//
//	i_setup[27]	Indicates whether or not to use one or two stop bits.
//		Set this to one to expect two stop bits, zero for one.
//
//	i_setup[26]	Indicates whether or not a parity bit exists.  Set this
//		to 1'b1 to include parity.
//
//	i_setup[25]	Indicates whether or not the parity bit is fixed.  Set
//		to 1'b1 to include a fixed bit of parity, 1'b0 to allow the
//		parity to be set based upon data.  (Both assume the parity
//		enable value is set.)
//
//	i_setup[24]	This bit is ignored if parity is not used.  Otherwise,
//		in the case of a fixed parity bit, this bit indicates whether
//		mark (1'b1) or space (1'b0) parity is used.  Likewise if the
//		parity is not fixed, a 1'b1 selects even parity, and 1'b0
//		selects odd.
//
//	i_setup[23:0]	Indicates the speed of the UART in terms of clocks.
//		So, for example, if you have a 200 MHz clock and wish to
//		run your UART at 9600 baud, you would take 200 MHz and divide
//		by 9600 to set this value to 24'd20834.  Likewise if you wished
//		to run this serial port at 115200 baud from a 200 MHz clock,
//		you would set the value to 24'd1736
//
//	Thus, to set the UART for the common setting of an 8-bit word, 
//	one stop bit, no parity, and 115200 baud over a 200 MHz clock, you
//	would want to set the setup value to:
//
//	32'h0006c8		// For 115,200 baud, 8 bit, no parity
//	32'h005161		// For 9600 baud, 8 bit, no parity
//	
//
// Creator:	Dan Gisselquist, Ph.D.
//		Gisselquist Technology, LLC
//
////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2015-2017, Gisselquist Technology, LLC
//
// This program is free software (firmware): you can redistribute it and/or
// modify it under the terms of  the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License, or (at
// your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTIBILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program.  (It's in the $(ROOT)/doc directory.  Run make with no
// target there if the PDF file isn't present.)  If not, see
// <http://www.gnu.org/licenses/> for a copy.
//
// License:	GPL, v3, as defined and found on www.gnu.org,
//		http://www.gnu.org/licenses/gpl.html
//
//
////////////////////////////////////////////////////////////////////////////////
//
//
`default_nettype	none
//
`define	TXU_BIT_ZERO	4'h0
`define	TXU_BIT_ONE	4'h1
`define	TXU_BIT_TWO	4'h2
`define	TXU_BIT_THREE	4'h3
`define	TXU_BIT_FOUR	4'h4
`define	TXU_BIT_FIVE	4'h5
`define	TXU_BIT_SIX	4'h6
`define	TXU_BIT_SEVEN	4'h7
`define	TXU_PARITY	4'h8	// Constant 1
`define	TXU_STOP	4'h9	// Constant 1
`define	TXU_SECOND_STOP	4'ha
// 4'hb	// Unused
// 4'hc	// Unused
// `define	TXU_START	4'hd	// An unused state
`define	TXU_BREAK	4'he
`define	TXU_IDLE	4'hf
//
//
module txuart(i_clk, i_reset, i_setup, i_break, i_wr, i_data,
		i_cts_n, o_uart_tx, o_busy);
	parameter	[30:0]	INITIAL_SETUP = 31'd868;
	input	wire		i_clk, i_reset;
	input	wire	[30:0]	i_setup;
	input	wire		i_break;
	input	wire		i_wr;
	input	wire	[7:0]	i_data;
	// Hardware flow control Ready-To-Send bit.  Set this to one to use
	// the core without flow control.  (A more appropriate name would be
	// the Ready-To-Receive bit ...)
	input	wire		i_cts_n;
	// And the UART input line itself
	output	reg		o_uart_tx;
	// A line to tell others when we are ready to accept data.  If
	// (i_wr)&&(!o_busy) is ever true, then the core has accepted a byte
	// for transmission.
	output	wire		o_busy;

	wire	[27:0]	clocks_per_baud, break_condition;
	wire	[1:0]	data_bits;
	wire		use_parity, parity_even, dblstop, fixd_parity,
			fixdp_value, hw_flow_control;
	reg	[30:0]	r_setup;
	assign	clocks_per_baud = { 4'h0, r_setup[23:0] };
	assign	break_condition = { r_setup[23:0], 4'h0 };
	assign	hw_flow_control = !r_setup[30];
	assign	data_bits       =  r_setup[29:28];
	assign	dblstop         =  r_setup[27];
	assign	use_parity      =  r_setup[26];
	assign	fixd_parity     =  r_setup[25];
	assign	parity_even     =  r_setup[24];
	assign	fixdp_value     =  r_setup[24];

	reg	[27:0]	baud_counter;
	reg	[3:0]	state;
	reg	[7:0]	lcl_data;
	reg		calc_parity, r_busy, zero_baud_counter;


	// First step ... handle any hardware flow control, if so enabled.
	//
	// Clock in the flow control data, two clocks to avoid metastability
	// Default to using hardware flow control (uart_setup[30]==0 to use it).
	// Set this high order bit off if you do not wish to use it.
	reg	q_cts_n, qq_cts_n, ck_cts;
	// While we might wish to give initial values to q_rts and ck_cts,
	// 1) it's not required since the transmitter starts in a long wait
	// state, and 2) doing so will prevent the synthesizer from optimizing
	// this pin in the case it is hard set to 1'b1 external to this
	// peripheral.
	//
	// initial	q_cts_n  = 1'b1;
	// initial	qq_cts_n = 1'b1;
	// initial	ck_cts   = 1'b0;
	always	@(posedge i_clk)
		q_cts_n <= i_cts_n;
	always	@(posedge i_clk)
		qq_cts_n <= q_cts_n;
	always	@(posedge i_clk)
		ck_cts <= (!qq_cts_n)||(!hw_flow_control);

	initial	o_uart_tx = 1'b1;
	initial	r_busy = 1'b1;
	initial	state  = `TXU_IDLE;
	initial	lcl_data= 8'h0;
	initial	calc_parity = 1'b0;
	// initial	baud_counter = clocks_per_baud;//ILLEGAL--not constant
	always @(posedge i_clk)
	begin
		if (i_reset)
		begin
			r_busy <= 1'b1;
			state <= `TXU_IDLE;
		end else if (i_break)
		begin
			state <= `TXU_BREAK;
			r_busy <= 1'b1;
		end else if (!zero_baud_counter)
		begin // r_busy needs to be set coming into here
			r_busy <= 1'b1;
		end else if (state == `TXU_BREAK)
		begin
			state <= `TXU_IDLE;
			r_busy <= 1'b1;
		end else if (state == `TXU_IDLE)	// STATE_IDLE
		begin
			if ((i_wr)&&(!r_busy))
			begin	// Immediately start us off with a start bit
				r_busy <= 1'b1;
				case(data_bits)
				2'b00: state <= `TXU_BIT_ZERO;
				2'b01: state <= `TXU_BIT_ONE;
				2'b10: state <= `TXU_BIT_TWO;
				2'b11: state <= `TXU_BIT_THREE;
				endcase
			end else begin // Stay in idle
				r_busy <= !ck_cts;
			end
		end else begin
			// One clock tick in each of these states ...
			// baud_counter <= clocks_per_baud - 28'h01;
			r_busy <= 1'b1;
			if (state[3] == 0) // First 8 bits
			begin
				if (state == `TXU_BIT_SEVEN)
					state <= (use_parity)?`TXU_PARITY:`TXU_STOP;
				else
					state <= state + 1;
			end else if (state == `TXU_PARITY)
			begin
				state <= `TXU_STOP;
			end else if (state == `TXU_STOP)
			begin // two stop bit(s)
				if (dblstop)
					state <= `TXU_SECOND_STOP;
				else
					state <= `TXU_IDLE;
			end else // `TXU_SECOND_STOP and default:
			begin
				state <= `TXU_IDLE; // Go back to idle
				// Still r_busy, since we need to wait
				// for the baud clock to finish counting
				// out this last bit.
			end
		end 
	end

	// o_busy
	//
	// This is a wire, designed to be true is we are ever busy above.
	// originally, this was going to be true if we were ever not in the
	// idle state.  The logic has since become more complex, hence we have
	// a register dedicated to this and just copy out that registers value.
	assign	o_busy = (r_busy);


	// r_setup
	//
	// Our setup register.  Accept changes between any pair of transmitted
	// words.  The register itself has many fields to it.  These are
	// broken out up top, and indicate what 1) our baud rate is, 2) our
	// number of stop bits, 3) what type of parity we are using, and 4)
	// the size of our data word.
	initial	r_setup = INITIAL_SETUP;
	always @(posedge i_clk)
		if (state == `TXU_IDLE)
			r_setup <= i_setup;

	// lcl_data
	//
	// This is our working copy of the i_data register which we use
	// when transmitting.  It is only of interest during transmit, and is
	// allowed to be whatever at any other time.  Hence, if r_busy isn't
	// true, we can always set it.  On the one clock where r_busy isn't
	// true and i_wr is, we set it and r_busy is true thereafter.
	// Then, on any zero_baud_counter (i.e. change between baud intervals)
	// we simple logically shift the register right to grab the next bit.
	always @(posedge i_clk)
		if (!r_busy)
			lcl_data <= i_data;
		else if (zero_baud_counter)
			lcl_data <= { 1'b0, lcl_data[7:1] };

	// o_uart_tx
	//
	// This is the final result/output desired of this core.  It's all
	// centered about o_uart_tx.  This is what finally needs to follow
	// the UART protocol.
	//
	// Ok, that said, our rules are:
	//	1'b0 on any break condition
	//	1'b0 on a start bit (IDLE, write, and not busy)
	//	lcl_data[0] during any data transfer, but only at the baud
	//		change
	//	PARITY -- During the parity bit.  This depends upon whether or
	//		not the parity bit is fixed, then what it's fixed to,
	//		or changing, and hence what it's calculated value is.
	//	1'b1 at all other times (stop bits, idle, etc)
	always @(posedge i_clk)
		if (i_reset)
			o_uart_tx <= 1'b1;
		else if ((i_break)||((i_wr)&&(!r_busy)))
			o_uart_tx <= 1'b0;
		else if (zero_baud_counter)
			casez(state)
			4'b0???:	o_uart_tx <= lcl_data[0];
			`TXU_PARITY:	o_uart_tx <= calc_parity;
			default:	o_uart_tx <= 1'b1;
			endcase


	// calc_parity
	//
	// Calculate the parity to be placed into the parity bit.  If the
	// parity is fixed, then the parity bit is given by the fixed parity
	// value (r_setup[24]).  Otherwise the parity is given by the GF2
	// sum of all the data bits (plus one for even parity).
	always @(posedge i_clk)
		if (fixd_parity)
			calc_parity <= fixdp_value;
		else if (zero_baud_counter)
		begin
			if (state[3] == 0) // First 8 bits of msg
				calc_parity <= calc_parity ^ lcl_data[0];
			else
				calc_parity <= parity_even;
		end else if (!r_busy)
			calc_parity <= parity_even;


	// All of the above logic is driven by the baud counter.  Bits must last
	// clocks_per_baud in length, and this baud counter is what we use to
	// make certain of that.
	//
	// The basic logic is this: at the beginning of a bit interval, start
	// the baud counter and set it to count clocks_per_baud.  When it gets
	// to zero, restart it.
	//
	// However, comparing a 28'bit number to zero can be rather complex--
	// especially if we wish to do anything else on that same clock.  For
	// that reason, we create "zero_baud_counter".  zero_baud_counter is
	// nothing more than a flag that is true anytime baud_counter is zero.
	// It's true when the logic (above) needs to step to the next bit.
	// Simple enough?
	//
	// I wish we could stop there, but there are some other (ugly)
	// conditions to deal with that offer exceptions to this basic logic.
	//
	// 1. When the user has commanded a BREAK across the line, we need to
	// wait several baud intervals following the break before we start
	// transmitting, to give any receiver a chance to recognize that we are
	// out of the break condition, and to know that the next bit will be
	// a stop bit.
	//
	// 2. A reset is similar to a break condition--on both we wait several
	// baud intervals before allowing a start bit.
	//
	// 3. In the idle state, we stop our counter--so that upon a request
	// to transmit when idle we can start transmitting immediately, rather
	// than waiting for the end of the next (fictitious and arbitrary) baud
	// interval.
	//
	// When (i_wr)&&(!r_busy)&&(state == `TXU_IDLE) then we're not only in
	// the idle state, but we also just accepted a command to start writing
	// the next word.  At this point, the baud counter needs to be reset
	// to the number of clocks per baud, and zero_baud_counter set to zero.
	//
	// The logic is a bit twisted here, in that it will only check for the
	// above condition when zero_baud_counter is false--so as to make
	// certain the STOP bit is complete.
	initial	zero_baud_counter = 1'b0;
	initial	baud_counter = 28'h05;
	always @(posedge i_clk)
	begin
		zero_baud_counter <= (baud_counter == 28'h01);
		if ((i_reset)||(i_break))
		begin
			// Give ourselves 16 bauds before being ready
			baud_counter <= break_condition;
			zero_baud_counter <= 1'b0;
		end else if (!zero_baud_counter)
			baud_counter <= baud_counter - 28'h01;
		else if (state == `TXU_BREAK)
			// Give us four idle baud intervals before becoming
			// available
			baud_counter <= clocks_per_baud<<2;
		else if (state == `TXU_IDLE)
		begin
			baud_counter <= 28'h0;
			zero_baud_counter <= 1'b1;
			if ((i_wr)&&(!r_busy))
			begin
				baud_counter <= clocks_per_baud - 28'h01;
				zero_baud_counter <= 1'b0;
			end
		end else
			baud_counter <= clocks_per_baud - 28'h01;
	end
endmodule

////////////////////////////////////////////////////////////////////////////////
//
// Filename: 	ufifo.v
//
// Project:	wbuart32, a full featured UART with simulator
//
// Purpose:	A synchronous data FIFO, designed for supporting the Wishbone
//		UART.  Particular features include the ability to read and
//	write on the same clock, while maintaining the correct output FIFO
//	parameters.  Two versions of the FIFO exist within this file, separated
//	by the RXFIFO parameter's value.  One, where RXFIFO = 1, produces status
//	values appropriate for reading and checking a read FIFO from logic, whereas
//	the RXFIFO = 0 applies to writing to the FIFO from bus logic and reading
//	it automatically any time the transmit UART is idle.
//
// Creator:	Dan Gisselquist, Ph.D.
//		Gisselquist Technology, LLC
//
////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2015-2017, Gisselquist Technology, LLC
//
// This program is free software (firmware): you can redistribute it and/or
// modify it under the terms of  the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License, or (at
// your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTIBILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program.  (It's in the $(ROOT)/doc directory.  Run make with no
// target there if the PDF file isn't present.)  If not, see
// <http://www.gnu.org/licenses/> for a copy.
//
// License:	GPL, v3, as defined and found on www.gnu.org,
//		http://www.gnu.org/licenses/gpl.html
//
//
////////////////////////////////////////////////////////////////////////////////
//
//
`default_nettype	none
//
module ufifo(i_clk, i_rst, i_wr, i_data, o_empty_n, i_rd, o_data, o_status, o_err);
	parameter	BW=8;	// Byte/data width
	parameter [3:0]	LGFLEN=4;
	parameter 	RXFIFO=1'b0;
	input	wire		i_clk, i_rst;
	input	wire		i_wr;
	input	wire [(BW-1):0]	i_data;
	output	wire		o_empty_n;	// True if something is in FIFO
	input	wire		i_rd;
	output	wire [(BW-1):0]	o_data;
	output	wire	[15:0]	o_status;
	output	wire		o_err;

	localparam	FLEN=(1<<LGFLEN);

	reg	[(BW-1):0]	fifo[0:(FLEN-1)];
	reg	[(LGFLEN-1):0]	r_first, r_last, r_next;

	wire	[(LGFLEN-1):0]	w_first_plus_one, w_first_plus_two,
				w_last_plus_one;
	assign	w_first_plus_two = r_first + {{(LGFLEN-2){1'b0}},2'b10};
	assign	w_first_plus_one = r_first + {{(LGFLEN-1){1'b0}},1'b1};
	assign	w_last_plus_one  = r_next; // r_last  + 1'b1;

	reg	will_overflow;
	initial	will_overflow = 1'b0;
	always @(posedge i_clk)
		if (i_rst)
			will_overflow <= 1'b0;
		else if (i_rd)
			will_overflow <= (will_overflow)&&(i_wr);
		else if (i_wr)
			will_overflow <= (will_overflow)||(w_first_plus_two == r_last);
		else if (w_first_plus_one == r_last)
			will_overflow <= 1'b1;

	// Write
	reg	r_ovfl;
	initial	r_first = 0;
	initial	r_ovfl  = 0;
	always @(posedge i_clk)
		if (i_rst)
		begin
			r_ovfl <= 1'b0;
			r_first <= { (LGFLEN){1'b0} };
		end else if (i_wr)
		begin // Cowardly refuse to overflow
			if ((i_rd)||(!will_overflow)) // (r_first+1 != r_last)
				r_first <= w_first_plus_one;
			else
				r_ovfl <= 1'b1;
		end
	always @(posedge i_clk)
		if (i_wr) // Write our new value regardless--on overflow or not
			fifo[r_first] <= i_data;

	// Reads
	//	Following a read, the next sample will be available on the
	//	next clock
	//	Clock	ReadCMD	ReadAddr	Output
	//	0	0	0		fifo[0]
	//	1	1	0		fifo[0]
	//	2	0	1		fifo[1]
	//	3	0	1		fifo[1]
	//	4	1	1		fifo[1]
	//	5	1	2		fifo[2]
	//	6	0	3		fifo[3]
	//	7	0	3		fifo[3]
	reg	will_underflow;
	initial	will_underflow = 1'b1;
	always @(posedge i_clk)
		if (i_rst)
			will_underflow <= 1'b1;
		else if (i_wr)
			will_underflow <= (will_underflow)&&(i_rd);
		else if (i_rd)
			will_underflow <= (will_underflow)||(w_last_plus_one == r_first);
		else
			will_underflow <= (r_last == r_first);

	//
	// Don't report FIFO underflow errors.  These'll be caught elsewhere
	// in the system, and the logic below makes it hard to reset them.
	// We'll still report FIFO overflow, however.
	//
	// reg		r_unfl;
	// initial	r_unfl = 1'b0;
	initial	r_last = 0;
	always @(posedge i_clk)
		if (i_rst)
		begin
			r_last <= 0;
			r_next <= { {(LGFLEN-1){1'b0}}, 1'b1 };
			// r_unfl <= 1'b0;
		end else if (i_rd)
		begin
			if ((i_wr)||(!will_underflow)) // (r_first != r_last)
			begin
				r_last <= r_next;
				r_next <= r_last +{{(LGFLEN-2){1'b0}},2'b10};
				// Last chases first
				// Need to be prepared for a possible two
				// reads in quick succession
				// o_data <= fifo[r_last+1];
			end
			// else r_unfl <= 1'b1;
		end

	reg	[(BW-1):0]	fifo_here, fifo_next, r_data;
	always @(posedge i_clk)
		fifo_here <= fifo[r_last];
	always @(posedge i_clk)
		fifo_next <= fifo[r_next];
	always @(posedge i_clk)
		r_data <= i_data;

	reg	[1:0]	osrc;
	always @(posedge i_clk)
		if (will_underflow)
			// o_data <= i_data;
			osrc <= 2'b00;
		else if ((i_rd)&&(r_first == w_last_plus_one))
			osrc <= 2'b01;
		else if (i_rd)
			osrc <= 2'b11;
		else
			osrc <= 2'b10;
	assign o_data = (osrc[1]) ? ((osrc[0])?fifo_next:fifo_here) : r_data;

	// wire	[(LGFLEN-1):0]	current_fill;
	// assign	current_fill = (r_first-r_last);

	reg	r_empty_n;
	initial	r_empty_n = 1'b0;
	always @(posedge i_clk)
		if (i_rst)
			r_empty_n <= 1'b0;
		else casez({i_wr, i_rd, will_underflow})
			3'b00?: r_empty_n <= (r_first != r_last);
			3'b11?: r_empty_n <= (r_first != r_last);
			3'b10?: r_empty_n <= 1'b1;
			3'b010: r_empty_n <= (r_first != w_last_plus_one);
			// 3'b001: r_empty_n <= 1'b0;
			default: begin end
		endcase

	wire	w_full_n;
	assign	w_full_n = will_overflow;

	//
	// If this is a receive FIFO, the FIFO count that matters is the number
	// of values yet to be read.  If instead this is a transmit FIFO, then 
	// the FIFO count that matters is the number of empty positions that
	// can still be filled before the FIFO is full.
	//
	// Adjust for these differences here.
	reg	[(LGFLEN-1):0]	r_fill;
	initial	r_fill = 0;
	always @(posedge i_clk)
		if (RXFIFO!=0) begin
			// Calculate the number of elements in our FIFO
			//
			// Although used for receive, this is actually the more
			// generic answer--should you wish to use the FIFO in
			// another context.
			if (i_rst)
				r_fill <= 0;
			else case({(i_wr)&&(!will_overflow), (i_rd)&&(!will_underflow)})
			2'b01:   r_fill <= r_first - r_next;
			2'b10:   r_fill <= r_first - r_last + 1'b1;
			default: r_fill <= r_first - r_last;
			endcase
		end else begin
			// Calculate the number of elements that are empty and
			// can be filled within our FIFO.  Hence, this is really
			// not the fill, but (SIZE-1)-fill.
			if (i_rst)
				r_fill <= { (LGFLEN){1'b1} };
			else case({i_wr, i_rd})
			2'b01:   r_fill <= r_last - r_first;
			2'b10:   r_fill <= r_last - w_first_plus_two;
			default: r_fill <= r_last - w_first_plus_one;
			endcase
		end

	// We don't report underflow errors.  These
	assign o_err = (r_ovfl); //  || (r_unfl);

	wire	[3:0]	lglen;
	assign lglen = LGFLEN;

	wire	[9:0]	w_fill;
	assign	w_fill[(LGFLEN-1):0] = r_fill;
	generate if (LGFLEN < 10)
		assign w_fill[9:(LGFLEN)] = 0;
	endgenerate

	wire	w_half_full;
	assign	w_half_full = r_fill[(LGFLEN-1)];

	assign	o_status = {
		// Our status includes a 4'bit nibble telling anyone reading
		// this the size of our FIFO.  The size is then given by
		// 2^(this value).  Hence a 4'h4 in this position means that the
		// FIFO has 2^4 or 16 values within it.
		lglen,
		// The FIFO fill--for a receive FIFO the number of elements
		// left to be read, and for a transmit FIFO the number of
		// empty elements within the FIFO that can yet be filled.
		w_fill,
		// A '1' here means a half FIFO length can be read (receive
		// FIFO) or written to (not a receive FIFO).
		// receive FIFO), or be written to (if it isn't).
		(RXFIFO!=0)?w_half_full:w_half_full,
		// A '1' here means the FIFO can be read from (if it is a
		// receive FIFO), or be written to (if it isn't).
		(RXFIFO!=0)?r_empty_n:w_full_n
	};

	assign	o_empty_n = r_empty_n;
	
endmodule
