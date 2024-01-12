LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY mouse_si_numarator IS
  PORT (
  		 clk             : IN     STD_LOGIC;                     --system clock input
	      reset          : IN     STD_LOGIC;                     --active low asynchronous reset
	      ps2_clk        : INOUT  STD_LOGIC;                     --clock signal from PS2 mouse
	      ps2_data       : INOUT  STD_LOGIC;                     --data signal from PS2 mouse
	      reverse        : in std_logic;
	      is_left        : out std_logic;
	      anozi 		 : out std_logic_vector (7 downto 0);
		 catozi		     : out std_logic_vector (6 downto 0)
    );
END mouse_si_numarator;

--------------------------------------------------------------------------------
--Complete your VHDL description below
--------------------------------------------------------------------------------

ARCHITECTURE TypeArchitecture OF mouse_si_numarator IS

component ps2_si_decoder IS
  PORT (
  		 clk            : IN     STD_LOGIC;                     --system clock input
	      reset          : IN     STD_LOGIC;                     --active low asynchronous reset
	      ps2_clk        : INOUT  STD_LOGIC;                     --clock signal from PS2 mouse
	      ps2_data       : INOUT  STD_LOGIC;                     --data signal from PS2 mouse
	  	 left_button    : out std_logic;
	  	 right_button   : out std_logic
    );
END component;

component nr_si_ssd_complet IS
  PORT (
  		 CLK : in  STD_LOGIC;     
   		 Reset: in STD_LOGIC;
   		 left_button: in STD_LOGIC;
   		 right_button: in STD_LOGIC;
   		 reverse : in STD_LOGIC;
		 anozi : out std_logic_vector (7 downto 0);
		 catozi: out std_logic_vector (6 downto 0)
    );
END component;
signal MOUSE_X_POS: std_logic_vector (11 downto 0);
signal MOUSE_Y_POS: std_logic_vector (11 downto 0);
signal left_button: std_logic;
signal right_button: std_logic;
BEGIN
A:ps2_si_decoder port map(clk, reset, ps2_clk, ps2_data, left_button, right_button);
B: nr_si_ssd_complet port map(CLK, reset, left_button, right_button, reverse, anozi, catozi);
IS_LEFT<=not reverse;
END TypeArchitecture;

------------------------------------MOUSE

LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY ps2_si_decoder IS
  PORT (
  		 clk            : IN     STD_LOGIC;                     --system clock input
	      reset          : IN     STD_LOGIC;                     --active low asynchronous reset
	      ps2_clk        : INOUT  STD_LOGIC;                     --clock signal from PS2 mouse
	      ps2_data       : INOUT  STD_LOGIC;                     --data signal from PS2 mouse
	  	 left_button    : out std_logic;
	  	 right_button   : out std_logic
    );
END ps2_si_decoder;

--------------------------------------------------------------------------------
--Complete your VHDL description below
--------------------------------------------------------------------------------

ARCHITECTURE TypeArchitecture OF ps2_si_decoder IS

COMPONENT ps2_mouse is
GENERIC(
      clk_freq                  : INTEGER := 50000000; --system clock frequency in Hz
      ps2_debounce_counter_size : INTEGER := 8);         --set such that 2^size/clk_freq = 5us (size = 8 for 50MHz)
  PORT(
      clk            : IN     STD_LOGIC;                     --system clock input
      reset_n        : IN     STD_LOGIC;                     --active low asynchronous reset
      ps2_clk        : INOUT  STD_LOGIC;                     --clock signal from PS2 mouse
      ps2_data       : INOUT  STD_LOGIC;                     --data signal from PS2 mouse
      mouse_data     : OUT    STD_LOGIC_VECTOR(23 DOWNTO 0); --data received from mouse
      mouse_data_new : OUT    STD_LOGIC);                    --new data packet available flag
END component;

component decoder_mouse IS
  PORT (
		mouse_bytes: in std_logic_vector(23 downto 0);
  		new_data: in std_logic;
  		button_left: out std_logic;
  		button_right: out std_logic
    );
END component;
signal mouse_data: std_logic_vector (23 downto 0);
signal mouse_data_new: std_logic;
BEGIN

A: ps2_mouse port map(clk, reset, ps2_clk, ps2_data, mouse_data, mouse_data_new);
B: decoder_mouse port map(mouse_data, mouse_data_new, left_button, right_button);

END TypeArchitecture;

----------------------------------------------PS2 MOUSE

LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY ps2_mouse IS
  GENERIC(
      clk_freq                  : INTEGER := 100000000; --system clock frequency in Hz
      ps2_debounce_counter_size : INTEGER := 9);         --set such that 2^size/clk_freq = 5us (size = 8 for 50MHz)
  PORT(
      clk            : IN     STD_LOGIC;                     --system clock input
      reset_n        : IN     STD_LOGIC;                     --active low asynchronous reset
      ps2_clk        : INOUT  STD_LOGIC;                     --clock signal from PS2 mouse
      ps2_data       : INOUT  STD_LOGIC;                     --data signal from PS2 mouse
      mouse_data     : OUT    STD_LOGIC_VECTOR(23 DOWNTO 0); --data received from mouse
      mouse_data_new : OUT    STD_LOGIC);                    --new data packet available flag
END ps2_mouse;

ARCHITECTURE logic OF ps2_mouse IS
  TYPE machine IS(reset, rx_ack1, rx_bat, rx_id, ena_reporting, rx_ack2, stream);  --needed states
  SIGNAL state             : machine := reset;              --state machine  
  SIGNAL tx_ena            : STD_LOGIC := '0';              --transmit enable for ps2_transceiver
  SIGNAL tx_cmd            : STD_LOGIC_VECTOR(8 DOWNTO 0);  --command to transmit
  SIGNAL tx_busy           : STD_LOGIC;                     --ps2_transceiver busy signal
  SIGNAL ps2_code          : STD_LOGIC_VECTOR(7 DOWNTO 0);  --PS/2 code received from ps2_transceiver
  SIGNAL ps2_code_new      : STD_LOGIC;                     --new PS/2 code available flag from ps2_transceiver
  SIGNAL ps2_code_new_prev : STD_LOGIC;                     --previous value of ps2_code_new
  SIGNAL packet_byte       : INTEGER RANGE 0 TO 2 := 2;     --counter to track which packet byte is being received
  SIGNAL mouse_data_int    : STD_LOGIC_VECTOR(23 DOWNTO 0); --internal mouse data register
  
  --component to control PS/2 bus interface to the mouse
  COMPONENT ps2_transceiver IS
    GENERIC(
      clk_freq              : INTEGER;   --system clock frequency in Hz
      debounce_counter_size : INTEGER);  --set such that (2^size)/clk_freq = 5us (size = 8 for 50MHz)
    PORT(
      clk          : IN     STD_LOGIC;                    --system clock
      reset_n      : IN     STD_LOGIC;                    --active low asynchronous reset
      tx_ena       : IN     STD_LOGIC;                    --enable transmit
      tx_cmd       : IN     STD_LOGIC_VECTOR(8 DOWNTO 0); --8-bit command to transmit, MSB is parity bit
      tx_busy      : OUT    STD_LOGIC;                    --indicates transmit in progress
      ack_error    : OUT    STD_LOGIC;                    --device acknowledge from transmit, '1' is error
      ps2_code     : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); --code received from PS/2 bus
      ps2_code_new : OUT    STD_LOGIC;                    --flag that new PS/2 code is available on ps2_code bus
      rx_error     : OUT    STD_LOGIC;                    --start, stop, or parity receive error detected, '1' is error
      ps2_clk      : INOUT  STD_LOGIC;                    --PS/2 port clock signal
      ps2_data     : INOUT  STD_LOGIC);                   --PS/2 port data signal
  END COMPONENT;

BEGIN

  --PS/2 transceiver to control transactions with mouse
  ps2_transceiver_0:  ps2_transceiver
  GENERIC MAP(clk_freq => clk_freq, debounce_counter_size => ps2_debounce_counter_size)
  PORT MAP(clk => clk, reset_n => reset_n, tx_ena => tx_ena, tx_cmd => tx_cmd, tx_busy => tx_busy, ack_error => OPEN,
        ps2_code => ps2_code, ps2_code_new => ps2_code_new, rx_error => OPEN, ps2_clk => ps2_clk, ps2_data => ps2_data);

  PROCESS(clk, reset_n)
  BEGIN
    IF(reset_n = '0') THEN              --asynchronous reset
      mouse_data_new <= '0';              --clear new mouse data available flag
      mouse_data <= (OTHERS => '0');      --clear last mouse data packet received
      state <= reset;                     --set state machine to reset the mouse
    ELSIF(clk'EVENT AND clk = '1') THEN
      ps2_code_new_prev <= ps2_code_new;  --store previous value of the new PS/2 code flag

      CASE state IS

        WHEN reset =>
          IF(tx_busy = '0') THEN        --transmit to mouse not yet in process
            tx_ena <= '1';                --enable transmit to PS/2 mouse
            tx_cmd <= "111111111";        --send reset command (0xFF)
            state <= reset;               --remain in reset state
          ELSIF(tx_busy = '1') THEN     --transmit to mouse is in process
            tx_ena <= '0';                --clear transmit enable
            state <= rx_ack1;             --wait to receive an acknowledge from mouse
          END IF;
        
        WHEN rx_ack1 =>
          IF(ps2_code_new_prev = '0' AND ps2_code_new = '1') THEN --new PS/2 code received
            IF(ps2_code = "11111010") THEN                          --new PS/2 code is acknowledge (0xFA)
              state <= rx_bat;                                        --wait to receive new BAT completion code
            ELSE                                                    --new PS/2 code was not an acknowledge
              state <= reset;                                         --reset mouse again
            END IF;
          ELSE                                                    --new PS/2 code not yet received
            state <= rx_ack1;                                       --wait to receive a code from mouse
          END IF;
        
        WHEN rx_bat =>          
          IF(ps2_code_new_prev = '0' AND ps2_code_new = '1') THEN --new PS/2 code received
            IF(ps2_code = "10101010") THEN                          --new PS/2 code is BAT completion (0xAA)
              state <= rx_id;                                         --wait to receive device ID code
            ELSE                                                    --new PS/2 code was not BAT completion
              state <= reset;                                         --reset mouse again
            END IF;
          ELSE                                                    --new PS/2 code not yet received
            state <= rx_bat;                                        --wait to receive a code from mouse
          END IF;
        
        WHEN rx_id =>
          IF(ps2_code_new_prev = '0' AND ps2_code_new = '1') THEN --new PS/2 code received
            IF(ps2_code = "00000000") THEN                          --new PS/2 code is a mouse device ID (0x00)
              state <= ena_reporting;                                 --send command to enable data reporting
            ELSE                                                    --new PS/2 code is not a mouse device ID
              state <= reset;                                         --reset mouse again
            END IF;
          ELSE                                                    --new PS/2 code not yet received
            state <= rx_id;                                         --wait to receive a code from mouse
          END IF;
        
        WHEN ena_reporting =>
          IF(tx_busy = '0') THEN     --transmit to mouse not yet in process
            tx_ena <= '1';             --enable transmit to PS/2 mouse
            tx_cmd <= "011110100";     --send enable reporting command (0xF4)
            state <= ena_reporting;    --remain in ena_reporting state
          ELSIF(tx_busy = '1') THEN  --transmit to mouse is in process
            tx_ena <= '0';             --clear transmit enable
            state <= rx_ack2;          --wait to receive an acknowledge from mouse
          END IF;
        
        WHEN rx_ack2 =>
          IF(ps2_code_new_prev = '0' AND ps2_code_new = '1') THEN --new PS/2 code received
            IF(ps2_code = "11111010") THEN                          --new PS/2 code is acknowledge (0xFA)
              state <= stream;                                        --proceed to collect and output data from mouse
            ELSE                                                    --new PS/2 code was not an acknowledge
              state <= reset;                                         --reset mouse again
            END IF;
          ELSE                                                    --new PS/2 code not yet received
            state <= rx_ack2;                                       --wait to receive a code from mouse
          END IF;
        
        WHEN stream =>
          IF(ps2_code_new_prev = '0' AND ps2_code_new = '1') THEN                     --new PS/2 code received
            mouse_data_new <= '0';                                                      --clear new data packet available flag
            mouse_data_int(7+packet_byte*8 DOWNTO packet_byte*8) <= ps2_code;           --store new mouse data byte
            IF(packet_byte = 0) THEN                                                    --all bytes in packet received and presented
              packet_byte <= 2;                                                           --clear packet byte counter
            ELSE                                                                        --not all bytes in packet received yet
              packet_byte <= packet_byte - 1;                                             --increment packet byte counter
            END IF;
          END IF;
          IF(ps2_code_new_prev = '1' AND ps2_code_new = '1' AND packet_byte = 2) THEN --mouse data receive is complete
            mouse_data <= mouse_data_int;                                               --present new mouse data at output
            mouse_data_new <= '1';                                                      --set new data packet available flag
          END IF;
          
      END CASE;    
    END IF;  
  END PROCESS;      
        
END logic;




LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY ps2_transceiver IS
  GENERIC(
    clk_freq              : INTEGER := 100000000; --system clock frequency in Hz
    debounce_counter_size : INTEGER := 9);         --set such that (2^size)/clk_freq = 5us (size = 8 for 50MHz)
  PORT(
    clk          : IN     STD_LOGIC;                    --system clock
    reset_n      : IN     STD_LOGIC;                    --active low asynchronous reset
    tx_ena       : IN     STD_LOGIC;                    --enable transmit
    tx_cmd       : IN     STD_LOGIC_VECTOR(8 DOWNTO 0); --8-bit command to transmit, MSB is parity bit
    tx_busy      : OUT    STD_LOGIC;                    --indicates transmit in progress
    ack_error    : OUT    STD_LOGIC;                    --device acknowledge from transmit, '1' is error
    ps2_code     : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); --code received from PS/2
    ps2_code_new : OUT    STD_LOGIC;                    --flag that new PS/2 code is available on ps2_code bus
    rx_error     : OUT    STD_LOGIC;                    --start, stop, or parity receive error detected, '1' is error
    ps2_clk      : INOUT  STD_LOGIC;                    --PS/2 port clock signal
    ps2_data     : INOUT  STD_LOGIC);                   --PS/2 port data signal
END ps2_transceiver;

ARCHITECTURE logic OF ps2_transceiver IS
  TYPE machine IS(receive, inhibit, transact, tx_complete);          --needed states
  SIGNAL state            : machine := receive;                      --state machine
  SIGNAL sync_ffs         : STD_LOGIC_VECTOR(1 DOWNTO 0);            --synchronizer flip-flops for PS/2 signals
  SIGNAL ps2_clk_int      : STD_LOGIC;                               --debounced input clock signal from PS/2 port
  SIGNAL ps2_clk_int_prev : STD_LOGIC;                               --previous state of the ps2_clk_int signal
  SIGNAL ps2_data_int     : STD_LOGIC;                               --debounced input data signal from PS/2 port
  SIGNAL ps2_word         : STD_LOGIC_VECTOR(10 DOWNTO 0);           --stores the ps2 data word (both tx and rx)
  SIGNAL error            : STD_LOGIC;                               --validate parity, start, and stop bits for received data
  SIGNAL timer            : INTEGER RANGE 0 TO clk_freq/10000 := 0; --counter to determine both inhibit period and when PS/2 is idle
  SIGNAL bit_cnt          : INTEGER RANGE 0 TO 11 := 0;              --count the number of clock pulses during transmit
  
  --declare debounce component for debouncing PS2 input signals
  COMPONENT debounce IS
    GENERIC(
      counter_size  :  INTEGER); --debounce period (in seconds) = 2^counter_size/(clk freq in Hz)
    PORT(
      clk     : IN  STD_LOGIC;   --input clock
      button  : IN  STD_LOGIC;   --input signal to be debounced
      result  : OUT STD_LOGIC);  --debounced signal
  END COMPONENT;
BEGIN

  --synchronizer flip-flops
  PROCESS(clk)
  BEGIN
    IF(clk'EVENT AND clk = '1') THEN  --rising edge of system clock
      sync_ffs(0) <= ps2_clk;           --synchronize PS/2 clock signal
      sync_ffs(1) <= ps2_data;          --synchronize PS/2 data signal
    END IF;
  END PROCESS;

  --debounce PS2 input signals
  debounce_ps2_clk: debounce
    GENERIC MAP(counter_size => debounce_counter_size)
    PORT MAP(clk => clk, button => sync_ffs(0), result => ps2_clk_int);
  debounce_ps2_data: debounce
    GENERIC MAP(counter_size => debounce_counter_size)
    PORT MAP(clk => clk, button => sync_ffs(1), result => ps2_data_int);

  --verify that parity, start, and stop bits are all correct for received data
  error <= NOT (NOT ps2_word(0) AND ps2_word(10) AND (ps2_word(9) XOR ps2_word(8) XOR
        ps2_word(7) XOR ps2_word(6) XOR ps2_word(5) XOR ps2_word(4) XOR ps2_word(3) XOR 
        ps2_word(2) XOR ps2_word(1)));  

  --state machine to control transmit and receive processes
  PROCESS(clk, reset_n)
  BEGIN
    IF(reset_n = '0') THEN                  --reset PS/2 transceiver
      ps2_clk <= '0';                         --inhibit communication on PS/2 bus
      ps2_data <= 'Z';                        --release PS/2 data line
      tx_busy <= '1';                         --indicate that no transmit is in progress
      ack_error <= '0';                       --clear acknowledge error flag
      ps2_code <= (OTHERS => '0');            --clear received PS/2 code
      ps2_code_new <= '0';                    --clear new received PS/2 code flag
      rx_error <= '0';                        --clear receive error flag
      state <= receive;                       --set state machine to receive state
    ELSIF(clk'EVENT AND clk = '1') THEN       --rising edge of system clock
      ps2_clk_int_prev <= ps2_clk_int;        --store previous value of the PS/2 clock signal
      CASE state IS                           --implement state machine
      
        WHEN receive =>
          IF(tx_ena = '1') THEN                                  --transmit requested
            tx_busy <= '1';                                        --indicate transmit in progress
            timer <= 0;                                            --reset timer for inhibit timing
            ps2_word(9 DOWNTO 0) <= tx_cmd & '0';                  --load parity, command, and start bit into PS/2 data buffer
            bit_cnt <= 0;                                          --clear bit counter            
            state <= inhibit;                                      --inhibit communication to begin transaction
          ELSE                                                   --transmit not requested
            tx_busy <= '0';                                        --indicate no transmit in progress
            ps2_clk <= 'Z';                                        --release PS/2 clock port
            ps2_data <= 'Z';                                       --release PS/2 data port    
            --clock in receive data
            IF(ps2_clk_int_prev = '1' AND ps2_clk_int = '0') THEN  --falling edge of PS2 clock
              ps2_word <= ps2_data_int & ps2_word(10 DOWNTO 1);      --shift contents of PS/2 data buffer
            END IF;  
            --determine if PS/2 port is idle
            IF(ps2_clk_int = '0') THEN                             --low PS2 clock, PS/2 is active
              timer <= 0;                                            --reset idle counter
            ELSIF(timer < clk_freq/18000) THEN                    --PS2 clock has been high less than a half clock period (<55us)
              timer <= timer + 1;                                    --continue counting
            END IF;
            --output received data and port status          
            IF(timer = clk_freq/18000) THEN                       --idle threshold reached
              IF(error = '0') THEN                                   --no error detected
                ps2_code_new <= '1';                                   --set flag that new PS/2 code is available
                ps2_code <= ps2_word(8 DOWNTO 1);                      --output new PS/2 code
              ELSIF(error = '1') THEN                                --error detected
                rx_error <= '1';                                       --set receive error flag
              END IF;
            ELSE                                                   --PS/2 port active
              rx_error <= '0';                                       --clear receive error flag
              ps2_code_new <= '0';                                   --set flag that PS/2 transaction is in progress
            END IF;
            state <= receive;                                      --continue streaming receive transactions
          END IF;
        
        WHEN inhibit =>
          IF(timer < clk_freq/10000) THEN     --first 100us not complete
            timer <= timer + 1;                  --increment timer
            ps2_data <= 'Z';                     --release data port
            ps2_clk <= '0';                      --inhibit communication
            state <= inhibit;                    --continue inhibit
          ELSE                                 --100us complete
            ps2_data <= ps2_word(0);             --output start bit to PS/2 data port
            state <= transact;                   --proceed to send bits
          END IF;
          
        WHEN transact =>
          ps2_clk <= 'Z';                                       --release clock port
          IF(ps2_clk_int_prev = '1' AND ps2_clk_int = '0') THEN --falling edge of PS2 clock
            ps2_word <= ps2_data_int & ps2_word(10 DOWNTO 1);     --shift contents of PS/2 data buffer
            bit_cnt <= bit_cnt + 1;                               --count clock falling edges
          END IF;
          IF(bit_cnt < 10) THEN                                 --all bits not sent
            ps2_data <= ps2_word(0);                              --connect serial output of PS/2 data buffer to data port
          ELSE                                                  --all bits sent
            ps2_data <= 'Z';                                      --release data port
          END IF;
          IF(bit_cnt = 11) THEN                                 --acknowledge bit received
            ack_error <= ps2_data_int;                            --set error flag if acknowledge is not '0'
            state <= tx_complete;                                 --proceed to wait until the slave releases the bus
          ELSE                                                  --acknowledge bit not received
            state <= transact;                                    --continue transaction
          END IF;
        
        WHEN tx_complete =>
          IF(ps2_clk_int = '1' AND ps2_data_int = '1') THEN    --device has released the bus
            state <= receive;                                    --proceed to receive data state
          ELSE                                                 --bus not released by device
            state <= tx_complete;                                --wait for device to release bus                    
          END IF;
      
      END CASE;
    END IF;
  END PROCESS;
  
END logic;





LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

ENTITY debounce IS
  GENERIC(
    counter_size  :  INTEGER := 20); --counter size (19 bits gives 10.5ms with 50MHz clock)
  PORT(
    clk     : IN  STD_LOGIC;  --input clock
    button  : IN  STD_LOGIC;  --input signal to be debounced
    result  : OUT STD_LOGIC); --debounced signal
END debounce;

ARCHITECTURE logic OF debounce IS
  SIGNAL flipflops   : STD_LOGIC_VECTOR(1 DOWNTO 0); --input flip flops
  SIGNAL counter_set : STD_LOGIC;                    --sync reset to zero
  SIGNAL counter_out : STD_LOGIC_VECTOR(counter_size DOWNTO 0) := (OTHERS => '0'); --counter output
BEGIN

  counter_set <= flipflops(0) xor flipflops(1);   --determine when to start/reset counter
  
  PROCESS(clk)
  BEGIN
    IF(clk'EVENT and clk = '1') THEN
      flipflops(0) <= button;
      flipflops(1) <= flipflops(0);
      If(counter_set = '1') THEN                  --reset counter because input is changing
        counter_out <= (OTHERS => '0');
      ELSIF(counter_out(counter_size) = '0') THEN --stable input time is not yet met
        counter_out <= counter_out + 1;
      ELSE                                        --stable input time is met
        result <= flipflops(1);
      END IF;    
    END IF;
  END PROCESS;
END logic;

    
-----------------------------------------------DECODER MOUSE


LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY decoder_mouse IS
  PORT (
mouse_bytes: in std_logic_vector(23 downto 0);
  		new_data: in std_logic;
  		button_left: out std_logic;
  		button_right: out std_logic
    );
END decoder_mouse;

--------------------------------------------------------------------------------
--Complete your VHDL description below
--------------------------------------------------------------------------------

ARCHITECTURE TypeArchitecture OF decoder_mouse IS

BEGIN
process(new_data)
begin
	if new_data = '1' then
		button_left <= mouse_bytes(16);
		button_right <= mouse_bytes(17);
	end if;
end process;

END TypeArchitecture;





    

--------------------------------------------------------NUMARATOR SI SSD


LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY nr_si_ssd_complet IS
  PORT (
  		 CLK : in  STD_LOGIC;     
   		 Reset: in STD_LOGIC;
   		 left_button: in STD_LOGIC;
   		 right_button: in STD_LOGIC;
   		 reverse : in STD_LOGIC;
		 anozi : out std_logic_vector (7 downto 0);
		 catozi: out std_logic_vector (6 downto 0)
    );
END nr_si_ssd_complet;

--------------------------------------------------------------------------------
--Complete your VHDL description below
--------------------------------------------------------------------------------

ARCHITECTURE TypeArchitecture OF nr_si_ssd_complet IS
component lab5_divizor IS
  PORT (
 		CLK_100: in std_logic;
 		CLK_1HZ: out std_logic;
 		Reset: in std_logic
    );
END component;

component nr_bidirectional is
    Port ( CLK : in  STD_LOGIC;     
   		 Reset: in STD_LOGIC;
   		 left_button: in STD_LOGIC;
   		 right_button: in STD_LOGIC;
   		 reverse : in STD_LOGIC;
		 rez: out STD_LOGIC_VECTOR (7 downto 0)
           
          );    -- direction of counter (up or down)
end component;

component Binary_to_decimal_converter IS
  PORT (
 		Bin_num: in std_logic_vector(7 downto 0);
  		Unit: out std_logic_vector(3 downto 0);
  		Decimal: out std_logic_vector(3 downto 0)  
    );
END component;

component bcd_to_ssd IS
  PORT (	
unit, decimal: in std_logic_vector (3 downto 0);
  		unit_out, dec_out: out std_logic_vector (6 downto 0)
  		   );
END component;

component ssd is
    Port ( CLK_100: in std_logic;
    		 reset : in std_logic;
    		 zecimal : in std_logic_vector (6 downto 0);
    		 unitar: in std_logic_vector (6 downto 0);
    		 anozi: out std_logic_vector (7 downto 0);
    		 catozi: out std_logic_vector (6 downto 0)
    		 );
end component;

signal CLK_1: std_logic;
signal rez: std_logic_vector (7 downto 0);
signal unit: std_logic_vector(3 downto 0);
signal decimal: std_logic_vector(3 downto 0);
signal unit_out: std_logic_vector (6 downto 0);
signal dec_out: std_logic_vector (6 downto 0);
BEGIN
A: lab5_divizor port map(CLK, CLK_1, reset);
B: nr_bidirectional port map (clk_1, reset, left_button, right_button, reverse, rez);
C: binary_to_decimal_converter port map(rez, unit, decimal);
D: bcd_to_ssd port map(unit, decimal, unit_out, dec_out);
E: ssd port map(CLK, reset, dec_out, unit_out, anozi, catozi);

END TypeArchitecture;

----------------------------------------------------------NUMARATORUL

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity nr_bidirectional is
    Port ( CLK : in  STD_LOGIC;     
   		 Reset: in STD_LOGIC;
   		 left_button: in STD_LOGIC;
   		 right_button: in STD_LOGIC;
   		 reverse : in STD_LOGIC;
		 rez: out STD_LOGIC_VECTOR (7 downto 0)
           
          );    -- direction of counter (up or down)
end nr_bidirectional;

architecture Behavioral of nr_bidirectional is
begin
    process (clk, reverse)
    variable counter : std_logic_vector (7 downto 0) := (others => '0');
    begin
        if RISING_EDGE(CLK) then
        	if reset = '0' then
            if (reverse = '0') then
                if (counter /= "01100011" and left_button = '1' and right_button = '0') then
                	counter := counter + '1';
                elsif (counter /= "00000000" and right_button = '1' and left_button = '0') then
                	counter := counter - '1';
                end if;
            elsif (reverse = '1') then
               if (counter /= "00000000" and left_button = '1'  and right_button = '0') then
                	counter := counter - '1';  
               elsif (counter /= "01100011" and right_button = '1' and left_button = '0') then
                	counter := counter + '1';
                end if;
            end if;
        end if;
       end if;
        if reset = '1' then
        	counter := (others => '0');
        end if;
    rez <= counter; 
end process;
    
    
end Behavioral;

----------------------------------------------------BINARY TO DECIMAL CONVERTER

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;
USE ieee.std_logic_arith.all;

ENTITY Binary_to_decimal_converter IS
  PORT (
 		Bin_num: in std_logic_vector(7 downto 0);
  		Unit: out std_logic_vector(3 downto 0);
  		Decimal: out std_logic_vector(3 downto 0)  
    );
END Binary_to_decimal_converter;

--------------------------------------------------------------------------------
--Complete your VHDL description below
--------------------------------------------------------------------------------

ARCHITECTURE comport OF Binary_to_decimal_converter IS

signal num_int: integer:=0;
signal u,z: integer:=0;
BEGIN
	num_int<=conv_integer(bin_num);
	z<=num_int / 10;
	u<=num_int rem 10;
	process(u)
	begin
		case u is
		when 0 => unit <= x"0";
		when 1 => unit <= x"1";
		when 2 => unit <= x"2";
		when 3 => unit <= x"3";
		when 4 => unit <= x"4";
		when 5 => unit <= x"5";
		when 6 => unit <= x"6";
		when 7 => unit <= x"7";
		when 8 => unit <= x"8";
		when 9 => unit <= x"9";
		when others => unit <=x"f";
		end case;
	end process;
	process(z)
	begin
		case z is
		when 0 => Decimal <= x"0";
		when 1 => Decimal <= x"1";
		when 2 => Decimal <= x"2";
		when 3 => Decimal <= x"3";
		when 4 => Decimal <= x"4";
		when 5 => Decimal <= x"5";
		when 6 => Decimal <= x"6";
		when 7 => Decimal <= x"7";
		when 8 => Decimal <= x"8";
		when 9 => Decimal <= x"9";
		when others => Decimal <=x"f";
		end case;
	end process;
END comport;

-------------------------------------------------------------BCD TO SSD


LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY bcd_to_ssd IS
  PORT (	
unit, decimal: in std_logic_vector (3 downto 0);
  		unit_out, dec_out: out std_logic_vector (6 downto 0)
  		   );
END bcd_to_ssd;


ARCHITECTURE TypeArchitecture OF bcd_to_ssd IS

BEGIN
process(unit, decimal)
begin
	case decimal is
		    when "0000" => dec_out <= "0000001"; -- "0"     
            when "0001" => dec_out <= "1001111"; -- "1" 
            when "0010" => dec_out <= "0010010"; -- "2" 
            when "0011" => dec_out <= "0000110"; -- "3" 
            when "0100" => dec_out <= "1001100"; -- "4" 
            when "0101" => dec_out <= "0100100"; -- "5" 
            when "0110" => dec_out <= "0100000"; -- "6" 
            when "0111" => dec_out <= "0001111"; -- "7" 
            when "1000" => dec_out <= "0000000"; -- "8"     
            when "1001" => dec_out <= "0000100"; -- "9" 
            when others => dec_out <= "1111111";
	end case;
	case unit is
		    when "0000" => unit_out <= "0000001"; -- "0"     
            when "0001" => unit_out <= "1001111"; -- "1" 
            when "0010" => unit_out <= "0010010"; -- "2" 
            when "0011" => unit_out <= "0000110"; -- "3" 
            when "0100" => unit_out <= "1001100"; -- "4" 
            when "0101" => unit_out <= "0100100"; -- "5" 
            when "0110" => unit_out <= "0100000"; -- "6" 
            when "0111" => unit_out <= "0001111"; -- "7" 
            when "1000" => unit_out <= "0000000"; -- "8"     
            when "1001" => unit_out <= "0000100"; -- "9"
            when others => unit_out <= "1111111" ;
	end case;

end process;

END TypeArchitecture;

-----------------------------------------------------------SSD

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.all;
entity ssd is
    Port ( CLK_100: in std_logic;
    		 reset : in std_logic;
    		 zecimal : in std_logic_vector (6 downto 0);
    		 unitar: in std_logic_vector (6 downto 0);
    		 anozi: out std_logic_vector (7 downto 0);
    		 catozi: out std_logic_vector (6 downto 0)
    		 );
end ssd;



architecture Behavioral of ssd is

component mux_anozi is
	port(
		intrare3, intrare4 : in std_logic_vector (7 downto 0);
		sel: in std_logic_vector (1 downto 0);
		iesire: out std_logic_vector (7 downto 0)
	);
end component;

component mux_catozi is
	port(
		intrare3, intrare4 : in std_logic_vector (6 downto 0);
		sel: in std_logic_vector (1 downto 0);
		iesire: out std_logic_vector (6 downto 0)
	);
end component;

component nr_15 is
	port(CLK: in std_logic;
		Reset: in std_logic;
		iesire: out std_logic_vector (15 downto 0)
	);
end component;

signal delayer : std_logic_vector (15 downto 0);
begin

A: nr_15 port map (CLK_100, reset, delayer);
B: mux_catozi port map (zecimal,unitar,delayer(15 downto 14),catozi);
C: mux_anozi port map ("11111101", "11111110",delayer(15 downto 14),anozi);

end Behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.all;
entity mux_anozi is
	port(
		intrare3, intrare4 : in std_logic_vector (7 downto 0);
		sel: in std_logic_vector (1 downto 0);
		iesire: out std_logic_vector (7 downto 0)
	);
end entity;

architecture mux2_1_4 of mux_anozi is

begin
with sel select iesire<= intrare3 when "10",
					intrare4 when "11",
					"11111111" when others ;
	
end architecture;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.all;
entity mux_catozi is
	port(
		intrare3, intrare4 : in std_logic_vector (6 downto 0);
		sel: in std_logic_vector (1 downto 0);
		iesire: out std_logic_vector (6 downto 0)
	);
end entity;

architecture mux4_1_8 of mux_catozi is

begin
with sel select iesire<= intrare3 when "10",
					intrare4 when "11",
					"1111111" when others ;
	
end architecture;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.all;
entity nr_15 is
	port(CLK: in std_logic;
		Reset: in std_logic;
		iesire: out std_logic_vector (15 downto 0)
	);
end entity;

architecture arh_nr of nr_15 is
signal counter : std_logic_vector (15 downto 0) := (others => '0');
begin
process(CLK)

begin
	if Reset='1' then
		counter <= (others => '0');
	elsif RISING_EDGE(CLK) then
		counter <= counter+1;
	end if;
end process;
iesire<=counter;
end architecture;

-----------------------------------------DIVIZOR FRECVENTA

LIBRARY ieee;
USE ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
ENTITY lab5_divizor IS
  PORT (
 		CLK_100: in std_logic;
 		CLK_1HZ: out std_logic;
 		Reset: in std_logic
    );
END lab5_divizor;

--------------------------------------------------------------------------------
--Complete your VHDL description below
--------------------------------------------------------------------------------

ARCHITECTURE TypeArchitecture OF lab5_divizor IS

BEGIN

process(CLK_100, reset)
variable nr: std_logic_vector (25 downto 0) := (others => '0');
begin
	if reset = '1' then
		nr := (others => '0');
	elsif RISING_EDGE(CLK_100)  then
		nr := nr+1;
	end if;
	CLK_1HZ <= nr(25);
	
end process;

END TypeArchitecture;

