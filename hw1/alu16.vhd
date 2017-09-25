-- Wai Hon Wong
-- CpE 5120
-- Assignment 1

--------------------------
-- 16-Bit ALU
--------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_arith.unsigned;
use IEEE.numeric_std.unsigned;

PACKAGE ALUPackage is
	type Operation is (ADD, SUB, MPY, CMP, AND16, OR16, XOR16, NOT16);
	type Relation is (LESS, EQUAL, GREATER);
end PACKAGE ALUPackage;

Package body ALUPackage is
end package body ALUPackage;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_arith.unsigned;
use IEEE.numeric_std.unsigned;
use work.ALUPackage.all;
	
entity ALU_16 is
port(
	A_bus, B_bus: in std_logic_vector(15 downto 0):=(others => '0');
	Result_bus: inout std_logic_vector(31 downto 0):=(others => '0');
	Opcode: in Operation;
	clk, reset: in std_logic;
	Status_code: out std_logic_vector (2 downto 0));
end ALU_16;

architecture ALU_Behavior of ALU_16 is
Signal overflow: std_logic := '0';
Signal cmp_relation: Relation;
Signal A, B, Cin: std_logic_vector (16 downto 0):=(others => '0');
Signal Temp_output: std_logic_vector (31 downto 0):=(others => '0');

begin
A <= '0' &A_bus;
B <= '0' &B_bus;

ALU_Behavioral_Execution: process(clk, reset, Opcode)
	begin
		if (reset = '0' and clk'event and clk = '1') then
			case Opcode is
				when ADD =>
				-- Not sure why it doesn't work
					Temp_output(31 downto 17) <= "000000000000000";
					Temp_output(16 downto 0) <= (A + B);
					
				when SUB =>
					Temp_output(31 downto 17) <= "000000000000000";
					Temp_output(16 downto 0) <= (A - B);
					overflow <= Temp_output(16);
				
				when MPY =>
					Temp_output(16) <= '0';
					Temp_output <= A(15 downto 0)*B(15 downto 0);
					overflow <= '0';
				
				when CMP =>
					if (A<B) then cmp_relation <= LESS;
					elsif (A=B) then cmp_relation <= EQUAL;
					else cmp_relation <= GREATER;
					end if;
				
				when AND16 =>
					Temp_output(31 downto 17) <= "000000000000000";
					Temp_output(16 downto 0) <= A and B;
					overflow <= '0';
				
				when OR16 =>
					Temp_output(31 downto 17) <= "000000000000000";
					Temp_output(16 downto 0) <= A or B;
					overflow <= '0';
					
				when NOT16 =>
					Temp_output(31 downto 16) <= "0000000000000000";
					Temp_output(15 downto 0) <= not A(15 downto 0);
					overflow <= '0';
				
				when XOR16 =>
					Temp_output(31 downto 17) <= "000000000000000";
					Temp_output(16 downto 0) <= A xor B;
					overflow <= '0';
					
			end case;
		end if;
		
		Result_bus <= Temp_output;
	
	case cmp_relation is
		when EQUAL => Status_code <= "000";
		when LESS => Status_code <= "001";
		when GREATER => Status_code <= "010";
		when others => Status_code <= "011";
	end case;
	Status_code(2) <= overflow;
	
	end process;
end ALU_Behavior;

--------------------------
-- RTL
--------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_arith.unsigned;
use IEEE.numeric_std.unsigned;

PACKAGE ALUPackage is
	type Operation is (ADD, SUB, MPY, CMP, AND16, OR16, XOR16, NOT16);
	type Relation is (LESS, EQUAL, GREATER);
end PACKAGE ALUPackage;

Package body ALUPackage is
end package body ALUPackage;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_arith.unsigned;
use IEEE.numeric_std.unsigned;
use work.ALUPackage.all;

entity ALU_RTL is
port(
	A_bus, B_bus: in std_logic_vector(15 downto 0):=(others => '0');
	Result_bus: inout std_logic_vector(31 downto 0):=(others => '0');
	Opcode: in Operation;
	clk, reset: in std_logic;
	Status_code: out std_logic_vector (2 downto 0));
end ALU_RTL;

architecture ALU_RTL_Imp of ALU_RTL is
Signal overflow: std_logic := '0';
Signal cmp_relation: Relation;
Signal B: std_logic_vector (15 downto 0):=(others => '0');
Signal Cin: std_logic_vector (16 downto 0):=(others => '0');
Signal Temp_output: std_logic_vector (31 downto 0):=(others => '0');

begin
	ALU_RTL_Execution: process(clk, reset, Opcode)
	begin
	if (reset = '0' and clk'event and clk = '1') then
		case Opcode is
		when ADD =>
			Temp_output(31 downto 16) <= "0000000000000000";
			Cin(0) <= '1';
			for i in 0 to 15 loop
				Temp_output(i) <= A_bus(i) XOR B_bus(i) XOR Cin(i);
					Cin(i) <= ((A_bus(i) and B_bus(i)) or (A_bus(i) and Cin(i)) or (B_bus(i) and Cin(i)));
			end loop;
			overflow <= Cin(16);
			
		when SUB =>
		-- Does not work
			B <= not B_bus;
			Cin(0) <= '1';
			for i in 0 to 15 loop
				Temp_output(i) <= A_bus(i) XOR B(i) XOR Cin(i);
					Cin(i) <= ((A_bus(i) and B(i)) or (A_bus(i) and Cin(i)) or (B(i) and Cin(i)));
			end loop;
			
		when MPY =>
		
			--Multiplication did not work
		
		when CMP =>
			if (A_bus<B_bus) then cmp_relation <= LESS;
				elsif (A_bus=B_bus) then cmp_relation <= EQUAL;
					else cmp_relation <= GREATER;
			end if;
			overflow <= '0';
			
		when AND16 =>
			Temp_output(31 downto 16) <= "0000000000000000";
			for i in 0 to 15 loop
				Temp_output(i) <= A_bus(i) and B_bus(i);
			end loop;
			overflow <= '0';
		
		when OR16 =>
			Temp_output(31 downto 16) <= "0000000000000000";
			for i in 0 to 15 loop
				Temp_output(i) <= A_bus(i) or B_bus(i);
			end loop;
			overflow <= '0';
		
		when XOR16 =>
			Temp_output(31 downto 16) <= "0000000000000000";
			for i in 0 to 15 loop
				Temp_output(i) <= A_bus(i) xor B_bus(i);
			end loop;
			overflow <= '0';
		
		when NOT16 =>
			Temp_output(31 downto 16) <= "0000000000000000";
			for i in 0 to 15 loop
				Temp_output(i) <= not A_bus(i);
			end loop;
			overflow <= '0';

		end case;
	end if;
	
	Result_bus <= Temp_output;
	
	case cmp_relation is
		when EQUAL => Status_code <= "000";
		when LESS => Status_code <= "001";
		when GREATER => Status_code <= "010";
		when others => Status_code <= "011";
	end case;
	Status_code(2) <= overflow;
	
	end process;
end architecture ALU_RTL_Imp;

--------------------------
-- Testbench
--------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_arith.unsigned;
use IEEE.numeric_std.unsigned;

PACKAGE ALUPackage is
	type Operation is (ADD, SUB, MPY, CMP, AND16, OR16, XOR16, NOT16);
	type Relation is (LESS, EQUAL, GREATER);
end PACKAGE ALUPackage;

Package body ALUPackage is
end package body ALUPackage;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_arith.unsigned;
use IEEE.numeric_std.unsigned;
use work.ALUPackage.all;

entity test is
end entity test;

architecture ALU_testbench of test is

component ALU_RTL
port(A_bus, B_bus: in std_logic_vector(15 downto 0):=(others => '0');
	Result_bus: inout std_logic_vector(31 downto 0):=(others => '0');
	Opcode: in Operation;
	clk, reset: in std_logic;
	Status_code: out std_logic_vector (2 downto 0));
end component ALU_RTL;

component ALU_16
port(A_bus, B_bus: in std_logic_vector(15 downto 0):=(others => '0');
	Result_bus: inout std_logic_vector(31 downto 0):=(others => '0');
	Opcode: in Operation;
	clk, reset: in std_logic;
	Status_code: out std_logic_vector (2 downto 0));
end component ALU_16;

Signal T_A, T_B: std_logic_vector (15 downto 0);
Signal RTL_Result, Behavior_Result: std_logic_vector(31 downto 0);
Signal T_clk, T_reset: std_logic;
Signal RTL_Status_code: std_logic_vector(2 downto 0);
Signal Behavior_Status_code: std_logic_vector(2 downto 0);
Signal T_opcode: Operation;

begin
	ALU_Test:ALU_16 port map(T_A, T_B, Behavior_Result, T_opcode, T_clk, T_reset, Behavior_Status_code);
	RTL_Test:ALU_RTL port map(T_A, T_B, RTL_Result, T_opcode, T_clk, T_reset, RTL_Status_code);

	CLK_Process: process
		begin
			T_clk <= '1';
			wait for 5 ps;
			T_clk <= '0';
			wait for 5 ps;
		end process;
	
	T_reset <= '1', '0' after 10 ps;
	
	Stimulate: Process
	begin
	
	--- Test #1 ---
	
		T_A <= "0000000000000000";
		T_B <= "0000000000000000";
		
		T_opcode <= ADD;
		wait for 10 ps;
		
		T_opcode <= SUB;
		wait for 10 ps;
		
		T_opcode <= MPY;
		wait for 10 ps;
		
		T_opcode <= CMP;
		wait for 10 ps;
		
		T_opcode <= AND16;
		wait for 10 ps;
		
		T_opcode <= OR16;
		wait for 10 ps;
		
		T_opcode <= NOT16;
		wait for 10 ps;
		
		T_opcode <= XOR16;
		wait for 10 ps;
		
	--- Test #2 --- 
	
		T_A <= "0000000100100100"; --292
		T_B <= "0000000001001000"; --72
	
		T_opcode <= ADD;
		wait for 10 ps;
		
		T_opcode <= SUB;
		wait for 10 ps;
		
		T_opcode <= MPY;
		wait for 10 ps;
		
		T_opcode <= CMP;
		wait for 10 ps;
		
		T_opcode <= AND16;
		wait for 10 ps;
		
		T_opcode <= OR16;
		wait for 10 ps;
		
		T_opcode <= NOT16;
		wait for 10 ps;
		
		T_opcode <= XOR16;
		wait for 10 ps;
	
	--- Test #3 --- 
	
		T_A <= "0001000101100100"; --4452
		T_B <= "0000010001001000"; --1096
	
		T_opcode <= ADD;
		wait for 10 ps;
		
		T_opcode <= SUB;
		wait for 10 ps;
		
		T_opcode <= MPY;
		wait for 10 ps;
		
		T_opcode <= CMP;
		wait for 10 ps;
		
		T_opcode <= AND16;
		wait for 10 ps;
		
		T_opcode <= OR16;
		wait for 10 ps;
		
		T_opcode <= NOT16;
		wait for 10 ps;
		
		T_opcode <= XOR16;
		wait for 10 ps;

	--- Test #4 --- 
	
		T_A <= "0011010100100101"; --13605
		T_B <= "1000000001001000"; --32840
	
		T_opcode <= ADD;
		wait for 10 ps;
		
		T_opcode <= SUB;
		wait for 10 ps;
		
		T_opcode <= MPY;
		wait for 10 ps;
		
		T_opcode <= CMP;
		wait for 10 ps;
		
		T_opcode <= AND16;
		wait for 10 ps;
		
		T_opcode <= OR16;
		wait for 10 ps;
		
		T_opcode <= NOT16;
		wait for 10 ps;
		
		T_opcode <= XOR16;
		wait for 10 ps;
		
	--- Test #5 --- 
	
		T_A <= "1111111111111111"; --65535
		T_B <= "1111111111111111"; --65535
	
		T_opcode <= ADD;
		wait for 10 ps;
		
		T_opcode <= SUB;
		wait for 10 ps;
		
		T_opcode <= MPY;
		wait for 10 ps;
		
		T_opcode <= CMP;
		wait for 10 ps;
		
		T_opcode <= AND16;
		wait for 10 ps;
		
		T_opcode <= OR16;
		wait for 10 ps;
		
		T_opcode <= NOT16;
		wait for 10 ps;
		
		T_opcode <= XOR16;
		wait for 10 ps;
		
	end Process;

end architecture ALU_testbench;
	
		