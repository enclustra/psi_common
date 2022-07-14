------------------------------------------------------------------------------
--  Copyright (c) 2018 by Paul Scherrer Institute, Switzerland
--  All rights reserved.
--  Authors: Oliver Bruendler
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Description
------------------------------------------------------------------------------
-- This is a pure VHDL and vendor indpendent simple dual port RAM.

------------------------------------------------------------------------------
-- Libraries
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.psi_common_math_pkg.all;

------------------------------------------------------------------------------
-- Entity Declaration
------------------------------------------------------------------------------
entity psi_common_sdp_ram is
  generic(
    Depth_g     : positive := 1024;
    Width_g     : positive := 16;
    IsAsync_g   : boolean  := false;    -- True = Separate Rd clock is used (clk is WrClk in this case)
    RamStyle_g  : string   := "auto";   -- "auto", "distributed" or "block"
    Behavior_g  : string   := "RBW";    -- "RBW" = read-before-write, "WBR" = write-before-read
    RdLatency_g : positive := 1
  );
  port(
    -- Control Signals
    Clk    : in  std_logic                                        := '0';
    RdClk  : in  std_logic                                        := '0';
    -- Write Port
    WrAddr : in  std_logic_vector(log2ceil(Depth_g) - 1 downto 0) := (others => '0');
    Wr     : in  std_logic                                        := '0';
    WrData : in  std_logic_vector(Width_g - 1 downto 0)           := (others => '0');
    -- Read Port
    RdAddr : in  std_logic_vector(log2ceil(Depth_g) - 1 downto 0) := (others => '0');
    Rd     : in  std_logic                                        := '1';
    RdData : out std_logic_vector(Width_g - 1 downto 0)
  );
end entity;

------------------------------------------------------------------------------
-- Architecture Declaration
------------------------------------------------------------------------------
architecture rtl of psi_common_sdp_ram is

  -- memory array
  type data_t is array (natural range<>) of std_logic_vector(Width_g - 1 downto 0);
  shared variable mem : data_t(Depth_g - 1 downto 0) := (others => (others => '0'));
  attribute ram_style : string;
  attribute ram_style of mem : variable is RamStyle_g;
  
  signal rd_pipe      : data_t(1 to RdLatency_g);
  
  -- Prevent pipeline registers from being extracted into shift registers
  attribute shreg_extract : string;
  attribute shreg_extract of rd_pipe : signal is "no";
  
begin
  -- Synchronous Implementation
  g_sync : if not IsAsync_g generate
    ram_p : process(Clk)
    begin
      if rising_edge(Clk) then
        if Behavior_g = "RBW" then
          if Rd = '1' then
            rd_pipe(1) <= mem(to_integer(unsigned(RdAddr)));
          end if;
        end if;
        if Wr = '1' then
          mem(to_integer(unsigned(WrAddr))) := WrData;
        end if;
        if Behavior_g = "WBR" then
          if Rd = '1' then
            rd_pipe(1) <= mem(to_integer(unsigned(RdAddr)));
          end if;
        end if;
      
        -- Read-data pipeline registers
        rd_pipe(2 to RdLatency_g) <= rd_pipe(1 to RdLatency_g-1);
      end if;
    end process;
  end generate;

  -- Asynchronous implementation
  g_async : if IsAsync_g generate

    write_p : process(Clk)
    begin
      if rising_edge(Clk) then
        if Wr = '1' then
          mem(to_integer(unsigned(WrAddr))) := WrData;
        end if;
      end if;
    end process;

    read_p : process(RdClk)
    begin
      if rising_edge(RdClk) then
        if Rd = '1' then
          rd_pipe(1) <= mem(to_integer(unsigned(RdAddr)));
        end if;
        
        -- Read-data pipeline registers
        rd_pipe(2 to RdLatency_g) <= rd_pipe(1 to RdLatency_g-1);
      end if;
    end process;

  end generate;
  
  -- Output
  RdData <= rd_pipe(RdLatency_g);
  
end;
