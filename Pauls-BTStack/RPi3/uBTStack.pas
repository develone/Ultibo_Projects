unit uBTStack;

{$mode delphi}{$H+}

(* BTStack
 * Copyright (C) 2014 BlueKitchen GmbH
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 * 4. Any redistribution, use, or modification is done solely for
 *    personal benefit and not for any commercial purpose or for
 *    monetary gain.
 *
 * THIS SOFTWARE IS PROVIDED BY BLUEKITCHEN GMBH AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL MATTHIAS
 * RINGWALD OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * Please inquire about commercial licensing options at
 * contact@bluekitchen-gmbh.com
 *
 * Ultibo adaption pjde 2018
 *)

interface

uses
  Classes, SysUtils, SysCalls, GlobalTypes;

{$linklib btstack}

const
  LOG_LEVEL_DEBUG                          = 0;
  LOG_LEVEL_INFO                           = 1;
  LOG_LEVEL_ERROR                          = 2;

  HCI_DUMP_BLUEZ                           = 0;
  HCI_DUMP_PACKETLOGGER                    = 1;
  HCI_DUMP_STDOUT                          = 2;

  BTSTACK_UART_SLEEP_OFF                   = 0; // UART active, sleep off
  BTSTACK_UART_SLEEP_RTS_HIGH_WAKE_ON_CTS_PULSE = 1; // used for eHCILL
  BTSTACK_UART_SLEEP_RTS_LOW_WAKE_ON_RX_EDGE = 2; // used for H5 and for eHCILL without support for wake on CTS pulse

  BTSTACK_UART_SLEEP_MASK_RTS_HIGH_WAKE_ON_CTS_PULSE = 1 shl BTSTACK_UART_SLEEP_RTS_HIGH_WAKE_ON_CTS_PULSE;
  BTSTACK_UART_SLEEP_MASK_RTS_LOW_WAKE_ON_RX_EDGE = 1 shl BTSTACK_UART_SLEEP_RTS_LOW_WAKE_ON_RX_EDGE;

  DATA_SOURCE_CALLBACK_POLL                = 1 shl 0;
  DATA_SOURCE_CALLBACK_READ                = 1 shl 1;
  DATA_SOURCE_CALLBACK_WRITE               = 1 shl 2;

  HCI_TRANSPORT_CONFIG_UART                = 0;
  HCI_TRANSPORT_CONFIG_USB                 = 1;

type
  uint8_t                                  = uint8;
  uint16_t                                 = uint16;
  uint32_t                                 = uint32;

  btstack_uart_sleep_mode_t                = LongWord; // enum
  hci_dump_format_t                        = LongWord; // enum
  btstack_uart_sleep_mode_mask_t           = LongWord; // enum
  btstack_data_source_callback_type_t      = LongWord; // enum
  hci_transport_config_type_t              = LongWord; // enum

  Puint8_t                                 = ^uint8_t;
  Puint16_t                                = ^uint16_t;
  Puint32_t                                = ^uint32_t;

  Phci_transport_t                         = ^hci_transport_t;
  Pbtstack_run_loop_t                      = ^btstack_run_loop_t;
  Pbtstack_uart_config_t                   = ^btstack_uart_config_t;
  Pbtstack_uart_block_t                    = ^btstack_uart_block_t;
  Pbtstack_data_source_t                   = ^btstack_data_source_t;
  Pbtstack_timer_source_t                  = ^btstack_timer_source_t;
  Pbtstack_linked_item_t                   = ^btstack_linked_item_t;
  btstack_linked_list_t                    = ^btstack_linked_item_t;
  Pbtstack_linked_list_t                   = ^btstack_linked_list_t;
  Pbtstack_linked_list_iterator_t          = ^btstack_linked_list_iterator_t;
  Pbtstack_packet_callback_registration_t  = ^btstack_packet_callback_registration_t;

  {$PACKRECORDS C}

  btstack_uart_config_t = record
    baudrate : uint32_t;
    flowcontrol : integer;
    device_name : PChar;
  end;

  hci_transport_config_uart_t = record
    type_ : hci_transport_config_type_t;  // = HCI_TRANSPORT_CONFIG_UART
    baudrate_init : uint32_t; // initial baud rate
    baudrate_main : uint32_t; // = 0: same as initial baudrate
    flowcontrol : integer;   //
    device_name : PChar;
  end;

  // uart block functions
  TBlockHandler                            = procedure; cdecl;
  TUartInitFunc                            = function (const uart_config : Pbtstack_uart_config_t) : integer; cdecl;
  TUartOpenFunc                            = function : integer; cdecl;
  TUartCloseFunc                           = function : integer; cdecl;
  TReceivedFunc                            = procedure (block_handler : TBlockHandler); cdecl;
  TSentFunc                                = procedure (block_handler : TBlockHandler); cdecl;
  TBaudRateFunc                            = procedure (baudrate : uint32_t); cdecl;
  TParityFunc                              = procedure (parity : integer); cdecl;
  TFlowControlFunc                         = procedure (flowcontrol : integer); cdecl;
  TReceiveBlockFunc                        = procedure (buffer : Puint8_t; len : uint16_t); cdecl;
  TSendBlockFunc                           = procedure (const buffer : Puint8_t; length : uint16_t); cdecl;
  TSleepModesFunc                          = function : Pinteger; cdecl;
  TSetSleepFunc                            = procedure (sleep_mode : btstack_uart_sleep_mode_t); cdecl;
  TWakeUpHandlerFunc                       = procedure; cdecl;
  TWakeUpFunc                              = procedure (wakeup_handler : TWakeUpHandlerFunc); cdecl;

  btstack_uart_block_t = record       // size 52 bytes
    Init : TUartInitFunc;
    Open : TUartOpenFunc;
    Close : TUartCloseFunc;
    SetBlockReceived : TReceivedFunc;
    SetBlockSent : TSentFunc;
    SetBaudRate : TBaudRateFunc;
    SetParity : TParityFunc;
    SetFlowControl : TFlowControlFunc;
    ReceiveBlock : TReceiveBlockFunc;
    SendBlock : TSendBlockFunc;
    GetSupportedSleepModes : TSleepModesFunc;
    SetSleep : TSetSleepFunc;
    SetWakeUpHandler : TWakeUpFunc;
  end;

  btstack_linked_item_t = record      // size 4 bytes
    next : Pbtstack_linked_item_t;    // <-- next element in list, or NULL
  end;

  btstack_linked_list_iterator_t = record
	  advance_on_next : integer;
    prev : Pbtstack_linked_item_t;	  // points to the item before the current one
    curr : Pbtstack_linked_item_t;	  // points to the current item (to detect item removal)
  end;

  TDataProcessFunc                         = procedure (ds : Pbtstack_data_source_t; callback_type : btstack_data_source_callback_type_t); cdecl;

  btstack_data_source_t = record      // size 16 bytes
    item : btstack_linked_item_t;   	// linked item
    handle : HANDLE;                  // handle on windows
    Process : TDataProcessFunc;       // callback to call for enabled callback types
    flags : uint16_t;                 // flags storing enabled callback types
  end;

  TTimerProcessFunc = procedure (ts : Pbtstack_timer_source_t); cdecl;

  btstack_timer_source_t = record     // size 16 bytes
    item : btstack_linked_item_t;
    timeout : uint32_t;               // timeout in system ticks (HAVE_EMBEDDED_TICK) or milliseconds (HAVE_EMBEDDED_TIME_MS)
    Process : TTimerProcessFunc;      // will be called when timer fired
    context : pointer;
  end;

  TLoopInitFunc                            = procedure; cdecl;
  TAddDataSourceFunc                       = procedure (data_source : Pbtstack_data_source_t); cdecl;
  TRemoveDataSourceFunc                    = function (data_source : Pbtstack_data_source_t) : integer; cdecl;
  TEnableDataSourceCallbacksFunc           = procedure (data_source : Pbtstack_data_source_t; callbacks : uint16_t); cdecl;
  TDisableDataSourceCallbacksFunc          = procedure (data_source : Pbtstack_data_source_t; callbacks : uint16_t); cdecl;
  TSetTimerFunc                            = procedure (timer : Pbtstack_timer_source_t; timeout_in_ms : uint32_t); cdecl;
  TAddTimerFunc                            = procedure (timer : Pbtstack_timer_source_t); cdecl;
  TRemoveTimerFunc                         = function (timer : Pbtstack_timer_source_t) : integer; cdecl;
  TLoopExecuteFunc                         = procedure; cdecl;
  TDumpTimerFunc                           = procedure; cdecl;
  TGetTimeMSFunc                           = function : uint32_t; cdecl;

  btstack_run_loop_t = record         // size 44 bytes
    Init : TLoopInitFunc;
    AddDataSource : TAddDataSourceFunc;
    RemoveDataSource : TRemoveDataSourceFunc;
    EnableDataSourceCallbacks : TEnableDataSourceCallbacksFunc;
    DisableDataSourceCallbacks : TDisableDataSourceCallbacksFunc;
    SetTimer : TSetTimerFunc;
    AddTimer : TAddTimerFunc;
    RemoveTimer : TRemoveTimerFunc;
    Execute : TLoopExecuteFunc;
    DumpTimer : TDumpTimerFunc;
    GetTimeMS : TGetTimeMSFunc;
  end;

 { hci_transport_t = record            // size 40 bytes
    stuffing : array [0..39] of byte;
  end;    }

  TTransInitFunc = procedure (transport_config : pointer); cdecl;
  TTransOpenFunc = procedure; cdecl;
  TTransCloseFunc = procedure; cdecl;
  TTransHandlerFunc = procedure (packet_type : uint8_t; packet : Puint8_t; size : uint16_t); cdecl;
  TTransRegHandlerFunc = procedure (handler : TTransHandlerFunc); cdecl;
  TTransCanSendFunc = function (packet_type : uint8_t) : integer; cdecl;
  TTransSendPacketFunc = function (packet_type : uint8_t; packet : Puint8_t; size : integer) : integer; cdecl;
  TTransSetBaudRateFunc = function (baudrate : uint32_t) : integer; cdecl;
  TTransResetLinkFunc = procedure; cdecl;
  TTransSetSCOConfigFunc = procedure (voice_setting : uint16_t; num_connections : integer); cdecl;

  hci_transport_t = record
    name : PChar;
    Init : TTransInitFunc;
    Open : TTransOpenFunc;
    Close : TTranscloseFunc;
    register_packet_handler : TTransRegHandlerFunc;
    CanSendPacketNow : TTransCanSendFunc;
    SendPacket : TTransSendPacketFunc;
    SetBaudRate : TTransSetBaudRateFunc;
    ResetLink : TTransResetLinkFunc;
    SetSCOConfig : TTransSetSCOConfigFunc;
  end;

 btstack_packet_handler_t = procedure (packet_type : uint8_t; channel :uint16_t; packet : Puint8_t; size : uint16_t); cdecl;

// packet callback supporting multiple registrations
  btstack_packet_callback_registration_t = record
    item : btstack_linked_item_t;
    callback : btstack_packet_handler_t;
  end;


// BTStack APIs

procedure btstack_memory_init; cdecl; external;

procedure btstack_run_loop_init (const run_loop : Pbtstack_run_loop_t); cdecl; external;
procedure btstack_run_loop_execute; cdecl; external;

function hci_transport_h4_instance (const uart_driver : Pbtstack_uart_block_t) : Phci_transport_t; cdecl; external;
(*procedure hci_transport_h4_init (const transport_config : pointer); cdecl; external;
function hci_transport_h4_open : integer; cdecl; external;
function hci_transport_h4_close : integer; cdecl; external;   *)

procedure hci_dump_open (const filename : PChar; formt : hci_dump_format_t); cdecl; external;
procedure hci_dump_set_max_packets (packets : integer); cdecl; external;  // -1 for unlimited
procedure hci_dump_packet (packet_type : uint8_t; in_ : uint8_t; packet : Puint8_t; len : uint16_t); cdecl; external;
procedure hci_dump_log (log_level : integer; const format_ : PChar); cdecl; varargs; external;
procedure hci_dump_enable_log_level (log_level : integer; enable : integer); cdecl; external;
procedure hci_dump_close; cdecl; external;

function btstack_linked_list_empty (list : Pbtstack_linked_list_t) : integer; cdecl; external;
procedure btstack_linked_list_add (list : Pbtstack_linked_list_t; item : Pbtstack_linked_item_t); cdecl; external;
procedure btstack_linked_list_add_tail (list : Pbtstack_linked_list_t; item : Pbtstack_linked_item_t); cdecl; external;
function btstack_linked_list_pop (list : Pbtstack_linked_list_t) : Pbtstack_linked_item_t; cdecl; external;
function btstack_linked_list_remove (list : Pbtstack_linked_list_t; item : Pbtstack_linked_item_t) : integer; cdecl; external;
function btstack_linked_list_get_first_item (list : Pbtstack_linked_list_t) : Pbtstack_linked_item_t; cdecl; external;
function btstack_linked_list_get_last_item (list : Pbtstack_linked_list_t) : Pbtstack_linked_item_t; cdecl; external;
function btstack_linked_list_count (list : Pbtstack_linked_list_t) : integer; cdecl; external;
procedure btstack_linked_list_iterator_init (it : Pbtstack_linked_list_iterator_t; list : Pbtstack_linked_list_t); cdecl; external;
function btstack_linked_list_iterator_has_next (it : Pbtstack_linked_list_iterator_t) : integer; cdecl; external;
function btstack_linked_list_iterator_next (it : Pbtstack_linked_list_iterator_t) : Pbtstack_linked_item_t; cdecl; external;
procedure btstack_linked_list_iterator_remove (it : Pbtstack_linked_list_iterator_t); cdecl; external;

procedure hci_init (const transport : Phci_transport_t; const config : pointer); cdecl; external;

procedure hci_close; cdecl; external;
procedure hci_add_event_handler (callback_handler : Pbtstack_packet_callback_registration_t); cdecl; external;
procedure hci_register_acl_packet_handler (handler : btstack_packet_handler_t); cdecl; external;
procedure hci_register_sco_packet_handler (handler : btstack_packet_handler_t); cdecl; external;

// my functions
procedure MyTest; cdecl; external;

// ultibo specific
function btstack_uart_block_ultibo_instance : Pbtstack_uart_block_t; cdecl;
function btstack_run_loop_ultibo_get_instance : Pbtstack_run_loop_t; cdecl;

implementation

uses
  uLog, Ultibo, GlobalConst;

var
  btstack_uart_ultibo : btstack_uart_block_t;
  btstack_run_loop_ultibo : btstack_run_loop_t;
  data_sources_modified : boolean = false;
  timers : btstack_linked_list_t = nil;
  data_sources : btstack_linked_list_t = nil;
  start_time : ULARGE_INTEGER;

function btstack_uart_block_ultibo_instance : Pbtstack_uart_block_t; cdecl;
begin
  Result := @btstack_uart_ultibo;
end;

function btstack_uart_ultibo_init (const uart_config : Pbtstack_uart_config_t) : integer; cdecl;
begin
  Log ('UART Initialising');
  Result := 0;
end;

function btstack_uart_ultibo_open : integer; cdecl;
begin
  Log ('open');
  Result := 0;
end;

function btstack_uart_ultibo_close : integer; cdecl;
begin
  Log ('close');
  Result := 0;
end;

procedure btstack_uart_ultibo_setbaudrate (baudrate : uint32_t); cdecl;
begin
//
  Log ('set baud rate');
end;

procedure btstack_uart_ultibo_setparity (parity : integer); cdecl;
begin
//
end;

procedure btstack_uart_ultibo_setflowcontrol (flowcontrol : integer); cdecl;
begin
//
end;

// ultibo run loop based of windows run loop
function btstack_run_loop_ultibo_get_instance : Pbtstack_run_loop_t; cdecl;
begin
  Result := @btstack_run_loop_ultibo;
end;

procedure btstack_run_loop_ultibo_init; cdecl;
var
  file_time : FILETIME;
  system_time : SYSTEMTIME;
begin
  Log ('Ultibo Run Loop init');
  data_sources := nil;
  timers := nil;
  GetSystemTime (system_time);   // store start time
  SystemTimeToFileTime (system_time, file_time);
  start_time.LowPart := file_time.dwLowDateTime;
  start_time.HighPart := file_time.dwHighDateTime;
end;

procedure btstack_run_loop_ultibo_dump_timer; cdecl;
var
  it : Pbtstack_linked_item_t;
  ts : Pbtstack_timer_source_t;
  i : integer;
begin
  i := 0;
  it := Pbtstack_linked_item_t (timers);
  while it <> nil do
    begin
      ts := Pbtstack_timer_source_t (it);
      log (format ('timer %u, timeout %u', [i, ts^.timeout]));
      it := it^.next;
      i := i + 1;
    end;
end;

procedure btstack_run_loop_ultibo_add_data_source (ds : Pbtstack_data_source_t); cdecl;
begin
  data_sources_modified := true;
  // log_info("btstack_run_loop_ultibo_add_data_source %x with fd %u\n", (int) ds, ds->fd);
  btstack_linked_list_add (@data_sources, Pbtstack_linked_item_t (ds));
end;

function btstack_run_loop_ultibo_remove_data_source (ds : Pbtstack_data_source_t) : integer; cdecl;
begin
  data_sources_modified := true;
    // log_info("btstack_run_loop_ultibo_remove_data_source %x\n", (int) ds);
  Result := btstack_linked_list_remove (@data_sources, Pbtstack_linked_item_t (ds));
end;

procedure btstack_run_loop_ultibo_add_timer (ts : Pbtstack_timer_source_t); cdecl;
var
  it : Pbtstack_linked_item_t;
  next : Pbtstack_timer_source_t;
begin
  log ('add timer');
  it := Pbtstack_linked_item_t (@timers);
  while it^.next <> nil do
    begin
      next := Pbtstack_timer_source_t (it^.next);
      if next = ts then
        begin
          log ('btstack_run_loop_timer_add error: timer to add already in list!');
          exit;
        end;
      if next^.timeout > ts^.timeout then break;
      it := it^.next;
    end;
  ts^.item.next := it^.next;
  it^.next := Pbtstack_linked_item_t (ts);
  log (format ('Added timer %p at %u', [ts, ts^.timeout]));
  // btstack_run_loop_ultibo_dump_timer;
end;

function btstack_run_loop_ultibo_remove_timer (ts : Pbtstack_timer_source_t) : integer; cdecl;
begin
  // log_info("Removed timer %x at %u\n", (int) ts, (unsigned int) ts->timeout.tv_sec);
  // btstack_run_loop_ultibo_dump_timer();
  Result := btstack_linked_list_remove (@timers, Pbtstack_linked_item_t (ts));
end;

procedure btstack_run_loop_ultibo_enable_data_source_callbacks (ds : Pbtstack_data_source_t; callback_types : uint16_t); cdecl;
begin
  ds^.flags := ds^.flags or callback_types;
end;

procedure btstack_run_loop_ultibo_disable_data_source_callbacks (ds : Pbtstack_data_source_t; callback_types : uint16_t); cdecl;
begin
  ds^.flags := ds^.flags and (not callback_types);
end;

function btstack_run_loop_ultibo_get_time_ms : uint32_t; cdecl;
var
  file_time : FILETIME;
  system_time : SYSTEMTIME;
  now_time : ULARGE_INTEGER;
  time_ms : uint32_t;
begin
  GetSystemTime (system_time);
  SystemTimeToFileTime (system_time, file_time);
  now_time.LowPart := file_time.dwLowDateTime;
  now_time.HighPart := file_time.dwHighDateTime;
  time_ms := (now_time.QuadPart - start_time.QuadPart) div 10000;
  log (format ('btstack_run_loop_ultibo_get_time_ms: %u', [time_ms]));
  Result := time_ms;
end;

procedure btstack_run_loop_ultibo_set_timer (a : Pbtstack_timer_source_t; timeout_in_ms : uint32_t); cdecl;
var
  time_ms : uint32_t;
begin
  time_ms := btstack_run_loop_ultibo_get_time_ms;
  a^.timeout := time_ms + timeout_in_ms;
  log (format ('btstack_run_loop_ultibo_set_timer to %u ms (now %u, timeout %u)', [a^.timeout, time_ms, timeout_in_ms]));
end;

procedure btstack_run_loop_ultibo_execute; cdecl;
var
  ts : Pbtstack_timer_source_t;
  ds : Pbtstack_data_source_t;
  it : btstack_linked_list_iterator_t;
  handles : array [0..99] of HANDLE;
  num_handles : integer;
  timeout_ms : uint32_t;
  now_ms : uint32_t;
  res : integer;
  triggered_handle : HANDLE;
begin
  while true do
    begin
      // collect handles to wait for
      FillChar (handles, sizeof (handles), 0);
      num_handles := 0;
      btstack_linked_list_iterator_init (@it, @data_sources);
      while btstack_linked_list_iterator_has_next (@it) <> 0 do
        begin
          ds := Pbtstack_data_source_t (btstack_linked_list_iterator_next (@it));
          if ds^.handle = 0 then continue;
          if (ds^.flags and (DATA_SOURCE_CALLBACK_READ or DATA_SOURCE_CALLBACK_WRITE)) > 0 then
            begin
              handles[num_handles] := ds^.handle;
              num_handles := num_handles + 1;
              log (format ('btstack_run_loop_execute adding handle %p', [ds^.handle]));
            end;
        end;

      // get next timeout
      timeout_ms := INFINITE;

      if timers <> nil then
        begin
          ts := Pbtstack_timer_source_t (timers);
          now_ms := btstack_run_loop_ultibo_get_time_ms;
          timeout_ms := ts^.timeout - now_ms;
       //   if timeout_ms < 0 then timeout_ms := 0;
          log (format ('btstack_run_loop_execute next timeout in %u ms', [timeout_ms]));
        end;

      if num_handles > 0 then
        // wait for ready Events or timeout
        res := WaitForMultipleObjects (num_handles, @handles[0], false, timeout_ms)
      else
        begin
          // just wait for timeout
          Sleep (timeout_ms);
          res := WAIT_TIMEOUT;
        end;

      // process data source
      if (WAIT_OBJECT_0 <= res) and (res < (WAIT_OBJECT_0 + num_handles)) then
        begin
          triggered_handle := handles[res - WAIT_OBJECT_0];
          btstack_linked_list_iterator_init (@it, @data_sources);
          while btstack_linked_list_iterator_has_next (@it) <> 0 do
            begin
              ds := Pbtstack_data_source_t (btstack_linked_list_iterator_next (@it));
              log (format ('btstack_run_loop_ultibo_execute: check ds %p with handle %p', [ds, ds^.handle]));
              if triggered_handle = ds^.handle then
                begin
                  if ds^.flags and DATA_SOURCE_CALLBACK_READ > 0 then
                    begin
                      log (format ('btstack_run_loop_ultibo_execute: process read ds %p with handle %p', [ds, ds^.handle]));
                      if assigned (ds^.Process) then ds^.Process (ds, DATA_SOURCE_CALLBACK_READ);
                    end
                  else if ds^.flags and DATA_SOURCE_CALLBACK_WRITE > 0 then
                    begin
                      log (format ('btstack_run_loop_ultibo_execute: process write ds %p with handle %p', [ds, ds^.handle]));
                      if assigned (ds^.Process) then ds^.Process (ds, DATA_SOURCE_CALLBACK_WRITE);
                    end;
                  break;
              end;
            end;
        end;

      // process timers
      now_ms := btstack_run_loop_ultibo_get_time_ms;
      while timers <> nil do
        begin
          ts := Pbtstack_timer_source_t (timers);
          if (ts^.timeout > now_ms) then break;
          log (format ('btstack_run_loop_ultibo_execute: process timer %p', [ts]));
          // remove timer before processing it to allow handler to re-register with run loop
          btstack_run_loop_ultibo_remove_timer (ts);
          if Assigned (ts^.Process) then ts^.Process (ts);
        end;
    end;
end;

initialization

  btstack_uart_ultibo.Init := btstack_uart_ultibo_init;
  btstack_uart_ultibo.Open := btstack_uart_ultibo_open;
  btstack_uart_ultibo.Close := btstack_uart_ultibo_close;
  btstack_uart_ultibo.SetBlockReceived := nil;
  btstack_uart_ultibo.SetBlockSent := nil;
  btstack_uart_ultibo.SetBaudRate := btstack_uart_ultibo_setbaudrate;
  btstack_uart_ultibo.SetParity := btstack_uart_ultibo_setparity;
  btstack_uart_ultibo.SetFlowControl := btstack_uart_ultibo_setflowcontrol;
  btstack_uart_ultibo.ReceiveBlock := nil;
  btstack_uart_ultibo.SendBlock := nil;
  btstack_uart_ultibo.GetSupportedSleepModes := nil;
  btstack_uart_ultibo.SetSleep := nil;
  btstack_uart_ultibo.SetWakeupHandler := nil;

  btstack_run_loop_ultibo.Init := btstack_run_loop_ultibo_init;
  btstack_run_loop_ultibo.AddDataSource := btstack_run_loop_ultibo_add_data_source;
  btstack_run_loop_ultibo.RemoveDataSource := btstack_run_loop_ultibo_remove_data_source;
  btstack_run_loop_ultibo.EnableDataSourceCallbacks := btstack_run_loop_ultibo_enable_data_source_callbacks;
  btstack_run_loop_ultibo.DisableDataSourceCallbacks := btstack_run_loop_ultibo_disable_data_source_callbacks;
  btstack_run_loop_ultibo.SetTimer := btstack_run_loop_ultibo_set_timer;
  btstack_run_loop_ultibo.AddTimer := btstack_run_loop_ultibo_add_timer;
  btstack_run_loop_ultibo.RemoveTimer := btstack_run_loop_ultibo_remove_timer;
  btstack_run_loop_ultibo.Execute := btstack_run_loop_ultibo_execute;
  btstack_run_loop_ultibo.DumpTimer := btstack_run_loop_ultibo_dump_timer;
  btstack_run_loop_ultibo.GetTimeMS := btstack_run_loop_ultibo_get_time_ms;

end.

