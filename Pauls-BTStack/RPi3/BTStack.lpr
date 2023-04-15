program BTStack;

{$mode delphi}{$H+}
{$hints off}
{$notes off}

uses
  RaspberryPi3,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Console, uTFTP, Winsock2, uLog,
  Ultibo, uBTStack
  { Add additional units here };

var
  Console1, Console2, Console3 : TWindowHandle;
  IPAddress : string;
  ch : char;

  uart_driver : Pbtstack_uart_block_t;
  transport : Phci_transport_t;
  config : hci_transport_config_uart_t;
  ms : cardinal;
  ts : Pbtstack_timer_source_t;
  hci_event_callback_registration : btstack_packet_callback_registration_t;

procedure Log1 (s : string);
begin
  ConsoleWindowWriteLn (Console1, s);
end;

procedure Log2 (s : string);
begin
  ConsoleWindowWriteLn (Console2, s);
end;

procedure Log3 (s : string);
begin
  ConsoleWindowWriteLn (Console3, s);
end;

procedure Msg2 (Sender : TObject; s : string);
begin
  Log2 ('TFTP : ' + s);
end;

function WaitForIPComplete : string;
var
  TCP : TWinsock2TCPClient;
begin
  TCP := TWinsock2TCPClient.Create;
  Result := TCP.LocalAddress;
  if (Result = '') or (Result = '0.0.0.0') or (Result = '255.255.255.255') then
    begin
      while (Result = '') or (Result = '0.0.0.0') or (Result = '255.255.255.255') do
        begin
          sleep (1000);
          Result := TCP.LocalAddress;
        end;
    end;
  TCP.Free;
end;

procedure WaitForSDDrive;
begin
  while not DirectoryExists ('C:\') do sleep (500);
end;

procedure packet_handler (packet_type : uint8_t; channel : uint16_t; packet : Puint8_t; size : uint16_t); cdecl;
begin
  log ('packet handler');
end;

procedure Init;
begin
  hci_dump_open ('hci_dump.pklg', HCI_DUMP_STDOUT);
  hci_dump_log (LOG_LEVEL_DEBUG, 'Initialising for Ultibo.');
  btstack_memory_init;
  uart_driver := btstack_uart_block_ultibo_instance;
  btstack_run_loop_init (btstack_run_loop_ultibo_get_instance);


  config.type_ := HCI_TRANSPORT_CONFIG_UART;
  config.baudrate_init := 115200;
  config.baudrate_main := 0;
  config.flowcontrol := 0;
  config.device_name := 'abc';

  transport := hci_transport_h4_instance (uart_driver);
  if transport = nil then Log ('Transport is nil') else Log ('Transport is ok');
  log ('Transport Name ' + transport^.name);
  //   const btstack_link_key_db_t * link_key_db = btstack_link_key_db_fs_instance();
	hci_init (transport, @config);

  hci_event_callback_registration.callback := @packet_handler;
  hci_add_event_handler (@hci_event_callback_registration);
  //  btstack_run_loop_execute;
end;

begin
  Console1 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_LEFT, true);
  Console2 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_TOPRIGHT, false);
  Console3 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_BOTTOMRIGHT, false);
  SetLogProc (@Log1);
  Log ('BTStack Tester.');
  Log ('');
  Log2 ('');
  WaitForSDDrive;
  IPAddress := WaitForIPComplete;
  Log2 ('TFTP : Usage tftp -i ' + IPAddress + ' put kernel7.img');
  SetOnMsg (@Msg2);
  Log2 ('');
  while true do
    begin
      if ConsoleGetKey (ch, nil) then
        case ch of
          '1' : MyTest;
          '2' : Init;
          '3' : hci_init (transport, nil);
          '4' :
            begin
              ms := btstack_run_loop_ultibo_get_instance^.GetTimeMS;
              Log ('ms ' + ms.ToString);
            end;
          '5' :
            begin
              New (ts);
              Log ('created.');
              btstack_run_loop_ultibo_get_instance^.SetTimer (ts, 3000);
              Log ('set timeout.');

              btstack_run_loop_ultibo_get_instance^.AddTimer (ts);
              Log ('added.');
            end;
           '6' : btstack_run_loop_ultibo_get_instance^.DumpTimer;
           '7' : btstack_run_loop_ultibo_get_instance^.RemoveTimer (ts);
          end;
    end;
  ThreadHalt (0);
end.



