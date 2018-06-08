unit uFPGA;

{$mode objfpc}{$H+}

interface

uses

  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  SysUtils,
  BCM2837,
  BCM2710,
  Threads,
  Classes,
  Console,
  Spi,
  Gpio,
  FileSystem,
  FATFS,
  MMC,
  Syscalls,
  Platform,
  Ultibo;








const

    RASPI_DIR = GPIO_PIN_20;
    RASPI_CLK = GPIO_PIN_8;
    RASPI_D0 = GPIO_PIN_16;
    RASPI_D1 = GPIO_PIN_19;
    RASPI_D2 = GPIO_PIN_17;
    RASPI_D3 = GPIO_PIN_5;
    RASPI_D4 = GPIO_PIN_6;
    RASPI_D5 = GPIO_PIN_23;
    RASPI_D6 = GPIO_PIN_24;
    RASPI_D7 = GPIO_PIN_18;
    RASPI_D8 = GPIO_PIN_7;


function initfpgagpio(): Boolean;
function SPISendFile2(const Filename: String; BlockSize: LongWord;Window:TWindowHandle): Boolean;
//function drclk(nr:integer; LBytes:PByte): integer;
function ProgFpga(const Filename: String; ProgWindow:TWindowHandle):Boolean;
function ReadFpga(ProgWindow:TWindowHandle):integer;
implementation
  function SPISendFile2(const Filename: String; BlockSize: LongWord;Window:TWindowHandle): Boolean;
var
  Size:LongWord;
  Remain:LongWord;
  Offset:PtrUInt;
  Count:LongWord = 0;
  SPIDevice: PSPIDevice;
  MemoryStream: TMemoryStream;
begin
  Result := False;

  //Check the file
  if not FileExists(Filename) then
    Exit;

  //Open the file
  MemoryStream := TMemoryStream.Create;
  try
   //Load the file
   MemoryStream.LoadFromFile(Filename);

   //Locate the SPI device (Adjust for boards other than Pi3)
   SPIDevice := SPIDeviceFindByDescription(BCM2710_SPI0_DESCRIPTION);

   if SPIDevice = nil then
     Exit;

   //Configure SPI Chip Select 0
   if SPIDeviceSetClockRate(SPIDevice ,SPI_CS_0, 1000000) <> ERROR_SUCCESS then
     Exit;

   //Start the SPI device
   if SPIDeviceStart(SPIDevice, SPI_MODE_4WIRE, 1000000, SPI_CLOCK_PHASE_LOW, SPI_CLOCK_POLARITY_LOW) <> ERROR_SUCCESS then
     Exit;

   //Send block size pieces to SPI device
   Remain := MemoryStream.Size;
   Offset := 0;
   while Remain > 0 do
   begin
     //Determine write size
     Size := BlockSize;
     if Size > Remain then Size := Remain;

     //Write the data (Note: You can also pass the flag SPI_TRANSFER_DMA to enable DMA transfers)
     if SPIDeviceWrite(SPIDevice, SPI_CS_0, Pointer(MemoryStream.Memory + Offset), Size, SPI_TRANSFER_NONE, Count) <> ERROR_SUCCESS then
       Exit;
     ConsoleWindowWriteLn(Window,'In SPISendFile2 '+InttoStr(Remain));
     //Update Remain and Offset
     Dec(Remain, Size);
     Inc(Offset, Size);
   end;

   //Close the SPI device
   SPIDeviceStop(SPIDevice);
   ConsoleWindowWriteLn(Window,'Setting True in SPISendFile2');
   Result := True;
  finally
    //Close File
    MemoryStream.Free;
  end;
end;
  function ProgFpga(const Filename: String; ProgWindow:TWindowHandle):Boolean;
  var
  fn:String;
  fpgaprog:Boolean;
  flg1:Boolean;
  const
      CDONE = GPIO_PIN_17;
      CRESET_B = GPIO_PIN_22;
      IOB_108_SS = GPIO_PIN_25;
   begin

     ConsoleWindowWriteLn(ProgWindow,'Program FPGA');
     Sleep(5000);

     fpgaprog:=False;
     {No pullup pins 17.22,and 25}

     GPIOPullSelect(CDONE,GPIO_PULL_NONE);
     GPIOPullSelect(CRESET_B,GPIO_PULL_NONE);
     GPIOPullSelect(IOB_108_SS,GPIO_PULL_NONE);

     {pin 17 set as input}
     {BCM 17 CDONE}

     GPIOFunctionSelect(CDONE,GPIO_FUNCTION_IN);

     {pins 15 & 22  set as output}
     {BCM 22 CRESET_B}

     GPIOFunctionSelect(CRESET_B,GPIO_FUNCTION_OUT);

     {BCM 25 R12 IOB_108_SS}

     GPIOFunctionSelect(GPIO_PIN_25,GPIO_FUNCTION_OUT);
     {BCM 25 R12 IOB_108_SS setting low}
     ConsoleWindowWriteLn(ProgWindow,'setting IOB_108_SS low');
     GPIOOutputSet(IOB_108_SS,GPIO_LEVEL_LOW);
     ConsoleWindowWriteLn(ProgWindow,'IOB_108_SS '+ inttostr(GPIOInputGet(IOB_108_SS)));
     {Resetting FPGA}
     ConsoleWindowWriteLn(ProgWindow,'Resetting FPGA');
     ConsoleWindowWriteLn(ProgWindow,'Setting Reset low');
     GPIOOutputSet(GPIO_PIN_22,GPIO_LEVEL_LOW);
     ConsoleWindowWriteLn(ProgWindow,'Reset '+ inttostr(GPIOInputGet(GPIO_PIN_22)));

     Sleep(1000);
     ConsoleWindowWriteLn(ProgWindow,'Setting Reset high');
     GPIOOutputSet(GPIO_PIN_22,GPIO_LEVEL_HIGH);
     ConsoleWindowWriteLn(ProgWindow,'Reset '+ inttostr(GPIOInputGet(GPIO_PIN_22)));

     ConsoleWindowWriteLn(ProgWindow,'CDONE '+ inttostr(GPIOInputGet(GPIO_PIN_17)));



     ConsoleWindowWriteLn(ProgWindow,'Sending to SPI ' + Filename);
      flg1:=SPISendFile2(Filename,4096,ProgWindow);
      if (flg1 )  then ConsoleWindowWriteLn(ProgWindow,'True returned from SPI wr '+Filename);


      Fn:='sixzeros.bin';
      flg1:=SPISendFile2(Fn,6,ProgWindow);
      if (flg1 )  then ConsoleWindowWriteLn(ProgWindow,'True returned from SPI wr '+Fn);

      ConsoleWindowWriteLn(ProgWindow,'CDONE '+ inttostr(GPIOInputGet(CDONE)));

      {BCM 25 R12 IOB_108_SS setting high}
      ConsoleWindowWriteLn(ProgWindow,'setting IOB_108_SS high');
      GPIOOutputSet(IOB_108_SS,GPIO_LEVEL_HIGH);
      ConsoleWindowWriteLn(ProgWindow,'IOB_108_SS '+ inttostr(GPIOInputGet(IOB_108_SS)));
      ConsoleWindowWriteLn(ProgWindow,'CDONE '+ inttostr(GPIOInputGet(CDONE)));
      fpgaprog:=True;
      Sleep(5000);
      Result := fpgaprog;
   end;

 function initfpgagpio(): Boolean;


begin
  Result := False;

  GPIOPullSelect(RASPI_D0,GPIO_PULL_NONE);
  GPIOPullSelect(RASPI_D1,GPIO_PULL_NONE);
  GPIOPullSelect(RASPI_D2,GPIO_PULL_NONE);
  GPIOPullSelect(RASPI_D3,GPIO_PULL_NONE);
  GPIOPullSelect(RASPI_D4,GPIO_PULL_NONE);
  GPIOPullSelect(RASPI_D5,GPIO_PULL_NONE);
  GPIOPullSelect(RASPI_D6,GPIO_PULL_NONE);
  GPIOPullSelect(RASPI_D7,GPIO_PULL_NONE);
  GPIOPullSelect(RASPI_D8,GPIO_PULL_NONE);


  Result := True;

end;

 function ReadFpga(ProgWindow:TWindowHandle):integer;
 var
   datab:byte;
   Stream: TStream;
   ff:Boolean;
   flg1:Boolean;
   flgmain:Boolean;
   nr:integer;
   line:String;

 begin
   flg1:=initfpgagpio();

   //ConsoleWindowClear(ProgWindow);
   Stream := TStream.Create;
   ff:=True;
   flgmain:=True;
   GPIOPullSelect(RASPI_CLK, GPIO_PULL_NONE);
   GPIOPullSelect(RASPI_DIR, GPIO_PULL_NONE);

   GPIOFunctionSelect(RASPI_CLK, GPIO_FUNCTION_OUT);
   GPIOFunctionSelect(RASPI_DIR, GPIO_FUNCTION_OUT);




   GPIOFunctionSelect(RASPI_D0,GPIO_FUNCTION_IN);
   GPIOFunctionSelect(RASPI_D1,GPIO_FUNCTION_IN);
   GPIOFunctionSelect(RASPI_D2,GPIO_FUNCTION_IN);
   GPIOFunctionSelect(RASPI_D3,GPIO_FUNCTION_IN);
   GPIOFunctionSelect(RASPI_D4,GPIO_FUNCTION_IN);
   GPIOFunctionSelect(RASPI_D5,GPIO_FUNCTION_IN);
   GPIOFunctionSelect(RASPI_D6,GPIO_FUNCTION_IN);
   GPIOFunctionSelect(RASPI_D7,GPIO_FUNCTION_IN);
   GPIOFunctionSelect(RASPI_D8,GPIO_FUNCTION_IN);

   while flgmain do
     begin
   nr:=0;
   ff:=True;
   line:='';
   while (ff)
   do
   begin
     //ConsoleWindowWriteLn(DemoUDPListener.FWindowHandle,'Setting dir hi clk lo');
     GPIOOutputSet(RASPI_DIR, GPIO_LEVEL_HIGH);
     GPIOOutputSet(RASPI_CLK, GPIO_LEVEL_LOW);

     //ConsoleWindowWriteLn(DemoUDPListener.FWindowHandle,'Setting dir lo');
     GPIOOutputSet(RASPI_DIR,GPIO_LEVEL_LOW);
     //GPIOOutputSet(RASPI_CLK,GPIO_LEVEL_LOw);

     while( GPIOInputGet(RASPI_D8) <> 0)
       do
         begin
        end;
     GPIOOutputSet(RASPI_CLK,GPIO_LEVEL_HIGH);
       //ConsoleWindowWriteLn(DemoUDPListener.FWindowHandle,'Setting clk hi');
       //GPIOOutputSet(RASPI_DIR,GPIO_LEVEL_HIGH);
     //while( GPIOInputGet(RASPI_D8) = 0)
     //do
     //begin

     datab := 0;

       //while( GPIOInputGet(RASPI_D8) = 0)
       //do
     datab :=  GPIOInputGet(RASPI_D7) << 7;
     datab :=  datab + GPIOInputGet(RASPI_D6) << 6;
     datab :=  datab + GPIOInputGet(RASPI_D5) << 5;
     datab :=  datab + GPIOInputGet(RASPI_D4) << 4;
     datab :=  datab + GPIOInputGet(RASPI_D3) << 3;
     datab :=  datab + GPIOInputGet(RASPI_D2) << 2;
     datab :=  datab + GPIOInputGet(RASPI_D1) << 1;
     datab :=  datab + GPIOInputGet(RASPI_D0);


     GPIOOutputSet(RASPI_CLK,GPIO_LEVEL_LOW);

     Inc(nr);
       if (datab = 255) then
         begin
           ff:=False;

           GPIOOutputSet(RASPI_DIR, GPIO_LEVEL_LOW);
           GPIOOutputSet(RASPI_CLK, GPIO_LEVEL_HIGH);
         end;
       if(datab<>13) then
        begin
         line+=chr(datab);
        end;
       //ConsoleWindowWrite(DemoUDPListener.FWindowHandle,chr(datab));
       if(datab=13) then
        begin
         ConsoleWindowWriteln(ProgWindow,line);
         LoggingOutput(line);
         line:='';
        end;
       //Stream.WriteByte(datab);


   end;
   flgmain:=False;

 end;
   Result:=nr;
 end;
end.


