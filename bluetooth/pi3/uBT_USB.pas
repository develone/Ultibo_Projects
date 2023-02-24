{


Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

References
==========

BT_USB Devices
=============

 This unit provides both the BT_USB device interface and the generic USB BT_USB BT_USB driver.

USB BT_USB Devices
=================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit uBT_USB;

interface

uses GlobalConfig, GlobalConst, GlobalTypes, Platform, Threads, Devices, USB, SysUtils, uBT;

{==============================================================================}
{Global definitions}
{--$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
  {BT_USB specific constants}
  BT_USB_NAME_PREFIX = 'BT_USB';    {Name prefix for BT_USB Devices}

  {BT_USB Buffer Size}
  USB_BT_EVENT_MAX_PACKET_SIZE              = 512;

  {BT_USB logging}
  BT_USB_LOG_LEVEL_DEBUG          = LOG_LEVEL_DEBUG;  {BT_USB debugging messages}
  BT_USB_LOG_LEVEL_INFO           = LOG_LEVEL_INFO;   {BT_USB informational messages, such as a device being attached or detached}
  BT_USB_LOG_LEVEL_ERROR          = LOG_LEVEL_ERROR;  {BT_USB error messages}
  BT_USB_LOG_LEVEL_NONE           = LOG_LEVEL_NONE;   {No BT_USB messages}

  ny : array [boolean] of string = ('NO', 'YES');

var 
  BT_USB_DEFAULT_LOG_LEVEL : LongWord = BT_USB_LOG_LEVEL_DEBUG;
 
var 
  BT_USB_LOG_ENABLED : Boolean;
  Report_Size : Longword = 5;

const
  BT_USB_DRIVER_NAME                 = 'BT (USB) Driver'; {Name of USB BT driver}
  BT_USB_DESCRIPTION                 = 'BT (USB)';        {Description of USB BT device}

type

  {BT_USB Device}
  PBT_USBDevice = ^TBT_USBDevice;
 
  {BT_USB Enumeration Callback}
  TBT_USBEnumerate = function (BT_USB : PBT_USBDevice; Data : Pointer) : LongWord;
  {BT_USB Notification Callback}
  TBT_USBNotification = function (Device : PDevice; Data : Pointer; Notification : LongWord) : LongWord;

  TBT_USBDevice = record
    BT : TBTDevice;                         // bt device workings
    Parent : PUSBDevice;                    // Hub attached to
    HubPort : byte;                         // Port on Hub
    EventRequest,
    BulkInRequest : PUSBRequest;            // USB request for BT_USB event data via Interrupt IN report data
    EventEP,                                // event endpoint
    BulkInEP,
    BulkOutEP : PUSBEndpointDescriptor;     // bulk transfer endpoints
//    WaiterThread : TThreadId;               // Thread waiting for pending requests to complete
    EventRemaining, EventPos : LongWord;    // number of bytes still remaining of event
    EventResponse : array of byte;
  end;
 
procedure BT_USBInit;

function BT_USBDriverBind (Device : PUSBDevice; Interrface : PUSBInterface) : LongWord;
function BT_USBDriverUnbind (Device : PUSBDevice; Interrface : PUSBInterface) : LongWord;

function BT_USBDeviceCommand (BT : PBTDevice; Buffer : Pointer; Size : Longword; var Count : Longword) : LongWord;
function BT_USBDeviceEvent (BT : PBTDevice; Buffer : Pointer; Size  : LongWord; var Count : LongWord) : LongWord;

implementation

uses uLog;

var
  BT_USBInitialized : Boolean = false;
  BT_USBDriver : PUSBDriver;  {Generic BT_USB Driver}

procedure BT_USBInit;
var
  Status : LongWord;
begin

  if BT_USBInitialized then Exit;
  BT_USBDriver := USBDriverCreate;
  if BT_USBDriver <> nil then
    begin
      BT_USBDriver.Driver.DriverName := BT_USB_DRIVER_NAME;
      BT_USBDriver.DriverBind := BT_USBDriverBind;
      BT_USBDriver.DriverUnbind := BT_USBDriverUnbind;
      Status := USBDriverRegister (BT_USBDriver);
      if Status <> USB_STATUS_SUCCESS then
        begin
          Log ('Failed to register generic BT_USB driver: ' + USBStatusToString (Status));
        end;
    end
  else
    begin
      Log ('BT_USB: Failed to create generic BT_USB driver');
    end;
  BT_USBInitialized := True;
end;

function BT_USBDeviceCommand (BT : PBTDevice; Buffer : Pointer; Size : Longword; var Count : Longword) : LongWord;
var
  res : LongWord;
begin
  Result := USB_STATUS_INVALID_PARAMETER;
  if BT = nil then exit;
  count := 0;
  res := USBControlTransfer (PUSBDevice (BT.Device.DeviceData),         // usb device
                             nil,                                       // endpoint 0
                             $00,                                       // class specific reuqest for this device
                             $20,                                       // request type
                             $00,                                       // value
                             $00,                                       // index
                             Buffer,                                    // command buffer
                             Size,                                      // command buffer length
                             Count,                                     // return count
                             5000);                                     // timeout 5 secs
  if (res <> USB_STATUS_SUCCESS) then
    log ('Command Result ' + USBStatusToString (res) + ' Return ' + count.ToString);
  Result := res; // USB_STATUS_SUCCESS;
end;

function BT_USBDeviceEvent (BT : PBTDevice; Buffer : Pointer; Size  : LongWord; var Count : LongWord) : LongWord;
begin
  log ('device event');
  Result := USB_STATUS_SUCCESS;
end;

procedure EventReportWorker (Request : PUSBRequest);
var
  res : LongWord;
  BT : PBT_USBDevice;
  b : array of byte;
//  s : string;
//  i : integer;
  count : integer;
begin
  if Request = nil then exit;
  BT := PBT_USBDevice (Request.DriverData);
//  Log ('report actual size ' + Request.ActualSize.ToString);
  //Log ('report size ' + Request.CurrentData .ActualSize.ToString);
  if BT <> nil then
    begin
      if MutexLock (BT.BT.Lock) = ERROR_SUCCESS then
        begin
          try
            if BT.BT.BTState = BT_STATE_DETACHING then
              Request.Status := USB_STATUS_DEVICE_DETACHED;
            if (Request.Status = USB_STATUS_SUCCESS) then
              begin
                b := Request.Data;
                if (BT.EventRemaining = 0)  then // new packet
                  begin
                    if (Request.ActualSize > 1) then
                      begin
                        BT.EventRemaining := b[1] + 2 - Request.ActualSize;
                        SetLength (BT.EventResponse, b[1] + 2);
                        if Request.ActualSize <= length (BT.EventResponse) then
                          Move (Request.Data^, BT.EventResponse[0], Request.ActualSize)
                        else
                          begin
                            BTLogError (@BT.BT, 'Packet Size Error.');
                          end;
                        BT.EventPos := Request.ActualSize;
                      end
                    else
                      begin
                        BTLogError (@BT.BT, 'Too short a response.');
                      end;
                  end
                else
                  begin
                    BT.EventRemaining := BT.EventRemaining - Request.ActualSize;
                    if BT.EventPos + Request.ActualSize <= length (BT.EventResponse) then
                      Move (Request.Data^, BT.EventResponse[BT.EventPos], Request.ActualSize)
                    else
                      begin
                        BTLogError (@BT.BT, 'Packet Size Error.');
                      end;
                    BT.EventPos := BT.EventPos + Request.ActualSize;
                  end;
                if (BT.EventRemaining = 0) and (Assigned (BT.BT.DeviceEvent)) then
                  begin
                    res := BT.BT.DeviceEvent (@BT.BT, @BT.EventResponse[0], length (BT.EventResponse), count);
                    if res <> ERROR_SUCCESS then
                      begin
                        log ('error device event ' + res.ToString);

                      end;
                 (*   s := '';
                    for i := 0 to length (BT.EventResponse) - 1  do
                      s := s + ' ' + BT.EventResponse[i].ToHexString (2);
                    Log ('<-- ' + s); *)
                  end;
              end;
          finally
            MutexUnlock (BT.BT.Lock);
          end;
        end;
    end;
  res := USBRequestSubmit (Request);
  if res <> USB_STATUS_SUCCESS then Log ('resubmit ' + USBStatusTostring (res));
end;

procedure EventReportComplete (Request : PUSBRequest);
begin
  if Request = nil then Exit;
  WorkerSchedule (0, TWorkerTask (EventReportWorker), Request, nil);
end;

procedure BulkInReportWorker (Request : PUSBRequest);
var
  res : LongWord;
  BT : PBT_USBDevice;
//  b : array of byte;
 // s : string;
 // i : integer;
 // count : integer;
begin
//  Log ('bulk in worker.');
  if Request = nil then exit;
  BT := PBT_USBDevice (Request.DriverData);
  Log ('bulk report actual size ' + Request.ActualSize.ToString);
 // Log ('bulk report size ' + Request.CurrentData .ActualSize.ToString);
  if BT <> nil then
    begin
   (*   if MutexLock (BT.BT.Lock) = ERROR_SUCCESS then
        begin
          try
            if BT.BT.BTState = BT_STATE_DETACHING then
              Request.Status := USB_STATUS_DEVICE_DETACHED;
            if (Request.Status = USB_STATUS_SUCCESS) then
              begin
                b := Request.Data;
                if (BT.EventRemaining = 0)  then // new packet
                  begin
                    if (Request.ActualSize > 1) then
                      begin
                        BT.EventRemaining := b[1] + 2 - Request.ActualSize;
                        SetLength (BT.EventResponse, b[1] + 2);
                        if Request.ActualSize <= length (BT.EventResponse) then
                          Move (Request.Data^, BT.EventResponse[0], Request.ActualSize)
                        else
                          begin
                            BTLogError (@BT.BT, 'Packet Size Error.');
                          end;
                        BT.EventPos := Request.ActualSize;
                      end
                    else
                      begin
                        BTLogError (@BT.BT, 'Too short a response.');
                      end;
                  end
                else
                  begin
                    BT.EventRemaining := BT.EventRemaining - Request.ActualSize;
                 //     Log ('event pos ' + BT.EventPos.ToString + ' actual size ' + Request.ActualSize.ToString + ' length ' + length (BT.EventResponse).ToString);
                    if BT.EventPos + Request.ActualSize <= length (BT.EventResponse) then
                      Move (Request.Data^, BT.EventResponse[BT.EventPos], Request.ActualSize)
                    else
                      begin
                        BTLogError (@BT.BT, 'Packet Size Error.');
                      end;
                    BT.EventPos := BT.EventPos + Request.ActualSize;
                  end;
//                Log ('remaining ' + BT.eventremaining.tostring);
                if (BT.EventRemaining = 0) and (Assigned (BT.BT.DeviceEvent)) then
                  begin
                    res := BT.BT.DeviceEvent (@BT.BT, @BT.EventResponse[0], length (BT.EventResponse), count);
                    if res <> ERROR_SUCCESS then
                      begin
                        log ('error device event ' + res.ToString);

                      end;
                  //  else
                  //    BT.BT.QueueEvent.SetEvent;

                    s := '';
                    for i := 0 to length (BT.EventResponse) - 1  do
                      s := s + ' ' + BT.EventResponse[i].ToHexString (2);
                    Log ('Event Response ' + s);
               ( *     i := 6;
                    s := '';
                    while (BT.EventResponse[i] <> 0) do
                      begin
                        s := s + chr (BT.EventResponse[i]);
                        i := i + 1;
                      end;
                    Log (s);        * )
                  end;
              end;
          finally
            MutexUnlock (BT.BT.Lock);
          end;
        end;     *)
    end;
  res := USBRequestSubmit (Request);
  if res <> USB_STATUS_SUCCESS then Log ('resubmit ' + USBStatusTostring (res));
end;

procedure BulkInReportComplete (Request : PUSBRequest);
begin
  if Request = nil then Exit;
  WorkerSchedule (0, TWorkerTask (BulkInReportWorker), Request, nil);
end;

function BT_USBDriverBind (Device : PUSBDevice; Interrface : PUSBInterface) : LongWord;
var
  i : integer;
  EventEP, BulkInEP, BulkOutEP : PUSBEndpointDescriptor;
  anInterface : PUSBInterface;
  BT : PBT_USBDevice;
  Status : LongWord;
begin
  Result := USB_STATUS_DEVICE_UNSUPPORTED;
  if Device = nil then exit;
  if Interrface <> nil then exit;
  if not ((Device.Descriptor.bDeviceClass = USB_CLASS_CODE_WIRELESS_CONTROLLER) and // wireless controller
     (Device.Descriptor.bDeviceSubClass = $01) and                                  // rf controller
     (Device.Descriptor.bDeviceProtocol = $01)) then exit;                          // bluetooth primary controller
  Log ('USB device : VID ' + Device.Descriptor.idVendor.ToHexString (4) + ' PID '
        + Device.Descriptor.idProduct.ToHexString (4));
  Log (Device.Manufacturer + ' ' + Device.Product + ' SNo. ' + Device^.SerialNumber);
  Log ('binding');
  EventEP := nil; BulkInEP := nil; BulkOutEP := nil;
  for i := low (Device.Configuration.Interfaces) to high (Device.Configuration.Interfaces) do
    begin
      anInterface := Device.Configuration.Interfaces[i];
      if EventEP = nil then EventEP := USBDeviceFindEndpointByType (Device, anInterface, USB_DIRECTION_IN, USB_TRANSFER_TYPE_INTERRUPT);
      if BulkInEP = nil then BulkInEP := USBDeviceFindEndpointByType (Device, anInterface, USB_DIRECTION_IN, USB_TRANSFER_TYPE_BULK);
      if BulkOutEP = nil then BulkOutEP := USBDeviceFindEndpointByType (Device, anInterface, USB_DIRECTION_OUT, USB_TRANSFER_TYPE_BULK);
      if (EventEP <> nil) and (BulkInEP <> nil) and (BulkOutEP <> nil) then break; // found them all
    end;
  BT := PBT_USBDevice (BTDeviceCreateEx (SizeOf (TBT_USBDevice)));
  if BT = nil then exit;
  BT.BT.Device.DeviceBus := DEVICE_BUS_USB;
  BT.BT.Device.DeviceType := BT_TYPE_USB; //  as opposed to BT_TYPE_UART or others
  BT.BT.Device.DeviceData := Device;            // are this and below the same
  BT.BT.DeviceCommand := BT_USBDeviceCommand;
  BT.BT.DeviceEvent := BTDeviceEvent;
  Device.Device.DeviceData := BT;         // check this
//  if @bt.BT.Device.DeviceData = @Device.Device.DeviceData then
//    log ('both device datas are the same')
 // else
 //    log ('device datas are different');

  BT.BT.Device.DeviceDescription := BT_USB_DESCRIPTION;
  BT.BT.BTState := BT_STATE_ATTACHING;
  BT.EventEP := EventEP;
  BT.BulkInEP := BulkInEP;
  BT.BulkOutEP := BulkOutEP;
//  BT.WaiterThread := INVALID_HANDLE_VALUE;
  BT.EventRequest := USBRequestAllocate (Device, EventEP, EventReportComplete, USB_BT_EVENT_MAX_PACKET_SIZE, BT);
  if BT.EventRequest = nil then
    begin
      BTDeviceDestroy (@BT.BT);
      Result := USB_STATUS_DEVICE_UNSUPPORTED;
      exit;
    end;
  BT.BulkInRequest := USBRequestAllocate (Device, BulkInEP, BulkInReportComplete, USB_BT_EVENT_MAX_PACKET_SIZE, BT);
  if BT.BulkInRequest = nil then
    begin
      BTDeviceDestroy (@BT.BT);
      Result := USB_STATUS_DEVICE_UNSUPPORTED;
      exit;
    end;

  BT.BT.BTId := DEVICE_ID_ANY;
  if BTDeviceRegister (@BT.BT) <> ERROR_SUCCESS then
    begin
      USBRequestRelease (BT.EventRequest);
      BTDeviceDestroy (@BT.BT);
      Result := USB_STATUS_DEVICE_UNSUPPORTED;
      exit;
    end;
  Status := USBRequestSubmit (BT.EventRequest);
  if Status <> USB_STATUS_SUCCESS then
    begin
      USBRequestRelease (BT.EventRequest);
      BTDeviceDeregister (@BT.BT);
      BTDeviceDestroy (@BT.BT);
      Result := Status;
      exit;
    end;
  Status := USBRequestSubmit (BT.BulkInRequest);
  if Status <> USB_STATUS_SUCCESS then
    begin
      USBRequestRelease (BT.EventRequest);
      BTDeviceDeregister (@BT.BT);
      BTDeviceDestroy (@BT.BT);
      Result := Status;
      exit;
    end;
  if BTDeviceSetState (@BT.BT, BT_STATE_ATTACHED) <> ERROR_SUCCESS then exit;
  Result := USB_STATUS_SUCCESS;
  Log ('bound');
  Log ('');
end;
 
function BT_USBDriverUnbind (Device : PUSBDevice; Interrface : PUSBInterface) : LongWord;
var
  BT : PBT_USBDevice;
begin
   Result := USB_STATUS_INVALID_PARAMETER;
   if Device = nil then exit;
   if Interrface <> nil then exit;
    if Device.Driver <> BT_USBDriver then Exit;
   Log ('unbinding');
   BT := PBT_USBDevice (Device.Device.DeviceData);
   if BT = nil then exit;
   if BT.BT.Device.Signature <> DEVICE_SIGNATURE then exit;
   Result := USB_STATUS_OPERATION_FAILED;
   if BTDeviceSetState (@BT.BT, BT_STATE_DETACHING) <> ERROR_SUCCESS then exit;
   if MutexLock (BT.BT.Lock) <> ERROR_SUCCESS then Exit;
   USBRequestCancel (BT.EventRequest);
   MutexUnlock (BT.BT.Lock);
   if BTDeviceSetState (@BT.BT, BT_STATE_DETACHED) <> ERROR_SUCCESS then exit;
   USBRequestRelease (BT.EventRequest);
   Device.DriverData := nil;
   if BTDeviceDeregister (@BT.BT) <> ERROR_SUCCESS then exit;
   BTDeviceDestroy (@BT.BT);
   Result := USB_STATUS_SUCCESS;
   log ('unbound');
   log ('');

   (*


function USBMouseDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the Mouse driver from a USB device}
{Device: The USB device to unbind from}
{Interrface: The USB interface to unbind from (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Message:TMessage;
 Mouse:PUSBMouseDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface = nil then Exit;

 {Check Driver}
 if Interrface.Driver <> USBMouseDriver then Exit;

 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Mouse: Unbinding USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 {Get Mouse}
 Mouse:=PUSBMouseDevice(Interrface.DriverData);
 if Mouse = nil then Exit;
 if Mouse.Mouse.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Set State to Detaching}
 Result:=USB_STATUS_OPERATION_FAILED;
 if MouseDeviceSetState(@Mouse.Mouse,MOUSE_STATE_DETACHING) <> ERROR_SUCCESS then Exit;

 {Acquire the Lock}
 if MutexLock(Mouse.Mouse.Lock) <> ERROR_SUCCESS then Exit;

 {Cancel Report Request}
 USBRequestCancel(Mouse.ReportRequest);

 {Check Pending}
 if Mouse.PendingCount <> 0 then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Mouse: Waiting for ' + IntToStr(Mouse.PendingCount) + ' pending requests to complete');
   {$ENDIF}

   {Wait for Pending}

   {Setup Waiter}
   Mouse.WaiterThread:=GetCurrentThreadId;

   {Release the Lock}
   MutexUnlock(Mouse.Mouse.Lock);

   {Wait for Message}
   ThreadReceiveMessage(Message);
  end
 else
  begin
   {Release the Lock}
   MutexUnlock(Mouse.Mouse.Lock);
  end;

 {Set State to Detached}
 if MouseDeviceSetState(@Mouse.Mouse,MOUSE_STATE_DETACHED) <> ERROR_SUCCESS then Exit;

 {Update Interface}
 Interrface.DriverData:=nil;

 {Release Report Request}
 USBRequestRelease(Mouse.ReportRequest);

 {Deregister Mouse}
 if MouseDeviceDeregister(@Mouse.Mouse) <> ERROR_SUCCESS then Exit;

 {Destroy Mouse}
 MouseDeviceDestroy(@Mouse.Mouse);

 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

*)
end;




initialization
//  BT_USBInit;

finalization

                                (*

        {Check Flags}
        if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_DIRECT_READ) = 0 then
         begin
          {Global Buffer}
          {Acquire the Lock}
          if MutexLock(MouseBufferLock) = ERROR_SUCCESS then
           begin
            try
             {Check Buffer}
             if (MouseBuffer.Count < MOUSE_BUFFER_SIZE) then
              begin
               Data:=@MouseBuffer.Buffer[(MouseBuffer.Start + MouseBuffer.Count) mod MOUSE_BUFFER_SIZE];
               if Data <> nil then
                begin
                 {Byte 0 is the Mouse buttons}
                 Data.Buttons:=0;
                 if (PByte(Buffer)^ and USB_HID_BOOT_LEFT_BUTTON) <> 0 then
                  begin
                   {Check Flags}
                   if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_SWAP_BUTTONS) = 0 then
                    begin
                     Data.Buttons:=Data.Buttons or MOUSE_LEFT_BUTTON;
                    end
                   else
                    begin
                     Data.Buttons:=Data.Buttons or MOUSE_RIGHT_BUTTON;
                    end;
                  end;
                 if (PByte(Buffer)^ and USB_HID_BOOT_RIGHT_BUTTON) <> 0 then
                  begin
                   {Check Flags}
                   if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_SWAP_BUTTONS) = 0 then
                    begin
                     Data.Buttons:=Data.Buttons or MOUSE_RIGHT_BUTTON;
                    end
                   else
                    begin
                     Data.Buttons:=Data.Buttons or MOUSE_LEFT_BUTTON;
                    end;
                  end;
                 if (PByte(Buffer)^ and USB_HID_BOOT_MIDDLE_BUTTON) <> 0 then Data.Buttons:=Data.Buttons or MOUSE_MIDDLE_BUTTON;
                 if (PByte(Buffer)^ and USB_HID_BOOT_SIDE_BUTTON) <> 0 then Data.Buttons:=Data.Buttons or MOUSE_SIDE_BUTTON;
                 if (PByte(Buffer)^ and USB_HID_BOOT_EXTRA_BUTTON) <> 0 then Data.Buttons:=Data.Buttons or MOUSE_EXTRA_BUTTON;

                 {Byte 1 is the Mouse X offset}
                 Data.OffsetX:=PShortInt(PtrUInt(Buffer) + 1)^;

                 {Byte 2 is the Mouse Y offset}
                 Data.OffsetY:=PShortInt(PtrUInt(Buffer) + 2)^;

                 {Byte 3 is the Mouse Wheel offset}
                 Data.OffsetWheel:=PShortInt(PtrUInt(Buffer) + 3)^;

                 {Update Count}
                 Inc(MouseBuffer.Count);

                 {Signal Data Received}
                 SemaphoreSignal(MouseBuffer.Wait);
                end;
              end
             else
              begin
               if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Buffer overflow, report discarded');

               {Update Statistics}
               Inc(Mouse.Mouse.BufferOverruns);
              end;
            finally
             {Release the Lock}
             MutexUnlock (MouseBufferLock);
            end;
           end
          else
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Failed to acquire lock on buffer');
           end;
         end
        else
         begin
          {Direct Buffer}
          {Check Buffer}
          if (Mouse.Mouse.Buffer.Count < MOUSE_BUFFER_SIZE) then
           begin
            Data:=@Mouse.Mouse.Buffer.Buffer[(Mouse.Mouse.Buffer.Start + Mouse.Mouse.Buffer.Count) mod MOUSE_BUFFER_SIZE];
            if Data <> nil then
             begin
              {Byte 0 is the Mouse buttons}
              Data.Buttons:=0;
              if (PByte(Buffer)^ and USB_HID_BOOT_LEFT_BUTTON) <> 0 then
               begin
                {Check Flags}
                if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_SWAP_BUTTONS) = 0 then
                 begin
                  Data.Buttons:=Data.Buttons or MOUSE_LEFT_BUTTON;
                 end
                else
                 begin
                  Data.Buttons:=Data.Buttons or MOUSE_RIGHT_BUTTON;
                 end;
               end;
              if (PByte(Buffer)^ and USB_HID_BOOT_RIGHT_BUTTON) <> 0 then
               begin
                {Check Flags}
                if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_SWAP_BUTTONS) = 0 then
                 begin
                  Data.Buttons:=Data.Buttons or MOUSE_RIGHT_BUTTON;
                 end
                else
                 begin
                  Data.Buttons:=Data.Buttons or MOUSE_LEFT_BUTTON;
                 end;
               end;
              if (PByte(Buffer)^ and USB_HID_BOOT_MIDDLE_BUTTON) <> 0 then Data.Buttons:=Data.Buttons or MOUSE_MIDDLE_BUTTON;
              if (PByte(Buffer)^ and USB_HID_BOOT_SIDE_BUTTON) <> 0 then Data.Buttons:=Data.Buttons or MOUSE_SIDE_BUTTON;
              if (PByte(Buffer)^ and USB_HID_BOOT_EXTRA_BUTTON) <> 0 then Data.Buttons:=Data.Buttons or MOUSE_EXTRA_BUTTON;

              {Byte 1 is the Mouse X offset}
              Data.OffsetX:=PShortInt(PtrUInt(Buffer) + 1)^;

              {Byte 2 is the Mouse Y offset}
              Data.OffsetY:=PShortInt(PtrUInt(Buffer) + 2)^;

              {Byte 3 is the Mouse Wheel offset}
              Data.OffsetWheel:=PShortInt(PtrUInt(Buffer) + 3)^;

              {Update Count}
              Inc(Mouse.Mouse.Buffer.Count);

              {Signal Data Received}
              SemaphoreSignal(Mouse.Mouse.Buffer.Wait);
             end;
           end
          else
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Buffer overflow, report discarded');

            {Update Statistics}
            Inc(Mouse.Mouse.BufferOverruns);
           end;
         end;
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Failed report request (Status=' + USBStatusToString(Request.Status) + ', ActualSize=' + IntToStr(Request.ActualSize) + ')');

        {Update Statistics}
        Inc(Mouse.Mouse.ReceiveErrors);
       end;

      {Update Pending}
      Dec(Mouse.PendingCount);

      {Check State}
      if Mouse.Mouse.MouseState = MOUSE_STATE_DETACHING then
       begin
        {Check Pending}
        if Mouse.PendingCount = 0 then
         begin
          {Check Waiter}
          if Mouse.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IFDEF USB_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Mouse: Detachment pending, sending message to waiter thread (Thread=' + IntToHex(Mouse.WaiterThread,8) + ')');
            {$ENDIF}

            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Mouse.WaiterThread,Message);
            Mouse.WaiterThread:=INVALID_HANDLE_VALUE;
           end;
         end;
       end
      else
       begin
        {Update Pending}
        Inc(Mouse.PendingCount);

        {$IFDEF USB_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Mouse: Resubmitting report request');
        {$ENDIF}

        {Resubmit Request}
        Status:=USBRequestSubmit(Request);
        if Status <> USB_STATUS_SUCCESS then
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Failed to resubmit report request: ' + USBStatusToString(Status));

          {Update Pending}
          Dec(Mouse.PendingCount);
         end;
       end;
     finally
      {Release the Lock}
      MutexUnlock(Mouse.Mouse.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Failed to acquire lock');
    end;
  end


  *)

end.
