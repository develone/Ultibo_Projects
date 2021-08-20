procedure hexdump(p : pbyte; len : word);
var
  rows : integer;
  remainder : integer;
  i : integer;

  function line(bytecount : integer) : string;
  var
    s : string;
    asc : string;
    j : integer;
    b : byte;
  begin
     s := '';
     asc := '';

     s := s + inttohex(i*16, 4) + ' ';
     for j := 0 to bytecount-1 do
     begin
       b := (p+(i*16)+j)^;
       s := s + inttohex(b, 2) +' ' ;
       if (b in [28..126]) then
         asc := asc + chr(b)
       else
         asc := asc + '.';
     end;

     if (bytecount < 16) then
       for j := 15 downto bytecount do
         s := s + '   ';

     s := s + ' ' + asc;

     Result := s;
  end;

begin
  rows := len div 16;
  remainder := len mod 16;
{
  WIFILogInfo(nil, 'hexdump address 0x'+inttohex(longword(p),8) + ' for ' + inttostr(len) + ' bytes');
  for i := 0 to rows-1 do
  begin
     WIFILogInfo(nil, line(16));
  end;

  if (remainder > 0) then
  begin
    i:= rows;
    WIFILogInfo(nil, line(remainder));
  end;
}
end;
