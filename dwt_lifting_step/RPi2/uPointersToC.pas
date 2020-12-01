unit uPointersToC;

{$linklib test}

interface

procedure PtoCptrs(w: Integer; ibuf: PInteger; tmpbuf: PInteger); cdecl; external 'libtest' name 'PtoCptrs';
implementation
end.


