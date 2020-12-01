unit uBufferToC;

{$linklib test}

interface
procedure Pbuff(ii: word; ss: LongWord; var tmpbuf: Pointer); cdecl; external 'libtest' name 'xyz';
implementation
end.
