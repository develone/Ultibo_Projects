unit uBufferToC;

{$linklib dwtlift}

interface
procedure lift_config(decom: word; enc:word; TCP_DISTORATIO:word;FILTER:word; COMPRESSION_RATIO : word; FLG : word; ii: word; ss,him: LongWord; wim: LongWord;var tmpbuf: Pointer); cdecl; external 'libdwtlift' name 'lift_config';
implementation
end.
