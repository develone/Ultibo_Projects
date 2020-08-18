unit initunit;

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

interface

uses GlobalConst,GlobalConfig;

implementation

initialization
 {Disable Console Autocreate}
 FRAMEBUFFER_CONSOLE_AUTOCREATE:=False;

end.
