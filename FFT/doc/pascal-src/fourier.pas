(*==========================================================================

    fourier.pas  -  Don Cross <dcross@intersrv.com>

    This is a Turbo Pascal Unit for calculating the Fast Fourier Transform
    (FFT) and the Inverse Fast Fourier Transform (IFFT).
    Visit the following URL for the latest version of this code.
    This page also has a C/C++ version, and a brief discussion of the
    theory behind the FFT algorithm.

       http://www.intersrv.com/~dcross/fft.html#pascal

    Revision history [most recent first]:

1996 December 11 [Don Cross]
    Improved documentation of the procedure CalcFrequency.
    Fixed some messed up comments in procedure ifft.

1996 December 6 [Don Cross]
    Made procedure 'fft_integer' more efficient when buffer size changes
    in successive calls:  the buffer is now only resized when the input
    has more samples, not a differing number of samples.
    Also changed the way 'fft_integer_cleanup' works so that it is
    more "bullet-proof".

1996 December 4 [Don Cross]
    Adding the procedure 'CalcFrequency', which calculates the FFT
    at a specific frequency index p=0..n-1, instead of the whole
    FFT.  This is O(n) instead of O(n*log(n)).

1996 November 30 [Don Cross]
    Adding a routine to allow FFT of an input array of integers.
    It is called 'fft_integer'.

1996 November 18 [Don Cross]
    Added some comments.

1996 November 17 [Don Cross]
    Wrote and debugged first version.

==========================================================================*)

//{$N+,E+}   (* Allows code to use type 'double' and run on any iX86 machine *)
{$R-}      (* Turn off range checking...we violate array bounds rules *)


unit Fourier;


interface

//uses definition;

(*---------------------------------------------------------------------------
  procedure fft

  Calculates the Fast Fourier Transform of the array of complex numbers
  represented by 'RealIn' and 'ImagIn' to produce the output complex
  numbers in 'RealOut' and 'ImagOut'.
---------------------------------------------------------------------------*)
procedure fft (
    NumSamples:   word;   { must be a positive integer power of 2 }
    var  RealIn:   array of double;
    var  ImagIn:   array of double;
    var  RealOut:  array of double;
    var  ImagOut:  array of double );


(*---------------------------------------------------------------------------
  procedure ifft

  Calculates the Inverse Fast Fourier Transform of the array of complex
  numbers represented by 'RealIn' and 'ImagIn' to produce the output complex
  numbers in 'RealOut' and 'ImagOut'.
---------------------------------------------------------------------------*)
procedure ifft (
    NumSamples:   word;   { must be a positive integer power of 2 }
    var  RealIn:   array of double;
    var  ImagIn:   array of double;
    var  RealOut:  array of double;
    var  ImagOut:  array of double );



(*---------------------------------------------------------------------------
  procedure fft_integer

  Same as procedure fft, but uses integer input arrays instead of
  double.  Make sure you call fft_integer_cleanup after the last
  time you call fft_integer to free up memory it allocates.
---------------------------------------------------------------------------*)
procedure fft_integer (
    NumSamples:   word;
    var  RealIn:   array of integer;
    var  ImagIn:   array of integer;
    var  RealOut:  array of double;
    var  ImagOut:  array of double );


(*--------------------------------------------------------------------------
   procedure fft_integer_cleanup

   If you call the procedure 'fft_integer', you must call
   'fft_integer_cleanup' after the last time you call 'fft_integer'
   in order to free up dynamic memory.
--------------------------------------------------------------------------*)
procedure fft_integer_cleanup;


(*--------------------------------------------------------------------------
   procedure CalcFrequency

   This procedure calculates the complex frequency sample at a given
   index directly.  Use this instead of 'fft' when you only need one
   or two frequency samples, not the whole spectrum.

   It is also useful for calculating the Discrete Fourier Transform (DFT)
   of a number of data which is not an integer power of 2.  For example,
   you could calculate the DFT of 100 points instead of rounding up to
   128 and padding the extra 28 array slots with zeroes.
--------------------------------------------------------------------------*)
procedure CalcFrequency (
    NumSamples: word;       { can be any positive integer }
    FrequencyIndex: word;   { must be in the range 0 .. NumSamples-1 }
    var  RealIn:  array of double;
    var  ImagIn:  array of double;
    var  RealOut: double;
    var  ImagOut: double );


implementation


function IsPowerOfTwo ( x: word ): boolean;
var   i, y:  word;
begin
    y := 2;
    for i := 1 to 15 do begin
        if x = y then begin
            IsPowerOfTwo := TRUE;
            exit;
        end;
        y := y SHL 1;
    end;

    IsPowerOfTwo := FALSE;
end;


function NumberOfBitsNeeded ( PowerOfTwo: word ): word;
var     i: word;
begin
    Result:=1;
    for i := 0 to 16 do begin
        if (PowerOfTwo AND (1 SHL i)) <> 0 then begin
            Result := i;
            exit;
        end;
    end;
end;


function ReverseBits ( index, NumBits: word ): word;
var     i, rev: word;
begin
    rev := 0;
    for i := 0 to NumBits-1 do begin
        rev := (rev SHL 1) OR (index AND 1);
        index := index SHR 1;
    end;

    ReverseBits := rev;
end;


procedure FourierTransform (
    AngleNumerator:  double;
    NumSamples:   word;
    var  RealIn:   array of double;
    var  ImagIn:   array of double;
    var  RealOut:  array of double;
    var  ImagOut:  array of double );
var
    NumBits, i, j, k, n, BlockSize, BlockEnd: word;
    delta_angle, delta_ar: double;
    alpha, beta: double;
    tr, ti, ar, ai: double;
begin
    if not IsPowerOfTwo(NumSamples) or (NumSamples<2) then begin
        write ( 'Error in procedure Fourier:  NumSamples=', NumSamples );
        writeln ( ' is not a positive integer power of 2.' );
        halt;
    end;

    NumBits := NumberOfBitsNeeded (NumSamples);
    for i := 0 to NumSamples-1 do begin
        j := ReverseBits ( i, NumBits );
        RealOut[j] := RealIn[i];
        ImagOut[j] := ImagIn[i];
    end;

    BlockEnd := 1;
    BlockSize := 2;
    while BlockSize <= NumSamples do begin
        delta_angle := AngleNumerator / BlockSize;
        alpha := sin ( 0.5 * delta_angle );
        alpha := 2.0 * alpha * alpha;
        beta := sin ( delta_angle );

        i := 0;
        while i < NumSamples do begin
            ar := 1.0;    (* cos(0) *)
            ai := 0.0;    (* sin(0) *)

            j := i;
            for n := 0 to BlockEnd-1 do begin
                k := j + BlockEnd;
                tr := ar*RealOut[k] - ai*ImagOut[k];
                ti := ar*ImagOut[k] + ai*RealOut[k];
                RealOut[k] := RealOut[j] - tr;
                ImagOut[k] := ImagOut[j] - ti;
                RealOut[j] := RealOut[j] + tr;
                ImagOut[j] := ImagOut[j] + ti;
                delta_ar := alpha*ar + beta*ai;
                ai := ai - (alpha*ai - beta*ar);
                ar := ar - delta_ar;
                INC(j);
            end;

            i := i + BlockSize;
        end;

        BlockEnd := BlockSize;
        BlockSize := BlockSize SHL 1;
    end;
end;


procedure fft (
    NumSamples:   word;
    var  RealIn:   array of double;
    var  ImagIn:   array of double;
    var  RealOut:  array of double;
    var  ImagOut:  array of double );
begin
    FourierTransform ( 2*PI, NumSamples, RealIn, ImagIn, RealOut, ImagOut );
end;


procedure ifft (
    NumSamples:   word;
    var  RealIn:   array of double;
    var  ImagIn:   array of double;
    var  RealOut:  array of double;
    var  ImagOut:  array of double );
var
    i: word;
begin
    FourierTransform ( -2*PI, NumSamples, RealIn, ImagIn, RealOut, ImagOut );

    (* Normalize the resulting time samples... *)
    for i := 0 to NumSamples-1 do begin
        RealOut[i] := RealOut[i] / NumSamples;
        ImagOut[i] := ImagOut[i] / NumSamples;
    end;
end;


type
    doubleArray = array [0..0] of double;
var
    RealTemp, ImagTemp: ^doubleArray;
    TempArraySize:  word;


procedure fft_integer (
    NumSamples:   word;
    var  RealIn:   array of integer;
    var  ImagIn:   array of integer;
    var  RealOut:  array of double;
    var  ImagOut:  array of double );
var
    i: word;
begin
    if NumSamples > TempArraySize then begin
        fft_integer_cleanup;  { free up memory in case we already have some. }
        GetMem ( RealTemp, NumSamples * sizeof(double) );
        GetMem ( ImagTemp, NumSamples * sizeof(double) );
        TempArraySize := NumSamples;
    end;

    for i := 0 to NumSamples-1 do begin
        RealTemp^[i] := RealIn[i];
        ImagTemp^[i] := ImagIn[i];
    end;

    FourierTransform (
        2*PI,
        NumSamples,
        RealTemp^, ImagTemp^,
        RealOut, ImagOut );
end;


procedure fft_integer_cleanup;
begin
    if TempArraySize > 0 then begin
        if RealTemp <> NIL then begin
            FreeMem ( RealTemp, TempArraySize * sizeof(double) );
            RealTemp := NIL;
        end;

        if ImagTemp <> NIL then begin
            FreeMem ( ImagTemp, TempArraySize * sizeof(double) );
            ImagTemp := NIL;
        end;

        TempArraySize := 0;
    end;
end;


procedure CalcFrequency (
    NumSamples: word;       { must be integer power of 2 }
    FrequencyIndex: word;   { must be in the range 0 .. NumSamples-1 }
    var  RealIn:  array of double;
    var  ImagIn:  array of double;
    var  RealOut: double;
    var  ImagOut: double );
var
    k: word;
    cos1, cos2, cos3, theta, beta: double;
    sin1, sin2, sin3: double;
begin
    RealOut := 0.0;
    ImagOut := 0.0;
    theta := 2*PI * FrequencyIndex / NumSamples;
    sin1 := sin ( -2 * theta );
    sin2 := sin ( -theta );
    cos1 := cos ( -2 * theta );
    cos2 := cos ( -theta );
    beta := 2 * cos2;
    for k := 0 to NumSamples-1 do begin
        { Update trig values }
        sin3 := beta*sin2 - sin1;
        sin1 := sin2;
        sin2 := sin3;

        cos3 := beta*cos2 - cos1;
        cos1 := cos2;
        cos2 := cos3;

        RealOut := RealOut + RealIn[k]*cos3 - ImagIn[k]*sin3;
        ImagOut := ImagOut + ImagIn[k]*cos3 + RealIn[k]*sin3;
    end;
end;


begin  { Unit initialization code }
    TempArraySize := 0; {flag that buffers RealTemp, RealImag not allocated}
    RealTemp := NIL;
    ImagTemp := NIL;
end.


(*--- end of file fourier.pas ---*)
