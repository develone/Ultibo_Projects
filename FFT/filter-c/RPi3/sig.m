clear 
close all
pkg load signal
order = 6;
fs = 1000;
T = 1/fs;             % Sampling period       
L = 2048;             % Length of signal
t = (0:L-1)*T;
%mysig was created with gensindata(ptrsignal); in example.c
fid = fopen('mysig.bin','r'); mysig = fread(fid, 2048, 'double');
fid = fopen('myfilt.bin','r'); myfilt = fread(fid, 2048, 'double');
figure

plot(1000*t(1:50),mysig(1:50));

Ys = fft(mysig);

P2 = abs(Ys/L);
P1 = P2(1:L/2+1);
P1(2:end-1) = 2*P1(2:end-1);
f = fs*(0:(L/2))/L;
figure 
plot(f,P1) 

%myfilt was created with bw_low_pass(filter, signal[i])); in example.c
figure

plot(1000*t(1:50),myfilt(1:50));

Ys = fft(myfilt);
P2 = abs(Ys/L);
P1 = P2(1:L/2+1);
P1(2:end-1) = 2*P1(2:end-1);
f = fs*(0:(L/2))/L;
figure 
plot(f,P1) 
