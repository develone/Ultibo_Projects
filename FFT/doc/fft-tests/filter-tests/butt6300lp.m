%Design a 6th-order lowpass Butterworth filter with a cutoff frequency of 300 Hz, 
%which, for data sampled at 1000 Hz, corresponds to 0.6π rad/sample. 
%Plot its magnitude and phase responses. Use it to filter a 1000-sample random signal.


clear
close all
pkg load signal
order = 6;
fs = 1000;
fc = 300;
disp('normalize freq')
nf = fc/(fs/2)
[z, p, g] = butter(order,nf);
disp('zeros')
z(1:order)
disp('poles')
p(1:order)
%This is defined as, theta = atan2 (y, x), in radians. 
theta = angle(p(1:order))
 
[b,a] = butter(order,nf);
disp('b')
b
disp('a')
a

freqz(b,a);
