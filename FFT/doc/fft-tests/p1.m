clear
close all
%https://www.mathworks.com/matlabcentral/answers/1828-remove-dc-component-from-eeg-signals
Fs = 1000;            % Sampling frequency                    
T = 1/Fs;             % Sampling period       
L = 512;             % Length of signal
t = (0:L-1)*T;        % Time vector

A = imread('red.pgm');
figure
x = A(1:512,50);
xbar = mean(x)

plot(t,x)

title("Signal with DC")

f = fft(x);
f(1) = 0;
x_ac = real(ifft(f));
[x_ac, x - mean(x)];

figure 
plot(t,x_ac)
title("Signal with DC removed sampled every msec")






Ys = fft(x_ac);

P2 = abs(Ys/L);
P1 = P2(1:L/2+1);
P1(2:end-1) = 2*P1(2:end-1);

figure 
f = Fs*(0:(L/2))/L;
plot(f,P1) 
title('Single-Sided Amplitude Spectrum of X(t)')
xlabel('f (Hz)')
ylabel('|P1(f)|')
  




