clear
close all
pkg load signal
Fs = 1000;            % Sampling frequency                    
T = 1/Fs;             % Sampling period       
L = 15000;             % Length of signal
t = (0:L-1)*T;        % Time vectorc

%S = 0.7*sin(2*pi*50*t) + sin(2*pi*120*t);
S = 0.7*sin(2*pi*50*t) + sin(2*pi*120*t) + sin(2*pi*200*t);
SS = S;
X = S + 2*randn(size(t));
SSS = X;
figure 
plot(1000*t(1:50),S(1:50))
title("no nosie")

Ys = fft(S);

P2 = abs(Ys/L);
P1 = P2(1:L/2+1);
P1(2:end-1) = 2*P1(2:end-1);

figure 
f = Fs*(0:(L/2))/L;
plot(f,P1) 
title('Single-Sided Amplitude Spectrum of X(t)')
xlabel('f (Hz)')
ylabel('|P1(f)|')

 

figure 
plot(1000*t(1:50),X(1:50))
title('Signal Corrupted with Zero-Mean Random Noise')
xlabel('t (milliseconds)')
ylabel('X(t)')

Y = fft(X);

P2 = abs(Y/L);
P1 = P2(1:L/2+1);
P1(2:end-1) = 2*P1(2:end-1);

figure 
f = Fs*(0:(L/2))/L;
plot(f,P1) 




disp('size of data before decimation')
size(SS)

figure
subplot (3,1,1)
plot(1000*t(1:50),SS(1:50))

nhp=4;                  % Order of Butter HPF = 8             %changed 04/2/04
R=4;
 
fc=150;
Wlp=fc/Fs/2;
%[B,A]=butter(nhp,Wlp,'low');
[B,A]=butter(nhp,Wlp);

fSS = filter(B, A,SS)*25;

subplot(3,1,2)
%plot(1000*t(1:50),fSS(1:50))
plot(t,fSS)
 
Yss = fft(fSS);
P2 = abs(Yss/L);
P1 = P2(1:L/2+1);
P1(2:end-1) = 2*P1(2:end-1);

subplot(3,1,3) 
f = Fs*(0:(L/2))/L;
plot(f,P1)
[x,ix] = max(Yss);
