%Convert transfer function filter parameters to state-space form

%Testing https://www.mathworks.com/help/signal/ref/tf2ss.html

%Design a 6th-order lowpass Butterworth filter with a cutoff frequency of 120 Hz, 
%which, for data sampled at 1000 Hz, corresponds to 0.6π rad/sample. 
%Plot its magnitude and phase responses. Use it to filter a 1000-sample random signal.

%The input signal x is 3 sin waves 50, 120, and 300.

clear
close all
pkg load signal
order = 6;
fs = 1000;
fc = 120;

                    
T = 1/fs;             % Sampling period       
L = 1500;             % Length of signal
t = (0:L-1)*T;        % Time vectorc
x = 0.7*sin(2*pi*50*t) + sin(2*pi*120*t) + sin(2*pi*300*t);
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Ys = fft(x);

P2 = abs(Ys/L);
P1 = P2(1:L/2+1);
P1(2:end-1) = 2*P1(2:end-1);

f = fs*(0:(L/2))/L;

[A,B,C,D] = tf2ss(b,a);
yf = filter(b,a,x);
figure
subplot(4,1,1)
plot(t,x)

subplot(4,1,2)
plot(f,P1)

subplot(4,1,3)
plot(t,yf)

yff = fft(yf);

P2 = abs(yff/L);
P1 = P2(1:L/2+1);
P1(2:end-1) = 2*P1(2:end-1);

f = fs*(0:(L/2))/L;

subplot(4,1,4)
plot(f,P1)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[A,B,C,D] = tf2ss(b,a);

%yf = filtfilt(b,a,x);
%figure
%subplot(2,1,1)
%plot(1000*t(1:50),yf(1:50));
%subplot(2,1,2)
%plot(1000*t(1:50),x(1:50));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
figure
for order = 1:3
%|H(w)| = Ao/sqrt(1+(w/wo)2n
ff =  (0:1000-1)*T;
ww = 2*pi*ff;
pow = 2*order
ww = (ww/nf);
ww = ww(1:1000).^pow;
ww = sqrt(ww + 1);
ww = (ww).^-1;

%figure 
%subplot(2,1,1)
if order == 1 subplot(3,1,1) end
if order == 2 subplot(3,1,2) end
if order == 3 subplot(3,1,3) end
%if order == 4 subplot(3,1,3) end
%if order == 5 subplot(3,1,3) end
%if order == 1 subplot(3,1,3) end
plot((1:100),ww(1:100))
%ylabel('Magnitude (dB)')
xlabel ('Freq') 



%grid minor on
end

figure
for order = 3:6
%|H(w)| = Ao/sqrt(1+(w/wo)2n
ff =  (0:1000-1)*T;
ww = 2*pi*ff;
pow = 2*order
ww = (ww/nf);
ww = ww(1:1000).^pow;
ww = sqrt(ww + 1);
ww = (ww).^-1;

%figure 
%subplot(2,1,1)
%if order == 1 subplot(3,1,1) end
%if order == 2 subplot(3,1,2) end
%if order == 3 subplot(3,1,3) end
if order == 4 subplot(3,1,1) end
if order == 5 subplot(3,1,2) end
if order == 6 subplot(3,1,3) end
plot((1:100),ww(1:100))
%ylabel('Magnitude (dB)')
xlabel ('Freq') 



%grid minor on
end

