f = 7;
fs = 1000;  
t = 1.5;    
n = [0:1/fs:t];
cycles = t*f;
x = ones(1,length(n));
duty = 50;
oc_samp = fs/f; 
on_samp = (oc_samp * duty)/100;
off_samp = oc_samp - on_samp;
temp = 0;
for i = 1 : ceil(cycles);
    x(temp+on_samp+1:i*oc_samp) = 0;
    temp = temp + oc_samp;
end
plot(n,x(1:length(n)),'LineWidth',2);ylim([-1 1.5]);
% stem(n,x); ylim([-1.5 1.5]);
