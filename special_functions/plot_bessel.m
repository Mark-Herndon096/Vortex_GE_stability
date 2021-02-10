clc; clear all; close all;

dir = '/home/markherndon/Vortex_Codes/FORTRAN/special_functions/';
fname = sprintf('%sbessel.x',dir);

fid = fopen(fname,'r','ieee-le');

ni = fread(fid,1,'int');
x  = zeros(1,ni);
y  = zeros(1,ni);
y2 = zeros(ni,1);
x = fread(fid,ni,'double');
y = fread(fid,ni,'double');

for i = 1:ni
    y2(i) = besselj(0,x(i));
end


plot(x,y,'k-','LineWidth',1.5), hold on
plot(x,y2,'r+','LineWidth',1.5)
