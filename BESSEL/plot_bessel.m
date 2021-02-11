clc; clear all; close all;

dir = '/home/markherndon/FORTRAN/';
fname = sprintf('%sBESSEL.X',dir);

fid = fopen(fname,'r','ieee-le');

ni = fread(fid,1,'int');

x  = zeros(ni,1);
j0 = zeros(ni,1);
j1 = zeros(ni,1);
j2 = zeros(ni,1);
y0 = zeros(ni,1);
y1 = zeros(ni,1);
y2 = zeros(ni,1);
i0 = zeros(ni,1);
i1 = zeros(ni,1);
i2 = zeros(ni,1);
k0 = zeros(ni,1);
k1 = zeros(ni,1);
k2 = zeros(ni,1);

x  = fread(fid,ni,'double');
j0 = fread(fid,ni,'double');
j1 = fread(fid,ni,'double');
j2 = fread(fid,ni,'double');
y0 = fread(fid,ni,'double');
y1 = fread(fid,ni,'double');
y2 = fread(fid,ni,'double');
i0 = fread(fid,ni,'double');
i1 = fread(fid,ni,'double');
i2 = fread(fid,ni,'double');
k0 = fread(fid,ni,'double');
k1 = fread(fid,ni,'double');
k2 = fread(fid,ni,'double');

%% Generate MATLABS bessel functions
xx = linspace(0,40,1000);
j02 = zeros(1000,1);
j12 = zeros(1000,1);
j22 = zeros(1000,1);
y02 = zeros(1000,1);
y12 = zeros(1000,1);
y12 = zeros(1000,1);
i02 = zeros(1000,1);
i12 = zeros(1000,1);
i22 = zeros(1000,1);
k02 = zeros(1000,1);
k12 = zeros(1000,1);
k22 = zeros(1000,1);

for i = 1:1000
	j02(i) = besselj(0,xx(i));
	j12(i) = besselj(1,xx(i));
	j22(i) = besselj(2,xx(i));
	y02(i) = bessely(0,xx(i));
	y12(i) = bessely(1,xx(i));
	y22(i) = bessely(2,xx(i));
	i02(i) = besseli(0,xx(i));
	i12(i) = besseli(1,xx(i));
	i22(i) = besseli(2,xx(i));
	k02(i) = besselk(0,xx(i));
	k12(i) = besselk(1,xx(i));
	k22(i) = besselk(2,xx(i));
end

%% Plot J and K
figure(1)
plot(x,j0,'r-','LineWidth',1.5), hold on
plot(x,j1,'b-','LineWidth',1.5), hold on
plot(x,j2,'k-','LineWidth',1.5), hold on
plot(x,y0,'r--','LineWidth',1.5), hold on
plot(x,y1,'b--','LineWidth',1.5), hold on
plot(x,y2,'k--','LineWidth',1.5), hold on
plot(xx,j02,'r+','LineWidth',1.5), hold on
plot(xx,j12,'b+','LineWidth',1.5), hold on
plot(xx,j22,'k+','LineWidth',1.5), hold on
plot(xx,y02,'r*','LineWidth',1.5), hold on
plot(xx,y12,'b*','LineWidth',1.5), hold on
plot(xx,y22,'k*','LineWidth',1.5)
ylim([-2, 1.25])
xlim([0, 40])

figure(2)
plot(x,i0,'r-','LineWidth',1.5), hold on
plot(x,i1,'b-','LineWidth',1.5), hold on
plot(x,i2,'k-','LineWidth',1.5), hold on
plot(x,k0,'r--','LineWidth',1.5), hold on
plot(x,k1,'b--','LineWidth',1.5), hold on
plot(x,k2,'k--','LineWidth',1.5), hold on
plot(xx,i02,'r+','LineWidth',1.5), hold on
plot(xx,i12,'b+','LineWidth',1.5), hold on
plot(xx,i22,'k+','LineWidth',1.5), hold on
plot(xx,k02,'r*','LineWidth',1.5), hold on
plot(xx,k12,'b*','LineWidth',1.5), hold on
plot(xx,k22,'k*','LineWidth',1.5)
ylim([0, 400])
xlim([0, 40])
