%% Plotting script for vortex positions
%  Written by Mark A. Herndon
% Lehigh University, Department of Mechanical Engineering and Mechanics
clc; clear all; close all;

dir   = '/home/markherndon/Vortex_Codes/FORTRAN/DATA/';
fname = sprintf('%svortices.x',dir);


fid = fopen(fname,'r','ieee-le');

nvt = fread(fid,1,'int');
nt  = fread(fid,1,'int');


Y = zeros(nvt,nt);
Z = zeros(nvt,nt);

eta  = zeros(nvt,nt);
zeta = zeros(nvt,nt);

tau = zeros(nt);

for i = 1:nt
	Y(:,i) = fread(fid,nvt,'double');
end

for i = 1:nt
	Z(:,i) = fread(fid,nvt,'double');
end

for i = 1:nt
	eta(:,i) = fread(fid,nvt,'double');
end

for i = 1:nt
	zeta(:,i) = fread(fid,nvt,'double');
end

tau = fread(fid,nt,'double');
tau = tau * (1/(2*pi*1.5));
%% Plot Vortex trajectories
figure(1)
plot(Y(1,:),Z(1,:),'k--','LineWidth',1.5), hold on
plot(Y(2,:),Z(2,:),'r--','LineWidth',1.5), hold on
k = yline(0, 'b--', 'LineWidth', 3);
xx1 = [0.2 0.2];
yy1 = [0.6 0.53];
annotation('textarrow',xx1,yy1,'String','Wall boundary')
xx2 = [0.5175 0.5175];
yy2 = [0.8 0.7];
annotation('textarrow',xx2,yy2,'String','Vortex trajectory')
xx3 = [0.5175 0.5175];
yy3 = [0.45 0.35];
annotation('textarrow',xx3,yy3,'String','No GE trajectory')
ylim([-5,5])
title('Vortex trajectory in and out of ground effect')
xlabel('x')
ylabel('y')
legend('Vortex 1','Vortex 2','Vortex 1 GE', 'Vortex 2 GE')
grid on

%% Plot perturbation amplitudes
r1 = zeros(nt,1);
r2 = zeros(nt,1);
for k = 1:nt
    r1(k) = (eta(1,k)^2 + zeta(1,k)^2)^(1/2); 
    r2(k) = (eta(2,k)^2 + zeta(2,k)^2)^(1/2); 
end
%%
figure(2)
plot(tau,r1,'k-','LineWidth',1.5), hold on
plot(tau,r2,'r-','LineWidth',1.5), hold on
k2 = xline(5,'b--','LineWidth',3);
xx = [0.45 0.48];
yy = [0.35 0.35];
annotation('textarrow',xx,yy,'String','Time to reach wall boundary')
title('Perturbation Amplitude')
xlabel('Time [t\Gamma/2\pib]')
ylabel('Amplitude [r/b]')
xlim([0,tau(end)])
%%
figure(3)
plot(tau,eta(1,:),'r--','LineWidth',1.5), hold on
plot(tau,eta(2,:),'r-','LineWidth',1.5), hold on
plot(tau,zeta(1,:),'k--','LineWidth',1.5), hold on
plot(tau,zeta(2,:),'k-','LineWidth',1.5), hold on
k2 = xline(5,'b--','LineWidth',3);
xx = [0.45 0.48];
yy = [0.35 0.35];
annotation('textarrow',xx,yy,'String','Time to reach wall boundary')
title('Perturbation Amplitude Components')
xlabel('Time [t\Gamma/2\pib]')
ylabel('Amplitude [r/b]')
xlim([0,tau(end)])

