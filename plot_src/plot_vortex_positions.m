%% Plotting script for vortex positions
%  Written by Mark A. Herndon
% Lehigh University, Department of Mechanical Engineering and Mechanics

dir   = '/home/markherndon/Vortex_Codes/FORTRAN/src/';
fname = sprintf('%svortices.x',dir);

fid = fopen(fname,'r','ieee-le');

nvt = fread(fid,1,'int');
nt  = fread(fid,1,'int');

Y = zeros(nvt,nt);
Z = zeros(nvt,nt);
tau = zeros(nt);

for i = 1:nt
	Y(:,i) = fread(fid,nvt,'double');
end

for i = 1:nt
	Z(:,i) = fread(fid,nvt,'double');
end

tau = fread(fid,nt,'double');

%% Plot Vortex trajectories

plot(Y(1,:),Z(1,:),'k-','LineWidth',2), hold on
plot(Y(2,:),Z(2,:),'k-','LineWidth',2), hold on
%plot(Y(3,:),Z(3,:),'k--','LineWidth',2), hold on
%plot(Y(4,:),Z(4,:),'k--','LineWidth',2)
xlim([-5,5])
ylim([0,5])
grid on
