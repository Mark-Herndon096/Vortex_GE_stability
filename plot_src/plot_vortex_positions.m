%% Plotting script for vortex positions
%  Written by Mark A. Herndon
% Lehigh University, Department of Mechanical Engineering and Mechanics

dir   = '/home/markherndon/Vortex_Codes/FORTRAN/';
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
figure(1)
plot(Y(1,:),Z(1,:),'k-','LineWidth',1.5), hold on
plot(Y(2,:),Z(2,:),'r-','LineWidth',1.5), hold on
plot(Y(5,:),Z(5,:),'b-','LineWidth',1.5), hold on
plot(Y(6,:),Z(6,:),'k--','LineWidth',1.5)

grid on
