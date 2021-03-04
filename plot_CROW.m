
%% Plotting script for vortex positions
%  Written by Mark A. Herndon
% Lehigh University, Department of Mechanical Engineering and Mechanics

dir   = '/home/markherndon/Vortex_Codes/FORTRAN/';
fname = sprintf('%sCROW.x',dir);

fid = fopen(fname,'r','ieee-le');

nt  = fread(fid,1,'int');

X = zeros(nt,1);
Y = zeros(nt,1);


	X(:,1) = fread(fid,nt,'double');

	Y(:,1) = fread(fid,nt,'double');




%% Plot Vortex trajectories
figure(1)
plot(X(:,1),Y(:,1),'k--','LineWidth',1.5), hold on
grid on
