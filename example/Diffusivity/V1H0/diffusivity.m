clear;clc;close all;
addpath('..\lammps');
set(0,'defaultfigurecolor','w');

dimension=[1];          %需要统计哪几个维度上的扩散。两次输出之间，粒子运动距离不能超过该维度上盒子长度的一半
timestep=1E12;          %每步时间步长
filenum=[0:1:500];      %输出文件编号


N_data=length(filenum);
xyz(N_data,3)=0;
for i=1:N_data
    filename = [sprintf( '%03d', filenum(i)) '.lmp'];
    data=readdump_one(filename);
    box(1)=data.x_bound(2);
    box(2)=data.y_bound(2);
    box(3)=data.z_bound(2);
    data=data.atom_data;
    
    %仅追踪同位素所在团簇（第六列==1）的坐标
    xyz(i,:)=data(data(:,6)==1,1:3);
end


distance1=abs(xyz(2:N_data,:)-xyz(1:N_data-1,:));
distance2=abs(box-distance1);
distance=min(distance1,distance2);
square_d=sum(distance(:,dimension).^2,2);
for i=1:N_data-1
    msd(i)=mean(square_d(1:i));
end
msd=msd'/100;%单位转换为nm^2
D=msd/timestep/(size(dimension,2)*2);




figure
plot([1:N_data-1],msd,'r- ','linewidth',1.5)
lx=xlabel('No. of simulation','FontSize',16);
ly=ylabel('MSD (nm^2)','FontSize',16);
saveas(gcf,['MSD.jpg']);
saveas(gcf,['MSD.fig']);
set(gcf,'Position',[200 200 600 400]);
set(gca,'Position',[.15 .15 .80 .80]);

figure
plot([1:N_data-1],D,'r- ','linewidth',1.5)
lx=xlabel('No. of simulation','FontSize',16);
ly=ylabel('Diffusivity (nm^2/s)','FontSize',16);
saveas(gcf,['Diffusivity.jpg']);
saveas(gcf,['Diffusivity.fig']);
set(gcf,'Position',[200 200 600 400]);
set(gca,'Position',[.15 .15 .80 .80]);

fid=fopen('MSD_Diffusivity.txt','w+');
for i = 1:N_data-1
    fprintf(fid,'%10.5e\t',[msd(i) D(i)]);
    fprintf(fid,'\n');
end
fclose all
