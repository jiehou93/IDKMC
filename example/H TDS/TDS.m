%TDS画图程序
clear;
clc;
set(0,'defaultfigurecolor','w');
figure
set(gcf,'Position',[100 100 600 400]);
set(gca,'Position',[.14 .14 .80 .80]);



ramp_rate=0.15;%每步升温K/step
time_step=10/ramp_rate;%每步时间s/step
tem_step=10;
box_size=(10*60*1e-10)^2;

loaded_data=importdata('defect_remain.txt');
loaded_data=loaded_data.data;

x=loaded_data(:,5);
y=loaded_data(:,2);

T(size(x,1)-1)=0;
desorb=T;


for i=1:1:size(x,1)-1
    T(i)=(x(i)+x(i+1))/2;
    desorb(i)=y(i)-y(i+1);
end
desorb=desorb/box_size/tem_step;


h11=plot(T,desorb,'ko-','markerfacecolor','b','markersize',8,'linewidth',1);


lx=xlabel('Temperature(K)');
ly=ylabel('Desorption rate(ion/m^{2}/K)');
xlim([300 900])
set(gca,'xtick',[300 400 500 600 700 800 900])
%ylim([0 2e18])


figure_frontsize=12;
set(get(gca,'XLabel'),'FontSize',figure_frontsize);
set(get(gca,'YLabel'),'FontSize',figure_frontsize);
set(gca,'FontSize',figure_frontsize);
set(gca,'LineWidth',1.5,'ticklength',[0.03 0.03])
set(get(gca,'XLabel'),'FontSize',14,'FontName','Times New Roman','FontWeight','bold');
set(get(gca,'YLabel'),'FontSize',14,'FontName','Times New Roman','FontWeight','bold');
set(gca,'FontSize',14,'FontName','Times New Roman','FontWeight','bold');






saveas(gcf,'TDS.fig');
%saveas(gcf,'E:\Paper\D retention\figure\for_manuscript\3kev_0ppm.bmp');
%saveas(gcf,'E:\Paper\D retention\figure\for_manuscript\3kev_0ppm.eps');
fid=fopen('TDS.txt','w+');
for i=1:1:size(x,1)-1
    fprintf(fid,'%20.5f %20.5e\n',T(i),desorb(i));
end

