clear;
clc;
set(0,'defaultfigurecolor','w');
figure
set(gcf,'Position',[100 100 1000 400]);
set(gca,'Position',[.14 .14 .80 .80]);

%figure
path='.';
tem_step=10;
time_step=60;%每步时间s/step
box_size=(10*30*1e-10)^2;

range=[1 2 4 8 16 32 64];
color=['g'; 'k'; 'c'; 'm'; 'r'; 'b'];
tem=310:tem_step:990;
D_distribution(1:size(range,2)-1,1:size(tem,2))=0;
subTDS(1:size(range,2)-1,1:size(tem,2))=0;
for i=1:size(tem,2)
    data=load(strcat(path,'\TDS',num2str(tem(i)),'.txt'));
    for j=1:size(data,1)
        for k=1:1:size(range,2)-1
            if data(j,4)>=range(k)&data(j,4)<range(k+1)
                D_distribution(k,i)=D_distribution(k,i)+data(j,5);
            end
        end
    end
end

for  i=1:size(range,2)-1
    for j=1:size(tem,2)-1
        subTDS(i,j)=D_distribution(i,j)-D_distribution(i,j+1);
    end
end
subTDS=subTDS/box_size/time_step;


hold on
for i=1:size(range,2)-1
    h(:,i)=plot(tem,subTDS(i,:),'-','markerfacecolor',color(i),'markersize',8,'color',color(i),'linewidth',2);
end
%set(gca,'xtick',[300 400 500 600 700 800 900])
%l1=legend(h,'V_1','V_{2-3}','V_{4-7}','V_{8-15}','V_{16-31}','V_{>=32}','FontSize',14,'FontName','Times New Roman','FontWeight','bold');
xlim([300 1300])
%ylim([0 1e18])
lx=xlabel('Temperature(K)');
ly=ylabel('Desorption rate(D/m^2/s)');


%set(l1,'Box','off')
figure_frontsize=12;
set(get(gca,'XLabel'),'FontSize',figure_frontsize);
set(get(gca,'YLabel'),'FontSize',figure_frontsize);
set(gca,'FontSize',figure_frontsize);
set(gca,'LineWidth',1.5,'ticklength',[0.03 0.03])
set(get(gca,'XLabel'),'FontSize',14,'FontName','Times New Roman','FontWeight','bold');
set(get(gca,'YLabel'),'FontSize',14,'FontName','Times New Roman','FontWeight','bold');
set(gca,'FontSize',12,'FontName','Times New Roman','FontWeight','bold');
%set(l1,'FontSize',10);

subTDS=subTDS';
tem=tem';

saveas(gcf,'TDS_sub.fig');
