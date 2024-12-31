clear;clc; close all; fclose all;

box=[1000 100 100];      %box size in anstrom
a0=3.1652;              %lattice constant
formula1=[1 5 0 0];     %defect type 1, containing 1 vacancy, 5 H and no D
C1=1000e-6;             %concentration of defect type 1 
formula2=[1 4 1 0];     %defect type 2

nW=box(1)*box(2)*box(3)/a0^3*2; %number of matrix W atoms
n1=ceil(C1*nW);         %number of defect type 1
n2=1;                   %number of defect type 2

data=zeros(n1+n2,8)+1;
data(:,1:3)=rand(n1+n2,3).*box;             %random x y z for all defects
data(1:n1,4:7)=data(1:n1,4:7).*formula1;    %set as defect type 1
data(n1+1:n1+n2,4:7)=formula2;              %set as defect type 2


%output POSITION generated
filename = ['POSITION'];
natom=n1+n2;

fid=fopen([filename '.lmp'],'w+');
fprintf(fid,'ITEM: TIMESTEP\n');
fprintf(fid,'0\n');
fprintf(fid,'ITEM: NUMBER OF ATOMS\n');
fprintf(fid,'%d\n',natom);
fprintf(fid,'ITEM: BOX BOUNDS pp pp pp\n');
fprintf(fid,'0.0 %15.6f \n',box(1));
fprintf(fid,'0.0 %15.6f \n',box(2));
fprintf(fid,'0.0 %15.6f \n',box(3));
fprintf(fid,'ITEM: ATOMS x y z f1 f2 f3 f4 orient\n');
fprintf(fid, '%15.6f %15.6f %15.6f %6d %6d %6d %6d %6d\n', data');

fclose all;