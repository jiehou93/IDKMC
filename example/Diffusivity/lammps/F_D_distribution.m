clear;clc;close all;

kb=8.6173303e-5;
T=300;

E=-([1000 1.4065 1.3854 1.3404 1.2661 1.1578 1.0232 0.8472 0.6291 0.3660 0.0561 -1000]-0.22);
N=size(E,2);
Ef=-0.886;%这个费米能对应的V1H5浓度最吻合
M=size(Ef,2);

p=zeros(N,M);
P=p;
for i=1:M
    p(:,i)=1./(1+exp((E(:)-Ef(i))/kb/T));
    for n=1:N
        P(n,i)=prod(p(1:n,i))*prod(1-p(n+1:N,i));
    end
    %P(:,i)=P(:,i)/sum(P(:,i));
end

sum(P)

D0=2.58e13*0.11191^2*exp(-0.22/kb/T)/6;
D=D0/(1+1e-3*exp(-Ef/kb/T))