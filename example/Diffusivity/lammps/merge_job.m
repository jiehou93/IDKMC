clear;clc;

njob=5;
nstep=120000;
thermo_step=100;
dump_step=10000;
dump_head='gcmc';
dump_suffix='.lmp';
logfile='log.lammps';

mkdir 'merged';
for jobid=1:njob
    %read lammps log file and convert to string array
    cd (['./job' num2str(jobid)]);
    fid = fopen(logfile,'r');
    logtxt = textscan(fid,'%s','delimiter','\n'); 
    logstr = string(logtxt{:});
    
    %locate thermo output lines
    index = find(contains(logstr,['run ' num2str(nstep)]));
    index = find(contains(logstr(index:index+20),'Step Temp'))+index;
    
    %merge thermos for different jobs
    if jobid==1
        thermo_data=logstr(index-1:index+nstep/thermo_step);
    else
        thermo_data=[thermo_data; logstr(index+1:index+nstep/thermo_step)];
    end
    fclose(fid);
    
    %move dump files to a new folder and rename them
    for step=0:dump_step:nstep
        stepsum=step+nstep*(jobid-1);
        status=movefile([dump_head num2str(step) dump_suffix], ['../merged/' dump_head num2str(stepsum) dump_suffix]);
    end
    cd ..
end

cd './merged'
fid = fopen('thermo_merge.log','w');
fprintf(fid, '%s\n', thermo_data);
fclose(fid);


