function writedump(data,filename)
%write data to a .lmp file



fid = fopen(filename,'w');
fprintf(fid, 'ITEM: TIMESTEP\n');
fprintf(fid, '%d\n',data.timestep);
fprintf(fid, 'ITEM: NUMBER OF ATOMS\n');
fprintf(fid, '%d\n',data.Natoms);
fprintf(fid, '%s\n',data.boundary);
fprintf(fid, '%s\n',num2str(data.x_bound));
fprintf(fid, '%s\n',num2str(data.y_bound));
fprintf(fid, '%s\n',num2str(data.z_bound));
fprintf(fid, '%s\n',data.var_name);
for i=1:size(data.atom_data,1)
    fprintf(fid, '%s\n',num2str(data.atom_data(i,:)));
end

fclose(fid);