function [varargout] = readdump_one(varargin)
% Read LAMMPS dump file that contain only one timestep at a time
% Input
%       Dump file name with path
%       Starting file pointer position in dump file
%       Number of columns in the dump file
% Output is in the form of a structure with following variables
% .timestep     --> Vector containing all time steps
% .Natoms       --> Vector containing number of atoms at each time step
% .x_bound      --> [t,2] array with xlo,xhi at each time step
% .y_bound      --> [t,2] array with ylo,yhi at each time step
% .z_bound      --> [t,2] array with zlo,zhi at each time step
% .var          --> string that indicates per-atom variable names
% .atom_data    --> 2 dimensional array with data
%
% Example
%       data = readdump_one('dump.LAMMPS');
%
% See also readdump, scandump
%
%  Author :  Arun K. Subramaniyan
%            sarunkarthi@gmail.com
%            http://web.ics.purdue.edu/~asubrama/pages/Research_Main.htm
%            School of Aeronautics and Astronautics
%            Purdue University, West Lafayette, IN - 47907, USA.

try
    dump = fopen(varargin{1},'r');
catch
    error('Dumpfile not found!');
end


while ~feof(dump)
    id = fgetl(dump);
    if (strncmpi(id,'ITEM: TIMESTEP',numel('ITEM: TIMESTEP')))
            timestep = str2num(fgetl(dump));
    elseif (strcmpi(id,'ITEM: NUMBER OF ATOMS'))
        Natoms = str2num(fgetl(dump));
    else
        if (strncmpi(id,'ITEM: BOX BOUNDS',numel('ITEM: BOX BOUNDS')))
            boundary=id;
            x_bound(1,:) = str2num(fgetl(dump));
            y_bound(1,:) = str2num(fgetl(dump));
            z_bound(1,:) = str2num(fgetl(dump));
        else
            if (strncmpi(id,'ITEM: ATOMS',numel('ITEM: ATOMS')))
                var_name=id;
                ncol=size(split(var_name),1)-2;
                atom_data = zeros(Natoms,ncol);%Allocate memory for atom data
                for j = 1 : 1: Natoms
                    atom_data(j,:) = str2num(fgetl(dump));
                end
            end
        end
    end
end


%----------Outputs-------------

%OUTPUTS IN SAME VARIABLE STRUCTURE
varargout{1}.timestep = timestep;
varargout{1}.Natoms = Natoms;
varargout{1}.boundary=boundary;
varargout{1}.x_bound = x_bound;
varargout{1}.y_bound = y_bound;
varargout{1}.z_bound = z_bound;
varargout{1}.var_name=var_name;
varargout{1}.atom_data = atom_data;
%------------------------------

fclose(dump);

