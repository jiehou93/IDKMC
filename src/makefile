fc = ifort
target = IDKMC
source1 = *.f90
source0 = default_setting.f90 global.f90 interface.f90
object = *.o
 
$(target): $(source1)
	$(fc) -c $(source0)
	$(fc) -c $(source1)
	$(fc) -O2 -o $(target) $(object)
	
clean:
	rm *.o *.mod *.x