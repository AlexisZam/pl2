CXXFLAGS = -O3 -Wall -Wextra

.PHONY:	all clean

all: gc

gc: gc.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^
	
clean:
	$(RM) gc
