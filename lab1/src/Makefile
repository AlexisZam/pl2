CXXFLAGS = -O3 -Wall -Wextra

.PHONY:	all clean

all: vm

vm: vm.cpp
	$(CXX) $(CXXFLAGS) -o vm vm.cpp

clean:
	$(RM) vm
