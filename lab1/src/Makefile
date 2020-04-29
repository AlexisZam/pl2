CXXFLAGS = -g -Wall -Wextra -Wno-char-subscripts

.PHONY:	all clean

all: vm

vm: vm.cpp
	$(CXX) $(CXXFLAGS) -o vm vm.cpp

clean:
	$(RM) vm
