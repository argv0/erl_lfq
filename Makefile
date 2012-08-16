
all: compile test

compile:
	./rebar compile

test-compile:
	$(CXX) -g c_src/test_lfq.cpp -Ic_src/ -o c_src/test_lfq -lpthread

test: compile test-compile
	./rebar eunit
	@time c_src/test_lfq

test-clean:
	rm -f c_src/test_lfq

clean: test-clean
	./rebar clean
