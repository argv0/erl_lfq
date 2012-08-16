
all: compile test

compile:
	./rebar compile

test-compile:
	$(CXX) c_src/test_lfq.cpp -o c_src/test_lfq

test: compile test-compile
	./rebar eunit
	@time c_src/test_lfq

test-clean:
	rm -f c_src/test_lfq

clean: test-clean
	./rebar clean
