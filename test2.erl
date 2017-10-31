-module(test2).

-compile(export_all).

run(Module) -> 
	N = test:start(Module),
	test:start(Module,10, N),
	N ! probe.



% erl -sname a -setcookie chordy      	erl -sname b -setcookie chordy

% N1 = test:start(node1).				N3 = test:start(node1, {node,'a@n129-p226'}).
% register(node, N1). 					test:start(node1,10,N3).
%										
% N1 ! probe.
%
%
%