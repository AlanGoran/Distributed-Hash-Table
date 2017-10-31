-module(testlookup).
-compile(export_all).

buildkeys(From,To,List)->
	%NewKey=To,
	NewKey=key:generate(), % 
	case To > From of 
		true -> ListOfKeys=[NewKey|List],
		buildkeys(From,To-1,ListOfKeys);
		false->List
	end.

generateAllKeysAndAddThem(From,To,P)->
	ListOfKeys=buildkeys(From,To,[]),
	test:add(ListOfKeys,P),
	ListOfKeys.

generateOnlyKeys(From,To)->
	buildkeys(From,To,[]).




% N = test:start(node2).
% List = testlookup:generateAllKeysAndAddThem(0,4000,N).
% test:check(List,N).
%
% ANSWER: 100000 lookup operation in 31 ms 

% ADD MORE NODES:   test:start(node2,1000,N).  
% test:check(List,N).
%
% Answer: 4000 lookup operation in 1073 ms 



%% DIFFERENT MACHINE 
% erl -sname a -setcookie chordy
% N2 = test:start(node2,{N,'a@130.229.178.89'}).
