assign_proctors(AllTAs,Quizzes, TeachingSchedule, ProctoringSchedule):-
		free_schedule(AllTAs,TeachingSchedule,FreeSchedule),!,
		assign_quizzes(Quizzes, FreeSchedule,ProctoringSchedule).





free_schedule(_,[],[]).
free_schedule(_,[day(_,[])|_],[day(_,[])|_]).
free_schedule(AllTAs, [day(Day,[Slot1|Rest])|TeachingSchedule], [day(Day,[Free_List|List])|FreeSchedule]):-
		is_ta_free(AllTAs,Day,Slot1,[],Free_List),
		free_schedule(AllTAs, [day(Day,Rest)|TeachingSchedule], [day(Day,List)|FreeSchedule]),
		free_schedule(AllTAs,TeachingSchedule, FreeSchedule).		


is_ta_free([],_,_,New_Slot,New_Slot2):-
		permutation(New_Slot,New_Slot2).
is_ta_free([ta(Name,Day_Off)|AllTAs],Day,Slot,New_Slot,Free_List):-
		Day_Off \= Day,
		\+(member(Name,Slot)),
		append([Name],New_Slot,Result),
		is_ta_free(AllTAs,Day,Slot,Result,Free_List).
is_ta_free([ta(Name,Day_Off)|AllTAs],Day,Slot,New_Slot,Free_List):-
		Day_Off \= Day,
		member(Name,Slot),
		is_ta_free(AllTAs,Day,Slot,New_Slot,Free_List).
is_ta_free([ta(Name,Day)|AllTAs],Day,Slot,New_Slot,Free_List):-
		is_ta_free(AllTAs,Day,Slot,New_Slot,Free_List).


assign_quizzes([], _, []).
assign_quizzes([Q|Qs], FreeSchedule, [proctors(Q, Tas)|ProctoringSchedule]) :-
    assign_quiz(Q, FreeSchedule, Tas),
    assign_quizzes(Qs, FreeSchedule, ProctoringSchedule).





assign_quiz(quiz(Course, Day, Slot, Count), [day(Day,FreeSchedule)|Tail_teachingSchedule], TA_Names):-
					which_slot(Slot,1,FreeSchedule,Names),
					arrangement(Names,Count,0,TA_Names).																			
assign_quiz(quiz(Course, Day1, Slot, Count), [day(Day2,FreeSchedule)|Tail_teachingSchedule], TA_Names):-
					Day1 \= Day2,
					assign_quiz(quiz(Course, Day1, Slot, Count), Tail_teachingSchedule, TA_Names).


which_slot(Slot,Count,[Slots|FreeSchedule],Names):-
					Count < Slot,
					Count1 is Count + 1,
					which_slot(Slot,Count1,FreeSchedule,Names).									
which_slot(Slot,Slot,[Slots|FreeSchedule],Slots).


arrangement(_,X,X,[]).
arrangement(Names,Count,Counter,[First_TA|TA_Names]):-
					Counter < Count,
					Counter1 is Counter + 1,
					member(First_TA,Names),
					remove(First_TA,Names,New_Names),
					arrangement(New_Names,Count,Counter1,TA_Names).


remove(_,[],[]).
remove(H,[H|T],R):-
					remove(H,T,R).
remove(X,[H|T],[H|T1]):-
					X \= H,
					remove(X,T,T1).