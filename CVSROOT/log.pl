:- ensure_loaded(common).
cvs_id('$Id: log.pl,v 1.1 2005-10-06 13:01:00 arrigo Exp $').
/*

  Tools for handling logging of events (especially errors) in a server.

  Works with message_hook.pl  --- when we want that -- it can be dangerous!
  
*/

:- dynamic log_data/2.
:- multifile log_data/2.

log_message(Logname, Prefix, Lines) :-
	is_list(Lines), 
	assertz(log_data(Logname, message(Prefix, Lines))).
log_message(Logname, Prefix, Line) :-
	\+ is_list(Line),
	assertz(log_data(Logname, message(Prefix, [Line]))).
log_message(Logname, Lines) :-
	log_message(Logname, '', Lines).

log_term(Logname, Term) :-
	assertz(log_data(Logname, term(Term))).

log_term(Term) :-
	functor(Term, Functor, _Arity),
	log_term(Functor, Term).

log_format(Logname, Template, Args) :-
	assertz(log_data(Logname, format(Template, Args))).


start_log_writer :- 
	thread_create(log_write_loop, _, [detached(true),
					 alias(logwriter),
					 global(1024),
					 local(1024),
					 trail(1024),
					 argument(1024),
					 stack(1024)
					]).

log_write_loop :-
	thread_set_state(waiting),
	repeat,
	log_write_all,
	sleep(0.2),
	fail.

log_write_all :-
	set_prolog_flag(float_format, '%.16g'),   % is this per thread?!
	get_time(Now),
	convert_time(Now, NowString),
	setofsome(999999, _, Logname, log_data(Logname, _), Lognames),
	(   Lognames = []
	->  true
	;   forall( member(Logname, Lognames),
		    log_write_one_log(Logname, NowString)
		  ),
	    thread_set_state(waiting)
	).

log_write_one_log(Logname, NowString) :-
	thread_set_state(writing(Logname)),
	app_config_file(log_prefix, LP),
	atom_concat(LP, Logname, AbsFilename),
	debug(log, 'logging to ~q', AbsFilename),
	open(AbsFilename, append, Stream, [lock(exclusive),  buffer(full)]),
        format(Stream, '%~n% log flushed ~w~n', [NowString]),
	!,
	forall(  retract(log_data(Logname, Item)),
		 log_write_one_message(Stream, Item)
	      ),
	close(Stream),
	thread_set_state(waiting).

reload_log(Logname) :-
	app_config_file(log_prefix, LP),
	atom_concat(LP, Logname, AbsFilename),
	catch(consult(AbsFilename),
	      error(existence_error(source_sink, AbsFilename), _),
	      E=error
	     ),
	var(E).
	    
	
log_write_one_message(Stream, message(Prefix, Lines)) :-
	%%%%% not worth geting this to work right now.
	%(   print_message_lines(Stream, Prefix, Lines)
	%->  true
	format(Stream, '~q.~n', [unknown_message(Prefix, Lines)]).
	%).
log_write_one_message(Stream, term(Term)) :-
	format(Stream, '~q.~n', [Term]).
log_write_one_message(Stream, format(Template, Args)) :-
	format(Stream, Template, Args).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Give nice format to our messages
%%


:- multifile
	prolog:message/3.

prolog:message(http(Status, Req, _ReqId, N, StartTime, EndTime)) -->
	{ isotime_of_day(StartTime, Time),
	  Dur is  EndTime-StartTime,
	  app_config(site_name_4_char, Name),
	  thread_self(Thread),
	  sub_atom(Thread, _, 2, 0, ThreadSuffix),
	  ( memberchk(path(Path), Req) ; Path='< no path? >' ), !,
	  status(Status, StatusOut),
	  true
	},
	[ '~a ~a ~d ~a ~5f '-[Name, ThreadSuffix, N, Time, Dur],
	  StatusOut, ' ',
	  Path
	  ].

prolog:message(new_harvest(Address, Ret)) -->
	[ 'fetched from ~q (~q)' - [Address, Ret] ].
prolog:message(new_harvest(Address, Ret)) -->
	[ 'something else from ~q (~q)' - [Address, Ret] ].


status(okay, okay).
status(failed, fail).
status(error(E), Out) :-
	term_to_atom(E, Out).

prolog:message(deref(_Status, _Ret)) -->
	[ 'deref!' ].
/*
	{ isotime_of_day(StartTime, Time),
	  Dur is  EndTime-StartTime,
          app_config(site_name_4_char, Name),
	  thread_self(Thread),
	  sub_atom(Thread, _, 2, 0, ThreadSuffix),
	  ( memberchk(path(Path), Req) ; Path='< no path? >' ), !,
	  status(Status, StatusOut),
	  true
	},
	[ '~a ~a ~d ~a ~5f '-[Name, ThreadSuffix, N, Time, Dur],
	  StatusOut, ' ',
	  Path
	  ].
	
*/