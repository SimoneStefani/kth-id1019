*Programming II (ID1019) - Royal Institute of Technology KTH*
# Dining Philosophers

### 1. Introduction
Several philosopher are sitting around a dining table with a bowl of noodles in front of them and a chopstick between each person. The problem is to allow each philosopher to dine orchestrating the use of the chopsticks which are half of the needed in number.

A philosopher spends most of the time dreaming. When she decides to eat, she picks up the chopsticks (if available) and start eating. When she is done, she will return the chopsticks. The problem arise if many philosophers decide to eat at the same time.

### 2. The chopstick

A chopstick is initially `available` and waits for a *`request`* message. When the request is received the chopstick's state changes to `gone` and only a *`return`* message can bring its state back to available. The state of the chopstick is represented by a **process**. A `quit` message terminates the process independently of which states is represented. The following *finite state machine* graph depicts the states of the chopstick: 
<p align="center"><img src=""></p>

The two states of the chopsticks are defined by the following two processes:

```erlang
available() ->
  receive
    {request, Ref, From} ->
	    From ! {granted, Ref},
	    gone(Ref);
    quit ->
      ok
  end.

gone(Ref) ->
  receive
    {return, Ref} ->
	    available();
	  quit ->
	    ok
  end.
```

The public API functions `start/0`, `request/3`, `return/2`, `quit/1` are used to simplify the transition between states and to hide the implementation of the processes.


### 3. The philosopher

The philosopher can be in three states: dreaming, waiting, eating. A philosopher in `dreaming` state may move to `waiting` after a certain amount of time. If both the *Left* and *Right* chopsticks are available they will be claimed and the philosopher moves to `eating` state. After spending some time in eating state, the philosopher returns the two chopsticks and moves to the dreaming state. This process repeats until the *Hungry* indicator reaches the value 0.

<p align="center"><img src=""></p>

```erlang
dreaming(0, _Left, _Right, Name, Ctrl) ->
  Ctrl ! done;
dreaming(Hungry, Left, Right, Name, Ctrl) ->
  delay(400),
  waiting(Hungry, Left, Right, Name, Ctrl).
```
  
```erlang 
waiting(Hungry, Left, Right, Name, Ctrl) ->
  Ref = make_ref(),
  case chopstick:request(Left, Ref, 1000) of
     ok ->
       delay(400),
       case chopstick:request(Right, Ref, 1000) of
          ok ->
            eating(Hungry, Left, Right, Name, Ctrl, Ref);
          no ->
            chopstick:return(Left, Ref),
            chopstick:return(Right, Ref),
            dreaming(Hungry, Left, Right, Name, Ctrl)
       end;
    no ->
      chopstick:return(Left, Ref),
      dreaming(Hungry, Left, Right, Name, Ctrl)
  end.
```

```erlang 
eating(Hungry, Left, Right, Name, Ctrl, Ref) ->
  delay(400),
  chopstick:return(Left, Ref),
  chopstick:return(Right, Ref),
  dreaming(Hungry - 1, Left, Right, Name, Ctrl).
```

The waiting function uses *1 sec* timeouts to avoid **deadlocks** when requesting chopsticks. Consequently we have to keep track of the requests in order to break the deadlocks (together with timeouts) and to avoid that two functions wait infinitely for a reciprocal change of state. The references `Ref` are generated with a built-in function `make_ref/0`.


### 4. The dinner

The dinner module simply provides 5 philosopher, 5 chopsticks and start the dinner.

```erlang
init() ->
  C1 = chopstick:start(),
  C2 = chopstick:start(),
  C3 = chopstick:start(),
  C4 = chopstick:start(),
  C5 = chopstick:start(),
  Ctrl = self(),
  philosopher:start(5, C1, C2, "Arendt", Ctrl, 1),
  philosopher:start(5, C2, C3, "Hypatia", Ctrl, 2),
  philosopher:start(5, C3, C4, "Simone", Ctrl, 3),
  philosopher:start(5, C4, C5, "Elizabeth", Ctrl, 4),
  philosopher:start(5, C5, C1, "Ayn", Ctrl, 5),
  wait(5, [C1, C2, C3, C4, C5]).
```