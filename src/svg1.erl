%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(svg1).
-export([start/1]).

start(Browser) ->
    Browser ! [{ cmd , add_canvas },
            { tag , svg },
            { width , 800 },
            { height , 600 }],
    running(Browser, 10, 10).

running(Browser, X, Y) ->
    receive
	{Browser, {struct,[{clicked , <<"draw rectangle">>}]}} -> 
	    Browser ! [{cmd , add_svg_thing},
	            { type , rect},
		        {rx , 3},
		        {ry , 3},
		        {x , X},
		        {y , Y},
		        {width , 100},
		        {height , 50},
		        {stroke , blue},
		        {'stroke-width' , 2},
                {fill ,  red}],
	    running(Browser, X+10, Y+10)
    after 1000 ->
 	    Browser ! [{cmd , add_svg_thing},
	            { type , circle},
		        {cx , X},
		        {cy , Y},
		        {r, 2},
		        {stroke , black},
		        {'stroke-width' , 2},
                {fill ,  blue}],
                X = if 
                    X > 800 -> 10;
                    true -> X
                end,
                Y = if 
                    Y > 600 -> 10;
                    true -> Y
                end,
	    running(Browser, X+2, Y+2)       
        %% <circle cx="50" cy="50" r="40" stroke="black" stroke-width="3" fill="red" />
    end.
