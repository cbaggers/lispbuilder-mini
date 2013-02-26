lispbuilder-mini
================

This is a stripped down version of the fantastic lispbuilder package. 

Why?
----
I want to have a package with the bare minimum for making modern 
opengl games (or graphical demos). 
SDL is wonderful but it has loads of facilities for non-opengl 2D
rendering that I don't need.

So...Use lispbuilder and just ignore the bits you dont need.
------------------------------------------------------------
Aye, I could do that but quite often when you strip away all the
parts you don't need there appears the possibility of neater 
abstractions over what remains. 

I really don't want to cast any negative light on lispbuilder.
It's wonderful. It's just that when everything, even 2D, is 
being handled by opengl there are many practices and options
that can be hardwired and thus can be ingored by the user (the
person using this library).


**WARNING** 
Things are almost certainly broken in here!
Until things have solidified a bit this warning will remain.


*Forked from a Git conversion of lispbuilder's SVN repository.*
