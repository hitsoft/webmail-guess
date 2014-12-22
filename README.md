webmail-guess
=============

Component tries to guess the webmail service by specified email address.

Usage examples:

val guess = GuessWebmail(GuessWebmail.servers.all)
guess.domainExists("test@mail.ru")