Erlang session types
=====

An OTP application

Build
-----

    $ rebar3 compile

## Run
In order to change which file is being type checked you need to go to the file that is currently being type-checked and comment this line out:
```erlang
-compile([{parse_transform, session_check}]).
```
Then you can go to the file that you do want to type-check and make sure that the line above is not commented out.