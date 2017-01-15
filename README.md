fun_test
=====

![Build status]
(https://api.travis-ci.org/Elzor/fun_test.svg?branch=master)

Test OTP application

*Build History:*

    https://travis-ci.org/Elzor/fun_test/ (erlang versions from 17 to 19.2)


*Production Config:*

    ./config/sys.config (after changing run `make prod`)


Build
-----

    $ make

Test
-----

    $ make test


Release
-----

    $ make prod

Run release
-----

    $ ./_build/prod/rel/fun_test/bin/fun_test console