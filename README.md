another snap example application
================================

A web application based on the Snap framework to demonstrate basic
authentication functionality. This example shows the following technologies
working together:

* [snap framework](http://hackage.haskell.org/package/snap)
* [heist templating system](http://hackage.haskell.org/package/heist)
* [snap authentication library](http://hackage.haskell.org/packages/archive/snap/0.8.1/doc/html/Snap-Snaplet-Auth.html)
* [hdbc database connector library](http://hackage.haskell.org/package/HDBC)
* [composition of libraries via snaplets](http://hackage.haskell.org/packages/archive/snap/0.8.1/doc/html/Snap-Snaplet.html)
* [digestive functors form library](http://hackage.haskell.org/package/digestive-functors)
* [twitter's bootstrap toolkit](http://twitter.github.com/bootstrap/)

The code has been separated into several modules how this can be found in
(more or less) conventional snap programs. Additionally all the code of those
files has also been integrated into a single file that can be run standalone.
This should make it easier to understand the code without jumping from one
module (file) to another.

A CSS prototyping toolkit is used here to add a bit of "optic sugar" but this
is not required by the program to work correctly. Thus a minimal version would
be the standalone.hs file plus the snaplet folder which contains the necessary
heist templates.

Note that this project is still a first version and offers a lot of potential for
improvements. So feel free to fork and modify the code as you think it can
work best or just write suggestions to me via Hannes\_E@gmx.de (or to hannes\_
at #snapframework.)

More information as well as a general introduction can be found here
[http://snapframework.com](http://snapframework.com).


installation (optional)
-----------------------

    cabal install

run
---

    runhaskell standalone

or (after install)

    digestive-functors-snap-auth-example

todo
----

* store the firstname and the lastname of the registration
* let users choose a password
* add email functionality (see housetab project)
* ~~replace the json storage backend with hdbc or redis store~~
* make the Util.Form library independent from the application
* add development mode like in a default initialized snap application
* create a full working web application based on this example ;)
