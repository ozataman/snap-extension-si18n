# Simple I18n For Snap Applications


## Introduction


## Status


## TODO

Here are some pieces we might be able to integrate into this package:

  * A ready-to-go "translate" Heist splice binding that gets the key from the
  	tag and splices in the translation.
  * extension-session integration that looks up the "locale" variable from the
  	session to determine the correct locale for the active user/session
  * A function that returns a "translator" inside the Snap monad, which itself
  	is a function that accepts a key and possibly a set of substitution tokens
  	and returns the translated value. This function can be passed to pure
  	view/template constructors.


These ideas are being tested and will be integrated if it turns out they are
useful. 

