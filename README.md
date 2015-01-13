# Coverbot

It's a bot that assumes control of a Trello account with a configurable API key
and token, and it automatically adds a random cover to a card when assigned or
mentioned.

It's also written in Haskell, and it's also the first non-trivial thing I've
written in Haskell. It's bad.

## Things that work

* Iterates all boards you're assigned to
* Keeps track of the last action scanned per board, and only processes new
  actions
* Persists these action tail markers to disk
* When added to card, adds a random cover and removes self
* When mentioned on card, takes the message (or card name if message is blank)
  and adds a random cover

That's like, feature completion right there.

## Things that should be better

* Development. Commenting out chunks of code so that it runs once and doesn't
  persist the tail marker is a little frustrating when trying to test.
* Error handling parsing json responses. I abuse `fromJust` all over the place.
* Error handling with non-200 status codes from Trello. Especially when it comes
  to advancing the action tail marker.
* Parsing the jpg.to response. Yeah, it's html, but man. I'm dropping the first
  75 characters right now and taking everything up to the first double quote.
  That's first-class clown town.
* Modularity. I can split a lot of logic into separate files, even.
* Doing things right. The action triggers smell pretty bad.
* Better model representation. `Action ObjectId ActionType ActionData` is not a
  good way to represent an action. One would say it's not even close. I think
  the answers is lenses somewhere.
* A full Haskell Trello client. Or a part of a Haskell Trello client. Something
  like that.
* Pull the majority of the core logic into a package. Let the world write their
  own action-tailing bots.
* Tests. I think those are a thing.
* Concurrency? To some extent? When testing it, it was running a lot slower than
  I would have liked. The requests seemed like the likely reason.
* Learning! More learning.
