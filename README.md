# purescript-monad-delayed-state

Implements a functionality to delay state modifications. Especially useful when `modify` is expensive, i.e. causes the DOM rerender.
It accumulates state updates for a specified amount of time and only commits it to the underlying state monad when there were no new modifies for a specified period.

The underlying monad needs to implement the `MonadBaseControl Aff` constraint.
