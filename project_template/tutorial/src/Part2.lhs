> {-# LANGUAGE OverloadedStrings #-}
> module Part2 where

> import           Snap.Snaplet

> data Foo = Foo
> 
> data Bar = Bar
> 
> fooInit = makeSnaplet "foo" "Foo snaplet" Nothing $ do
>     return Foo
> 
> barInit h = makeSnaplet "bar" "Bar snaplet" Nothing $ do
>     return Bar
