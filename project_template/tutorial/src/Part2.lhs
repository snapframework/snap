> {-# LANGUAGE OverloadedStrings #-}
> module Part2 where

> import           Snap.Snaplet

> data Foo = Foo
> 
> data Bar = Bar
> 
> fooInit :: SnapletInit b Foo
> fooInit = makeSnaplet "foo" "Foo snaplet" Nothing $ do
>     return Foo
> 
> barInit :: SnapletLens b Foo -> SnapletInit b Bar
> barInit h = makeSnaplet "bar" "Bar snaplet" Nothing $ do
>     return Bar
