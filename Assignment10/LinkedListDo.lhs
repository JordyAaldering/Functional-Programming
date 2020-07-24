
Exercise 10.4
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292

Multiple versions of LinkedList where shown in the practicum,
so that's why we have two versions of LinkedList.
(One version shown in the practicum did not work.)

> {-# LANGUAGE UnicodeSyntax #-}
> module LinkedList
> where
> import Data.IORef

> type ListRef elem = IORef (List elem)

> data List elem = Nil | Cons elem (ListRef elem)

> nil :: IO (ListRef elem)
> nil = newIORef Nil

> cons :: elem -> ListRef elem -> IO (ListRef elem)
> cons e l = newIORef $ Cons e l

> fromList :: [elem] -> IO (ListRef elem)
> fromList []     = nil
> fromList (x:xs) = do l <- fromList xs
>                      cons x l

> toList :: ListRef elem -> IO [elem]
> toList ref = do r <- readIORef ref
>                 case r of
>                         Nil -> pure []
>                         Cons e l -> (e:) <$> toList l

> foreach :: ListRef a -> (a -> IO b) -> IO (ListRef b)
> foreach ref f = do link <- readIORef ref
>                    case link of
>                           Nil -> nil
>                           Cons e l -> do {q <- f e; z <- foreach l f; cons q z}

