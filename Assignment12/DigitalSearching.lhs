
Exercise 12.3
Student names and numbers:
Thomas van Harskamp, s1007576
Jordy Aaldering, s1004292


> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeFamilyDependencies #-}

> module DigitalSearching
> where
> import Prelude hiding (lookup)
> import Data.Maybe


> data family Map key :: * -> *
>
> class (Ord key) => Key key where
>   empty  :: Map key val
>   insert :: key -> (Maybe val -> val) -> Map key val -> Map key val
>   lookup :: key -> Map key val -> Maybe val

> data instance Map () val = Empty | Single val
>
> instance Key () where
>   empty                 = Empty
>   insert _ f (Empty)    = Single (f Nothing)
>   insert _ f (Single v) = Single (f (Just v))
>   lookup _   (Empty)    = Nothing
>   lookup _   (Single v) = Just v


> -- Exercise 12.3.1
> data instance Map (Either key1 key2) val = EMap (Map key1 val) (Map key2 val)
>
> instance (Key key1, Key key2) => Key (Either key1 key2) where
>   empty                         = EMap empty empty
>   insert (Left v)  f (EMap l r) = EMap (insert v f l) r
>   insert (Right v) f (EMap l r) = EMap l (insert v f r)
>   lookup (Left v)    (EMap l _) = lookup v l
>   lookup (Right v)   (EMap _ r) = lookup v r


> -- Exercise 12.3.2
> data instance Map (key1, key2) val = T2Map (Map key1 (Map key2 val))
>
> instance (Key key1, Key key2) => Key (key1, key2) where
>   empty                     = T2Map empty
>   insert (l, r) f (T2Map e) = T2Map (insert l (insert r f . fromMaybe empty) e)
>   lookup (l, r)   (T2Map e) = case lookup l e of
> 					Nothing -> Nothing
> 					Just e  -> lookup r e


> -- Exercise 12.3.3
> type List elem = Either () (elem, [elem])
>
> toList :: [elem] -> List elem
> toList []     = Left  ()
> toList (a:as) = Right (a,as)

> data instance Map [key] val = TMap (Map (List key) val)
>
> instance (Key key) => Key [key] where
>   empty               = TMap empty
>   insert v f (TMap e) = TMap (insert (toList v) f e)
>   lookup v (TMap e)   = lookup (toList v) e

